(in-package :laiko.vm)

;; Stack frame structure (matches C FX)
;; Per data-model.md

(defstruct (stack-frame (:conc-name sf-))
  "Stack frame structure"
  (next-block 0 :type laiko.utils:lisp-ptr)
  (link 0 :type laiko.utils:lisp-ptr)
  (fn-header 0 :type laiko.utils:lisp-ptr)
  (pc-offset 0 :type laiko.utils:dlword))

;; Binding marker constants
(defconstant +bind-marker-msb+ #x80000000
  "MSB set in upper word of binding marker")

(defconstant +unbound-marker+ #xFFFFFFFF
  "Marker for unbound PVAR slots")

;; VM state structure
;; CRITICAL: Stack pointers reference virtual memory directly, not a separate array!
;; Per zaiko/src/vm/stack.zig:69-71:
;;   stack_base: [*]DLword - pointer into virtual_memory
;;   stack_ptr: [*]DLword - current stack pointer
;;   stack_end: [*]DLword - end of stack area
;; The stack area is within virtual_memory at Stackspace offset.
(defstruct (vm (:conc-name vm-))
  "VM state structure"
  (current-frame nil :type (or null stack-frame))
  ;; Frame pointer offset into virtual memory (matches C's PVar calculation)
  (frame-pointer-offset 0 :type (unsigned-byte 32))
  ;; Stack pointers into virtual memory (byte offsets from Lisp_world base)
  ;; These replace the separate stack array approach
  (stack-base-offset 0 :type (unsigned-byte 32))  ; Byte offset of Stackspace
  (stack-ptr-offset 0 :type (unsigned-byte 32))  ; Byte offset of CurrentStackPTR
  (stack-end-offset 0 :type (unsigned-byte 32))  ; Byte offset of EndSTKP
  ;; Legacy stack array (deprecated - will be removed)
  (stack nil :type (or null (simple-array laiko.utils:lisp-ptr (*))))
  (stack-ptr 0 :type (integer 0 *))
  (stack-size 0 :type (integer 0 *))
  (pvar-base 0 :type (integer 0 *))
  (pvar-ptr 0 :type (integer 0 *))
  (pvar nil :type (simple-array laiko.utils:lisp-ptr (*)))
  (pvar-size 0 :type (integer 0 *))
  (storage nil :type (or null laiko.memory:storage))
  (virtual-memory nil :type (or null (simple-array (or null (simple-array (unsigned-byte 8) (*))) (*))))
  (fptovp nil :type (or null (simple-array (unsigned-byte 32) (*))))
  (gc nil :type (or null laiko.memory:gc))
  (interrupt-state nil :type (or null interrupt-state))
  (pc 0 :type laiko.utils:lisp-ptr)
  (return-pc nil :type (or null laiko.utils:lisp-ptr))
  (top-of-stack 0 :type laiko.utils:lisp-ptr)  ; Cached TOS value (per C)
  (registers nil :type (simple-array laiko.utils:lisp-ptr (*))))

(defun create-vm (stack-size &key (pvar-size #x100))
  "Initialize VM with given stack size and PVAR size.
   Creates a virtual memory space for the stack to support new VM stack operations."
  (declare (type (integer 1 *) stack-size pvar-size))
  (let* ((stack-mem (make-array stack-size
                                :element-type 'laiko.utils:lisp-ptr
                                :initial-element 0))
         (pvar-mem (make-array pvar-size
                               :element-type 'laiko.utils:lisp-ptr
                               :initial-element 0))
         (regs (make-array 4
                           :element-type 'laiko.utils:lisp-ptr
                           :initial-element 0))
         ;; Initialize virtual memory for stack
         ;; Stack size is in DLwords (2 bytes), so bytes = stack-size * 2
         (stack-bytes (* stack-size 2))
         (page-size 512)
         (num-pages (ceiling stack-bytes page-size))
         (vmem (make-array num-pages :initial-element nil)))
    
    ;; Allocate pages
    (loop for i from 0 below num-pages do
      (setf (aref vmem i) 
            (make-array page-size :element-type '(unsigned-byte 8) :initial-element 0)))

    (make-vm :stack stack-mem
             :stack-ptr 0
             :stack-size stack-size
             :pvar-base 0
             :pvar-ptr 0
             :pvar pvar-mem
             :pvar-size pvar-size
             :pc 0
             :return-pc nil
             :registers regs
             ;; New stack architecture initialization
             :virtual-memory vmem
             :stack-base-offset 0
             :stack-ptr-offset 0
             :stack-end-offset stack-bytes
             :top-of-stack 0)))

(defun allocate-stack-frame (vm size)
  "Allocate stack frame of given size (in DLwords)"
  (declare (type vm vm)
           (type (integer 1 *) size))
  (let ((stack-ptr (vm-stack-ptr-offset vm))
        (stack-end (vm-stack-end-offset vm))
        (byte-size (* size 2)))
    (when (> (+ stack-ptr byte-size) stack-end)
      (laiko.vm:set-interrupt-flag vm :stack-overflow)
      (error 'laiko.utils:stack-overflow
             :message (format nil "Stack frame allocation would overflow: need ~A, have ~A"
                              byte-size (- stack-end stack-ptr))))
    (let ((new-frame (make-stack-frame
                      :next-block 0
                      :link (if (vm-current-frame vm)
                                stack-ptr
                                0)
                      :fn-header 0
                      :pc-offset 0)))
      (setf (vm-stack-ptr-offset vm) (+ stack-ptr byte-size))
      (setf (vm-current-frame vm) new-frame)
      new-frame)))

(defun free-stack-frame (vm frame)
  "Free stack frame (restore previous frame)"
  (declare (type vm vm)
           (type stack-frame frame))
  (let ((link (sf-link frame)))
    ;; Restore stack pointer to the link (start of this frame)
    ;; In real implementation, this would involve more complex return logic
    (setf (vm-stack-ptr-offset vm) link)
    (if (zerop link)
        (setf (vm-current-frame vm) nil)
        ;; TODO: We should restore the previous frame struct here if we were tracking it
        (setf (vm-current-frame vm) nil))))

;; DEPRECATED: Old array-based stack functions
;; These now redirect to VM-based stack operations for consistency
;; The old array-based stack is no longer used - all operations go through virtual memory

(defun push-stack (vm value)
  "Push value onto stack. DEPRECATED - use vm-push instead.
   This redirects to vm-push for consistency with C/Zig implementations."
  (declare (type vm vm)
           (type laiko.utils:lisp-ptr value))
  (vm-push vm value))

(defun pop-stack (vm)
  "Pop value from stack. DEPRECATED - use vm-pop instead.
   This redirects to vm-pop for consistency with C/Zig implementations."
  (declare (type vm vm))
  (vm-pop vm))

(defun get-top-of-stack (vm)
  "Get top of stack without popping. DEPRECATED - use vm-tos instead.
   This redirects to vm-tos for consistency with C/Zig implementations."
  (declare (type vm vm))
  (vm-tos vm))

(defun set-top-of-stack (vm value)
  "Set top of stack. DEPRECATED - use vm-set-tos instead.
   This redirects to vm-set-tos for consistency with C/Zig implementations."
  (declare (type vm vm)
           (type laiko.utils:lisp-ptr value))
  (vm-set-tos vm value))

;;;============================================================================
;;; Virtual Memory Stack Operations
;;;============================================================================
;;;
;;; These operations use virtual memory directly, matching C/Zig behavior.
;;; Stack pointers are byte offsets into virtual_memory.
;;; Per zaiko/src/vm/stack.zig and maiko/inc/tos1defs.h

(defun vm-read-lispptr (vm byte-offset)
  "Read 32-bit LispPTR from virtual memory at byte offset.
   Reads Little Endian (consistent with swapped memory)."
  (declare (type vm vm)
           (type (unsigned-byte 32) byte-offset))
  (let ((vmem (vm-virtual-memory vm)))
    (unless vmem
      (return-from vm-read-lispptr 0))
    ;; No XOR 2 addressing for 32-bit access!
    ;; Access is naturally aligned or unaligned but sequential.
    (let* ((page-num (ash byte-offset -9))
           (page-offset (logand byte-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from vm-read-lispptr 0))
      (let ((page (aref vmem page-num)))
        (if (null page)
            0
            ;; Read Little Endian: byte[0] is LSB, byte[3] is MSB
            (logior (ash (aref page (+ page-offset 3)) 24)
                    (ash (aref page (+ page-offset 2)) 16)
                    (ash (aref page (1+ page-offset)) 8)
                    (aref page page-offset)))))))

(defun vm-read-byte (vm byte-offset)
  "Read 8-bit byte from virtual memory at byte offset."
  (declare (type vm vm)
           (type (unsigned-byte 32) byte-offset))
  (let ((vmem (vm-virtual-memory vm)))
    (unless vmem
      (return-from vm-read-byte 0))
    (let* ((page-num (ash byte-offset -9))
           (page-offset (logand byte-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from vm-read-byte 0))
      (let ((page (aref vmem page-num)))
        (if (null page)
            0
            (aref page page-offset))))))

(defun vm-write-byte (vm byte-offset value)
  "Write 8-bit byte to virtual memory at byte offset."
  (declare (type vm vm)
           (type (unsigned-byte 32) byte-offset)
           (type (unsigned-byte 8) value))
  (let ((vmem (vm-virtual-memory vm)))
    (unless vmem
      (return-from vm-write-byte nil))
    (let* ((page-num (ash byte-offset -9))
           (page-offset (logand byte-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from vm-write-byte nil))
      (let ((page (aref vmem page-num)))
        (when page
          (setf (aref page page-offset) value)))))
  value)

(defun vm-read-word (vm byte-offset)
  "Read 16-bit word from virtual memory at byte offset."
  (declare (type vm vm)
           (type (unsigned-byte 32) byte-offset))
  (let ((vmem (vm-virtual-memory vm)))
    (unless vmem
      (return-from vm-read-word 0))
    (let* ((page-num (ash byte-offset -9))
           (page-offset (logand byte-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from vm-read-word 0))
      (let ((page (aref vmem page-num)))
        (if (null page)
            0
            (if (< (1+ page-offset) 512)
                (logior (aref page page-offset)
                        (ash (aref page (1+ page-offset)) 8))
                ;; Handle page crossing (rare but possible)
                (logior (aref page page-offset)
                        (ash (vm-read-byte vm (1+ byte-offset)) 8))))))))

(defun vm-write-word (vm byte-offset value)
  "Write 16-bit word to virtual memory at byte offset."
  (declare (type vm vm)
           (type (unsigned-byte 32) byte-offset)
           (type (unsigned-byte 16) value))
  (let ((vmem (vm-virtual-memory vm)))
    (unless vmem
      (return-from vm-write-word nil))
    (let* ((page-num (ash byte-offset -9))
           (page-offset (logand byte-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from vm-write-word nil))
      (let ((page (aref vmem page-num)))
        (when page
          (if (< (1+ page-offset) 512)
              (setf (aref page page-offset) (logand value #xFF)
                    (aref page (1+ page-offset)) (logand (ash value -8) #xFF))
              ;; Handle page crossing
              (progn
                (setf (aref page page-offset) (logand value #xFF))
                (vm-write-byte vm (1+ byte-offset) (logand (ash value -8) #xFF))))))))
  value)

(defun vm-write-lispptr (vm byte-offset value)
  "Write 32-bit LispPTR to virtual memory at byte offset.
   Writes Little Endian (consistent with swapped memory)."
  (declare (type vm vm)
           (type (unsigned-byte 32) byte-offset)
           (type (unsigned-byte 32) value))
  (let ((vmem (vm-virtual-memory vm)))
    (unless vmem
      (return-from vm-write-lispptr nil))
    (let* ((page-num (ash byte-offset -9))
           (page-offset (logand byte-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from vm-write-lispptr nil))
      (let ((page (aref vmem page-num)))
        (when page
          ;; Write Little Endian: LSB at offset 0, MSB at offset 3
          (when (< (+ page-offset 3) #x200)
            (setf (aref page (+ page-offset 3)) (logand (ash value -24) #xFF)
                  (aref page (+ page-offset 2)) (logand (ash value -16) #xFF)
                  (aref page (1+ page-offset))  (logand (ash value -8) #xFF)
                  (aref page page-offset)       (logand value #xFF)))))))
  value)

(defun vm-push (vm value)
  "Push value onto stack using virtual memory.
   Stack grows UP (increasing byte offset).
   Per C: PSTKPTR += 2; *(PSTKPTR-2) = value
   Actually, with SP pointing to TOS:
   Inc SP; Write at SP."
  (declare (type vm vm)
           (type (unsigned-byte 32) value))
  (let ((sp (vm-stack-ptr-offset vm)))
    (incf sp 4)  ; Move stack pointer UP by 4 bytes (1 LispPTR)
    (vm-write-lispptr vm sp value)
    (setf (vm-stack-ptr-offset vm) sp)
    (setf (vm-top-of-stack vm) value)))

(defun vm-pop (vm)
  "Pop value from stack using virtual memory.
   Returns value and decrements stack pointer.
   Stack grows UP."
  (declare (type vm vm))
  (let ((sp (vm-stack-ptr-offset vm)))
    (let ((value (vm-read-lispptr vm sp)))
      (decf sp 4)  ; Move stack pointer DOWN by 4 bytes
      (setf (vm-stack-ptr-offset vm) sp)
      ;; Update cached TOS
      (setf (vm-top-of-stack vm) (vm-read-lispptr vm sp))
      value)))

(defun vm-tos (vm)
  "Get top-of-stack value from virtual memory.
   Per C: TopOfStack = *(CurrentStackPTR)"
  (declare (type vm vm))
  (vm-top-of-stack vm))

(defun vm-set-tos (vm value)
  "Set top-of-stack value in virtual memory.
   Per C: *CSTKPTRL = value; TopOfStack = value"
  (declare (type vm vm)
           (type (unsigned-byte 32) value))
  (let ((sp (vm-stack-ptr-offset vm)))
    (vm-write-lispptr vm sp value)
    (setf (vm-top-of-stack vm) value)))

(defun vm-peek (vm)
  "Peek at top-of-stack value without popping.
   Returns the value at the current stack pointer."
  (declare (type vm vm))
  (vm-read-lispptr vm (vm-stack-ptr-offset vm)))

(defun vm-poke (vm value)
  "Poke value onto top of stack without advancing stack pointer.
   Overwrites the current top value."
  (declare (type vm vm)
           (type (unsigned-byte 32) value))
  (let ((sp (vm-stack-ptr-offset vm)))
    (vm-write-lispptr vm sp value)
    (setf (vm-top-of-stack vm) value)))
