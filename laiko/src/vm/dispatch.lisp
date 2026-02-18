(in-package :maiko-lisp.vm)

;; dispatch loop
;; Per rewrite documentation vm-core/execution-model.md
;; 
;; This file provides the main dispatch loop that fetches, decodes, and
;; executes bytecode instructions. It uses the arrays populated by DEFOP
;; in op-macros.lisp for O(1) instruction length and handler lookup.

(defun fetch-instruction-byte (pc code)
  "Fetch instruction byte at PC.
Returns the opcode byte (0-255) or 0 if past end of code."
  (declare (type maiko-lisp.utils:lisp-ptr pc)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code))
  (if (< pc (length code))
      (aref code pc)
      0))

(defun fetch-operands (pc code length)
  "Fetch operands for instruction starting at PC.
Returns list of operand bytes (excluding opcode byte).
LENGTH is total instruction length including opcode."
  (declare (type maiko-lisp.utils:lisp-ptr pc)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code)
           (type (integer 1 *) length))
  (let ((operand-count (1- length)))
    (if (<= (+ pc length) (length code))
        (loop for i from 1 to operand-count
              collect (aref code (+ pc i)))
        nil)))

(defun decode-opcode (byte)
  "Decode opcode from byte. Returns opcode value (identity for now)."
  (declare (type maiko-lisp.utils:bytecode byte))
  byte)

(defun get-instruction-length (opcode)
  "Get instruction length for opcode.
Uses the array populated by DEFOP macros in op-macros.lisp.
Returns total instruction length in bytes (opcode + operands).
Defaults to 1 for undefined opcodes."
  (declare (type (unsigned-byte 8) opcode))
  (aref *instruction-lengths* opcode))

(defun get-opcode-handler-function (opcode)
  "Get handler function for opcode byte.
Uses the array populated by DEFOP macros.
Returns function or NIL if undefined."
  (declare (type (unsigned-byte 8) opcode))
  (aref *opcode-handlers-array* opcode))

(defun get-opcode-name (opcode)
  "Get symbolic name for opcode byte.
Uses the array populated by DEFOP macros.
Returns symbol or NIL if undefined."
  (declare (type (unsigned-byte 8) opcode))
  (aref *opcode-names* opcode))

(defun dispatch (vm code &optional (base-pc 0))
  "Main dispatch loop.
  
  VM: The virtual machine instance.
  CODE: Extracted bytecode array (starts at index 0).
  BASE-PC: Original PC address for trace reporting (default 0).
  
  The dispatch loop:
  1. Fetches opcode byte at current PC
  2. Looks up instruction length (O(1) array lookup)
  3. Looks up handler function (O(1) array lookup)
  4. Executes handler
  5. Advances PC by instruction length (unless handler modified PC)
  
  Handlers that need operands read them directly from the instruction
  stream using read-pc-* helpers, not via the operands list.
"
  (declare (type vm vm)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code)
           (type maiko-lisp.utils:lisp-ptr base-pc))
  
  ;; Ensure undefined opcode handlers are initialized
  (initialize-undefined-opcode-handlers)
  
  ;; Initialize tracing
  (trace-begin)
  
  ;; Reset PC to 0 for array indexing (code array starts at index 0)
  (setf (vm-pc vm) 0)
  
  (format t "DEBUG: Dispatch starting, base-PC=0x~X, code-length=~D, max-steps=~D~%"
          base-pc (length code) *max-trace-steps*)
  
  (let ((pc 0)
        (step-count 0))
    (loop while (and (< pc (length code))
                     (or (zerop *max-trace-steps*)
                         (< step-count *max-trace-steps*)))
          do
          (incf step-count)
          
          ;; Check interrupts before execution
          (when (check-interrupts vm)
            (handle-pending-interrupts vm))
          
          ;; Fetch instruction
          (let* ((opcode-byte (fetch-instruction-byte pc code))
                 (instruction-len (get-instruction-length opcode-byte))
                 (handler-fn (get-opcode-handler-function opcode-byte))
                 (opcode-name (get-opcode-name opcode-byte)))
            
            (format t "DEBUG: step=~D PC=0x~X opcode=0x~2,'0X len=~D name=~A~%"
                    step-count pc opcode-byte instruction-len opcode-name)
            
            ;; Log trace before execution
            (when *vm-trace-output*
              (let ((operands (fetch-operands pc code instruction-len)))
                (trace-log vm (+ base-pc pc) opcode-byte operands 
                           :instruction-name opcode-name)))
            
            ;; Execute handler
            (handler-case
                (if handler-fn
                    (funcall handler-fn vm)
                    (error 'maiko-lisp.utils:vm-error
                           :message (format nil "No handler for opcode 0x~2,'0X" opcode-byte)))
              (maiko-lisp.utils:vm-error (err)
                (error err))
              (error (err)
                (error 'maiko-lisp.utils:vm-error
                       :message (format nil "Error in opcode 0x~2,'0X: ~A" opcode-byte err))))
            
            ;; Update PC (unless handler modified it)
            (let ((current-pc (vm-pc vm)))
              (if (= current-pc pc)
                  ;; PC wasn't modified, advance normally
                  (progn
                    (setf pc (+ pc instruction-len))
                    (setf (vm-pc vm) pc))
                  ;; PC was modified by opcode (e.g., JUMP, RETURN)
                  (setf pc current-pc))))
          
          ;; Check interrupts after execution
          (when (check-interrupts vm)
            (handle-pending-interrupts vm)))))

(defun handle-pending-interrupts (vm)
  "Handle any pending interrupts."
  (let ((int-state (vm-interrupt-state vm)))
    (when int-state
      (cond
        ((int-storage-full int-state)
         (handle-interrupt vm :storage-full))
        ((int-stack-overflow int-state)
         (handle-interrupt vm :stack-overflow))
        ((int-io-interrupt int-state)
         (handle-interrupt vm :io))
        ((int-log-file-io int-state)
         (handle-interrupt vm :log-file-io))
        ((int-ether-interrupt int-state)
         (handle-interrupt vm :ether-interrupt))
        ((int-timer-interrupt int-state)
         (handle-interrupt vm :timer))))))

;;; ===========================================================================
;; COMPATIBILITY - Old API for gradual migration
;;; ===========================================================================

;; These functions maintain backward compatibility with existing handler
;; registration code during migration to DEFOP. Eventually these can be removed.

(defvar *byte-opcode-map* (make-hash-table :test 'eql)
  "DEPRECATED: Use *opcode-names* array instead. Maps opcode byte to name symbol.")

(defvar *opcode-handlers* (make-hash-table :test 'eq)
  "DEPRECATED: Use *opcode-handlers-array* instead. Maps name to handler function.")

(defun register-opcode-handler (opcode handler)
  "DEPRECATED: Use DEFOP macro instead. Register handler for opcode name."
  (setf (gethash opcode *opcode-handlers*) handler))

(defun get-opcode-handler (opcode)
  "DEPRECATED: Use get-opcode-handler-function instead."
  (gethash opcode *opcode-handlers*))

(defun initialize-byte-opcode-map ()
  "DEPRECATED: Arrays are initialized by DEFOP. This is now a no-op."
  ;; Arrays are populated by DEFOP at load time
  ;; This function exists for backward compatibility
  nil)
