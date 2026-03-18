(in-package :laiko.vm)

;; dispatch loop
;; Per rewrite documentation vm-core/execution-model.md
;;
;; This file provides the main dispatch loop that fetches, decodes, and
;; executes bytecode instructions. It uses the arrays populated by DEFOP
;; in op-macros.lisp for O(1) instruction length and handler lookup.

;;;============================================================================
;;; Special Variables for Instruction Stream Access
;;;============================================================================

(defvar *current-code* nil
  "Bound to the current bytecode array during dispatch.
   Allows handlers to read operands from the instruction stream.")

(defvar *current-base-pc* 0
  "Bound to the base PC offset during dispatch.
   Used for trace reporting.")

(defun fetch-instruction-byte (pc code)
  "Fetch instruction byte at PC.
Returns the opcode byte (0-#xFF) or 0 if past end of code."
  (declare (type laiko.utils:lisp-ptr pc)
           (type (simple-array laiko.utils:bytecode (*)) code))
  (if (< pc (length code))
      (aref code pc)
      0))

(defun fetch-operands (pc code length)
  "Fetch operands for instruction starting at PC.
Returns list of operand bytes (excluding opcode byte).
LENGTH is total instruction length including opcode."
  (declare (type laiko.utils:lisp-ptr pc)
           (type (simple-array laiko.utils:bytecode (*)) code)
           (type (integer 1 *) length))
  (let ((operand-count (1- length)))
    (if (<= (+ pc length) (length code))
        (loop for i from 1 to operand-count
              collect (aref code (+ pc i)))
        nil)))

(defun decode-opcode (byte)
  "Decode opcode from byte. Returns opcode value (identity for now)."
  (declare (type laiko.utils:bytecode byte))
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
           (type (simple-array laiko.utils:bytecode (*)) code)
           (type laiko.utils:lisp-ptr base-pc))

  ;; Ensure undefined opcode handlers are initialized
  (initialize-undefined-opcode-handlers)

  ;; Initialize tracing
  (trace-begin)

  ;; Reset PC to 0 for array indexing (code array starts at index 0)
  (setf (vm-pc vm) 0)

  ;; Bind special variables for handler access to instruction stream
  (let ((*current-code* code)
         (*current-base-pc* base-pc)
         (pc 0)
         (step-count 0))
    (declare (special *current-code* *current-base-pc*))
    (loop while (and (< pc (length *current-code*))
                     (or (zerop *max-trace-steps*)
                          (< step-count *max-trace-steps*)))
          do
             (incf step-count)

             ;; Check interrupts before execution
             (when (check-interrupts vm)
               (handle-pending-interrupts vm))

             ;; Fetch instruction
              (let* ((opcode-byte (fetch-instruction-byte pc *current-code*))
                     (instruction-len (get-instruction-length opcode-byte))
                     (handler-fn (get-opcode-handler-function opcode-byte))
                     (opcode-name (get-opcode-name opcode-byte)))

               ;; Log trace before execution
               (when (trace-enabled-p)
                  (let ((operands (fetch-operands pc *current-code* instruction-len)))
                    (trace-log vm (+ *current-base-pc* pc) opcode-byte operands
                               :instruction-name opcode-name)))

               ;; Initialize VM-PC to point to operands (PC + 1)
               ;; Handlers that read operands will increment this further
               (setf (vm-pc vm) (+ pc 1))

               ;; Execute handler
               (handler-case
                   (if handler-fn
                       (funcall handler-fn vm)
                       (error 'laiko.utils:vm-error
                              :message (format nil "No handler for opcode 0x~2,'0X" opcode-byte)))
                 (laiko.utils:vm-error (err)
                   (error err))
                 (error (err)
                   (error 'laiko.utils:vm-error
                          :message (format nil "Error in opcode 0x~2,'0X: ~A" opcode-byte err))))

                ;; Update PC
                ;; If handler didn't modify VM-PC (beyond the initial +1), advance by instruction-len
                ;; If handler modified VM-PC (jump, or read operands), use VM-PC
                (let ((new-pc (vm-pc vm)))
                  (cond
                    ((= new-pc (+ pc 1))
                     ;; Handler didn't touch VM-PC (e.g. POP, ADD)
                     (setf pc (+ pc instruction-len)))
                    ((>= new-pc (length *current-code*))
                     ;; Handler switched to a different absolute PC/code region.
                     (setf *current-base-pc* new-pc
                           *current-code* (laiko.data:extract-bytecode-from-vm
                                           (vm-virtual-memory vm)
                                           new-pc)
                           pc 0
                           (vm-pc vm) 0))
                    (t
                     ;; Handler modified VM-PC within the current extracted block.
                     (setf pc new-pc)))))

             ;; Check interrupts after execution
             (when (check-interrupts vm)
               (handle-pending-interrupts vm)))))

;;; ===========================================================================
;;; INSTRUCTION STREAM READING HELPERS
;;; ===========================================================================

(defun read-pc-8 (vm)
  "Read 8-bit value from PC and advance VM-PC by 1.
   Reads from *current-code* at index (vm-pc vm)."
  (declare (type vm vm))
  (prog1 (fetch-instruction-byte (vm-pc vm) *current-code*)
    (incf (vm-pc vm) 1)))

(defun read-pc-8-signed (vm)
  "Read signed 8-bit value from PC and advance VM-PC by 1."
  (declare (type vm vm))
  (let ((unsigned (read-pc-8 vm)))
    (if (>= unsigned #x80) (- unsigned #x100) unsigned)))

(defun read-pc-16-be (vm)
  "Read 16-bit big-endian value from PC and advance VM-PC by 2."
  (declare (type vm vm))
  (let* ((pc (vm-pc vm))
         (b1 (fetch-instruction-byte pc *current-code*))
         (b2 (fetch-instruction-byte (1+ pc) *current-code*)))
    (incf (vm-pc vm) 2)
    (logior (ash b1 8) b2)))

(defun read-pc-16-be-signed (vm)
  "Read signed 16-bit big-endian value from PC and advance VM-PC by 2."
  (declare (type vm vm))
  (let ((unsigned (read-pc-16-be vm)))
    (if (>= unsigned #x8000) (- unsigned #x10000) unsigned)))

(defun read-pc-32-be (vm)
  "Read 32-bit big-endian value from PC and advance VM-PC by 4."
  (declare (type vm vm))
  (let* ((pc (vm-pc vm))
         (b1 (fetch-instruction-byte pc *current-code*))
         (b2 (fetch-instruction-byte (+ pc 1) *current-code*))
         (b3 (fetch-instruction-byte (+ pc 2) *current-code*))
         (b4 (fetch-instruction-byte (+ pc 3) *current-code*)))
    (incf (vm-pc vm) 4)
    (logior (ash b1 24)
            (ash b2 16)
            (ash b3 8)
            b4)))


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
