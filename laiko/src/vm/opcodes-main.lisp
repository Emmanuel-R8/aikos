(in-package :maiko-lisp.vm)

;; Opcode dispatch table and handler registration
;; 
;; With the DEFOP macro, handlers are automatically registered when the
;; opcode definition files are loaded. This file now only needs to:
;;   1. Ensure undefined opcode handlers are installed
;;   2. Report coverage statistics

(defun initialize-opcode-handlers ()
  "Initialize all opcode handlers.
  
  With DEFOP, handlers are registered at load time when the opcode
  files (op-*.lisp) are loaded. This function:
  1. Fills in handlers for undefined opcodes (error signaling stubs)
  2. Reports coverage statistics
  
  Call this once after all opcode files are loaded.
"
  ;; Install handlers for undefined opcodes
  (initialize-undefined-opcode-handlers)
  
  ;; Report coverage
  (format t "~&Opcode coverage: ~A~%" (report-opcode-coverage)))

(defun dispatch-opcode (vm opcode &optional operands)
  "Dispatch an opcode to its handler.
  
  VM: The virtual machine instance.
  OPCODE: Opcode byte (0-255) or symbolic name (deprecated).
  OPERANDS: Optional list of operand bytes (deprecated - handlers read from PC).
  
  Note: This is a compatibility wrapper. The dispatch loop now uses
  get-opcode-handler-function for O(1) lookup.
"
  (let ((handler (etypecase opcode
                   ((unsigned-byte 8)
                    (aref *opcode-handlers-array* opcode))
                   (symbol
                    (gethash opcode *opcode-handlers*)))))
    (if handler
        (funcall handler vm)
        (error 'maiko-lisp.utils:vm-error
               :message (format nil "Unknown opcode: ~A" opcode)))))

;; Note: initialize-opcode-handlers is called in main.lisp after all
;; handler files are loaded.
