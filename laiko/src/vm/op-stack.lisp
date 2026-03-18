(in-package :laiko.vm)

;; Stack and constant operations
;; nil, t, push, pop, constants
;;
;; CRITICAL: Stack operations use virtual memory directly!
;; Per C/Zig, the stack is within virtual_memory at Stackspace offset.
;; Use vm-push/vm-pop for virtual memory stack operations.

;;; ===========================================================================
;;; CONSTANTS
;;; ===========================================================================

;; Constants (NIL, T, 0, 1) are defined in laiko/src/vm/op-const.lisp
;; to avoid symbol collision with CL:NIL and CL:T.


;;; ===========================================================================
;;; STACK OPERATIONS
;;; ===========================================================================

(defop pop-op :hexcode #xBF :instruction-length 1
  "POP: Pop and discard the top of stack."
  :operands nil
  :stack-effect (:pop 1)
  :category :stack-operations
  :side-effects nil
  (vm-pop vm)
  nil)

(defop copy :hexcode #x64 :instruction-length 1
  "Duplicate the top of stack (COPY)."
  :operands nil
  :stack-effect (:pop 1 :push 2)
  :category :stack-operations
  :side-effects nil
  (let ((tos (vm-peek vm)))
    (vm-push vm tos)))

;;; ===========================================================================
;;; UNDEFINED OPCODES IN THIS RANGE
;;; ===========================================================================

;; Opcodes 0x6D-0x6F are SIC, SNIC, SICX, GCONST - defined elsewhere
;; Mark any truly unused opcodes in this range
