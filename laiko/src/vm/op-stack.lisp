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

(defop copy-n :hexcode #x3D :instruction-length 2
  "COPY_N: Copy the N-th element from the top of the stack.
   Operand N is a byte value.
   Maiko: PUSH(*(CSTKPTR - (((n) + 2) >> 1)))
   CSTKPTR points at TOS (already written).
   offset = (n+2)>>1: n=0 => 1 => TOS, n=2 => 2 => TOS-1, etc."
  :operands ((n :uint8 "Offset specifier"))
  :stack-effect (:push 1)
  :category :stack-operations
  :side-effects nil
  (let* ((n (read-pc-8 vm))
         ;; Maiko: PUSH(*(CSTKPTR - (((n) + 2) >> 1)))
         ;; CSTKPTR points at current TOS (already written slot).
         ;; offset = (n + 2) >> 1
         ;; n=0 => offset=1 => CSTKPTR-1 => TOS
         ;; n=2 => offset=2 => CSTKPTR-2 => TOS-1
         (offset (ash (+ n 2) -1))
         ;; vm-stack-ptr-offset points to current TOS slot.
         ;; TOS is at sp, TOS-1 is at sp-4, etc.
         ;; We want item at sp - (offset-1)*4
         (sp (vm-stack-ptr-offset vm))
         (addr (- sp (* (1- offset) 4)))
         (val (vm-read-lispptr vm addr)))
    (vm-push vm val)))

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
