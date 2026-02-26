(in-package :maiko-lisp.vm)

;; Stack and constant operations
;; nil, t, push, pop, constants
;;
;; CRITICAL: Stack operations use virtual memory directly!
;; Per C/Zig, the stack is within virtual_memory at Stackspace offset.
;; Use vm-push/vm-pop for virtual memory stack operations.

;;; ===========================================================================
;;; CONSTANTS
;;; ===========================================================================


;;; Per maiko/inc/opcodes.h: opc_NIL=104(0x68), opc_T=105(0x69), opc_0=106(0x6A), opc_1=107(0x6B)

(defop nil :hexcode #x68 :instruction-length 1
  "NIL: Push NIL (0) onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 0))

(defop t :hexcode #x69 :instruction-length 1
  "T: Push T (the symbol T, value 1) onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 1))

(defop const-0 :hexcode #x6A :instruction-length 1
  "CONST_0 (opc_0): Push the small positive integer 0 onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 0))

(defop const-1 :hexcode #x6B :instruction-length 1
  "CONST_1 (opc_1): Push the small positive integer 1 onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 1))

;;; ===========================================================================
;;; STACK OPERATIONS
;;; ===========================================================================

(defop pop :hexcode #xBF :instruction-length 1
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
