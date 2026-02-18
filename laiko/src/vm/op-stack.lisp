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

(defop nil #x69 1
  "NIL: Push NIL (0) onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 0))

(defop t #x6A 1
  "T: Push T (the symbol T, value 1) onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 1))

(defop const-0 #x6B 1
  "CONST_0: Push the small positive integer 0 onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 0))

(defop const-1 #x6C 1
  "CONST_1: Push the small positive integer 1 onto the stack."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (vm-push vm 1))

;;; ===========================================================================
;;; STACK OPERATIONS
;;; ===========================================================================

(defop pop #xBF 1
  "POP: Pop and discard the top of stack."
  :operands nil
  :stack-effect (:pop 1)
  :category :stack-operations
  :side-effects nil
  (vm-pop vm)
  nil)

(defop copy #x64 1
  "COPY: Duplicate the top of stack."
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

(defop-undefined unused-6B #x6B
  :reason "Opcode 0x6B not used"
  :category :constants)
