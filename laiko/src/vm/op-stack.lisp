(in-package :maiko-lisp.vm)

;; Stack and constant operations
;; nil, t, push, pop, constants
;;
;; CRITICAL: Stack operations use virtual memory directly!
;; Per C/Zig, the stack is within virtual_memory at Stackspace offset.
;; Use vm-push/vm-pop for virtual memory stack operations.

(defun handle-nil (vm)
  "NIL: Push NIL (0) onto stack"
  (declare (type vm vm))
  (vm-push vm 0))

(defun handle-t (vm)
  "T: Push T (1) onto stack"
  (declare (type vm vm))
  (vm-push vm 1))

(defun handle-pop (vm)
  "POP: Pop value from stack.
   Per C: POP macro pops and discards top of stack."
  (declare (type vm vm))
  (vm-pop vm)
  nil)

(defun handle-push (vm)
  "PUSH: Push value onto stack"
  (declare (type vm vm))
  (vm-push vm 0))

(defun handle-push-constant (vm value)
  "PUSH with constant value"
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (vm-push vm value))

(defun handle-const-0 (vm)
  "CONST_0: Push 0 onto stack"
  (declare (type vm vm))
  (vm-push vm 0))

(defun handle-const-1 (vm)
  "CONST_1: Push 1 onto stack"
  (declare (type vm vm))
  (vm-push vm 1))
