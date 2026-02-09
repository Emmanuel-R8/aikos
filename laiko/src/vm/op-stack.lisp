(in-package :maiko-lisp.vm)

;; Stack and constant operations
;; nil, t, push, pop, constants

(defun handle-nil (vm)
  "NIL: Push NIL (0) onto stack"
  (declare (type vm vm))
  (push-stack vm 0))

(defun handle-t (vm)
  "T: Push T (1) onto stack"
  (declare (type vm vm))
  (push-stack vm 1))

(defun handle-pop (vm)
  "POP: Pop value from stack"
  (declare (type vm vm))
  (pop-stack vm)
  nil)

(defun handle-push (vm)
  "PUSH: Push value onto stack"
  (declare (type vm vm))
  (push-stack vm 0))

(defun handle-push-constant (vm value)
  "PUSH with constant value"
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (push-stack vm value))

(defun handle-const-0 (vm)
  "CONST_0: Push 0 onto stack"
  (declare (type vm vm))
  (push-stack vm 0))

(defun handle-const-1 (vm)
  "CONST_1: Push 1 onto stack"
  (declare (type vm vm))
  (push-stack vm 1))
