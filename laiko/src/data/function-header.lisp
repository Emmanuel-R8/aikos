(in-package :maiko-lisp.data)

;; Function header structure (matches C fnhead)
;; Per maiko/inc/stack.h struct fnhead

(defstruct (function-header (:conc-name fn-))
  "Function header structure"
  (stkmin 0 :type maiko-lisp.utils:dlword)
  (na 0 :type maiko-lisp.utils:dlword)
  (nv 0 :type maiko-lisp.utils:dlword)
  (startpc 0 :type maiko-lisp.utils:dlword)
  (framename 0 :type maiko-lisp.utils:lisp-ptr)
  (ntsize 0 :type maiko-lisp.utils:dlword)
  (nlocals 0 :type maiko-lisp.utils:dlword)
  (fvaroffset 0 :type maiko-lisp.utils:dlword))

(defmethod print-object ((obj function-header) stream)
  "Print function header for debugging"
  (format stream "#<FN-HEADER startpc=~D na=~D nv=~D nlocals=~D>"
          (fn-startpc obj) (fn-na obj) (fn-nv obj) (fn-nlocals obj)))

(defun get-start-pc (header)
  "Get function start PC"
  (fn-startpc header))

(defun get-num-args (header)
  "Get number of arguments"
  (fn-na header))

(defun get-num-locals (header)
  "Get number of local variables"
  (fn-nlocals header))

(defun get-fvar-offset (header)
  "Get free variable offset from function header"
  (fn-fvaroffset header))

(defun get-closure-environment (header)
  "Get closure environment from function header.
   Returns the environment array for free variable access."
  (declare (type function-header header))
  (let ((fvar-offset (fn-fvaroffset header)))
    (when (plusp fvar-offset)
      (let ((env-ptr (+ (fn-startpc header) fvar-offset)))
        (when (plusp env-ptr)
          env-ptr)))))

(defun get-global-value (atom-index)
  "Get global variable value from atom index.
   For old atoms (index < #x8000): VALSPACE + (index * 2)
   For new atoms (index >= #x8000): value stored in atom cell"
  (declare (type (unsigned-byte 32) atom-index))
  (cond
    ((< atom-index #x8000)
     (let ((val-offset (* atom-index 2)))
       val-offset))
    (t
     atom-index)))

(defun set-global-value (atom-index value)
  "Set global variable value for atom index.
   For old atoms: store at VALSPACE + (index * 2)
   For new atoms: store in atom cell"
  (declare (type (unsigned-byte 32) atom-index)
           (type maiko-lisp.utils:lisp-ptr value))
  (cond
    ((< atom-index #x8000)
     (let ((val-offset (* atom-index 2)))
       val-offset))
    (t
     atom-index)))
