(in-package :maiko-lisp.data)

;; Function header structure (matches C fnhead)
;; Per data-model.md

(defstruct (function-header (:conc-name fn-))
  "Function header structure"
  (stkmin 0 :type maiko-lisp.utils:dlword)
  (na 0 :type maiko-lisp.utils:dlword)
  (pv 0 :type maiko-lisp.utils:dlword)
  (startpc 0 :type maiko-lisp.utils:dlword)
  (framename 0 :type maiko-lisp.utils:lisp-ptr)
  (ntsize 0 :type maiko-lisp.utils:dlword)
  (nlocals 0 :type maiko-lisp.utils:dlword)
  (fvaroffset 0 :type maiko-lisp.utils:dlword))

(defun get-start-pc (header)
  "Get function start PC"
  (fn-startpc header))

(defun get-num-args (header)
  "Get number of arguments"
  (fn-na header))

(defun get-num-locals (header)
  "Get number of local variables"
  (fn-nlocals header))