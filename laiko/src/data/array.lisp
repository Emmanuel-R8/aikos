(in-package :laiko.data)

;; Array structures
;; Per data-model.md

(defstruct (array-header (:conc-name array-))
  "Array header structure"
  (type 0 :type (unsigned-byte 8))
  (fill-pointer 0 :type laiko.utils:lisp-ptr)
  (data-ptr 0 :type laiko.utils:lisp-ptr))

;; TODO: Implement array operations
