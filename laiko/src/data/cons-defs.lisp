(in-package :laiko.data)

;; Cons cell base definitions - separated to break circular dependency
;; with storage.lisp. Contains only type definitions and accessors.

(defstruct (cons-cell (:conc-name cons-))
  "Cons cell structure"
  (car-field 0 :type laiko.utils:lisp-ptr)
  (cdr-code 0 :type (unsigned-byte 8)))

;; CDR coding constants
(defconstant +cdr-nil+ 8 "NEWCDRCODING: CDR is NIL")
(defconstant +cdr-indirect+ 0 "CDR stored indirectly")
(defconstant +cdr-onpage-min+ 8 "Same page encoding start")
(defconstant +cdr-onpage-max+ 15 "Same page encoding end")

;; Accessor functions
(defun get-car (cons-cell)
  "Get CAR value"
  (cons-car-field cons-cell))

(defun set-car (cons-cell value)
  "Set CAR value"
  (setf (cons-car-field cons-cell) value))

(defun get-cdr-code (cons-cell)
  "Get raw CDR code"
  (cons-cdr-code cons-cell))

(defun set-cdr-code (cons-cell code)
  "Set raw CDR code"
  (setf (cons-cdr-code cons-cell) code))
