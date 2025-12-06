(in-package :maiko-lisp.data)

;; Cons cell structure (matches C ConsCell)
;; Per data-model.md

(defstruct (cons-cell (:conc-name cons-))
  "Cons cell structure"
  (car-field 0 :type maiko-lisp.utils:lisp-ptr)
  (cdr-code 0 :type (unsigned-byte 8)))

;; CDR coding constants
(defconstant +cdr-nil+ 8 "NEWCDRCODING: CDR is NIL")
(defconstant +cdr-indirect+ 0 "CDR stored indirectly")
(defconstant +cdr-onpage-min+ 8 "Same page encoding start")
(defconstant +cdr-onpage-max+ 15 "Same page encoding end")

(defun decode-cdr (cons-cell cell-address)
  "Decode CDR from cons cell"
  (declare (type cons-cell cons-cell)
           (type maiko-lisp.utils:lisp-ptr cell-address))
  (let ((cdr-code (cons-cdr-code cons-cell)))
    (cond
      ((= cdr-code +cdr-nil+)
       maiko-lisp.utils:+nil-ptr+)
      ((= cdr-code +cdr-indirect+)
       ;; Indirect encoding: CAR points to indirect cell
       ;; TODO: Implement indirect decoding
       (cons-car-field cons-cell))
      ((<= +cdr-onpage-min+ cdr-code +cdr-onpage-max+)
       ;; NEWCDRCODING: Same page encoding (3-bit offset)
       (let ((offset (ash (logand cdr-code 7) 1)))
         (+ cell-address offset)))
      (t
       ;; Different page encoding
       (let ((offset (ash cdr-code 1)))
         (+ cell-address offset))))))

(defun get-car (cons-cell)
  "Get CAR value"
  (cons-car-field cons-cell))

(defun set-car (cons-cell value)
  "Set CAR value"
  (setf (cons-car-field cons-cell) value))

(defun set-cdr (cons-cell cell-address cdr-value)
  "Set CDR value with CDR coding"
  (declare (type cons-cell cons-cell)
           (type maiko-lisp.utils:lisp-ptr cell-address cdr-value))
  (cond
    ((zerop cdr-value)
     (setf (cons-cdr-code cons-cell) +cdr-nil+))
    ((< (abs (- cdr-value cell-address)) 16)
     ;; Same page encoding
     (let ((offset (ash (- cdr-value cell-address) -1)))
       (if (and (>= offset 0) (<= offset 7))
           (setf (cons-cdr-code cons-cell) (logior +cdr-onpage-min+ offset))
           (setf (cons-cdr-code cons-cell) +cdr-indirect+))))
    (t
     ;; Use indirect encoding
     (setf (cons-cdr-code cons-cell) +cdr-indirect+))))