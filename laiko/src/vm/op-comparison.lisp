(in-package :maiko-lisp.vm)

;; Comparison operations
;; eq, eql, lessp, greaterp, leq, geq, equal, numequal

(defun handle-eq (vm)
  "EQ: Test if two values are eq"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (eql a b) 1 0))))

(defun handle-eql (vm)
  "EQL: Test if two values are eql (numeric equality)"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (= a b) 1 0))))

(defun handle-lessp (vm)
  "LESSP: Test if a < b"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (< a-signed b-signed) 1 0)))))

(defun handle-greaterp (vm)
  "GREATERP: Test if a > b"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (> a-signed b-signed) 1 0)))))

(defun handle-leq (vm)
  "LEQ: Test if a <= b"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (<= a-signed b-signed) 1 0)))))

(defun handle-geq (vm)
  "GEQ: Test if a >= b"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (>= a-signed b-signed) 1 0)))))

(defun handle-equal (vm)
  "EQUAL: Deep equality test"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (equal a b) 1 0))))

(defun handle-numequal (vm)
  "NUMEQUAL: Numeric equality"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (= a b) 1 0))))

(defun handle-igreaterp (vm)
  "IGREATERP: Integer greater than"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (> a-signed b-signed) 1 0)))))

(defun handle-eq-alt (vm)
  "EQ alternative"
  (declare (type vm vm))
  (handle-eq vm))

(defun handle-greaterp-alt (vm)
  "GREATERP alternative"
  (declare (type vm vm))
  (handle-greaterp vm))

(defun handle-equal-alt (vm)
  "EQUAL alternative"
  (declare (type vm vm))
  (handle-equal vm))
