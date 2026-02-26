(in-package :maiko-lisp.vm)

;; Comparison operations
;; eq, eql, lessp, greaterp, leq, geq, equal, numequal, igreaterp

;;; ===========================================================================
;; POINTER COMPARISON
;;; ===========================================================================

(defop eq :hexcode #x3A :instruction-length 1
  "EQ: Test if two values are EQ (pointer equality).
Pops B and A, pushes T if same pointer, NIL otherwise."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (eql a b) 1 0))))

(defop eql :hexcode #x3B :instruction-length 1
  "EQL: Test if two values are EQL (numeric equality for numbers).
Pops B and A, pushes T if equal, NIL otherwise."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (= a b) 1 0))))

;;; ===========================================================================
;; NUMERIC COMPARISON
;;; ===========================================================================

(defop lessp :hexcode #x3C :instruction-length 1
  "LESSP: Test if A < B (signed comparison).
Pops B and A, pushes T if A < B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (< a-signed b-signed) 1 0)))))

(defop greaterp :hexcode #x3D :instruction-length 1
  "GREATERP: Test if A > B (signed comparison).
Pops B and A, pushes T if A > B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (> a-signed b-signed) 1 0)))))

(defop leq :hexcode #x3E :instruction-length 1
  "LEQ: Test if A <= B (signed comparison).
Pops B and A, pushes T if A <= B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (<= a-signed b-signed) 1 0)))))

(defop geq :hexcode #x3F :instruction-length 1
  "GEQ: Test if A >= B (signed comparison).
Pops B and A, pushes T if A >= B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (>= a-signed b-signed) 1 0)))))

;;; ===========================================================================
;; STRUCTURAL COMPARISON
;;; ===========================================================================

(defop equal :hexcode #x3D :instruction-length 1
  "EQUAL: Deep structural equality test.
Pops B and A, pushes T if structurally equal."
  :aliases (equal-alt)
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (equal a b) 1 0))))

(defop numequal :hexcode #x3D :instruction-length 1
  "NUMEQUAL: Numeric equality test.
Pops B and A, pushes T if numerically equal."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (= a b) 1 0))))

;;; ===========================================================================
;; INTEGER COMPARISON
;;; ===========================================================================

(defop igreaterp :hexcode #xF1 :instruction-length 1
  "IGREATERP: Integer greater-than comparison.
Pops B and A, pushes T if A > B (signed)."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (> a-signed b-signed) 1 0)))))
