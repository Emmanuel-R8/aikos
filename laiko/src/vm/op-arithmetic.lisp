(in-package :maiko-lisp.vm)

;; Integer arithmetic operations
;; iplus2, idifference, itimes2, iquo, irem, iminus, idivide, imod

;;; ===========================================================================
;; BASIC ARITHMETIC
;;; ===========================================================================

(defop iplus2 #xD8 1
  "IPLUS2: Integer addition.
Pops B and A, pushes A+B.
Operands are 32-bit signed integers (two's complement)."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (+ a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defop idifference #xD9 1
  "IDIFFERENCE: Integer subtraction.
Pops B and A, pushes A-B.
Operands are 32-bit signed integers (two's complement)."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (- a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defop itimes2 #xDA 1
  "ITIMES2: Integer multiplication.
Pops B and A, pushes A*B.
Operands are 32-bit signed integers (two's complement)."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (* a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defop iquo #xDB 1
  "IQUO: Integer quotient (truncation toward zero).
Pops B and A, pushes truncate(A/B).
Signals error on division by zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (when (zerop b) (error 'maiko-lisp.utils:vm-arithmetic-error :message "Division by zero"))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (truncate a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defop irem #xDC 1
  "IREM: Integer remainder (sign follows dividend).
Pops B and A, pushes rem(A,B) where sign matches A.
Signals error on division by zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (when (zerop b) (error 'maiko-lisp.utils:vm-arithmetic-error :message "Division by zero"))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (rem a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

;;; ===========================================================================
;; UNARY ARITHMETIC
;;; ===========================================================================

(defop iminus #x9A 1
  "IMINUS: Integer negation.
Pops A, pushes -A.
Negates a 32-bit signed integer."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a)))
      (let ((result (- a-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

;;; ===========================================================================
;; COMBINED ARITHMETIC
;;; ===========================================================================

(defop idivide #x9B 1
  "IDIVIDE: Integer division with remainder.
Pops B and A, pushes remainder then quotient.
Returns both values like (values remainder quotient).
Signals error on division by zero."
  :operands nil
  :stack-effect (:pop 2 :push 2)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (when (zerop b) (error 'maiko-lisp.utils:vm-arithmetic-error :message "Division by zero"))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((quot (truncate a-signed b-signed))
            (rem-val (rem a-signed b-signed)))
        (push-stack vm (if (minusp rem-val)
                          (logand (+ rem-val #x100000000) #xFFFFFFFF)
                          (logand rem-val #xFFFFFFFF)))
        (let ((quot-unsigned (if (minusp quot)
                                 (logand (+ quot #x100000000) #xFFFFFFFF)
                                 (logand quot #xFFFFFFFF))))
          (push-stack vm quot-unsigned))))))

(defop imod #x9C 1
  "IMOD: Integer modulus (sign follows divisor).
Pops B and A, pushes mod(A,B) where sign matches B.
Signals error on modulo by zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (when (zerop b) (error 'maiko-lisp.utils:vm-arithmetic-error :message "Modulo by zero"))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (mod a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))
