(in-package :laiko.vm)

;; Integer arithmetic operations
;; iplus2, idifference, itimes2, iquo, irem, iminus, idivide, imod

;;; ===========================================================================
;; BASIC ARITHMETIC
;;; ===========================================================================

(defop iplus2 :hexcode #xD8 :instruction-length 1
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

(defop idifference :hexcode #xD9 :instruction-length 1
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

(defop itimes2 :hexcode #xDA :instruction-length 1
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

(defop iquo :hexcode #xDB :instruction-length 1
  "IQUO: Integer quotient (truncation toward zero).
Pops B and A, pushes truncate(A/B).
Signals error on division by zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (when (zerop b) (error 'laiko.utils:vm-arithmetic-error :message "Division by zero"))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (let ((result (truncate a-signed b-signed)))
        (let ((result-unsigned (if (minusp result)
                                   (logand (+ result #x100000000) #xFFFFFFFF)
                                   (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defop irem :hexcode #xDC :instruction-length 1
  "IREM: Integer remainder (sign follows dividend).
Pops B and A, pushes rem(A,B) where sign matches A.
Signals error on division by zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (when (zerop b) (error 'laiko.utils:vm-arithmetic-error :message "Division by zero"))
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

;; IMINUS (0x9A) removed - duplicate/not standard opcode
;; IDIVIDE (0x9B) removed - covered by IQUO (0xDB) and IREM (0xDC)
;; IMOD (0x9C) removed - covered by IREM (0xDC)

