(in-package :laiko.vm)

;; Bitwise and logical operations
;; logand, logior, logxor, lognot, lsh, llsh1, llsh8, lrsh1, lrsh8
;; logor2, logand2, logxor2, hiloc, loloc

;;; ===========================================================================
;; BITWISE OPERATIONS (Binary)
;;; ===========================================================================

;; LOGAND (0xE0) removed - duplicate of LOGAND2 (0xE5)
;; LOGIOR (0xE1) removed - duplicate of LOGOR2 (0xE4)
;; LOGXOR (0xE2) removed - duplicate of LOGXOR2 (0xE6)

;;; ===========================================================================
;; BITWISE OPERATIONS (Unary)
;;; ===========================================================================

;; LOGNOT (0xE3) removed - not standard opcode

;;; ===========================================================================
;; SHIFT OPERATIONS
;;; ===========================================================================

(defop lsh :hexcode #xE7 :instruction-length 1
  "LSH: Logical shift.
Pops shift amount and value, shifts value left (positive) or right (negative).
Shift amount is signed 32-bit."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((shift-amount (pop-stack vm))
        (value (pop-stack vm)))
    (let ((shift-signed (if (>= shift-amount #x80000000)
                            (- shift-amount #x100000000)
                            shift-amount)))
      (if (minusp shift-signed)
          (push-stack vm (ash value shift-signed))
          (push-stack vm (logand (ash value shift-signed) #xFFFFFFFF))))))

(defop llsh1 :hexcode #xE0 :instruction-length 1
  "LLSH1: Logical left shift by 1.
Shifts TOS left by 1 bit, masks to 32 bits."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (ash value 1) #xFFFFFFFF))))

(defop llsh8 :hexcode #xE1 :instruction-length 1
  "LLSH8: Logical left shift by 8.
Shifts TOS left by 8 bits, masks to 32 bits."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (ash value 8) #xFFFFFFFF))))

(defop lrsh1 :hexcode #xE2 :instruction-length 1
  "LRSH1: Logical right shift by 1.
Shifts TOS right by 1 bit."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -1))))

(defop lrsh8 :hexcode #xE3 :instruction-length 1
  "LRSH8: Logical right shift by 8.
Shifts TOS right by 8 bits."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -8))))

;;; ===========================================================================
;; ALTERNATE NAMES
;;; ===========================================================================

(defop logor2 :hexcode #xE4 :instruction-length 1
  "LOGOR2: Bitwise OR (alternate for LOGIOR).
Pops B and A, pushes A OR B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (push-stack vm (logior a b))))

(defop logand2 :hexcode #xE5 :instruction-length 1
  "LOGAND2: Bitwise AND (alternate for LOGAND).
Pops B and A, pushes A AND B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (push-stack vm (logand a b))))

(defop logxor2 :hexcode #xE6 :instruction-length 1
  "LOGXOR2: Bitwise XOR (alternate for LOGXOR).
Pops B and A, pushes A XOR B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (push-stack vm (logxor a b))))

;;; ===========================================================================
;; ADDRESS EXTRACTION
;;; ===========================================================================

(defop vag2 :hexcode #xD1 :instruction-length 1
  "VAG2: Combine GET_TOS_1 as high 16 bits with TOS as low 16 bits.
Per Maiko N_OP_VAG2, this pops one in-memory stack cell and leaves the
combined 32-bit Lisp pointer in cached TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :bit-operations
  :side-effects nil
  (let* ((sp (vm-stack-ptr-offset vm))
         (high-word (logand (vm-read-lispptr vm (- sp 4)) #xFFFF))
         (result (logior (ash high-word 16)
                         (logand (vm-tos vm) #xFFFF))))
    (decf (vm-stack-ptr-offset vm) 4)
    (setf (vm-top-of-stack vm) result)))

(defop hiloc :hexcode #xD2 :instruction-length 1
  "HILOC: Extract high 16 bits.
Replaces TOS with upper 16 bits."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -16))))

(defop loloc :hexcode #xD3 :instruction-length 1
  "LOLOC: Extract low 16 bits.
Replaces TOS with lower 16 bits (masked)."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :bit-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand value #xFFFF))))
