(in-package :laiko.vm)

;; Constant operations
;; aconst, sic, snic, sicx, gconst, makenumber, swap, nop, cl_equal

;;; ===========================================================================
;; ATOM CONSTANT
;;; ===========================================================================

(defop aconst :hexcode #x67 :instruction-length 5
  "ACONST: Push atom constant onto stack.
Reads 4-byte atom index from instruction stream on BIGVM/BIGATOMS.
Pushes the atom index as a Lisp pointer."
  :operands ((atom-index :uint32-be "Atom index (4 bytes, big-endian)"))
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (let ((atom-idx (read-pc-32-be vm)))
    (push-stack vm atom-idx)))

(defop nil-op :hexcode #x68 :instruction-length 1
  "NIL: Push NIL (0)."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (push-stack vm 0))

(defop t-op :hexcode #x69 :instruction-length 1
  "T: Push T (Atom T)."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (push-stack vm laiko.data:+atom-t-index+))

(defop zero :hexcode #x6A :instruction-length 1
  "ZERO: Push 0 (Small Integer)."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (push-stack vm #xE0000))

(defop one :hexcode #x6B :instruction-length 1
  "ONE: Push 1 (Small Integer)."
  :operands nil
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (push-stack vm #xE0001))

;;; ===========================================================================
;; SMALL INTEGER CONSTANTS
;;; ===========================================================================

(defop sic :hexcode #x6C :instruction-length 2
  "SIC: Small Integer Constant (positive).
Reads 1-byte value, pushes as SMALLPOSP.
Format: (value | #xE0000) where #xE0000 = S_POSITIVE."
  :operands ((value :uint8 "8-bit positive integer"))
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (let ((val (read-pc-8 vm)))
    (push-stack vm (logior #xE0000 val))))

(defop snic :hexcode #x6D :instruction-length 2
  "SNIC: Small Negative Integer Constant.
Reads 1-byte value, pushes as SMALLNEG.
Format: (value | #xFF00) for negative values."
  :operands ((value :uint8 "8-bit magnitude"))
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (let ((val (read-pc-8 vm)))
    (push-stack vm (logior #xFF00 val))))

(defop sicx :hexcode #x6E :instruction-length 3
  "SICX: Small Integer Constant Extended.
Reads 2-byte value, pushes as SMALLPOSP.
Format: (value | #xE0000) for larger positive integers."
  :operands ((value :uint16-be "16-bit positive integer"))
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (let ((val (read-pc-16-be vm)))
    (push-stack vm (logior #xE0000 val))))

;;; ===========================================================================
;; GLOBAL CONSTANT
;;; ===========================================================================

(defop gconst :hexcode #x6F :instruction-length 3
  "GCONST: Global Constant.
Reads 2-byte index, looks up global constant value.
Pushes the constant value onto stack."
  :operands ((index :uint16-be "Global constant index"))
  :stack-effect (:push 1)
  :category :constants
  :side-effects nil
  (let ((idx (read-pc-16-be vm)))
    ;; TODO: Look up global constant table
    (push-stack vm 0)))

;;; ===========================================================================
;; NUMBER CONSTRUCTION
;;; ===========================================================================

(defop makenumber :hexcode #xF5 :instruction-length 1
  "MAKENUMBER: Construct number from raw value.
Pops value, applies number tag, pushes result."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((value (pop-stack vm)))
    (push-stack vm (logior value 2))))

;;; ===========================================================================
;; STACK MANIPULATION
;;; ===========================================================================

(defop swap :hexcode #xFD :instruction-length 1
  "SWAP: Exchange top two stack values.
Pops A, pops B, pushes A, pushes B."
  :operands nil
  :stack-effect (:pop 2 :push 2)
  :category :stack-operations
  :side-effects nil
  (let ((a (pop-stack vm))
        (b (pop-stack vm)))
    (push-stack vm a)
    (push-stack vm b)))

;;; ===========================================================================
;; MISC
;;; ===========================================================================

(defop nop :hexcode #xFE :instruction-length 1
  "NOP: No operation.
Does nothing, advances PC by 1."
  :operands nil
  :stack-effect nil
  :category :miscellaneous
  :side-effects nil
  nil)

(defop cl-equal :hexcode #xFF :instruction-length 1
  "CL_EQUAL: Common Lisp EQUAL (case-insensitive string compare).
Pops B and A, pushes T if equal, NIL otherwise."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :comparison
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; TODO: Implement proper CL EQUAL semantics
    (push-stack vm 0)))

;;; ===========================================================================
;; HELPER FUNCTIONS
;;; ===========================================================================

;; Helper functions moved to laiko/src/vm/dispatch.lisp
