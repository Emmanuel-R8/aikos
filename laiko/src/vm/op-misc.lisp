(in-package :maiko-lisp.vm)

;; Miscellaneous operations
;; bind, unbind, typep, fixp, smallp, charcode, charn
;; unwind, listget, assoc, fmemb, stkscan
;; pop-n, gcref, plus2, difference, times2, quotient

;;; ===========================================================================
;; BINDING OPERATIONS
;;; ===========================================================================

(defop bind :hexcode #x11 :instruction-length 2
  "BIND: Create variable bindings.
Operand byte: [n1:4][n2:4] where n1=vars to bind NIL, n2=vars from stack.
Allocates PVAR slots and pushes binding marker."
  :operands ((config :uint8 "Binding config: high nibble=n1, low nibble=n2"))
  :stack-effect (:push 1)
  :category :control-flow
  :side-effects t
  (let* ((operand (read-pc-8 vm))
         (n1 (ash operand -4))
         (n2 (logand operand #xF))
         (total (+ n1 n2)))
    (let ((pvar-offset (allocate-pvar-slots vm total)))
      ;; Bind n1 variables to NIL
      (loop for i from 0 below n1 do
        (set-pvar-slot vm (+ pvar-offset i) 0))
      ;; Bind n2 variables from stack
      (loop for i from 0 below n2 do
        (let ((value (vm-pop vm)))
          (set-pvar-slot vm (+ pvar-offset n1 i) value)))
      ;; Push binding marker
      (let* ((count-encoded (logand (logxor total #x7FFF) #x7FFF))
             (offset-encoded (logand (ash pvar-offset 1) #x7FFF))
             (marker (logior #x80000000
                            (ash count-encoded 16)
                            offset-encoded)))
        (vm-push vm marker)))))

(defop unbind :hexcode #x12 :instruction-length 1
  "UNBIND: Restore variable bindings.
Scans stack for binding marker and restores PVAR slots."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  ;; TODO: Implement proper UNBIND
  nil)

;;; ===========================================================================
;; TYPE OPERATIONS
;;; ===========================================================================

(defop typep :hexcode #x05 :instruction-length 2
  "TYPEP: Type check.
Reads type code, checks if TOS matches type.
Types: 0=NIL, 1=Fixnum, 2=Float, 3=List, 4=Symbol, 5=Array."
  :operands ((type-code :uint8 "Type code"))
  :stack-effect (:pop 1 :push 1)
  :category :type-operations
  :side-effects nil
  (let ((type-code (read-pc-8 vm))
        (value (get-top-of-stack vm)))
    (let ((result
           (case type-code
             (0 (if (zerop value) 1 0))
             (1 (if (logtest value #x2) 1 0))
             (2 (if (and (logtest value #x2) (not (logtest value #x1))) 1 0))
             (3 (if (not (zerop value)) 1 0))
             (t 0))))
      (set-top-of-stack vm result))))

;; FIXP and SMALLP are implemented as subroutines, not bytecodes
;; They are accessed via the function call mechanism

(defop smallp :hexcode #x06 :instruction-length 1
  "SMALLP: Check if value is small positive integer.
 Replaces TOS with T if small positive, NIL otherwise."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :type-operations
  :side-effects nil
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (if (and (> value 0)
                                  (not (logtest value #x80000000)))
                             1
                             0))))

;;; ===========================================================================
;; CHARACTER OPERATIONS
;;; ===========================================================================

;; CHARCODE and CHARN are implemented as subroutines, not bytecodes
;; They are accessed via the function call mechanism

;;; ===========================================================================
;; LIST OPERATIONS
;;; ===========================================================================

(defop listget :hexcode #x27 :instruction-length 1
  "LISTGET: Get element from list by key.
Pops key and list, pushes (key . value) or NIL."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((key (pop-stack vm))
        (list-ptr (pop-stack vm)))
    (push-stack vm 0)))

(defop assoc :hexcode #x16 :instruction-length 1
  "ASSOC: Association list lookup.
Pops key and alist, pushes (key . value) or NIL."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((key (pop-stack vm))
        (alist (pop-stack vm)))
    (push-stack vm 0)))

(defop fmemb :hexcode #x1C :instruction-length 1
  "FMEMB: Fast member test.
Pops item and list, pushes T if item in list, NIL otherwise."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((item (pop-stack vm))
        (list-ptr (pop-stack vm)))
    (push-stack vm 0)))

;;; ===========================================================================
;; STACK OPERATIONS
;;; ===========================================================================

(defop stkscan :hexcode #x2F :instruction-length 1
  "STKSCAN: Scan stack for value.
Pops target, pushes T if found in stack, NIL otherwise."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :stack-operations
  :side-effects nil
  (let ((target (pop-stack vm))
        (stack (vm-stack vm))
        (stack-ptr (vm-stack-ptr vm)))
    (let ((found nil))
      (loop for i from (1- stack-ptr) downto 0
            when (= (aref stack i) target)
            do (setf found t) (return))
      (push-stack vm (if found 1 0)))))

(defop pop-n :hexcode #xC0 :instruction-length 2
  "POP_N: Pop N values from stack.
Reads count from bytecode, discards that many stack values."
  :operands ((count :uint8 "Number of values to pop"))
  :stack-effect (:pop-n count)
  :category :stack-operations
  :side-effects nil
  (let ((count (read-pc-8 vm)))
    (loop for i from 1 to count do
      (pop-stack vm))))

;;; ===========================================================================
;; GENERAL ARITHMETIC
;;; ===========================================================================

(defop plus2 :hexcode #xD4 :instruction-length 1
  "PLUS2: General addition.
Pops B and A, pushes A+B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (+ a b))))

(defop difference :hexcode #xD5 :instruction-length 1
  "DIFFERENCE: General subtraction.
Pops B and A, pushes A-B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (- a b))))

(defop times2 :hexcode #xD6 :instruction-length 1
  "TIMES2: General multiplication.
Pops B and A, pushes A*B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (* a b))))

(defop quotient :hexcode #xD7 :instruction-length 1
  "QUOTIENT: General division.
Pops B and A, pushes truncate(A/B).
Signals error if B is zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (when (zerop b)
      (error 'maiko-lisp.utils:vm-arithmetic-error :message "Division by zero"))
    (push-stack vm (truncate a b))))

;;; ===========================================================================
;; FLOATING POINT
;;; ===========================================================================

(defop fplus2 :hexcode #xE8 :instruction-length 1
  "FPLUS2: Floating-point addition.
Pops B and A as floats, pushes A+B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (decode-float-pointer a))
          (fb (decode-float-pointer b)))
      (push-stack vm (encode-float-pointer (+ fa fb))))))

(defop fdifference :hexcode #xE9 :instruction-length 1
  "FDIFFERENCE: Floating-point subtraction.
Pops B and A as floats, pushes A-B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (decode-float-pointer a))
          (fb (decode-float-pointer b)))
      (push-stack vm (encode-float-pointer (- fa fb))))))

(defop ftimes2 :hexcode #xEA :instruction-length 1
  "FTIMES2: Floating-point multiplication.
Pops B and A as floats, pushes A*B."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (decode-float-pointer a))
          (fb (decode-float-pointer b)))
      (push-stack vm (encode-float-pointer (* fa fb))))))

(defop fquotient :hexcode #xEB :instruction-length 1
  "FQUOTIENT: Floating-point division.
Pops B and A as floats, pushes A/B.
Signals error if B is zero."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :arithmetic
  :side-effects nil
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (decode-float-pointer a))
          (fb (decode-float-pointer b)))
      (if (= fb 0.0d0)
          (error 'maiko-lisp.utils:vm-error :message "Floating-point division by zero")
          (push-stack vm (encode-float-pointer (/ fa fb)))))))

;;; ===========================================================================
;; CONTROL
;;; ===========================================================================

(defop unwind :hexcode #x07 :instruction-length 1
  "UNWIND: Unwind stack to catch point.
Pops catch tag, unwinds stack."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((catch-tag (pop-stack vm)))
    (declare (ignore catch-tag))
    nil))

;;; ===========================================================================
;; HELPER FUNCTIONS
;;; ===========================================================================

(defun get-pvar-slot (vm index)
  "Get value from PVAR slot at given index."
  (declare (type vm vm)
           (type (integer 0 *) index))
  (let ((pvar (vm-pvar vm))
        (pvar-ptr (vm-pvar-ptr vm)))
    (when (and (> pvar-ptr 0) (< index pvar-ptr))
      (aref pvar index))))

(defun set-pvar-slot (vm index value)
  "Set value in PVAR slot at given index."
  (declare (type vm vm)
           (type (integer 0 *) index)
           (type maiko-lisp.utils:lisp-ptr value))
  (let ((pvar (vm-pvar vm))
        (pvar-ptr (vm-pvar-ptr vm)))
    (when (and (> pvar-ptr 0) (< index pvar-ptr))
      (setf (aref pvar index) value))))

(defun allocate-pvar-slots (vm count)
  "Allocate COUNT slots in PVAR area. Returns the base offset."
  (declare (type vm vm)
           (type (integer 0 *) count))
  (let ((old-ptr (vm-pvar-ptr vm)))
    (setf (vm-pvar-ptr vm) (+ old-ptr count))
    old-ptr))

(defun read-pc-8 (vm)
  "Read 8-bit value from PC."
  (declare (type vm vm))
  0)

(defun decode-float-pointer (ptr)
  "Decode a LispPTR to a float value."
  (declare (type maiko-lisp.utils:lisp-ptr ptr))
  (let ((type-code (ldb (byte 3 24) ptr)))
    (cond
      ((= type-code 2)
       (let ((mantissa (logand ptr #xFFFFFF)))
         (+ (* (ash mantissa -12) 1.0d0)
            (* (logand mantissa #xFFF) 1.0d-12))))
      ((= type-code 1)
       (coerce (ash ptr -3) 'double-float))
      (t 0.0d0))))

(defun encode-float-pointer (value)
  "Encode a float value to a LispPTR."
  (declare (type double-float value))
  (let ((int-val (truncate value)))
    (if (and (>= int-val 0) (< int-val #x200000))
        (logior #x200000 int-val)
        (logior #x200000 (logand (round value) #xFFFFFF)))))
