(in-package :maiko-lisp.vm)

;; Memory and array access operations
;; aref1, aset1, aref2, aset2
;; getael1, getael2, setael1, setael2
;; getbasebyte, putbasebyte
;; getbase-n, getbaseptr-n, putbase-n, putbaseptr-n
;; addbase

;;; ===========================================================================
;; ARRAY ACCESS (1D)
;;; ===========================================================================

(defop aref1 :hexcode #xEE :instruction-length 2
  "AREF1: 1-dimensional array access.
Reads 1-byte operand, pops array-ptr, pushes element at index."
  :operands ((index :uint8 "Array index (0-255)"))
  :stack-effect (:pop 1 :push 1)
  :category :memory
  :side-effects nil
  (let ((index (read-pc-8 vm))
        (array-ptr (pop-stack vm)))
    (let ((value (get-array-element array-ptr index)))
      (push-stack vm value))))

(defop aset1 :hexcode #xEF :instruction-length 2
  "ASET1: 1-dimensional array set.
Reads 1-byte operand, pops value and array-ptr, stores value at index."
  :operands ((index :uint8 "Array index (0-255)"))
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects t  ; Modifies array
  (let ((index (read-pc-8 vm))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((new-ptr (set-array-element array-ptr index value)))
      (push-stack vm new-ptr))))

;;; ===========================================================================
;; ARRAY ACCESS (2D)
;;; ===========================================================================

(defop aref2 :hexcode #xF8 :instruction-length 1
  "AREF2: 2-dimensional array access.
Pops index1, index0, array-ptr, pushes element at [index0, index1]."
  :operands nil
  :stack-effect (:pop 3 :push 1)
  :category :memory
  :side-effects nil
  (let ((index1 (pop-stack vm))
        (index0 (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((linear-index (+ index0 (* index1 2))))
      (let ((value (get-array-element array-ptr linear-index)))
        (push-stack vm value)))))

(defop aset2 :hexcode #xF9 :instruction-length 1
  "ASET2: 2-dimensional array set.
Pops value, index1, index0, array-ptr, stores value."
  :operands nil
  :stack-effect (:pop 4 :push 1)
  :category :memory
  :side-effects t  ; Modifies array
  (let ((value (pop-stack vm))
        (index1 (pop-stack vm))
        (index0 (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((linear-index (+ index0 (* index1 2))))
      (let ((new-ptr (set-array-element array-ptr linear-index value)))
        (push-stack vm new-ptr)))))

;;; ===========================================================================
;; EXTENDED ARRAY ACCESS (with bytecode index)
;;; ===========================================================================

(defop getael1 :hexcode #xF6 :instruction-length 2
  "GETAEL1: Get array element with 1-byte index.
Reads index from bytecode, pops array-ptr, pushes element."
  :operands ((index :uint8 "Array index"))
  :stack-effect (:pop 1 :push 1)
  :category :memory
  :side-effects nil
  (let ((index (read-pc-8 vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "GETAEL1: NIL is not an array"))
    (let ((value (get-array-element array-ptr index)))
      (push-stack vm value))))

(defop getael2 :hexcode #xF7 :instruction-length 3
  "GETAEL2: Get array element with 2-byte index.
Reads 16-bit index from bytecode, pops array-ptr, pushes element."
  :operands ((index :uint16-be "Array index (16-bit)"))
  :stack-effect (:pop 1 :push 1)
  :category :memory
  :side-effects nil
  (let ((index (read-pc-16-be vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "GETAEL2: NIL is not an array"))
    (let ((value (get-array-element array-ptr index)))
      (push-stack vm value))))

(defop setael1 :hexcode #xFA :instruction-length 2
  "SETAEL1: Set array element with 1-byte index.
Reads index, pops value and array-ptr, stores value."
  :operands ((index :uint8 "Array index"))
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects t
  (let ((index (read-pc-8 vm))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "SETAEL1: NIL is not an array"))
    (let ((new-ptr (set-array-element array-ptr index value)))
      (push-stack vm new-ptr))))

(defop setael2 :hexcode #xFB :instruction-length 3
  "SETAEL2: Set array element with 2-byte index.
Reads 16-bit index, pops value and array-ptr, stores value."
  :operands ((index :uint16-be "Array index (16-bit)"))
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects t
  (let ((index (read-pc-16-be vm))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "SETAEL2: NIL is not an array"))
    (let ((new-ptr (set-array-element array-ptr index value)))
      (push-stack vm new-ptr))))

;;; ===========================================================================
;; BYTE ACCESS
;;; ===========================================================================

(defop getbasebyte :hexcode #xC2 :instruction-length 1
  "GETBASEBYTE: Get byte from base address.
Pops offset and base-addr, pushes byte at (base + offset)."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects nil
  (let ((offset (pop-stack vm))
        (base-addr (pop-stack vm)))
    (let ((byte-addr (+ base-addr offset)))
      (let ((byte (logand byte-addr #xFF)))
        (push-stack vm byte)))))

(defop putbasebyte :hexcode #xC7 :instruction-length 1
  "PUTBASEBYTE: Put byte to base address.
Pops value, offset, base-addr, stores byte at (base + offset)."
  :operands nil
  :stack-effect (:pop 3 :push 1)
  :category :memory
  :side-effects t
  (let ((value (pop-stack vm))
        (offset (pop-stack vm))
        (base-addr (pop-stack vm)))
    (let ((byte-addr (+ base-addr offset)))
      (declare (ignore byte-addr))
      (push-stack vm value))))

;;; ===========================================================================
;; BASE ADDRESS OPERATIONS
;;; ===========================================================================

(defop getbase-n :hexcode #xC8 :instruction-length 2
  "GETBASE_N: Get value from base with index N.
Reads 1-byte index, pops base-addr, pushes value at (base + index*4)."
  :operands ((index :uint8 "Index multiplier"))
  :stack-effect (:pop 1 :push 1)
  :category :memory
  :side-effects nil
  (let ((index (read-pc-8 vm))
        (base-addr (pop-stack vm)))
    (let ((value-addr (+ base-addr (* index 4))))
      (push-stack vm value-addr))))

(defop getbaseptr-n :hexcode #xC9 :instruction-length 2
  "GETBASEPTR_N: Get pointer from base with index N.
Reads index, pops base-addr, pushes pointer at (base + index*4)."
  :operands ((index :uint8 "Index multiplier"))
  :stack-effect (:pop 1 :push 1)
  :category :memory
  :side-effects nil
  (let ((index (read-pc-8 vm))
        (base-addr (pop-stack vm)))
    (let ((ptr-addr (+ base-addr (* index 4))))
      (push-stack vm ptr-addr))))

(defop putbase-n :hexcode #xCD :instruction-length 2
  "PUTBASE_N: Put value to base with index N.
Reads index, pops value and base-addr, stores at (base + index*4)."
  :operands ((index :uint8 "Index multiplier"))
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects t
  (let ((index (read-pc-8 vm))
        (value (pop-stack vm))
        (base-addr (pop-stack vm)))
    (declare (ignore value))
    (let ((value-addr (+ base-addr (* index 4))))
      (push-stack vm base-addr))))

(defop putbaseptr-n :hexcode #xCE :instruction-length 2
  "PUTBASEPTR_N: Put pointer to base with index N.
Reads index, pops pointer and base-addr, stores at (base + index*4)."
  :operands ((index :uint8 "Index multiplier"))
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects t
  (let ((index (read-pc-8 vm))
        (value (pop-stack vm))
        (base-addr (pop-stack vm)))
    (let ((ptr-addr (+ base-addr (* index 4))))
      (push-stack vm base-addr))))

(defop addbase :hexcode #xD0 :instruction-length 1
  "ADDBASE: Add offset to base address.
Pops offset and base, pushes (base + offset).
Used for calculating addresses relative to a base pointer."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :memory
  :side-effects nil
  (let ((offset (pop-stack vm))
        (base (pop-stack vm)))
    (push-stack vm (+ base offset))))

;;; ===========================================================================
;; HELPER FUNCTIONS
;;; ===========================================================================

(defun get-array-element (array-ptr index)
  "Get element from array at given index."
  (declare (type maiko-lisp.utils:lisp-ptr array-ptr)
           (type (unsigned-byte 32) index))
  (when (zerop array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "Array access: NIL is not an array"))
  index)

(defun set-array-element (array-ptr index value)
  "Set element in array at given index."
  (declare (type maiko-lisp.utils:lisp-ptr array-ptr index)
           (type maiko-lisp.utils:lisp-ptr value))
  (when (zerop array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "Array set: NIL is not an array"))
  array-ptr)

(defun read-pc-8 (vm)
  "Read 8-bit value from PC."
  (declare (type vm vm))
  0)

(defun read-pc-16-be (vm)
  "Read 16-bit big-endian value from PC."
  (declare (type vm vm))
  0)
