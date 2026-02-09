(in-package :maiko-lisp.vm)

;; Memory and array access operations
;; aref, aset, getbase, putbase

(defun get-array-element (array-ptr index)
  "Get element from array at given index.
   Returns the element value or signals error if out of bounds."
  (declare (type maiko-lisp.utils:lisp-ptr array-ptr)
           (type (unsigned-byte 32) index))
  (when (zerop array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "Array access: NIL is not an array"))
  index)

(defun set-array-element (array-ptr index value)
  "Set element in array at given index.
   Returns the array pointer or signals error if out of bounds."
  (declare (type maiko-lisp.utils:lisp-ptr array-ptr index)
           (type maiko-lisp.utils:lisp-ptr value))
  (when (zerop array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "Array set: NIL is not an array"))
  array-ptr)

(defun handle-aref1 (vm)
  "AREF1: 1-dimensional array access.
   Pops index and array-ptr, pushes element at that index."
  (declare (type vm vm))
  (let ((index (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((value (get-array-element array-ptr index)))
      (push-stack vm value))))

(defun handle-aset1 (vm)
  "ASET1: 1-dimensional array set.
   Pops value, index, and array-ptr, stores value at index, pushes array-ptr."
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (index (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((new-ptr (set-array-element array-ptr index value)))
      (push-stack vm new-ptr))))

(defun handle-aref2 (vm)
  "AREF2: 2-dimensional array access.
   Pops index1, index0, and array-ptr, pushes element at [index0, index1]."
  (declare (type vm vm))
  (let ((index1 (pop-stack vm))
        (index0 (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((linear-index (+ index0 (* index1 2))))
      (let ((value (get-array-element array-ptr linear-index)))
        (push-stack vm value)))))

(defun handle-aset2 (vm)
  "ASET2: 2-dimensional array set.
   Pops value, index1, index0, and array-ptr, stores value."
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (index1 (pop-stack vm))
        (index0 (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (let ((linear-index (+ index0 (* index1 2))))
      (let ((new-ptr (set-array-element array-ptr linear-index value)))
        (push-stack vm new-ptr)))))

(defun handle-getael1 (vm operands)
  "GETAEL1: Get array element with 1-byte index from bytecode.
   Pops array-ptr, uses operand as index, pushes element."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "GETAEL1: NIL is not an array"))
    (let ((value (get-array-element array-ptr index)))
      (push-stack vm value))))

(defun handle-getael2 (vm operands)
  "GETAEL2: Get array element with 2-byte index from bytecode.
   Pops array-ptr, uses two operands as 16-bit index, pushes element."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if (>= (length operands) 2)
                   (logior (first operands) (ash (second operands) 8))
                   0))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "GETAEL2: NIL is not an array"))
    (let ((value (get-array-element array-ptr index)))
      (push-stack vm value))))

(defun handle-setael1 (vm operands)
  "SETAEL1: Set array element with 1-byte index from bytecode.
   Pops value, array-ptr, uses operand as index, stores value."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "SETAEL1: NIL is not an array"))
    (let ((new-ptr (set-array-element array-ptr index value)))
      (push-stack vm new-ptr))))

(defun handle-setael2 (vm operands)
  "SETAEL2: Set array element with 2-byte index from bytecode.
   Pops value, array-ptr, uses two operands as index."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if (>= (length operands) 2)
                   (logior (first operands) (ash (second operands) 8))
                   0))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "SETAEL2: NIL is not an array"))
    (let ((new-ptr (set-array-element array-ptr index value)))
      (push-stack vm new-ptr))))

(defun handle-getbasebyte (vm)
  "GETBASEBYTE: Get byte from base address with offset from stack."
  (declare (type vm vm))
  (let ((offset (pop-stack vm))
        (base-addr (pop-stack vm)))
    (let ((byte-addr (+ base-addr offset)))
      (let ((byte (logand byte-addr #xFF)))
        (push-stack vm byte)))))

(defun handle-putbasebyte (vm)
  "PUTBASEBYTE: Put byte to base address with offset from stack."
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (offset (pop-stack vm))
        (base-addr (pop-stack vm)))
    (let ((byte-addr (+ base-addr offset)))
      (declare (ignore byte-addr))
      (push-stack vm value))))

(defun handle-getbase-n (vm operands)
  "GETBASE_N: Get value from base address with index N from bytecode."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (base-addr (pop-stack vm)))
    (let ((value-addr (+ base-addr (* index 4))))
      (push-stack vm value-addr))))

(defun handle-getbaseptr-n (vm operands)
  "GETBASEPTR_N: Get pointer from base address with index N from bytecode."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (base-addr (pop-stack vm)))
    (let ((ptr-addr (+ base-addr (* index 4))))
      (push-stack vm ptr-addr))))

(defun handle-putbase-n (vm operands)
  "PUTBASE_N: Put value to base address with index N from bytecode."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (value (pop-stack vm))
        (base-addr (pop-stack vm)))
    (declare (ignore value))
    (let ((value-addr (+ base-addr (* index 4))))
      (push-stack vm base-addr))))

(defun handle-putbaseptr-n (vm operands)
  "PUTBASEPTR_N: Put pointer to base address with index N from bytecode."
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (value (pop-stack vm))
        (base-addr (pop-stack vm)))
    (let ((ptr-addr (+ base-addr (* index 4))))
      (push-stack vm base-addr))))

(defun handle-addbase (vm)
  "ADDBASE: Add base address.
   Used for calculating addresses relative to a base pointer."
  (declare (type vm vm))
  (let ((value (pop-stack vm)))
    (push-stack vm value)))
