(in-package :maiko-lisp.vm)

;; Constant operations
;; aconst, sic, snic, sicx, gconst, makenumber, swap, nop

(defun handle-aconst (vm operands)
  "ACONST: Push atom constant"
  (declare (type vm vm)
           (type list operands))
  (let ((atom-index (if (>= (length operands) 2)
                        (logior (first operands) (ash (second operands) 8))
                        0)))
    (push-stack vm 0)))

(defun handle-sic (vm operands)
  "SIC: Small integer constant (positive)"
  (declare (type vm vm)
           (type list operands))
  (let ((value (if operands (first operands) 0)))
    (push-stack vm (logior #xE0000 value))))

(defun handle-snic (vm operands)
  "SNIC: Small integer constant (negative)"
  (declare (type vm vm)
           (type list operands))
  (let ((value (if operands (first operands) 0)))
    (push-stack vm (logior #xFF00 value))))

(defun handle-sicx (vm operands)
  "SICX: Small integer constant (extended)"
  (declare (type vm vm)
           (type list operands))
  (let ((value (if (>= (length operands) 2)
                    (logior (first operands) (ash (second operands) 8))
                    0)))
    (push-stack vm (logior #xE0000 value))))

(defun handle-gconst (vm operands)
  "GCONST: Global constant"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if (>= (length operands) 2)
                    (logior (first operands) (ash (second operands) 8))
                    0)))
    (push-stack vm 0)))

(defun handle-makenumber (vm)
  "MAKENUMBER: Make number from value"
  (declare (type vm vm))
  (let ((value (pop-stack vm)))
    (push-stack vm (logior value 2))))

(defun handle-cl-equal (vm)
  "CL_EQUAL: Case-insensitive string equal"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm 0)))

(defun handle-swap (vm)
  "SWAP: Swap top two stack values"
  (declare (type vm vm))
  (let ((a (pop-stack vm))
        (b (pop-stack vm)))
    (push-stack vm a)
    (push-stack vm b)))

(defun handle-nop (vm)
  "NOP: No operation"
  (declare (type vm vm))
  nil)

(defun handle-copy (vm)
  "COPY: Copy top of stack"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (push-stack vm value)))
