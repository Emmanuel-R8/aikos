(in-package :maiko-lisp.vm)

;; Bitwise and logical operations
;; logand, logior, logxor, lognot, lsh

(defun handle-logand (vm)
  "LOGAND: Bitwise AND"
  (declare (type vm vm))
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (push-stack vm (logand a b))))

(defun handle-logior (vm)
  "LOGIOR: Bitwise OR"
  (declare (type vm vm))
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (push-stack vm (logior a b))))

(defun handle-logxor (vm)
  "LOGXOR: Bitwise XOR"
  (declare (type vm vm))
  (let ((b (pop-stack vm)) (a (pop-stack vm)))
    (push-stack vm (logxor a b))))

(defun handle-lognot (vm)
  "LOGNOT: Bitwise NOT"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (lognot value) #xFFFFFFFF))))

(defun handle-lsh (vm)
  "LSH: Logical shift"
  (declare (type vm vm))
  (let ((shift-amount (pop-stack vm))
        (value (pop-stack vm)))
    (let ((shift-signed (if (>= shift-amount #x80000000)
                            (- shift-amount #x100000000)
                            shift-amount)))
      (if (minusp shift-signed)
          (push-stack vm (ash value shift-signed))
          (push-stack vm (logand (ash value shift-signed) #xFFFFFFFF))))))

(defun handle-llsh1 (vm)
  "LLSH1: Logical left shift by 1"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (ash value 1) #xFFFFFFFF))))

(defun handle-llsh8 (vm)
  "LLSH8: Logical left shift by 8"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (ash value 8) #xFFFFFFFF))))

(defun handle-lrsh1 (vm)
  "LRSH1: Logical right shift by 1"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -1))))

(defun handle-lrsh8 (vm)
  "LRSH8: Logical right shift by 8"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -8))))

(defun handle-logor2 (vm)
  "LOGOR2: Bitwise OR (alternative name)"
  (declare (type vm vm))
  (handle-logior vm))

(defun handle-logand2 (vm)
  "LOGAND2: Bitwise AND (alternative name)"
  (declare (type vm vm))
  (handle-logand vm))

(defun handle-logxor2 (vm)
  "LOGXOR2: Bitwise XOR (alternative name)"
  (declare (type vm vm))
  (handle-logxor vm))

(defun handle-hiloc (vm)
  "HILOC: Get high 16 bits of address"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -16))))

(defun handle-loloc (vm)
  "LOLOC: Get low 16 bits of address"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand value #xFFFF))))
