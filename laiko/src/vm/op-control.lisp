(in-package :maiko-lisp.vm)

;; Control flow operations
;; return, jump, fn0-4, fnx

(defun handle-return (vm)
  "RETURN: Return from function"
  (declare (type vm vm))
  (let ((return-value (pop-stack vm)))
    (maiko-lisp.vm:return-from-function vm return-value)))

(defun handle-jump (vm operands)
  "JUMP: Unconditional jump"
  (declare (type vm vm)
           (type list operands))
  (when operands
    (let ((offset (first operands)))
      (let ((signed-offset (if (>= offset #x80) (- offset #x100) offset)))
        (incf (vm-pc vm) signed-offset)))))

(defun handle-jumpif (vm operands)
  "JUMPIF: Conditional jump if TOS is non-NIL"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (get-top-of-stack vm)))
    (when (not (zerop tos))
      (handle-jump vm operands))))

(defun handle-jumpifnil (vm operands)
  "JUMPIFNIL: Conditional jump if TOS is NIL"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (get-top-of-stack vm)))
    (when (zerop tos)
      (handle-jump vm operands))))

(defun handle-jump-n (vm offset)
  "JUMP with offset N (0-15)"
  (declare (type vm vm)
           (type (integer 0 15) offset))
  (incf (vm-pc vm) offset))

(defun handle-fjump-n (vm offset)
  "FJUMP with offset N (0-15): Jump if false"
  (declare (type vm vm)
           (type (integer 0 15) offset))
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) offset))))

(defun handle-tjump-n (vm offset)
  "TJUMP with offset N (0-15): Jump if true"
  (declare (type vm vm)
           (type (integer 0 15) offset))
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) offset))))

(defun handle-jump0 (vm) (handle-jump-n vm 0))
(defun handle-jump1 (vm) (handle-jump-n vm 1))
(defun handle-jump2 (vm) (handle-jump-n vm 2))
(defun handle-jump3 (vm) (handle-jump-n vm 3))
(defun handle-jump4 (vm) (handle-jump-n vm 4))
(defun handle-jump5 (vm) (handle-jump-n vm 5))
(defun handle-jump6 (vm) (handle-jump-n vm 6))
(defun handle-jump7 (vm) (handle-jump-n vm 7))
(defun handle-jump8 (vm) (handle-jump-n vm 8))
(defun handle-jump9 (vm) (handle-jump-n vm 9))
(defun handle-jump10 (vm) (handle-jump-n vm 10))
(defun handle-jump11 (vm) (handle-jump-n vm 11))
(defun handle-jump12 (vm) (handle-jump-n vm 12))
(defun handle-jump13 (vm) (handle-jump-n vm 13))
(defun handle-jump14 (vm) (handle-jump-n vm 14))
(defun handle-jump15 (vm) (handle-jump-n vm 15))

(defun handle-fjump0 (vm) (handle-fjump-n vm 0))
(defun handle-fjump1 (vm) (handle-fjump-n vm 1))
(defun handle-fjump2 (vm) (handle-fjump-n vm 2))
(defun handle-fjump3 (vm) (handle-fjump-n vm 3))
(defun handle-fjump4 (vm) (handle-fjump-n vm 4))
(defun handle-fjump5 (vm) (handle-fjump-n vm 5))
(defun handle-fjump6 (vm) (handle-fjump-n vm 6))
(defun handle-fjump7 (vm) (handle-fjump-n vm 7))
(defun handle-fjump8 (vm) (handle-fjump-n vm 8))
(defun handle-fjump9 (vm) (handle-fjump-n vm 9))
(defun handle-fjump10 (vm) (handle-fjump-n vm 10))
(defun handle-fjump11 (vm) (handle-fjump-n vm 11))
(defun handle-fjump12 (vm) (handle-fjump-n vm 12))
(defun handle-fjump13 (vm) (handle-fjump-n vm 13))
(defun handle-fjump14 (vm) (handle-fjump-n vm 14))
(defun handle-fjump15 (vm) (handle-fjump-n vm 15))

(defun handle-tjump0 (vm) (handle-tjump-n vm 0))
(defun handle-tjump1 (vm) (handle-tjump-n vm 1))
(defun handle-tjump2 (vm) (handle-tjump-n vm 2))
(defun handle-tjump3 (vm) (handle-tjump-n vm 3))
(defun handle-tjump4 (vm) (handle-tjump-n vm 4))
(defun handle-tjump5 (vm) (handle-tjump-n vm 5))
(defun handle-tjump6 (vm) (handle-tjump-n vm 6))
(defun handle-tjump7 (vm) (handle-tjump-n vm 7))
(defun handle-tjump8 (vm) (handle-tjump-n vm 8))
(defun handle-tjump9 (vm) (handle-tjump-n vm 9))
(defun handle-tjump10 (vm) (handle-tjump-n vm 10))
(defun handle-tjump11 (vm) (handle-tjump-n vm 11))
(defun handle-tjump12 (vm) (handle-tjump-n vm 12))
(defun handle-tjump13 (vm) (handle-tjump-n vm 13))
(defun handle-tjump14 (vm) (handle-tjump-n vm 14))
(defun handle-tjump15 (vm) (handle-tjump-n vm 15))

(defun handle-jumpx (vm operands)
  "JUMPX: Unconditional jump with 16-bit offset"
  (declare (type vm vm)
           (type list operands))
  (when (>= (length operands) 2)
    (let ((offset (let ((low (first operands)) (high (second operands)))
                   (let ((unsigned (logior low (ash high 8))))
                     (if (>= unsigned #x8000) (- unsigned #x10000) unsigned)))))
      (incf (vm-pc vm) offset))))

(defun handle-fjumpx (vm operands)
  "FJUMPX: Conditional jump if false with 16-bit offset"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (handle-jumpx vm operands))))

(defun handle-tjumpx (vm operands)
  "TJUMPX: Conditional jump if true with 16-bit offset"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (handle-jumpx vm operands))))

(defun handle-fn0 (vm operands)
  "FN0: Call function with 0 arguments.
   Operands contain atom index (2 bytes for non-BIGATOMS)."
  (declare (type vm vm)
           (type list operands))
  (let* ((atom-index (if (>= (length operands) 2)
                         (logior (first operands) (ash (second operands) 8))
                         0))
         (defcell (maiko-lisp.data:read-defcell vm atom-index)))
    (when (maiko-lisp.data:is-c-code defcell)
      (error 'maiko-lisp.utils:vm-error :message "FN0: C code not supported"))
    (let ((fnheader-offset (maiko-lisp.data:get-function-header defcell)))
      (when (zerop fnheader-offset)
        (error 'maiko-lisp.utils:vm-error :message "FN0: Undefined function"))
      ;; Read function header
      (let ((fnheader (maiko-lisp.data:read-function-header vm fnheader-offset)))
        (unless fnheader
          (error 'maiko-lisp.utils:vm-error :message "FN0: Cannot read function header"))
        ;; TODO: Implement function call with frame setup
        (format t "FN0: atom=~X fnheader=~X~%" atom-index fnheader-offset)
        ;; For now, skip the call and continue
        nil))))

(defun handle-fn1 (vm operands)
  "FN1: Call function with 1 argument"
  (declare (type vm vm)
           (type list operands))
  (let ((arg1 (vm-pop vm))
        (atom-index (if (>= (length operands) 2)
                        (logior (first operands) (ash (second operands) 8))
                        0)))
    (declare (ignore arg1))
    (format t "FN1: atom=~X~%" atom-index)
    nil))

(defun handle-fn2 (vm operands)
  "FN2: Call function with 2 arguments"
  (declare (type vm vm)
           (type list operands))
  (let ((arg2 (vm-pop vm))
        (arg1 (vm-pop vm))
        (atom-index (if (>= (length operands) 2)
                        (logior (first operands) (ash (second operands) 8))
                        0)))
    (declare (ignore arg1 arg2))
    (format t "FN2: atom=~X~%" atom-index)
    nil))

(defun handle-fn3 (vm operands)
  "FN3: Call function with 3 arguments"
  (declare (type vm vm)
           (type list operands))
  (let ((arg3 (vm-pop vm))
        (arg2 (vm-pop vm))
        (arg1 (vm-pop vm))
        (atom-index (if (>= (length operands) 2)
                        (logior (first operands) (ash (second operands) 8))
                        0)))
    (declare (ignore arg1 arg2 arg3))
    (format t "FN3: atom=~X~%" atom-index)
    nil))

(defun handle-fn4 (vm operands)
  "FN4: Call function with 4 arguments"
  (declare (type vm vm)
           (type list operands))
  (let ((arg4 (vm-pop vm))
        (arg3 (vm-pop vm))
        (arg2 (vm-pop vm))
        (arg1 (vm-pop vm))
        (atom-index (if (>= (length operands) 2)
                        (logior (first operands) (ash (second operands) 8))
                        0)))
    (declare (ignore arg1 arg2 arg3 arg4))
    (format t "FN4: atom=~X~%" atom-index)
    nil))

(defun handle-fnx (vm operands)
  "FNX: Call function with variable arguments"
  (declare (type vm vm)
           (type list operands))
  (let ((atom-index (if (>= (length operands) 2)
                        (logior (first operands) (ash (second operands) 8))
                        0))
        (arg-count (if (>= (length operands) 3) (third operands) 0)))
    (declare (ignore arg-count))
    (format t "FNX: atom=~X args=~D~%" atom-index arg-count)
    nil))

(defun handle-applyfn (vm)
  "APPLYFN: Apply function to argument list"
  (declare (type vm vm))
  (let ((arg-list (vm-pop vm))
        (func-obj (vm-pop vm)))
    (declare (ignore arg-list func-obj))
    (format t "APPLYFN called~%")
    nil))
