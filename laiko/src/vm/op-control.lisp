(in-package :maiko-lisp.vm)

;; Control flow operations
;; return, jump0-15, fjump0-15, tjump0-15, jumpx, fjumpx, tjumpx
;; fn0-4, fnx, applyfn

;;; ===========================================================================
;; RETURN
;;; ===========================================================================

(defop return #x10 1
  "RETURN: Return from current function.
Pops return value from stack and returns to caller."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t  ; Modifies control flow
  (let ((return-value (pop-stack vm)))
    (maiko-lisp.vm:return-from-function vm return-value)))

;;; ===========================================================================
;; UNCONDITIONAL JUMPS (JUMP0-15)
;;; ===========================================================================

(defop jump0 #x80 1
  "JUMP0: Unconditional jump (no offset)."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 0))

(defop jump1 #x81 1
  "JUMP1: Unconditional jump with offset 1."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 1))

(defop jump2 #x82 1
  "JUMP2: Unconditional jump with offset 2."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 2))

(defop jump3 #x83 1
  "JUMP3: Unconditional jump with offset 3."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 3))

(defop jump4 #x84 1
  "JUMP4: Unconditional jump with offset 4."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 4))

(defop jump5 #x85 1
  "JUMP5: Unconditional jump with offset 5."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 5))

(defop jump6 #x86 1
  "JUMP6: Unconditional jump with offset 6."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 6))

(defop jump7 #x87 1
  "JUMP7: Unconditional jump with offset 7."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 7))

(defop jump8 #x88 1
  "JUMP8: Unconditional jump with offset 8."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 8))

(defop jump9 #x89 1
  "JUMP9: Unconditional jump with offset 9."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 9))

(defop jump10 #x8A 1
  "JUMP10: Unconditional jump with offset 10."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 10))

(defop jump11 #x8B 1
  "JUMP11: Unconditional jump with offset 11."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 11))

(defop jump12 #x8C 1
  "JUMP12: Unconditional jump with offset 12."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 12))

(defop jump13 #x8D 1
  "JUMP13: Unconditional jump with offset 13."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 13))

(defop jump14 #x8E 1
  "JUMP14: Unconditional jump with offset 14."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 14))

(defop jump15 #x8F 1
  "JUMP15: Unconditional jump with offset 15."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 15))

;;; ===========================================================================
;; FALSE JUMPS (FJUMP0-15) - Jump if TOS is NIL
;;; ===========================================================================

(defop fjump0 #x90 1
  "FJUMP0: Jump if TOS is NIL (no offset). Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 0))))

(defop fjump1 #x91 1
  "FJUMP1: Jump with offset 1 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 1))))

(defop fjump2 #x92 1
  "FJUMP2: Jump with offset 2 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 2))))

(defop fjump3 #x93 1
  "FJUMP3: Jump with offset 3 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 3))))

(defop fjump4 #x94 1
  "FJUMP4: Jump with offset 4 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 4))))

(defop fjump5 #x95 1
  "FJUMP5: Jump with offset 5 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 5))))

(defop fjump6 #x96 1
  "FJUMP6: Jump with offset 6 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 6))))

(defop fjump7 #x97 1
  "FJUMP7: Jump with offset 7 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 7))))

(defop fjump8 #x98 1
  "FJUMP8: Jump with offset 8 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 8))))

(defop fjump9 #x99 1
  "FJUMP9: Jump with offset 9 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 9))))

(defop fjump10 #x9A 1
  "FJUMP10: Jump with offset 10 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 10))))

(defop fjump11 #x9B 1
  "FJUMP11: Jump with offset 11 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 11))))

(defop fjump12 #x9C 1
  "FJUMP12: Jump with offset 12 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 12))))

(defop fjump13 #x9D 1
  "FJUMP13: Jump with offset 13 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 13))))

(defop fjump14 #x9E 1
  "FJUMP14: Jump with offset 14 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 14))))

(defop fjump15 #x9F 1
  "FJUMP15: Jump with offset 15 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 15))))

;;; ===========================================================================
;; TRUE JUMPS (TJUMP0-15) - Jump if TOS is non-NIL
;;; ===========================================================================

(defop tjump0 #xA0 1
  "TJUMP0: Jump if TOS is non-NIL (no offset). Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 0))))

(defop tjump1 #xA1 1
  "TJUMP1: Jump with offset 1 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 1))))

(defop tjump2 #xA2 1
  "TJUMP2: Jump with offset 2 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 2))))

(defop tjump3 #xA3 1
  "TJUMP3: Jump with offset 3 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 3))))

(defop tjump4 #xA4 1
  "TJUMP4: Jump with offset 4 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 4))))

(defop tjump5 #xA5 1
  "TJUMP5: Jump with offset 5 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 5))))

(defop tjump6 #xA6 1
  "TJUMP6: Jump with offset 6 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 6))))

(defop tjump7 #xA7 1
  "TJUMP7: Jump with offset 7 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 7))))

(defop tjump8 #xA8 1
  "TJUMP8: Jump with offset 8 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 8))))

(defop tjump9 #xA9 1
  "TJUMP9: Jump with offset 9 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 9))))

(defop tjump10 #xAA 1
  "TJUMP10: Jump with offset 10 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 10))))

(defop tjump11 #xAB 1
  "TJUMP11: Jump with offset 11 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 11))))

(defop tjump12 #xAC 1
  "TJUMP12: Jump with offset 12 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 12))))

(defop tjump13 #xAD 1
  "TJUMP13: Jump with offset 13 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 13))))

(defop tjump14 #xAE 1
  "TJUMP14: Jump with offset 14 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 14))))

(defop tjump15 #xAF 1
  "TJUMP15: Jump with offset 15 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 15))))

;;; ===========================================================================
;; EXTENDED JUMPS (16-bit offset)
;;; ===========================================================================

(defop jumpx #xB0 3
  "JUMPX: Unconditional jump with 16-bit signed offset.
Reads 2-byte offset from instruction stream."
  :operands ((offset :int16-be "Signed 16-bit jump offset"))
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (let ((offset (read-pc-16-be-signed vm)))
    (incf (vm-pc vm) offset)))

(defop fjumpx #xB1 3
  "FJUMPX: Jump if TOS is NIL with 16-bit offset.
Pops TOS, jumps if it was NIL."
  :operands ((offset :int16-be "Signed 16-bit jump offset"))
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (let ((offset (read-pc-16-be-signed vm)))
        (incf (vm-pc vm) offset)))))

(defop tjumpx #xB2 3
  "TJUMPX: Jump if TOS is non-NIL with 16-bit offset.
Pops TOS, jumps if it was non-NIL."
  :operands ((offset :int16-be "Signed 16-bit jump offset"))
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (let ((offset (read-pc-16-be-signed vm)))
        (incf (vm-pc vm) offset)))))

;;; ===========================================================================
;; FUNCTION CALL (FN0-4, FNX)
;;; ===========================================================================

(defop fn0 #x08 3
  "FN0: Call function with 0 arguments.
Reads 2-byte atom index, looks up function definition, calls it."
  :operands ((atom-index :atom-index "Atom index of function"))
  :stack-effect nil
  :category :function-call
  :side-effects t
  (let* ((atom-idx (read-pc-16-be vm))
         (defcell (maiko-lisp.data:read-defcell vm atom-idx)))
    (when (maiko-lisp.data:is-c-code defcell)
      (error 'maiko-lisp.utils:vm-error :message "FN0: C code not supported"))
    (let ((fnheader-offset (maiko-lisp.data:get-function-header defcell)))
      (when (zerop fnheader-offset)
        (error 'maiko-lisp.utils:vm-error :message "FN0: Undefined function"))
      ;; TODO: Implement function call
      nil)))

(defop fn1 #x09 3
  "FN1: Call function with 1 argument.
Reads 2-byte atom index, pops 1 argument, calls function."
  :operands ((atom-index :atom-index "Atom index of function"))
  :stack-effect (:pop 1)
  :category :function-call
  :side-effects t
  (let ((atom-idx (read-pc-16-be vm)))
    (declare (ignore atom-idx))
    ;; TODO: Implement
    nil))

(defop fn2 #x0A 3
  "FN2: Call function with 2 arguments.
Reads 2-byte atom index, pops 2 arguments, calls function."
  :operands ((atom-index :atom-index "Atom index of function"))
  :stack-effect (:pop 2)
  :category :function-call
  :side-effects t
  (let ((atom-idx (read-pc-16-be vm)))
    (declare (ignore atom-idx))
    ;; TODO: Implement
    nil))

(defop fn3 #x0B 3
  "FN3: Call function with 3 arguments.
Reads 2-byte atom index, pops 3 arguments, calls function."
  :operands ((atom-index :atom-index "Atom index of function"))
  :stack-effect (:pop 3)
  :category :function-call
  :side-effects t
  (let ((atom-idx (read-pc-16-be vm)))
    (declare (ignore atom-idx))
    ;; TODO: Implement
    nil))

(defop fn4 #x0C 3
  "FN4: Call function with 4 arguments.
Reads 2-byte atom index, pops 4 arguments, calls function."
  :operands ((atom-index :atom-index "Atom index of function"))
  :stack-effect (:pop 4)
  :category :function-call
  :side-effects t
  (let ((atom-idx (read-pc-16-be vm)))
    (declare (ignore atom-idx))
    ;; TODO: Implement
    nil))

(defop fnx #x0D 4
  "FNX: Call function with variable arguments.
Reads 1-byte argument count and 2-byte atom index."
  :operands ((arg-count :uint8 "Number of arguments")
             (atom-index :atom-index "Atom index of function"))
  :stack-effect (:pop-n arg-count)
  :category :function-call
  :side-effects t
  (let ((arg-count (read-pc-8 vm))
        (atom-idx (read-pc-16-be vm)))
    (declare (ignore arg-count atom-idx))
    ;; TODO: Implement
    nil))

(defop applyfn #x0E 1
  "APPLYFN: Apply function to argument list.
Pops function object and argument list, applies function."
  :operands nil
  :stack-effect (:pop 2)
  :category :function-call
  :side-effects t
  (let ((arg-list (pop-stack vm))
        (func-obj (pop-stack vm)))
    (declare (ignore arg-list func-obj))
    ;; TODO: Implement
    nil))

;;; ===========================================================================
;; HELPER FUNCTIONS (not opcodes)
;;; ===========================================================================

(defun read-pc-8 (vm)
  "Read 8-bit value from PC and advance PC by 1."
  (declare (type vm vm))
  ;; Placeholder
  0)

(defun read-pc-16-be (vm)
  "Read 16-bit big-endian value from PC and advance PC by 2."
  (declare (type vm vm))
  ;; Placeholder
  0)

(defun read-pc-16-be-signed (vm)
  "Read signed 16-bit big-endian value from PC."
  (declare (type vm vm))
  (let ((unsigned (read-pc-16-be vm)))
    (if (>= unsigned #x8000) (- unsigned #x10000) unsigned)))
