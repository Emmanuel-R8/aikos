(in-package :laiko.vm)

;; Control flow operations
;; return, jump0-15, fjump0-15, tjump0-15, jumpx, fjumpx, tjumpx
;; fn0-4, fnx, applyfn

;;; ===========================================================================
;; RETURN
;;; ===========================================================================

(defop return :hexcode #x10 :instruction-length 1
  "RETURN: Return from current function.
Uses cached TOS as the return value and returns to caller.
Per Maiko OPRETURN, RETURN preserves TOPOFSTACK through frame restoration."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t  ; Modifies control flow
  (let ((return-value (vm-tos vm)))
    (laiko.vm:return-from-function vm return-value)))

;;; ===========================================================================
;; UNCONDITIONAL JUMPS (JUMP0-15)
;;; ===========================================================================

(defop jump0 :hexcode #x80 :instruction-length 1
  "JUMP0: Unconditional jump (no offset)."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 1))

(defop jump1 :hexcode #x81 :instruction-length 1
  "JUMP1: Unconditional jump with offset 1."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 2))

(defop jump2 :hexcode #x82 :instruction-length 1
  "JUMP2: Unconditional jump with offset 2."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 3))

(defop jump3 :hexcode #x83 :instruction-length 1
  "JUMP3: Unconditional jump with offset 3."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 4))

(defop jump4 :hexcode #x84 :instruction-length 1
  "JUMP4: Unconditional jump with offset 4."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 5))

(defop jump5 :hexcode #x85 :instruction-length 1
  "JUMP5: Unconditional jump with offset 5."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 6))

(defop jump6 :hexcode #x86 :instruction-length 1
  "JUMP6: Unconditional jump with offset 6."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 7))

(defop jump7 :hexcode #x87 :instruction-length 1
  "JUMP7: Unconditional jump with offset 7."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 8))

(defop jump8 :hexcode #x88 :instruction-length 1
  "JUMP8: Unconditional jump with offset 8."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 9))

(defop jump9 :hexcode #x89 :instruction-length 1
  "JUMP9: Unconditional jump with offset 9."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 10))

(defop jump10 :hexcode #x8A :instruction-length 1
  "JUMP10: Unconditional jump with offset 10."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 11))

(defop jump11 :hexcode #x8B :instruction-length 1
  "JUMP11: Unconditional jump with offset 11."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 12))

(defop jump12 :hexcode #x8C :instruction-length 1
  "JUMP12: Unconditional jump with offset 12."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 13))

(defop jump13 :hexcode #x8D :instruction-length 1
  "JUMP13: Unconditional jump with offset 13."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 14))

(defop jump14 :hexcode #x8E :instruction-length 1
  "JUMP14: Unconditional jump with offset 14."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 15))

(defop jump15 :hexcode #x8F :instruction-length 1
  "JUMP15: Unconditional jump with offset 15."
  :operands nil
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (incf (vm-pc vm) 16))

;;; ===========================================================================
;; FALSE JUMPS (FJUMP0-15) - Jump if TOS is NIL
;;; ===========================================================================

(defop fjump0 :hexcode #x90 :instruction-length 1
  "FJUMP0: Jump if TOS is NIL (no offset). Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 1))))

(defop fjump1 :hexcode #x91 :instruction-length 1
  "FJUMP1: Jump with offset 1 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 2))))

(defop fjump2 :hexcode #x92 :instruction-length 1
  "FJUMP2: Jump with offset 2 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 3))))

(defop fjump3 :hexcode #x93 :instruction-length 1
  "FJUMP3: Jump with offset 3 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 4))))

(defop fjump4 :hexcode #x94 :instruction-length 1
  "FJUMP4: Jump with offset 4 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 5))))

(defop fjump5 :hexcode #x95 :instruction-length 1
  "FJUMP5: Jump with offset 5 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 6))))

(defop fjump6 :hexcode #x96 :instruction-length 1
  "FJUMP6: Jump with offset 6 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 7))))

(defop fjump7 :hexcode #x97 :instruction-length 1
  "FJUMP7: Jump with offset 7 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 8))))

(defop fjump8 :hexcode #x98 :instruction-length 1
  "FJUMP8: Jump with offset 8 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 9))))

(defop fjump9 :hexcode #x99 :instruction-length 1
  "FJUMP9: Jump with offset 9 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 10))))

(defop fjump10 :hexcode #x9A :instruction-length 1
  "FJUMP10: Jump with offset 10 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 11))))

(defop fjump11 :hexcode #x9B :instruction-length 1
  "FJUMP11: Jump with offset 11 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 12))))

(defop fjump12 :hexcode #x9C :instruction-length 1
  "FJUMP12: Jump with offset 12 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 13))))

(defop fjump13 :hexcode #x9D :instruction-length 1
  "FJUMP13: Jump with offset 13 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 14))))

(defop fjump14 :hexcode #x9E :instruction-length 1
  "FJUMP14: Jump with offset 14 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 15))))

(defop fjump15 :hexcode #x9F :instruction-length 1
  "FJUMP15: Jump with offset 15 if TOS is NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (zerop tos)
      (incf (vm-pc vm) 16))))

;;; ===========================================================================
;; TRUE JUMPS (TJUMP0-15) - Jump if TOS is non-NIL
;;; ===========================================================================

(defop tjump0 :hexcode #xA0 :instruction-length 1
  "TJUMP0: Jump if TOS is non-NIL (no offset). Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 1))))

(defop tjump1 :hexcode #xA1 :instruction-length 1
  "TJUMP1: Jump with offset 1 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 2))))

(defop tjump2 :hexcode #xA2 :instruction-length 1
  "TJUMP2: Jump with offset 2 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 3))))

(defop tjump3 :hexcode #xA3 :instruction-length 1
  "TJUMP3: Jump with offset 3 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 4))))

(defop tjump4 :hexcode #xA4 :instruction-length 1
  "TJUMP4: Jump with offset 4 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 5))))

(defop tjump5 :hexcode #xA5 :instruction-length 1
  "TJUMP5: Jump with offset 5 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 6))))

(defop tjump6 :hexcode #xA6 :instruction-length 1
  "TJUMP6: Jump with offset 6 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 7))))

(defop tjump7 :hexcode #xA7 :instruction-length 1
  "TJUMP7: Jump with offset 7 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 8))))

(defop tjump8 :hexcode #xA8 :instruction-length 1
  "TJUMP8: Jump with offset 8 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 9))))

(defop tjump9 :hexcode #xA9 :instruction-length 1
  "TJUMP9: Jump with offset 9 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 10))))

(defop tjump10 :hexcode #xAA :instruction-length 1
  "TJUMP10: Jump with offset 10 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 11))))

(defop tjump11 :hexcode #xAB :instruction-length 1
  "TJUMP11: Jump with offset 11 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 12))))

(defop tjump12 :hexcode #xAC :instruction-length 1
  "TJUMP12: Jump with offset 12 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 13))))

(defop tjump13 :hexcode #xAD :instruction-length 1
  "TJUMP13: Jump with offset 13 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 14))))

(defop tjump14 :hexcode #xAE :instruction-length 1
  "TJUMP14: Jump with offset 14 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 15))))

(defop tjump15 :hexcode #xAF :instruction-length 1
  "TJUMP15: Jump with offset 15 if TOS is non-NIL. Pops TOS."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos))
      (incf (vm-pc vm) 16))))

;;; ===========================================================================
;; EXTENDED JUMPS
;;; ===========================================================================

(defop jumpx :hexcode #xB0 :instruction-length 2
  "JUMPX: Unconditional jump with signed 8-bit offset.
Per Maiko, the offset is relative to the opcode byte, not the next PC."
  :operands ((offset :int8 "Signed 8-bit jump offset"))
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (let* ((base-pc (1- (vm-pc vm)))
         (offset (read-pc-8-signed vm)))
    (setf (vm-pc vm) (+ base-pc offset))))

(defop jumpxx :hexcode #xB1 :instruction-length 3
  "JUMPXX: Unconditional jump with signed 16-bit offset.
Per Maiko, the offset is relative to the opcode byte, not the next PC."
  :operands ((offset :int16-be "Signed 16-bit jump offset"))
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (let* ((base-pc (1- (vm-pc vm)))
         (offset (read-pc-16-be-signed vm)))
    (setf (vm-pc vm) (+ base-pc offset))))

(defop fjumpx :hexcode #xB2 :instruction-length 2
  "FJUMPX: Pop TOS; if it was NIL, jump by signed 8-bit offset."
  :operands ((offset :int8 "Signed 8-bit jump offset"))
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let* ((base-pc (1- (vm-pc vm)))
         (tos (pop-stack vm))
         (offset (read-pc-8-signed vm)))
    (when (zerop tos)
      (setf (vm-pc vm) (+ base-pc offset)))))

(defop tjumpx :hexcode #xB3 :instruction-length 2
  "TJUMPX: Pop TOS; if it was non-NIL, jump by signed 8-bit offset."
  :operands ((offset :int8 "Signed 8-bit jump offset"))
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let* ((base-pc (1- (vm-pc vm)))
         (tos (pop-stack vm))
         (offset (read-pc-8-signed vm)))
    (when (not (zerop tos))
      (setf (vm-pc vm) (+ base-pc offset)))))

(defop nfjumpx :hexcode #xB4 :instruction-length 2
  "NFJUMPX: If TOS is NIL, jump by signed 8-bit offset and keep TOS.
If TOS is non-NIL, pop it and continue."
  :operands ((offset :int8 "Signed 8-bit jump offset"))
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (let* ((base-pc (1- (vm-pc vm)))
         (tos (vm-tos vm))
         (offset (read-pc-8-signed vm)))
    (if (zerop tos)
        (setf (vm-pc vm) (+ base-pc offset))
        (vm-pop vm))))

(defop ntjumpx :hexcode #xB5 :instruction-length 2
  "NTJUMPX: If TOS is non-NIL, jump by signed 8-bit offset and keep TOS.
If TOS is NIL, pop it and continue."
  :operands ((offset :int8 "Signed 8-bit jump offset"))
  :stack-effect nil
  :category :control-flow
  :side-effects t
  (let* ((base-pc (1- (vm-pc vm)))
         (tos (vm-tos vm))
         (offset (read-pc-8-signed vm)))
    (if (not (zerop tos))
        (setf (vm-pc vm) (+ base-pc offset))
        (vm-pop vm))))

;;; ===========================================================================
;; FUNCTION CALL (FN0-4, FNX)
;;; ===========================================================================

(defop fn0 :hexcode #x08 :instruction-length 3
  "FN0: Call function with 0 arguments.
Reads 2-byte atom index, looks up function definition, calls it."
  :operands ((atom-index :atom-index "Atom index of function"))
  :stack-effect nil
  :category :function-call
  :side-effects t
  (let* ((atom-idx (read-pc-16-be vm))
         (defcell (laiko.data:read-defcell (vm-virtual-memory vm) atom-idx)))
    (when (laiko.data:is-c-code defcell)
      (error 'laiko.utils:vm-error :message "FN0: C code not supported"))
    (let ((fnheader-offset (laiko.data:get-function-header defcell)))
      (when (zerop fnheader-offset)
        (error 'laiko.utils:vm-error :message "FN0: Undefined function"))
      ;; TODO: Implement function call
      nil)))

(defop fn1 :hexcode #x09 :instruction-length 3
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

(defop fn2 :hexcode #x0A :instruction-length 3
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

(defop fn3 :hexcode #x0B :instruction-length 3
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

(defop fn4 :hexcode #x0C :instruction-length 3
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

(defop fnx :hexcode #x0D :instruction-length 4
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

(defop applyfn :hexcode #x0E :instruction-length 1
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

;; Helper functions moved to laiko/src/vm/dispatch.lisp
