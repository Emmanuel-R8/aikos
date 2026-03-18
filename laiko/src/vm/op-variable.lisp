(in-package :laiko.vm)

;; Variable access operations
;; ivar0-6, pvar0-6, fvar0-6, gvar, arg0, myargcount
;; pvarsetpop0-6

;;; ===========================================================================
;; INSTANCE VARIABLE ACCESS (IVAR)
;;; ===========================================================================

(defop ivar0 :hexcode #x40 :instruction-length 1
  "IVAR0: Push value of instance variable 0."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 0)))

(defop ivar1 :hexcode #x41 :instruction-length 1
  "IVAR1: Push value of instance variable 1."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 1)))

(defop ivar2 :hexcode #x42 :instruction-length 1
  "IVAR2: Push value of instance variable 2."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 2)))

(defop ivar3 :hexcode #x43 :instruction-length 1
  "IVAR3: Push value of instance variable 3."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 3)))

(defop ivar4 :hexcode #x44 :instruction-length 1
  "IVAR4: Push value of instance variable 4."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 4)))

(defop ivar5 :hexcode #x45 :instruction-length 1
  "IVAR5: Push value of instance variable 5."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 5)))

(defop ivar6 :hexcode #x46 :instruction-length 1
  "IVAR6: Push value of instance variable 6."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 6)))

;;; ===========================================================================
;; PARAMETER VARIABLE ACCESS (PVAR)
;;; ===========================================================================

(defop pvar0 :hexcode #x48 :instruction-length 1
  "PVAR0: Push value of parameter variable 0."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 0)))

(defop pvar1 :hexcode #x49 :instruction-length 1
  "PVAR1: Push value of parameter variable 1."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 1)))

(defop pvar2 :hexcode #x4A :instruction-length 1
  "PVAR2: Push value of parameter variable 2."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 2)))

(defop pvar3 :hexcode #x4B :instruction-length 1
  "PVAR3: Push value of parameter variable 3."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 3)))

(defop pvar4 :hexcode #x4C :instruction-length 1
  "PVAR4: Push value of parameter variable 4."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 4)))

(defop pvar5 :hexcode #x4D :instruction-length 1
  "PVAR5: Push value of parameter variable 5."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 5)))

(defop pvar6 :hexcode #x4E :instruction-length 1
  "PVAR6: Push value of parameter variable 6."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 6)))

;;; ===========================================================================
;; FREE VARIABLE ACCESS (FVAR)
;;; ===========================================================================

(defop fvar0 :hexcode #x50 :instruction-length 1
  "FVAR0: Push value of free variable 0 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 0)))

(defop fvar1 :hexcode #x51 :instruction-length 1
  "FVAR1: Push value of free variable 1 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 1)))

(defop fvar2 :hexcode #x52 :instruction-length 1
  "FVAR2: Push value of free variable 2 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 2)))

(defop fvar3 :hexcode #x53 :instruction-length 1
  "FVAR3: Push value of free variable 3 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 3)))

(defop fvar4 :hexcode #x54 :instruction-length 1
  "FVAR4: Push value of free variable 4 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 4)))

(defop fvar5 :hexcode #x55 :instruction-length 1
  "FVAR5: Push value of free variable 5 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 5)))

(defop fvar6 :hexcode #x56 :instruction-length 1
  "FVAR6: Push value of free variable 6 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 6)))

;;; ===========================================================================
;; PARAMETER VARIABLE SET WITHOUT POP (PVAR_)
;;; ===========================================================================

(defop pvar_0 :hexcode #x58 :instruction-length 1
  "PVAR_0: Store cached TOS into parameter variable 0 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 0))

(defop pvar_1 :hexcode #x59 :instruction-length 1
  "PVAR_1: Store cached TOS into parameter variable 1 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 1))

(defop pvar_2 :hexcode #x5A :instruction-length 1
  "PVAR_2: Store cached TOS into parameter variable 2 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 2))

(defop pvar_3 :hexcode #x5B :instruction-length 1
  "PVAR_3: Store cached TOS into parameter variable 3 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 3))

(defop pvar_4 :hexcode #x5C :instruction-length 1
  "PVAR_4: Store cached TOS into parameter variable 4 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 4))

(defop pvar_5 :hexcode #x5D :instruction-length 1
  "PVAR_5: Store cached TOS into parameter variable 5 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 5))

(defop pvar_6 :hexcode #x5E :instruction-length 1
  "PVAR_6: Store cached TOS into parameter variable 6 without popping."
  :operands nil
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm 6))

(defop pvarx_ :hexcode #x5F :instruction-length 2
  "PVARX_: Store cached TOS into parameter variable N without popping."
  :operands ((index :uint8 "Parameter variable index"))
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-pvar vm (read-pc-8 vm)))

;;; ===========================================================================
;; GLOBAL VARIABLE ACCESS (GVAR)
;;; ===========================================================================

(defop gvar :hexcode #x60 :instruction-length 5
  "GVAR: Push value of global variable (atom value cell).
 Reads 4-byte atom index from instruction stream.
 For BIGVM: atom_index = op0<<24 | op1<<16 | op2<<8 | op3.
 For BIGVM, uses full 32-bit atom index (no masking).
 For non-BIGATOMS, uses Valspace[atom_index]."
  :operands ((atom-index :uint32-be "Atom index (4 bytes, big-endian)"))
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  ;; Read full 32-bit atom index and pass to read-atom-value
  ;; For BIGVM, the full index is used (no 16-bit masking)
  ;; The read-atom-value function handles LITATOM vs NEWATOM dispatch
  (let ((atom-idx (read-pc-32-be vm)))
    (let ((value (laiko.data:read-atom-value (vm-virtual-memory vm) atom-idx)))
      (vm-push vm value))))

(defop gvar_ :hexcode #x17 :instruction-length 5
  "GVAR_: Store TOS into a global variable's value cell and leave TOS unchanged."
  :operands ((atom-index :uint32-be "Atom index (4 bytes, big-endian)"))
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (let ((atom-idx (read-pc-32-be vm)))
    (laiko.data:write-atom-value (vm-virtual-memory vm) atom-idx (vm-tos vm))))

;;; ===========================================================================
;; ARGUMENT ACCESS
;;; ===========================================================================

(defop arg0 :hexcode #x61 :instruction-length 1
  "ARG0: Push value of argument 0 (alias for PVAR0)."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 0)))

(defop myargcount :hexcode #x65 :instruction-length 1
  "MYARGCOUNT: Push the argument count for the current function."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (let ((frame (vm-current-frame vm)))
    (if frame
        (let ((fn-header (sf-fn-header frame)))
          (if fn-header
              (push-stack vm (laiko.data:get-num-args fn-header))
              (push-stack vm 0)))
        (push-stack vm 0))))

;;; ===========================================================================
;; PARAMETER VARIABLE SET (PVARSETPOP)
;;; ===========================================================================

(defop pvarsetpop0 :hexcode #xB8 :instruction-length 1
  "PVARSETPOP0: Pop stack and store in parameter variable 0."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t  ; Modifies frame
  (set-pvar vm 0))

(defop pvarsetpop1 :hexcode #xB9 :instruction-length 1
  "PVARSETPOP1: Pop stack and store in parameter variable 1."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 1))

(defop pvarsetpop2 :hexcode #xBA :instruction-length 1
  "PVARSETPOP2: Pop stack and store in parameter variable 2."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 2))

(defop pvarsetpop3 :hexcode #xBB :instruction-length 1
  "PVARSETPOP3: Pop stack and store in parameter variable 3."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 3))

(defop pvarsetpop4 :hexcode #xBC :instruction-length 1
  "PVARSETPOP4: Pop stack and store in parameter variable 4."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 4))

(defop pvarsetpop5 :hexcode #xBD :instruction-length 1
  "PVARSETPOP5: Pop stack and store in parameter variable 5."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 5))

(defop pvarsetpop6 :hexcode #xBE :instruction-length 1
  "PVARSETPOP6: Pop stack and store in parameter variable 6."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 6))

;;; ===========================================================================
;; HELPER FUNCTIONS (not opcodes)
;;; ===========================================================================

(defun get-ivar (vm index)
  "Get instance variable at index from current frame."
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (vm-read-lispptr vm (+ (vm-stack-ptr-offset vm)
                             (* index 4))))))

(defun get-pvar (vm index)
  "Get parameter variable at index from current frame."
  (declare (type vm vm)
            (type (integer 0 6) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (vm-read-lispptr vm (+ (vm-frame-pointer-offset vm)
                             (* laiko.data:+framesize+ 2)
                             (* index 4))))))

(defun get-fvar (vm index)
  "Get free variable at index from closure environment."
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((current-frame (vm-current-frame vm)))
    (when current-frame
      (let ((fn-header (sf-fn-header current-frame)))
        (when fn-header
          (let ((closure-env (laiko.data:get-closure-environment fn-header)))
            (when closure-env
              (aref closure-env index))))))))

(defun set-pvar (vm index)
  "Set parameter variable at index from stack (pops TOS)."
  (declare (type vm vm)
            (type (integer 0 6) index))
  (write-pvar vm index)
  (pop-stack vm))

(defun write-pvar (vm index)
  "Store cached TOS into parameter variable INDEX without popping."
  (declare (type vm vm)
           (type (integer 0 *) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (vm-write-lispptr vm (+ (vm-frame-pointer-offset vm)
                              (* laiko.data:+framesize+ 2)
                              (* index 4))
                       (vm-tos vm)))))

;; read-pc-32-be moved to laiko/src/vm/dispatch.lisp
