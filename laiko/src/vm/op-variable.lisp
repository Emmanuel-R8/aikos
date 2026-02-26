(in-package :maiko-lisp.vm)

;; Variable access operations
;; ivar0-6, pvar0-6, fvar0-6, gvar, arg0, myargcount
;; pvarsetpop0-6

;;; ===========================================================================
;; INSTANCE VARIABLE ACCESS (IVAR)
;;; ===========================================================================

(defop ivar0 #x40 1
  "IVAR0: Push value of instance variable 0."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 0)))

(defop ivar1 #x41 1
  "IVAR1: Push value of instance variable 1."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 1)))

(defop ivar2 #x42 1
  "IVAR2: Push value of instance variable 2."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 2)))

(defop ivar3 #x43 1
  "IVAR3: Push value of instance variable 3."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 3)))

(defop ivar4 #x44 1
  "IVAR4: Push value of instance variable 4."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 4)))

(defop ivar5 #x45 1
  "IVAR5: Push value of instance variable 5."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 5)))

(defop ivar6 #x46 1
  "IVAR6: Push value of instance variable 6."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-ivar vm 6)))

;;; ===========================================================================
;; PARAMETER VARIABLE ACCESS (PVAR)
;;; ===========================================================================

(defop pvar0 #x48 1
  "PVAR0: Push value of parameter variable 0."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 0)))

(defop pvar1 #x49 1
  "PVAR1: Push value of parameter variable 1."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 1)))

(defop pvar2 #x4A 1
  "PVAR2: Push value of parameter variable 2."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 2)))

(defop pvar3 #x4B 1
  "PVAR3: Push value of parameter variable 3."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 3)))

(defop pvar4 #x4C 1
  "PVAR4: Push value of parameter variable 4."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 4)))

(defop pvar5 #x4D 1
  "PVAR5: Push value of parameter variable 5."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 5)))

(defop pvar6 #x4E 1
  "PVAR6: Push value of parameter variable 6."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 6)))

;;; ===========================================================================
;; FREE VARIABLE ACCESS (FVAR)
;;; ===========================================================================

(defop fvar0 #x50 1
  "FVAR0: Push value of free variable 0 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 0)))

(defop fvar1 #x51 1
  "FVAR1: Push value of free variable 1 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 1)))

(defop fvar2 #x52 1
  "FVAR2: Push value of free variable 2 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 2)))

(defop fvar3 #x53 1
  "FVAR3: Push value of free variable 3 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 3)))

(defop fvar4 #x54 1
  "FVAR4: Push value of free variable 4 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 4)))

(defop fvar5 #x55 1
  "FVAR5: Push value of free variable 5 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 5)))

(defop fvar6 #x56 1
  "FVAR6: Push value of free variable 6 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 6)))

;;; ===========================================================================
;; GLOBAL VARIABLE ACCESS (GVAR)
;;; ===========================================================================

(defop gvar #x60 5
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
    (let ((value (maiko-lisp.data:read-atom-value vm atom-idx)))
      (vm-push vm value))))

;;; ===========================================================================
;; ARGUMENT ACCESS
;;; ===========================================================================

(defop arg0 #x61 1
  "ARG0: Push value of argument 0 (alias for PVAR0)."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm 0)))

(defop myargcount #x65 1
  "MYARGCOUNT: Push the argument count for the current function."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (let ((frame (vm-current-frame vm)))
    (if frame
        (let ((fn-header (sf-fn-header frame)))
          (if fn-header
              (push-stack vm (maiko-lisp.data:get-num-args fn-header))
              (push-stack vm 0)))
        (push-stack vm 0))))

;;; ===========================================================================
;; PARAMETER VARIABLE SET (PVARSETPOP)
;;; ===========================================================================

(defop pvarsetpop0 #xB8 1
  "PVARSETPOP0: Pop stack and store in parameter variable 0."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t  ; Modifies frame
  (set-pvar vm 0))

(defop pvarsetpop1 #xB9 1
  "PVARSETPOP1: Pop stack and store in parameter variable 1."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 1))

(defop pvarsetpop2 #xBA 1
  "PVARSETPOP2: Pop stack and store in parameter variable 2."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 2))

(defop pvarsetpop3 #xBB 1
  "PVARSETPOP3: Pop stack and store in parameter variable 3."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 3))

(defop pvarsetpop4 #xBC 1
  "PVARSETPOP4: Pop stack and store in parameter variable 4."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 4))

(defop pvarsetpop5 #xBD 1
  "PVARSETPOP5: Pop stack and store in parameter variable 5."
  :operands nil
  :stack-effect (:pop 1)
  :category :variable-access
  :side-effects t
  (set-pvar vm 5))

(defop pvarsetpop6 #xBE 1
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
      (aref (vm-stack vm) (+ (vm-stack-ptr vm) index)))))

(defun get-pvar (vm index)
  "Get parameter variable at index from current frame."
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (aref (vm-stack vm) (- (vm-stack-ptr vm) 1 index)))))

(defun get-fvar (vm index)
  "Get free variable at index from closure environment."
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((current-frame (vm-current-frame vm)))
    (when current-frame
      (let ((fn-header (sf-fn-header current-frame)))
        (when fn-header
          (let ((closure-env (maiko-lisp.data:get-closure-environment fn-header)))
            (when closure-env
              (aref closure-env index))))))))

(defun set-pvar (vm index)
  "Set parameter variable at index from stack (pops TOS)."
  (declare (type vm vm)
           (type (integer 0 6) index))
  (pop-stack vm))

(defun read-pc-32-be (vm)
  "Read 32-bit big-endian value from instruction stream at PC+1.

   Per C: For GVAR, the atom index is read as 4 bytes after the opcode.
   The bytes are in big-endian order: op0<<24 | op1<<16 | op2<<8 | op3.

   This function reads from *current-code* special variable bound by dispatch.
   Returns the 32-bit atom index."
  (declare (type vm vm)
           (special *current-code*))
  (let* ((pc (vm-pc vm))
         (code *current-code*))
    (if (and code
             (< (+ pc 4) (length code)))
        ;; Read 4 bytes after opcode (PC+1 to PC+4)
        (let ((b0 (aref code (+ pc 1)))
              (b1 (aref code (+ pc 2)))
              (b2 (aref code (+ pc 3)))
              (b3 (aref code (+ pc 4))))
          ;; Big-endian: first byte is MSB
          (logior (ash b0 24)
                  (ash b1 16)
                  (ash b2 8)
                  b3))
        ;; Fallback: return 0 if code not available
        0)))
