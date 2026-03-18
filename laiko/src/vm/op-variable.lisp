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

(defop pvarx :hexcode #x4F :instruction-length 2
  "PVARX: Push indexed parameter variable."
  :operands ((index :uint8 "Parameter variable index"))
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-pvar vm (read-pc-8 vm))))

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
  (push-stack vm (get-fvar vm 2)))

(defop fvar2 :hexcode #x52 :instruction-length 1
  "FVAR2: Push value of free variable 2 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 4)))

(defop fvar3 :hexcode #x53 :instruction-length 1
  "FVAR3: Push value of free variable 3 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 6)))

(defop fvar4 :hexcode #x54 :instruction-length 1
  "FVAR4: Push value of free variable 4 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 8)))

(defop fvar5 :hexcode #x55 :instruction-length 1
  "FVAR5: Push value of free variable 5 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 10)))

(defop fvar6 :hexcode #x56 :instruction-length 1
  "FVAR6: Push value of free variable 6 from closure environment."
  :operands nil
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm 12)))

(defop fvarx :hexcode #x57 :instruction-length 2
  "FVARX: Push indexed free variable using a DLword offset into the PVAR area."
  :operands ((offset :uint8 "Free-variable slot DLword offset"))
  :stack-effect (:push 1)
  :category :variable-access
  :side-effects nil
  (push-stack vm (get-fvar vm (read-pc-8 vm))))

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

(defop ivarx_ :hexcode #x62 :instruction-length 2
  "IVARX_: Store cached TOS into instance variable N without popping."
  :operands ((index :uint8 "Instance variable index"))
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-ivar vm (read-pc-8 vm)))

(defop fvarx_ :hexcode #x63 :instruction-length 2
  "FVARX_: Store cached TOS into free variable N without popping."
  :operands ((offset :uint8 "Free-variable slot DLword offset"))
  :stack-effect nil
  :category :variable-access
  :side-effects t
  (write-fvar vm (read-pc-8 vm)))

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
           (type (integer 0 *) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (vm-read-lispptr vm (+ (vm-stack-ptr-offset vm)
                             (* index 4))))))

(defun get-pvar (vm index)
  "Get parameter variable at index from current frame."
  (declare (type vm vm)
            (type (integer 0 *) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (vm-read-lispptr vm (+ (vm-frame-pointer-offset vm)
                             (* laiko.data:+framesize+ 2)
                             (* index 4))))))

(defun %fvar-chain-byte-offset (vm dlword-offset)
  (declare (type vm vm)
           (type (integer 0 *) dlword-offset))
  (+ (vm-frame-pointer-offset vm)
     (* laiko.data:+framesize+ 2)
     (* dlword-offset 2)))

(defun %stack-byte-offset->stack-offset (byte-offset)
  (declare (type (unsigned-byte 32) byte-offset))
  (- (ash byte-offset -1) laiko.data:+stk-offset-dlword+))

(defun %stack-offset->laddr (stack-offset)
  (declare (type (unsigned-byte 32) stack-offset))
  (+ laiko.data:+stk-offset-dlword+ stack-offset))

(defun %read-current-function-header (vm)
  (declare (type vm vm))
  (let ((frame (vm-current-frame vm)))
    (unless frame
      (error 'laiko.utils:vm-error :message "FVAR requires a current frame"))
    (let* ((fnheader-laddr (sf-fn-header frame))
           (fnheader-byte-offset (* fnheader-laddr 2))
           (ntsize (vm-read-word vm (+ fnheader-byte-offset 12)))
           (locals/fvars (vm-read-word vm (+ fnheader-byte-offset 14))))
      (list :laddr fnheader-laddr
            :byte-offset fnheader-byte-offset
            :na (vm-read-word vm (+ fnheader-byte-offset 2))
            :pv (vm-read-word vm (+ fnheader-byte-offset 4))
            :startpc (vm-read-word vm (+ fnheader-byte-offset 6))
            :ntsize ntsize
            :nlocals (ash locals/fvars -8)
            :fvaroffset (logand locals/fvars #xFF)))))

(defun %header-nametable-count (header)
  (ash (getf header :ntsize) -1))

(defun %header-name-entry-byte-offset (header index)
  (+ (getf header :byte-offset)
     (* 2 (logior 8 (getf header :fvaroffset)))
     (* index 4)))

(defun %header-type-entry-byte-offset (header index)
  (+ (%header-name-entry-byte-offset header 0)
     (* 2 (getf header :ntsize))
     (* index 4)))

(defun %read-name-entry (vm header index)
  (declare (type vm vm)
           (type (integer 0 *) index))
  (vm-read-lispptr vm (%header-name-entry-byte-offset header index)))

(defun %read-type-entry (vm header index)
  (declare (type vm vm)
           (type (integer 0 *) index))
  (vm-read-lispptr vm (%header-type-entry-byte-offset header index)))

(defun %find-free-variable-name (vm header slot-index)
  (declare (type vm vm)
           (type (integer 0 *) slot-index))
  (let ((name-index (- slot-index (getf header :nlocals)))
        (entry-count (%header-nametable-count header)))
    (when (minusp name-index)
      (error 'laiko.utils:vm-error
             :message (format nil "Invalid FVAR slot ~D before nlocals ~D"
                              slot-index (getf header :nlocals))))
    (vm-read-lispptr vm (+ (%header-name-entry-byte-offset header 0)
                           (* name-index 4)))))

(defun %read-frame-for-fvar-scan (vm fx-byte-offset)
  (declare (type vm vm)
           (type (unsigned-byte 32) fx-byte-offset))
  (laiko.data:read-fx-from-vm (vm-virtual-memory vm)
                              (%stack-byte-offset->stack-offset fx-byte-offset)))

(defun %frame-header-for-fvar-scan (vm fx)
  (declare (type vm vm)
           (type laiko.data:frame-extension fx))
  (let* ((header-laddr (if (logtest laiko.data::+fx-validnametable-mask+
                                   (laiko.data:fx-flags fx))
                           (laiko.data:fx-nametable fx)
                           (laiko.data:fx-fnheader fx)))
         (header-byte-offset (* header-laddr 2))
         (ntsize (vm-read-word vm (+ header-byte-offset 12)))
         (locals/fvars (vm-read-word vm (+ header-byte-offset 14))))
    (list :laddr header-laddr
          :byte-offset header-byte-offset
          :ntsize ntsize
          :nlocals (ash locals/fvars -8)
          :fvaroffset (logand locals/fvars #xFF))))

(defun %cache-fvar-global (vm chain-byte-offset atom-index)
  (declare (type vm vm)
           (type (unsigned-byte 32) chain-byte-offset atom-index))
  (vm-write-lispptr vm chain-byte-offset
                    (ash (laiko.data:get-valcell (vm-virtual-memory vm) atom-index) -1)))

(defun %cache-fvar-stack-address (vm chain-byte-offset stack-byte-offset)
  (declare (type vm vm)
           (type (unsigned-byte 32) chain-byte-offset stack-byte-offset))
  (vm-write-lispptr vm chain-byte-offset (ash stack-byte-offset -1)))

(defun %resolve-unbound-fvar (vm chain-byte-offset dlword-offset)
  (declare (type vm vm)
           (type (unsigned-byte 32) chain-byte-offset)
           (type (integer 0 *) dlword-offset))
  (let* ((header (%read-current-function-header vm))
         (slot-index (ash dlword-offset -1))
         (target-name (%find-free-variable-name vm header slot-index))
         (fx-byte-offset (vm-frame-pointer-offset vm)))
    (loop
      (let* ((current-fx (%read-frame-for-fvar-scan vm fx-byte-offset))
             (alink (laiko.data:fx-alink current-fx)))
        (when (zerop alink)
          (error 'laiko.utils:vm-error
                 :message "alink = 0 during FVAR lookup"))
        (when (= alink #x000B)
          (%cache-fvar-global vm chain-byte-offset target-name)
          (return))
        (setf fx-byte-offset (+ laiko.data:+stackspace-byte-offset+
                                (* (- (logand alink #xFFFE) laiko.data:+framesize+) 2)))
        (let* ((scan-fx (%read-frame-for-fvar-scan vm fx-byte-offset))
               (scan-header (%frame-header-for-fvar-scan vm scan-fx))
               (entry-count (%header-nametable-count scan-header))
               (matched nil))
          (loop for i from 0 below entry-count
                until matched
                do (when (= (%read-name-entry vm scan-header i) target-name)
                     (let* ((type-entry (%read-type-entry vm scan-header i))
                            (fvar-type (logand type-entry #xFF000000))
                            (fvar-offset (ash (logand type-entry #x0FFFFFFF) 1)))
                       (cond
                         ((= fvar-type #x80000000)
                          (let ((candidate-byte-offset (+ fx-byte-offset
                                                          (* laiko.data:+framesize+ 2)
                                                          (* fvar-offset 2))))
                            (unless (= (vm-read-word vm candidate-byte-offset) #xFFFF)
                              (%cache-fvar-stack-address vm chain-byte-offset candidate-byte-offset)
                              (setf matched t))))
                         ((= fvar-type #xC0000000)
                          (let ((candidate-byte-offset (+ fx-byte-offset
                                                          (* laiko.data:+framesize+ 2)
                                                          (* fvar-offset 2))))
                            (unless (logbitp 0 (vm-read-word vm candidate-byte-offset))
                              (vm-write-lispptr vm chain-byte-offset
                                                (vm-read-lispptr vm candidate-byte-offset))
                              (setf matched t))))
                         ((zerop fvar-type)
                          (let* ((ivar-stack-offset (vm-read-word vm (- fx-byte-offset 2)))
                                 (ivar-laddr (%stack-offset->laddr (+ ivar-stack-offset fvar-offset))))
                            (vm-write-lispptr vm chain-byte-offset ivar-laddr)
                            (setf matched t)))
                         (t
                          (error 'laiko.utils:vm-error
                                 :message (format nil "Bad FVAR nametable entry 0x~X"
                                                  type-entry)))))))
          (when matched
            (return)))))))

(defun %ensure-fvar-pointer (vm dlword-offset)
  (declare (type vm vm)
           (type (integer 0 *) dlword-offset))
  (let* ((chain-byte-offset (%fvar-chain-byte-offset vm dlword-offset))
         (low-word (vm-read-word vm chain-byte-offset)))
    (when (logbitp 0 low-word)
      (%resolve-unbound-fvar vm chain-byte-offset dlword-offset))
    (vm-read-lispptr vm chain-byte-offset)))

(defun get-fvar (vm dlword-offset)
  "Get free variable using a DLword offset from the current PVAR base.

Implements the bound-chain case of Maiko FVAR/FVARX. If the slot is still
marked unbound, resolve it through the caller nametable chain and cache the
resolved address back into the current frame slot."
  (declare (type vm vm)
           (type (integer 0 *) dlword-offset))
  (let ((pointer (%ensure-fvar-pointer vm dlword-offset)))
    (vm-read-lispptr vm (* (logand pointer +pointermask+) 2))))

(defun set-pvar (vm index)
  "Set parameter variable at index from stack (pops TOS)."
  (declare (type vm vm)
             (type (integer 0 6) index))
  (write-pvar vm index)
  (pop-stack vm))

(defun write-ivar (vm index)
  "Store cached TOS into instance variable INDEX without popping."
  (declare (type vm vm)
           (type (integer 0 *) index))
  (vm-write-lispptr vm (+ (vm-stack-ptr-offset vm)
                          (* index 4))
                   (vm-tos vm)))

(defun write-fvar (vm dlword-offset)
  "Store cached TOS into free variable at DLWORD-OFFSET without popping."
  (declare (type vm vm)
           (type (integer 0 *) dlword-offset))
  (let ((pointer (%ensure-fvar-pointer vm dlword-offset)))
    (vm-write-lispptr vm (* (logand pointer +pointermask+) 2)
                     (vm-tos vm))))

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
