(in-package :maiko-lisp.vm)

;; Variable access operations
;; ivar0-6, pvar0-6, fvar0-6, gvar

(defun get-ivar (vm index)
  "Get local variable at index from current frame"
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (aref (vm-stack vm) (+ (vm-stack-ptr vm) index)))))

(defun handle-ivar0 (vm) "IVAR0: Access local variable 0" (declare (type vm vm)) (push-stack vm (get-ivar vm 0)))
(defun handle-ivar1 (vm) "IVAR1: Access local variable 1" (declare (type vm vm)) (push-stack vm (get-ivar vm 1)))
(defun handle-ivar2 (vm) "IVAR2: Access local variable 2" (declare (type vm vm)) (push-stack vm (get-ivar vm 2)))
(defun handle-ivar3 (vm) "IVAR3: Access local variable 3" (declare (type vm vm)) (push-stack vm (get-ivar vm 3)))
(defun handle-ivar4 (vm) "IVAR4: Access local variable 4" (declare (type vm vm)) (push-stack vm (get-ivar vm 4)))
(defun handle-ivar5 (vm) "IVAR5: Access local variable 5" (declare (type vm vm)) (push-stack vm (get-ivar vm 5)))
(defun handle-ivar6 (vm) "IVAR6: Access local variable 6" (declare (type vm vm)) (push-stack vm (get-ivar vm 6)))

(defun get-pvar (vm index)
  "Get parameter variable at index from current frame"
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((frame (vm-current-frame vm)))
    (when frame
      (aref (vm-stack vm) (- (vm-stack-ptr vm) 1 index)))))

(defun handle-pvar0 (vm) "PVAR0: Access parameter 0" (declare (type vm vm)) (push-stack vm (get-pvar vm 0)))
(defun handle-pvar1 (vm) "PVAR1: Access parameter 1" (declare (type vm vm)) (push-stack vm (get-pvar vm 1)))
(defun handle-pvar2 (vm) "PVAR2: Access parameter 2" (declare (type vm vm)) (push-stack vm (get-pvar vm 2)))
(defun handle-pvar3 (vm) "PVAR3: Access parameter 3" (declare (type vm vm)) (push-stack vm (get-pvar vm 3)))
(defun handle-pvar4 (vm) "PVAR4: Access parameter 4" (declare (type vm vm)) (push-stack vm (get-pvar vm 4)))
(defun handle-pvar5 (vm) "PVAR5: Access parameter 5" (declare (type vm vm)) (push-stack vm (get-pvar vm 5)))
(defun handle-pvar6 (vm) "PVAR6: Access parameter 6" (declare (type vm vm)) (push-stack vm (get-pvar vm 6)))

(defun get-fvar (vm index)
  "Get free variable at index from closure.
   FVAR accesses variables from enclosing lexical scopes.
   The value is stored in the closure's environment."
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((current-frame (vm-current-frame vm)))
    (when current-frame
      (let ((fn-header (sf-fn-header current-frame)))
        (when fn-header
          (let ((closure-env (maiko-lisp.data:get-closure-environment fn-header)))
            (when closure-env
              (aref closure-env index))))))))

(defun handle-fvar0 (vm)
  "FVAR0: Access free variable 0 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 0)))

(defun handle-fvar1 (vm)
  "FVAR1: Access free variable 1 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 1)))

(defun handle-fvar2 (vm)
  "FVAR2: Access free variable 2 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 2)))

(defun handle-fvar3 (vm)
  "FVAR3: Access free variable 3 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 3)))

(defun handle-fvar4 (vm)
  "FVAR4: Access free variable 4 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 4)))

(defun handle-fvar5 (vm)
  "FVAR5: Access free variable 5 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 5)))

(defun handle-fvar6 (vm)
  "FVAR6: Access free variable 6 from closure"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 6)))

(defun handle-gvar (vm operands)
  "GVAR: Access global variable.
   GVAR looks up a global variable by atom index and pushes its value.
   Instruction: 5 bytes (opcode + 4-byte operand)
   Atom index = operand & 0xFFFF for Valspace access."
  (declare (type vm vm)
           (type list operands))
  ;; Need 4 bytes of operands
  (when (>= (length operands) 2)
    ;; Atom index is in first 2 bytes of operand (non-BIGATOMS mode)
    (let ((atom-index (logior (first operands) (ash (second operands) 8))))
      (format t "GVAR: atom-index=~X~%" atom-index)
      ;; Read value from value cell
      (let ((value (maiko-lisp.data:read-atom-value vm atom-index)))
        (vm-push vm value)
        (format t "  value=~X~%" value)))))

(defun handle-arg0 (vm)
  "ARG0: Access argument 0 (same as PVAR0)"
  (declare (type vm vm))
  (handle-pvar0 vm))

(defun handle-myargcount (vm)
  "MYARGCOUNT: Get argument count for current function"
  (declare (type vm vm))
  (let ((frame (vm-current-frame vm)))
    (if frame
        (let ((fn-header (sf-fn-header frame)))
          (if fn-header
              (push-stack vm (maiko-lisp.data:get-num-args fn-header))
              (push-stack vm 0)))
        (push-stack vm 0))))

(defun set-pvar (vm index)
  "Set parameter variable at index from stack"
  (declare (type vm vm)
           (type (integer 0 6) index))
  (pop-stack vm))

(defun handle-pvarsetpop0 (vm) "PVARSETPOP0: Set parameter 0" (declare (type vm vm)) (set-pvar vm 0))
(defun handle-pvarsetpop1 (vm) "PVARSETPOP1: Set parameter 1" (declare (type vm vm)) (set-pvar vm 1))
(defun handle-pvarsetpop2 (vm) "PVARSETPOP2: Set parameter 2" (declare (type vm vm)) (set-pvar vm 2))
(defun handle-pvarsetpop3 (vm) "PVARSETPOP3: Set parameter 3" (declare (type vm vm)) (set-pvar vm 3))
(defun handle-pvarsetpop4 (vm) "PVARSETPOP4: Set parameter 4" (declare (type vm vm)) (set-pvar vm 4))
(defun handle-pvarsetpop5 (vm) "PVARSETPOP5: Set parameter 5" (declare (type vm vm)) (set-pvar vm 5))
(defun handle-pvarsetpop6 (vm) "PVARSETPOP6: Set parameter 6" (declare (type vm vm)) (set-pvar vm 6))
