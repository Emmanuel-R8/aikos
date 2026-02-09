(in-package :maiko-lisp.vm)

;; Interrupt handling
;; Per rewrite documentation vm-core/interrupt-handling.md

;; Interrupt state structure (matches C INTSTAT)
(defstruct (interrupt-state (:conc-name int-))
  "Interrupt state structure matching C INTSTAT"
  (log-file-io nil :type boolean)
  (ether-interrupt nil :type boolean)
  (io-interrupt nil :type boolean)
  (gc-disabled nil :type boolean)
  (vmem-full nil :type boolean)
  (stack-overflow nil :type boolean)
  (storage-full nil :type boolean)
  (waiting-interrupt nil :type boolean)
  (int-char-code 0 :type maiko-lisp.utils:dlword))

(defun create-interrupt-state ()
  "Create new interrupt state with all flags cleared"
  (make-interrupt-state))

(defun check-interrupts (vm)
  "Check for pending interrupts. Returns true if interrupt is pending."
  (declare (type vm vm))
  (let ((int-state (vm-interrupt-state vm)))
    (if int-state
        (int-waiting-interrupt int-state)
        nil)))

(defun set-interrupt-flag (vm flag-name)
  "Set interrupt flag by name"
  (declare (type vm vm)
           (type symbol flag-name))
  (let ((int-state (or (vm-interrupt-state vm)
                       (setf (vm-interrupt-state vm) (make-interrupt-state)))))
    (ecase flag-name
      (:log-file-io (setf (int-log-file-io int-state) t))
      (:ether-interrupt (setf (int-ether-interrupt int-state) t))
      (:io-interrupt (setf (int-io-interrupt int-state) t))
      (:gc-disabled (setf (int-gc-disabled int-state) t))
      (:vmem-full (setf (int-vmem-full int-state) t))
      (:stack-overflow (setf (int-stack-overflow int-state) t))
      (:storage-full (setf (int-storage-full int-state) t))
      (:waiting-interrupt (setf (int-waiting-interrupt int-state) t)))
    (when (not (eq flag-name :waiting-interrupt))
      (setf (int-waiting-interrupt int-state) t))))

(defun clear-interrupt-flag (vm flag-name)
  "Clear interrupt flag by name"
  (declare (type vm vm)
           (type symbol flag-name))
  (let ((int-state (vm-interrupt-state vm)))
    (when int-state
      (ecase flag-name
        (:log-file-io (setf (int-log-file-io int-state) nil))
        (:ether-interrupt (setf (int-ether-interrupt int-state) nil))
        (:io-interrupt (setf (int-io-interrupt int-state) nil))
        (:gc-disabled (setf (int-gc-disabled int-state) nil))
        (:vmem-full (setf (int-vmem-full int-state) nil))
        (:stack-overflow (setf (int-stack-overflow int-state) nil))
        (:storage-full (setf (int-storage-full int-state) nil))
        (:waiting-interrupt (setf (int-waiting-interrupt int-state) nil))))))

(defun handle-interrupt (vm interrupt-type)
  "Handle interrupt of given type"
  (declare (type vm vm)
           (type symbol interrupt-type))
  (let ((int-state (vm-interrupt-state vm)))
    (unless int-state
      (return-from handle-interrupt))

    (ecase interrupt-type
      (:io
       (when (int-io-interrupt int-state)
         ;; TODO: Process I/O events
         (clear-interrupt-flag vm :io-interrupt)))
      (:timer
       ;; TODO: Process timer events
       )
      (:storage-full
       (when (int-storage-full int-state)
         ;; Trigger GC
         ;; TODO: Call GC function
         (clear-interrupt-flag vm :storage-full)))
      (:stack-overflow
       (when (int-stack-overflow int-state)
         (error 'maiko-lisp.utils:stack-overflow
                :message "Stack overflow detected")))
      (:log-file-io
       (when (int-log-file-io int-state)
         ;; TODO: Process log file I/O
         (clear-interrupt-flag vm :log-file-io)))
      (:ether-interrupt
       (when (int-ether-interrupt int-state)
         ;; TODO: Process network events
         (clear-interrupt-flag vm :ether-interrupt)))))

  ;; Clear waiting interrupt flag if no interrupts remain
  (let ((int-state (vm-interrupt-state vm)))
    (when (and int-state
               (not (int-log-file-io int-state))
               (not (int-ether-interrupt int-state))
               (not (int-io-interrupt int-state))
               (not (int-storage-full int-state))
               (not (int-stack-overflow int-state)))
      (setf (int-waiting-interrupt int-state) nil))))
