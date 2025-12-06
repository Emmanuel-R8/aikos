(in-package :maiko-lisp.vm)

;; Stack frame structure (matches C FX)
;; Per data-model.md

(defstruct (stack-frame (:conc-name sf-))
  "Stack frame structure"
  (next-block 0 :type maiko-lisp.utils:lisp-ptr)
  (link 0 :type maiko-lisp.utils:lisp-ptr)
  (fn-header 0 :type maiko-lisp.utils:lisp-ptr)
  (pc-offset 0 :type maiko-lisp.utils:dlword))

;; VM state structure
(defstruct (vm (:conc-name vm-))
  "VM state structure"
  (current-frame nil :type (or null stack-frame))
  (stack nil :type (simple-array maiko-lisp.utils:dlword (*)))
  (stack-ptr 0 :type (integer 0 *)) ; Index into stack array (stack grows down, so starts at 0)
  (stack-size 0 :type (integer 0 *)) ; Total stack size
  (storage nil :type (or null maiko-lisp.memory:storage))
  (virtual-memory nil :type (or null maiko-lisp.memory:virtual-memory))
  (gc nil :type (or null maiko-lisp.memory:gc))
  (interrupt-state nil :type (or null interrupt-state))
  (pc 0 :type maiko-lisp.utils:lisp-ptr)
  (return-pc nil :type (or null maiko-lisp.utils:lisp-ptr)))

(defun make-vm (stack-size)
  "Initialize VM with given stack size"
  (declare (type (integer 1 *) stack-size))
  (let ((stack-mem (make-array stack-size
                                :element-type 'maiko-lisp.utils:dlword
                                :initial-element 0)))
    (make-vm :stack stack-mem
             :stack-ptr 0 ; Stack grows down, so start at top (index 0)
             :stack-size stack-size
             :pc 0
             :return-pc nil)))

(defun allocate-stack-frame (vm size)
  "Allocate stack frame of given size (in DLwords)"
  (declare (type vm vm)
           (type (integer 1 *) size))
  (let ((stack-ptr (vm-stack-ptr vm))
        (stack-size (vm-stack-size vm))
        (stack (vm-stack vm)))
    ;; Check if we have enough space
    (when (> (+ stack-ptr size) stack-size)
      (maiko-lisp.vm:set-interrupt-flag vm :stack-overflow)
      (error 'maiko-lisp.utils:stack-overflow
             :message (format nil "Stack frame allocation would overflow: need ~A, have ~A"
                             size (- stack-size stack-ptr))))
    ;; Create new frame
    (let ((new-frame (make-stack-frame
                      :next-block 0
                      :link (if (vm-current-frame vm)
                               stack-ptr
                               0)
                      :fn-header 0
                      :pc-offset 0)))
      ;; Update stack pointer
      (setf (vm-stack-ptr vm) (+ stack-ptr size))
      ;; Set as current frame
      (setf (vm-current-frame vm) new-frame)
      new-frame)))

(defun free-stack-frame (vm frame)
  "Free stack frame (restore previous frame)"
  (declare (type vm vm)
           (type stack-frame frame))
  (let ((link (sf-link frame)))
    (if (zerop link)
        ;; No previous frame
        (setf (vm-current-frame vm) nil)
        ;; TODO: Restore previous frame from link
        (setf (vm-current-frame vm) nil))
    ;; Update stack pointer (simplified - full implementation needs frame size)
    (let ((stack-ptr (vm-stack-ptr vm)))
      (when (> stack-ptr 0)
        (decf (vm-stack-ptr vm))))))

(defun push-stack (vm value)
  "Push value onto stack. Stack grows down (higher indices = lower addresses)."
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (let ((stack-ptr-index (vm-stack-ptr vm))
        (stack-size (vm-stack-size vm))
        (stack (vm-stack vm)))
    ;; Check stack overflow (stack grows down, so check if we've reached the end)
    (when (>= stack-ptr-index stack-size)
      (error 'maiko-lisp.utils:stack-overflow
             :message "Stack overflow"))
    ;; Store value at current position
    (setf (aref stack stack-ptr-index) (maiko-lisp.utils:loloc value))
    ;; Move stack pointer down by 1 DLword (increment index)
    (setf (vm-stack-ptr vm) (1+ stack-ptr-index))
    value))

(defun pop-stack (vm)
  "Pop value from stack"
  (declare (type vm vm))
  (let ((stack-ptr-index (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    ;; Check stack underflow
    (when (<= stack-ptr-index 0)
      (error 'maiko-lisp.utils:stack-overflow
             :message "Stack underflow"))
    ;; Move stack pointer up by 1 DLword (decrement index)
    (decf (vm-stack-ptr vm))
    ;; Get value from top of stack
    (aref stack (vm-stack-ptr vm))))

(defun get-top-of-stack (vm)
  "Get top of stack without popping"
  (declare (type vm vm))
  (let ((stack-ptr-index (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    (if (<= stack-ptr-index 0)
        0 ; Stack empty
        (aref stack (1- stack-ptr-index)))))

(defun set-top-of-stack (vm value)
  "Set top of stack"
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (let ((stack-ptr-index (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    (when (<= stack-ptr-index 0)
      (error 'maiko-lisp.utils:stack-overflow
             :message "Stack empty, cannot set top"))
    (setf (aref stack (1- stack-ptr-index)) (maiko-lisp.utils:loloc value))
    value))