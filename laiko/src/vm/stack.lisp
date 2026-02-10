(in-package :maiko-lisp.vm)

;; Stack frame structure (matches C FX)
;; Per data-model.md

(defstruct (stack-frame (:conc-name sf-))
  "Stack frame structure"
  (next-block 0 :type maiko-lisp.utils:lisp-ptr)
  (link 0 :type maiko-lisp.utils:lisp-ptr)
  (fn-header 0 :type maiko-lisp.utils:lisp-ptr)
  (pc-offset 0 :type maiko-lisp.utils:dlword))

;; Binding marker constants
(defconstant +bind-marker-msb+ #x80000000
  "MSB set in upper word of binding marker")

(defconstant +unbound-marker+ #xFFFFFFFF
  "Marker for unbound PVAR slots")

;; VM state structure
(defstruct (vm (:conc-name vm-))
  "VM state structure"
  (current-frame nil :type (or null stack-frame))
  (stack nil :type (simple-array maiko-lisp.utils:lisp-ptr (*)))
  (stack-ptr 0 :type (integer 0 *))
  (stack-size 0 :type (integer 0 *))
  (pvar-base 0 :type (integer 0 *))
  (pvar-ptr 0 :type (integer 0 *))
  (pvar nil :type (simple-array maiko-lisp.utils:lisp-ptr (*)))
  (pvar-size 0 :type (integer 0 *))
  (storage nil :type (or null maiko-lisp.memory:storage))
  (virtual-memory nil :type (or null (simple-array (or null (simple-array (unsigned-byte 8) (*))) (*))))
  (fptovp nil :type (or null (simple-array (unsigned-byte 16) (*))))
  (gc nil :type (or null maiko-lisp.memory:gc))
  (interrupt-state nil :type (or null interrupt-state))
  (pc 0 :type maiko-lisp.utils:lisp-ptr)
  (return-pc nil :type (or null maiko-lisp.utils:lisp-ptr))
  (registers nil :type (simple-array maiko-lisp.utils:lisp-ptr (*))))

(defun create-vm (stack-size &key (pvar-size 256))
  "Initialize VM with given stack size and PVAR size"
  (declare (type (integer 1 *) stack-size pvar-size))
  (let ((stack-mem (make-array stack-size
                                :element-type 'maiko-lisp.utils:lisp-ptr
                                :initial-element 0))
        (pvar-mem (make-array pvar-size
                               :element-type 'maiko-lisp.utils:lisp-ptr
                               :initial-element 0))
        (regs (make-array 4
                          :element-type 'maiko-lisp.utils:lisp-ptr
                          :initial-element 0)))
    (make-vm :stack stack-mem
             :stack-ptr 0
             :stack-size stack-size
             :pvar-base 0
             :pvar-ptr 0
             :pvar pvar-mem
             :pvar-size pvar-size
             :pc 0
             :return-pc nil
             :registers regs)))

(defun allocate-stack-frame (vm size)
  "Allocate stack frame of given size (in DLwords)"
  (declare (type vm vm)
           (type (integer 1 *) size))
  (let ((stack-ptr (vm-stack-ptr vm))
        (stack-size (vm-stack-size vm)))
    (when (> (+ stack-ptr size) stack-size)
      (maiko-lisp.vm:set-interrupt-flag vm :stack-overflow)
      (error 'maiko-lisp.utils:stack-overflow
             :message (format nil "Stack frame allocation would overflow: need ~A, have ~A"
                             size (- stack-size stack-ptr))))
    (let ((new-frame (make-stack-frame
                      :next-block 0
                      :link (if (vm-current-frame vm)
                               stack-ptr
                               0)
                      :fn-header 0
                      :pc-offset 0)))
      (setf (vm-stack-ptr vm) (+ stack-ptr size))
      (setf (vm-current-frame vm) new-frame)
      new-frame)))

(defun free-stack-frame (vm frame)
  "Free stack frame (restore previous frame)"
  (declare (type vm vm)
           (type stack-frame frame))
  (let ((link (sf-link frame)))
    (if (zerop link)
        (setf (vm-current-frame vm) nil)
        (setf (vm-current-frame vm) nil))
    (let ((stack-ptr (vm-stack-ptr vm)))
      (when (> stack-ptr 0)
        (setf (vm-stack-ptr vm) (1- stack-ptr))))))

(defun push-stack (vm value)
  "Push value onto stack. Stack grows down."
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (let ((stack-ptr (vm-stack-ptr vm))
        (stack-size (vm-stack-size vm))
        (stack (vm-stack vm)))
    (when (>= stack-ptr stack-size)
      (error 'maiko-lisp.utils:stack-overflow
             :message "Stack overflow"))
    (setf (aref stack stack-ptr) value)
    (setf (vm-stack-ptr vm) (1+ stack-ptr))
    value))

(defun pop-stack (vm)
  "Pop value from stack"
  (declare (type vm vm))
  (let ((stack-ptr (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    (when (<= stack-ptr 0)
      (error 'maiko-lisp.utils:stack-overflow
             :message "Stack underflow"))
    (setf (vm-stack-ptr vm) (1- stack-ptr))
    (aref stack (vm-stack-ptr vm))))

(defun get-top-of-stack (vm)
  "Get top of stack without popping"
  (declare (type vm vm))
  (let ((stack-ptr (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    (if (<= stack-ptr 0)
        0
        (aref stack (1- stack-ptr)))))

(defun set-top-of-stack (vm value)
  "Set top of stack"
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (let ((stack-ptr (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    (when (<= stack-ptr 0)
      (error 'maiko-lisp.utils:stack-overflow
             :message "Stack empty, cannot set top"))
    (setf (aref stack (1- stack-ptr)) value)
    value))
