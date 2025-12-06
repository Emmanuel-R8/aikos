(in-package :maiko-lisp.vm)

;; Function call handling
;; Per rewrite documentation vm-core/function-calls.md

(defun call-function (vm func-header args)
  "Call function with arguments.
   func-header is a function-header structure.
   args is a list of argument values (LispPTR)."
  (declare (type vm vm)
           (type maiko-lisp.data:function-header func-header)
           (type list args))
  ;; Save current PC as return address
  (let ((return-pc (vm-pc vm))
        (current-frame (vm-current-frame vm))
        (num-args (maiko-lisp.data:get-num-args func-header))
        (num-locals (maiko-lisp.data:get-num-locals func-header)))

    ;; Validate argument count
    (when (/= (length args) num-args)
      (error 'maiko-lisp.utils:vm-error
             :message (format nil "Function expects ~A args, got ~A" num-args (length args))))

    ;; Allocate new stack frame
    ;; Frame size = args + locals + frame overhead (4 DLwords for frame structure)
    (let ((frame-size (+ num-args num-locals 4)))

      ;; Check stack overflow
      (let ((new-stack-ptr (+ (vm-stack-ptr vm) frame-size)))
        (when (> new-stack-ptr (vm-stack-size vm))
          (maiko-lisp.vm:set-interrupt-flag vm :stack-overflow)
          (error 'maiko-lisp.utils:stack-overflow
                 :message "Stack overflow in function call")))

      ;; Allocate stack frame
      (let ((new-frame (allocate-stack-frame vm frame-size)))
        ;; Set frame metadata
        (setf (sf-fn-header new-frame) (maiko-lisp.data:fn-startpc func-header))
        (setf (sf-link new-frame) (if current-frame
                                      (vm-stack-ptr vm)
                                      0))

        ;; Push arguments onto stack (in reverse order for Lisp calling convention)
        (dolist (arg (reverse args))
          (push-stack vm arg))

        ;; Set new frame as current
        (setf (vm-current-frame vm) new-frame)

        ;; Set PC to function start
        (setf (vm-pc vm) (maiko-lisp.data:get-start-pc func-header))
        (setf (vm-return-pc vm) return-pc)

        new-frame))))

(defun return-from-function (vm)
  "Return from function.
   Pops the current stack frame and restores PC to return address."
  (declare (type vm vm))
  (let ((current-frame (vm-current-frame vm)))
    (unless current-frame
      (error 'maiko-lisp.utils:vm-error
             :message "No current frame to return from"))

    ;; Get return PC
    (let ((return-pc (vm-return-pc vm)))
      (unless return-pc
        (error 'maiko-lisp.utils:vm-error
               :message "No return PC set")))

    ;; Restore previous frame
    (let ((link (sf-link current-frame)))
      (if (zerop link)
          (setf (vm-current-frame vm) nil)
          ;; TODO: Restore frame from link
          (setf (vm-current-frame vm) nil)))

    ;; Restore PC
    (setf (vm-pc vm) (vm-return-pc vm))
    (setf (vm-return-pc vm) nil)

    ;; Return value should be on top of stack
    (get-top-of-stack vm)))