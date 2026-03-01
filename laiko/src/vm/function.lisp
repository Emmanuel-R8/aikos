(in-package :laiko.vm)

;; Function call handling
;; Per rewrite documentation vm-core/function-calls.md

(defun call-function (vm func-header args)
  "Call function with arguments.
   func-header is a function-header structure.
   args is a list of argument values (LispPTR)."
  (declare (type vm vm)
           (type laiko.data:function-header func-header)
           (type list args))
  ;; Save current PC as return address
  (let ((return-pc (vm-pc vm))
        (current-frame (vm-current-frame vm))
        (num-args (laiko.data:get-num-args func-header))
        (num-locals (laiko.data:get-num-locals func-header)))

    ;; Validate argument count
    (when (/= (length args) num-args)
      (error 'laiko.utils:vm-error
             :message (format nil "Function expects ~A args, got ~A" num-args (length args))))

    ;; Allocate new stack frame
    ;; Frame size = args + locals + frame overhead (4 DLwords for frame structure)
    (let ((frame-size (+ num-args num-locals 4)))

      ;; Check stack overflow
      (let ((new-stack-ptr (+ (vm-stack-ptr vm) frame-size)))
        (when (> new-stack-ptr (vm-stack-size vm))
          (laiko.vm:set-interrupt-flag vm :stack-overflow)
          (error 'laiko.utils:stack-overflow
                 :message "Stack overflow in function call")))

      ;; Allocate stack frame
      (let ((new-frame (allocate-stack-frame vm frame-size)))
        ;; Set frame metadata
        (setf (sf-fn-header new-frame) (laiko.data:get-start-pc func-header))
        (setf (sf-link new-frame) (if current-frame
                                      (vm-stack-ptr vm)
                                      0))

        ;; Push arguments onto stack (in reverse order for Lisp calling convention)
        (dolist (arg (reverse args))
          (push-stack vm arg))

        ;; Set new frame as current
        (setf (vm-current-frame vm) new-frame)

        ;; Set PC to function start
        (setf (vm-pc vm) (laiko.data:get-start-pc func-header))
        (setf (vm-return-pc vm) return-pc)

        new-frame))))

(defun return-from-function (vm return-value)
  "Return from function.
   Pops the current stack frame, restores PC to return address, and returns VALUE."
  (declare (type vm vm)
           (type laiko.utils:lisp-ptr return-value))
  (let ((current-frame (vm-current-frame vm)))
    (unless current-frame
      (error 'laiko.utils:vm-error
             :message "No current frame to return from"))

    ;; Get the link (caller's stack pointer where frame data is stored)
    (let ((link (sf-link current-frame))
          (return-pc (vm-return-pc vm)))
      (unless return-pc
        (error 'laiko.utils:vm-error
               :message "No return PC set"))

      ;; Restore stack pointer to caller's position
      (setf (vm-stack-ptr vm) link)

      ;; Restore previous frame from stored data at caller's stack position
      (if (zerop link)
          (setf (vm-current-frame vm) nil)
          (let* ((stack (vm-stack vm))
                 (caller-link (aref stack link))
                 (caller-fn-header (aref stack (+ link 1)))
                 (caller-pc-offset (aref stack (+ link 2))))
            (setf (vm-current-frame vm)
                  (make-stack-frame
                   :next-block 0
                   :link caller-link
                   :fn-header caller-fn-header
                   :pc-offset caller-pc-offset))))

      ;; Restore PC
      (setf (vm-pc vm) return-pc)
      (setf (vm-return-pc vm) nil)

      ;; Push return value onto caller's stack
      (push-stack vm return-value)

      return-value)))
