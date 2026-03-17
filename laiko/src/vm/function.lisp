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
    ;; link is a DLword offset from Stackspace
    (let ((link (sf-link current-frame)))
      
      ;; If link is 0, we are at the top level and have nowhere to return to.
      (when (zerop link)
        (format t "Execution complete (returned from top frame)~%")
        (laiko.vm:trace-end)
        (laiko:quit 0))

      ;; Read caller's frame extension from VM
      (let ((caller-fx (laiko.data:read-fx-from-vm (vm-virtual-memory vm) link)))
        
        ;; Restore Frame Pointer (FP) to caller's FX
        (setf (vm-frame-pointer-offset vm) 
              (+ laiko.data:+stackspace-byte-offset+ (* link 2)))

        ;; Restore Stack Pointer (SP) to caller's nextblock
        ;; SP = nextblock * 2 - 4
        (let ((nextblock (laiko.data:fx-nextblock caller-fx)))
          (setf (vm-stack-ptr-offset vm) 
                (+ laiko.data:+stackspace-byte-offset+ (- (* nextblock 2) 4))))

        ;; Restore PC
        (let* ((fnheader (laiko.data:fx-fnheader caller-fx))
               (pc-offset (laiko.data:fx-pc caller-fx))
               ;; PC = (fnheader * 2) + pc-offset
               (new-pc (+ (* fnheader 2) pc-offset)))
          (setf (vm-pc vm) new-pc))
          
        ;; Restore current-frame struct
        (setf (vm-current-frame vm)
              (make-stack-frame
               :next-block (laiko.data:fx-nextblock caller-fx)
               :link (laiko.data:fx-alink caller-fx)
               :fn-header (laiko.data:fx-fnheader caller-fx)
               :pc-offset (laiko.data:fx-pc caller-fx)))

        ;; Push return value onto caller's stack
        (vm-push vm return-value)

        return-value))))
