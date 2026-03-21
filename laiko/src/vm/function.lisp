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
   Restores caller state using Maiko's fast OPRETURN path and preserves VALUE in cached TOS."
  (declare (type vm vm)
            (type laiko.utils:lisp-ptr return-value))
  (let ((current-frame-offset nil)
        (virtual-memory (vm-virtual-memory vm)))
    (unless virtual-memory
      (error 'laiko.utils:vm-error
             :message "No virtual memory available for RETURN"))
    (when (< (vm-frame-pointer-offset vm) laiko.data:+stackspace-byte-offset+)
      (error 'laiko.utils:vm-error
             :message (format nil "Invalid frame pointer 0x~X in RETURN"
                              (vm-frame-pointer-offset vm))))
    (setf current-frame-offset
          (ash (- (vm-frame-pointer-offset vm) laiko.data:+stackspace-byte-offset+) -1))
    (let* ((current-fx (laiko.data:read-fx-from-vm virtual-memory current-frame-offset))
           (alink (laiko.data:fx-alink current-fx)))
      (when (zerop alink)
        (format t "Execution complete (returned from top frame)~%")
        (laiko.vm:trace-end)
        (laiko:quit 0))
      (when (logbitp 0 alink)
        (error 'laiko.utils:vm-error
               :message (format nil "Slow RETURN path not yet implemented (alink=0x~X)" alink)))
      (when (< alink laiko.data:+framesize+)
        (error 'laiko.utils:vm-error
               :message (format nil "Invalid fast RETURN alink 0x~X" alink)))
      (let* ((return-fx-offset (- alink laiko.data:+framesize+))
             (return-fx-byte-offset (+ laiko.data:+stackspace-byte-offset+
                                       (* return-fx-offset 2)))
             (ivar-offset (vm-read-word vm (- return-fx-byte-offset 2)))
             (return-fx (laiko.data:read-fx-from-vm virtual-memory return-fx-offset))
             (new-pc (+ (* 2 (laiko.data:fx-fnheader return-fx))
                        (laiko.data:fx-pc return-fx))))
        ;; Maiko OPRETURN restores CSTKPTRL to IVAR, not to nextblock/TOS.
        ;; Cached TOPOFSTACK remains the return value through FastRetCALL.
        (setf (vm-frame-pointer-offset vm) return-fx-byte-offset)
        (setf (vm-stack-ptr-offset vm)
              (+ laiko.data:+stackspace-byte-offset+ (* ivar-offset 2)))
        (setf (vm-pc vm) new-pc)
        (setf (vm-current-frame vm)
              (make-stack-frame
               :next-block (laiko.data:fx-nextblock return-fx)
               :link (laiko.data:fx-alink return-fx)
               :fn-header (laiko.data:fx-fnheader return-fx)
               :pc-offset (laiko.data:fx-pc return-fx)))
        (setf (vm-top-of-stack vm) return-value)
        return-value))))
