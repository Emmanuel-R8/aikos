(in-package :laiko-tests)

(defun test-bind-debug ()
  "Debug BIND operation step by step"
  (format t "~%Debugging BIND operation...~%")
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Set up pvar-ptr
    (format t "  Initial pvar-ptr: ~A~%" (laiko.vm::vm-pvar-ptr vm))
    (setf (laiko.vm::vm-pvar-ptr vm) 10)
    (format t "  After set, pvar-ptr: ~A~%" (laiko.vm::vm-pvar-ptr vm))

    ;; Push value
    (laiko.vm:push-stack vm 42)
    (format t "  After push: stack-ptr=~A, stack[0]=~A~%"
            (laiko.vm:vm-stack-ptr vm)
            (aref (laiko.vm:vm-stack vm) 0))

    ;; Now call bind
    (format t "  Calling handle-bind with operand 0x11 (n1=1, n2=1)~%")
    (laiko.vm:handle-bind vm '(#x11))

    (format t "  After bind: stack-ptr=~A~%" (laiko.vm:vm-stack-ptr vm))
    (format t "  After bind: pvar-ptr=~A~%" (laiko.vm::vm-pvar-ptr vm))
    (format t "  After bind: marker at stack[1]=~X~%" (aref (laiko.vm:vm-stack vm) 1))

    ;; Check pvar slots
    (let ((slot0 (laiko.vm:get-pvar-slot vm 0))
          (slot1 (laiko.vm:get-pvar-slot vm 1)))
      (format t "  PVAR0 (at offset 10): ~A~%" slot0)
      (format t "  PVAR1 (at offset 11): ~A~%" slot1))))

(test-bind-debug)
