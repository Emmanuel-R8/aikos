(in-package :laiko-tests)

(defun test-call-return ()
  "Test basic function call and return"
  (format t "~%Testing function call and return...~%")
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Push argument and function object
    (laiko.vm:push-stack vm 42)
    (laiko.vm:push-stack vm 100)

    ;; Verify initial stack state (2 items * 4 bytes/item = 8 bytes)
    (assert (= (laiko.vm:vm-stack-ptr-offset vm) 8) nil "Stack should have 8 bytes (2 items)")
    (format t "  Initial stack-ptr-offset: ~A~%" (laiko.vm:vm-stack-ptr-offset vm))

    ;; Test push/pop
    (let ((val (laiko.vm:pop-stack vm)))
      (assert (= val 100) nil "Pop should return 100, got ~A" val)
      (format t "  Pop returned: ~A~%" val))
    (assert (= (laiko.vm:vm-stack-ptr-offset vm) 4) nil "Stack should have 4 bytes after pop")

    (let ((val (laiko.vm:pop-stack vm)))
      (assert (= val 42) nil "Pop should return 42, got ~A" val)
      (format t "  Pop returned: ~A~%" val))
    (assert (= (laiko.vm:vm-stack-ptr-offset vm) 0) nil "Stack should be empty")

    (format t "  Function call/return tests passed!~%")))

(defun test-pvar-operations ()
  "Test PVAR slot operations"
  (format t "~%Testing PVAR operations...~%")
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Allocate some PVAR slots
    (let ((offset (laiko.vm::allocate-pvar-slots vm 3)))
      (assert (= offset 0) nil "First allocation should start at 0, got ~A" offset)
      (format t "  Allocated 3 slots starting at: ~A~%" offset))

    ;; Set some values
    (laiko.vm::set-pvar-slot vm 0 10)
    (laiko.vm::set-pvar-slot vm 1 20)
    (laiko.vm::set-pvar-slot vm 2 30)

    ;; Read them back
    (assert (= (laiko.vm::get-pvar-slot vm 0) 10) nil "PVAR0 should be 10")
    (assert (= (laiko.vm::get-pvar-slot vm 1) 20) nil "PVAR1 should be 20")
    (assert (= (laiko.vm::get-pvar-slot vm 2) 30) nil "PVAR2 should be 30")

    (format t "  PVAR0: ~A~%" (laiko.vm::get-pvar-slot vm 0))
    (format t "  PVAR1: ~A~%" (laiko.vm::get-pvar-slot vm 1))
    (format t "  PVAR2: ~A~%" (laiko.vm::get-pvar-slot vm 2))

    (format t "  PVAR operation tests passed!~%")))

(defun test-stack-frame ()
  "Test stack frame operations"
  (format t "~%Testing stack frame operations...~%")
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Create a stack frame of 10 DLwords (20 bytes)
    (let ((frame (laiko.vm:allocate-stack-frame vm 10)))
      (assert frame nil "Frame allocation should succeed")
      (format t "  Allocated frame: ~A~%" frame)
      (assert (= (laiko.vm:vm-stack-ptr-offset vm) 20) nil
              "Stack ptr offset should be 20 after allocating frame of size 10 DLwords"))

    (format t "  Stack frame tests passed!~%")))

(defun run-vm-tests ()
  "Run all VM tests"
  (format t "~%=== Running VM Core Tests ===~%")
  (test-call-return)
  (test-pvar-operations)
  (test-stack-frame)
  (format t "~%=== All VM Core Tests Passed ===~%"))

;; Run tests
(run-vm-tests)
