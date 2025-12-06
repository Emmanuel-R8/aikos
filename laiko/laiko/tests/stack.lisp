(in-package :maiko-lisp-tests)

;; Stack management tests
;; Per task T013: Test cases for frame allocation and stack operations

(defun test-push-pop ()
  "Test basic push and pop operations"
  (let ((vm (maiko-lisp.vm:make-vm 1024)))
    ;; Push values
    (maiko-lisp.vm:push-stack vm 10)
    (maiko-lisp.vm:push-stack vm 20)
    (maiko-lisp.vm:push-stack vm 30)
    ;; Pop and check
    (assert (= (maiko-lisp.vm:pop-stack vm) 30) nil "Expected 30")
    (assert (= (maiko-lisp.vm:pop-stack vm) 20) nil "Expected 20")
    (assert (= (maiko-lisp.vm:pop-stack vm) 10) nil "Expected 10")))

(defun test-stack-frame-allocation ()
  "Test stack frame allocation"
  (let ((vm (maiko-lisp.vm:make-vm 1024)))
    ;; Allocate a stack frame
    (let ((frame (maiko-lisp.vm:allocate-stack-frame vm 16)))
      (assert frame nil "Frame allocation should succeed")
      (assert (maiko-lisp.vm:vm-current-frame vm) nil "Current frame should be set")
      (assert (= (sf-link frame) 0) nil "Link should be 0 for first frame"))))

(defun test-stack-overflow ()
  "Test stack overflow detection"
  (let ((vm (maiko-lisp.vm:make-vm 10))) ; Small stack
    ;; Try to allocate frame larger than stack
    (handler-case
        (progn
          (maiko-lisp.vm:allocate-stack-frame vm 20)
          (assert nil nil "Should have raised stack-overflow error"))
      (maiko-lisp.utils:stack-overflow (err)
        (assert t nil "Stack overflow correctly detected")))))

(defun test-get-top-of-stack ()
  "Test getting top of stack without popping"
  (let ((vm (maiko-lisp.vm:make-vm 1024)))
    (maiko-lisp.vm:push-stack vm 42)
    (let ((top (maiko-lisp.vm:get-top-of-stack vm)))
      (assert (= top 42) nil "Expected 42")
      ;; Stack should still have the value
      (assert (= (maiko-lisp.vm:pop-stack vm) 42) nil "Value should still be on stack"))))

(defun test-set-top-of-stack ()
  "Test setting top of stack"
  (let ((vm (maiko-lisp.vm:make-vm 1024)))
    (maiko-lisp.vm:push-stack vm 10)
    (maiko-lisp.vm:set-top-of-stack vm 99)
    (assert (= (maiko-lisp.vm:pop-stack vm) 99) nil "Expected 99")))

(defun run-stack-tests ()
  "Run all stack tests"
  (format t "Running stack tests...~%")
  (test-push-pop)
  (format t "  ✓ Push/pop test passed~%")
  (test-stack-frame-allocation)
  (format t "  ✓ Stack frame allocation test passed~%")
  (test-stack-overflow)
  (format t "  ✓ Stack overflow test passed~%")
  (test-get-top-of-stack)
  (format t "  ✓ Get top of stack test passed~%")
  (test-set-top-of-stack)
  (format t "  ✓ Set top of stack test passed~%")
  (format t "All stack tests passed!~%"))