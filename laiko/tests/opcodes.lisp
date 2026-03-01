(in-package :laiko-tests)

;; Opcode execution tests
;; Per task T012: Test cases for arithmetic opcodes (IPLUS2, IDIFFERENCE, etc.)

(defun test-iplus2 ()
  "Test IPLUS2 opcode: Integer addition"
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Push two values: 10 and 20
    (laiko.vm:push-stack vm 10)
    (laiko.vm:push-stack vm 20)
    ;; Execute IPLUS2 (should pop 20, pop 10, push 30)
    (laiko.vm::handle-iplus2 vm)
    ;; Check result
    (let ((result (laiko.vm:pop-stack vm)))
      (assert (= result 30) nil "IPLUS2: Expected 30, got ~A" result))))

(defun test-idifference ()
  "Test IDIFFERENCE opcode: Integer subtraction"
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Push two values: 20 and 10
    (laiko.vm:push-stack vm 20)
    (laiko.vm:push-stack vm 10)
    ;; Execute IDIFFERENCE (should pop 10, pop 20, push 10)
    (laiko.vm::handle-idifference vm)
    ;; Check result
    (let ((result (laiko.vm:pop-stack vm)))
      (assert (= result 10) nil "IDIFFERENCE: Expected 10, got ~A" result))))

(defun test-itimes2 ()
  "Test ITIMES2 opcode: Integer multiplication"
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Push two values: 5 and 6
    (laiko.vm:push-stack vm 5)
    (laiko.vm:push-stack vm 6)
    ;; Execute ITIMES2
    (laiko.vm::handle-itimes2 vm)
    ;; Check result
    (let ((result (laiko.vm:pop-stack vm)))
      (assert (= result 30) nil "ITIMES2: Expected 30, got ~A" result))))

(defun test-iquo ()
  "Test IQUO opcode: Integer division"
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Push two values: 20 and 5
    (laiko.vm:push-stack vm 20)
    (laiko.vm:push-stack vm 5)
    ;; Execute IQUO
    (laiko.vm::handle-iquo vm)
    ;; Check result
    (let ((result (laiko.vm:pop-stack vm)))
      (assert (= result 4) nil "IQUO: Expected 4, got ~A" result))))

(defun test-irem ()
  "Test IREM opcode: Integer remainder"
  (let ((vm (laiko.vm:create-vm #x400)))
    ;; Push two values: 23 and 5
    (laiko.vm:push-stack vm 23)
    (laiko.vm:push-stack vm 5)
    ;; Execute IREM
    (laiko.vm::handle-irem vm)
    ;; Check result
    (let ((result (laiko.vm:pop-stack vm)))
      (assert (= result 3) nil "IREM: Expected 3, got ~A" result))))

(defun run-opcode-tests ()
  "Run all opcode tests"
  (format t "Running opcode tests...~%")
  (test-iplus2)
  (format t "  ✓ IPLUS2 test passed~%")
  (test-idifference)
  (format t "  ✓ IDIFFERENCE test passed~%")
  (test-itimes2)
  (format t "  ✓ ITIMES2 test passed~%")
  (test-iquo)
  (format t "  ✓ IQUO test passed~%")
  (test-irem)
  (format t "  ✓ IREM test passed~%")
  (format t "All opcode tests passed!~%"))
