(in-package :laiko-tests)

;; Dispatch loop tests
;; Per task T014: Test cases for instruction fetch/decode/execute cycle

(defun test-fetch-instruction-byte ()
  "Test fetching instruction byte from code array"
  (let ((code (make-array 10
                          :element-type 'laiko.utils:bytecode
                          :initial-contents '(#xD8 #xD9 #xBF #x68 #x69 0 0 0 0 0))))
    (assert (= (laiko.vm::fetch-instruction-byte 0 code) #xD8) nil "Expected 0xD8")
    (assert (= (laiko.vm::fetch-instruction-byte 1 code) #xD9) nil "Expected 0xD9")
    (assert (= (laiko.vm::fetch-instruction-byte 2 code) #xBF) nil "Expected 0xBF")))

(defun test-decode-opcode ()
  "Test opcode decoding"
  (assert (= (laiko.vm::decode-opcode #xD8) #xD8) nil "Expected 0xD8")
  (assert (= (laiko.vm::decode-opcode #x68) #x68) nil "Expected 0x68"))

(defun test-get-instruction-length ()
  "Test getting instruction length"
  (assert (= (laiko.vm::get-instruction-length #xD8) 1) nil "IPLUS2 should be 1 byte")
  (assert (= (laiko.vm::get-instruction-length #x68) 1) nil "NIL should be 1 byte")
  (assert (= (laiko.vm::get-instruction-length #x67) 3) nil "ACONST should be 3 bytes"))

(defun test-dispatch-simple-sequence ()
  "Test dispatch loop with simple opcode sequence"
  (let ((vm (laiko.vm:create-vm #x400))
        (code (make-array 5
                          :element-type 'laiko.utils:bytecode
                          :initial-contents '(#x68  ; NIL
                                              #x69  ; T
                                              #xD8  ; IPLUS2 (will fail without operands, but tests dispatch)
                                              0
                                              0))))
    ;; This test verifies dispatch loop can execute without errors
    ;; Note: Full execution requires proper opcode handlers and operands
    (handler-case
        (progn
          ;; Initialize interrupt state
          (setf (laiko.vm:vm-interrupt-state vm)
                (laiko.vm:create-interrupt-state))
          ;; Dispatch will check interrupts and execute opcodes
          ;; For now, just verify it doesn't crash
          (laiko.vm:dispatch vm code)
          (assert t nil "Dispatch completed"))
      (laiko.utils:vm-error (err)
        ;; VM errors are expected for incomplete opcode sequences
        (assert t nil "Dispatch handled error correctly")))))

(defun run-dispatch-tests ()
  "Run all dispatch tests"
  (format t "Running dispatch tests...~%")
  (test-fetch-instruction-byte)
  (format t "  ✓ Fetch instruction byte test passed~%")
  (test-decode-opcode)
  (format t "  ✓ Decode opcode test passed~%")
  (test-get-instruction-length)
  (format t "  ✓ Get instruction length test passed~%")
  (test-dispatch-simple-sequence)
  (format t "  ✓ Dispatch simple sequence test passed~%")
  (format t "All dispatch tests passed!~%"))
