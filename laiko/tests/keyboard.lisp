(in-package :maiko-lisp-tests)

;; Keyboard event tests
;; Per task T049: Test cases for keycode translation

(defun test-translate-keycode-ascii ()
  "Test keycode translation for ASCII keys"
  (let ((ascii-key (char-code #\A))) ; 'A' = 65
    (let ((lisp-keycode (maiko-lisp.io:translate-keycode ascii-key)))
      (assert (= lisp-keycode ascii-key) nil "ASCII keycode should map directly"))))

(defun test-translate-keycode-extended ()
  "Test keycode translation for extended keys"
  (let ((extended-key 200)) ; Extended key
    (let ((lisp-keycode (maiko-lisp.io:translate-keycode extended-key)))
      (assert (>= lisp-keycode #x80) nil "Extended keycode should be >= 0x80"))))

(defun test-enqueue-dequeue-key-event ()
  "Test keyboard event queue"
  (let ((event (maiko-lisp.io:make-keyboard-event
                :keycode 65
                :modifiers 0
                :pressed-p t)))
    ;; Enqueue event
    (maiko-lisp.io:enqueue-key-event event)
    ;; Dequeue event
    (let ((dequeued (maiko-lisp.io:dequeue-key-event)))
      (assert dequeued nil "Event should be dequeued")
      (assert (= (maiko-lisp.io:kbd-keycode dequeued) 65) nil "Keycode should match")
      (assert (maiko-lisp.io:kbd-pressed-p dequeued) nil "Pressed flag should match"))))

(defun run-keyboard-tests ()
  "Run all keyboard tests"
  (format t "Running keyboard tests...~%")
  (test-translate-keycode-ascii)
  (format t "  ✓ Translate keycode ASCII test passed~%")
  (test-translate-keycode-extended)
  (format t "  ✓ Translate keycode extended test passed~%")
  (test-enqueue-dequeue-key-event)
  (format t "  ✓ Enqueue/dequeue key event test passed~%")
  (format t "All keyboard tests passed!~%"))
