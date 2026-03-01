(in-package :laiko-tests)

;; Mouse event tests
;; Per task T050: Test cases for mouse event translation

(defun test-update-get-mouse-position ()
  "Test mouse position update and retrieval"
  (laiko.io:update-mouse-position 100 200)
  (multiple-value-bind (x y)
      (laiko.io:get-mouse-position)
    (assert (= x 100) nil "X coordinate should be 100")
    (assert (= y 200) nil "Y coordinate should be 200")))

(defun test-translate-mouse-event ()
  "Test mouse event translation"
  (laiko.io:update-mouse-position 50 75)
  (let ((os-event nil)) ; Stub OS event
    (let ((mouse-event (laiko.io:translate-mouse-event os-event)))
      (assert mouse-event nil "Mouse event should be created")
      (assert (= (laiko.io:mouse-x mouse-event) 50) nil "X should match current position")
      (assert (= (laiko.io:mouse-y mouse-event) 75) nil "Y should match current position"))))

(defun test-mouse-event-structure ()
  "Test mouse event structure creation"
  (let ((event (laiko.io:make-mouse-event
                :x 10
                :y 20
                :buttons 1
                :pressed-p t)))
    (assert (= (laiko.io:mouse-x event) 10) nil "X should be 10")
    (assert (= (laiko.io:mouse-y event) 20) nil "Y should be 20")
    (assert (= (laiko.io:mouse-buttons event) 1) nil "Buttons should be 1")
    (assert (laiko.io:mouse-pressed-p event) nil "Pressed flag should be true")))

(defun run-mouse-tests ()
  "Run all mouse tests"
  (format t "Running mouse tests...~%")
  (test-update-get-mouse-position)
  (format t "  ✓ Update/get mouse position test passed~%")
  (test-translate-mouse-event)
  (format t "  ✓ Translate mouse event test passed~%")
  (test-mouse-event-structure)
  (format t "  ✓ Mouse event structure test passed~%")
  (format t "All mouse tests passed!~%"))
