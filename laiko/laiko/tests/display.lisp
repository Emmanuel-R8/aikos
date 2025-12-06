(in-package :maiko-lisp-tests)

;; Display rendering tests
;; Per task T051: Test cases for BitBLT operations

(defun test-init-destroy-display ()
  "Test display initialization and destruction"
  (let ((display (maiko-lisp.display:init-display 640 480)))
    (assert display nil "Display should be created")
    (assert (= (maiko-lisp.display:display-width display) 640) nil "Width should be 640")
    (assert (= (maiko-lisp.display:display-height display) 480) nil "Height should be 480")
    (maiko-lisp.display:destroy-display display)
    (format t "  Display destroyed successfully~%")))

(defun test-render-region ()
  "Test rendering a region to display"
  (let ((display (maiko-lisp.display:init-display 100 100))
        (buffer (make-array (* 10 10 4) ; 10x10 RGBA
                           :element-type '(unsigned-byte 8)
                           :initial-element 255))) ; White
    ;; Render region at (10, 10) with size 10x10
    (maiko-lisp.display:render-region display 10 10 10 10 buffer)
    ;; Verify buffer was copied (check a pixel)
    (let ((display-buffer (maiko-lisp.display:display-buffer display))
          (offset (+ (* 10 100 4) (* 10 4)))) ; (10, 10) in 100x100 display
      (assert (= (aref display-buffer offset) 255) nil "Pixel should be white"))
    (maiko-lisp.display:destroy-display display)))

(defun test-bitblt-copy ()
  "Test BitBLT COPY operation"
  (let ((display (maiko-lisp.display:init-display 100 100)))
    ;; Initialize source region with pattern
    (let ((buffer (maiko-lisp.display:display-buffer display)))
      ;; Set source region (10,10) to 20x20 with pattern
      (loop for y from 10 below 30
            do (loop for x from 10 below 30
                     for offset = (+ (* y 100 4) (* x 4))
                     do (setf (aref buffer offset) 128))) ; Gray
      ;; BitBLT from (10,10) to (50,50)
      (maiko-lisp.display:bitblt display 10 10 20 20 50 50 0) ; 0 = COPY
      ;; Verify destination region
      (let ((dst-offset (+ (* 50 100 4) (* 50 4))))
        (assert (= (aref buffer dst-offset) 128) nil "Destination pixel should be copied"))
      (maiko-lisp.display:destroy-display display))))

(defun run-display-tests ()
  "Run all display tests"
  (format t "Running display tests...~%")
  (test-init-destroy-display)
  (format t "  ✓ Init/destroy display test passed~%")
  (test-render-region)
  (format t "  ✓ Render region test passed~%")
  (test-bitblt-copy)
  (format t "  ✓ BitBLT copy test passed~%")
  (format t "All display tests passed!~%"))