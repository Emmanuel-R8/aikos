(in-package :maiko-lisp-tests)

;; Memory allocation tests
;; Per task T027: Test cases for cons cell and array allocation

(defun test-cons-cell-allocation ()
  "Test cons cell allocation"
  (let ((storage (maiko-lisp.memory:create-storage :size (* 1024 1024))))
    (let ((ptr (maiko-lisp.memory:allocate-cons-cell storage)))
      (assert (> ptr 0) nil "Cons cell allocation should return valid pointer")
      (let ((cell (maiko-lisp.memory:get-cons-cell storage ptr)))
        (assert cell nil "Should be able to get cons cell")
        (assert (= (maiko-lisp.data:cons-car-field cell) 0) nil "CAR should be 0 initially")
        (assert (= (maiko-lisp.data:cons-cdr-code cell) 0) nil "CDR code should be 0 initially")))))

(defun test-cons-cell-set-get ()
  "Test setting and getting cons cell values"
  (let ((storage (maiko-lisp.memory:create-storage :size (* 1024 1024))))
    (let ((ptr (maiko-lisp.memory:allocate-cons-cell storage)))
      (let ((cell (maiko-lisp.data:make-cons-cell :car-field 42 :cdr-code 8)))
        (maiko-lisp.memory:put-cons-cell storage ptr cell)
        (let ((retrieved (maiko-lisp.memory:get-cons-cell storage ptr)))
          (assert (= (maiko-lisp.data:cons-car-field retrieved) 42) nil "CAR should be 42")
          (assert (= (maiko-lisp.data:cons-cdr-code retrieved) 8) nil "CDR code should be 8")))))

(defun test-storage-full ()
  "Test storage full detection"
  (let ((storage (maiko-lisp.memory:create-storage :size 100)))
    (loop for i from 0 below 10
          do (maiko-lisp.memory:allocate-cons-cell storage))
    (let ((full (maiko-lisp.memory::check-storage-full storage)))
      (assert (typep full 'boolean) nil "check-storage-full should return boolean"))))

(defun run-memory-tests ()
  "Run all memory tests"
  (format t "Running memory tests...~%")
  (test-cons-cell-allocation)
  (format t "  ✓ Cons cell allocation test passed~%")
  (test-cons-cell-set-get)
  (format t "  ✓ Cons cell set/get test passed~%")
  (test-storage-full)
  (format t "  ✓ Storage full test passed~%")
  (format t "All memory tests passed!~%"))
)
