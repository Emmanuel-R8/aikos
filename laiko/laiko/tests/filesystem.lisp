(in-package :maiko-lisp-tests)

;; Filesystem tests
;; Per task T052: Test cases for pathname translation

(defun test-translate-pathname-unix ()
  "Test pathname translation on Unix-like systems"
  (let ((lisp-path "/home/user/file.lisp"))
    (let ((platform-path (maiko-lisp.io:translate-pathname lisp-path)))
      #+win32
      (assert (find #\\ platform-path) nil "Windows path should have backslashes")
      #-win32
      (assert (string= platform-path lisp-path) nil "Unix path should remain unchanged"))))

(defun test-open-close-file ()
  "Test file open and close operations"
  (let ((test-file (format nil "/tmp/maiko-test-~A.tmp" (random 1000000))))
    (handler-case
        (progn
          ;; Open file for output
          (let ((stream (maiko-lisp.io:open-file test-file :output)))
            (assert stream nil "File should be opened")
            (write-byte 65 stream) ; Write 'A'
            (maiko-lisp.io:close-file stream))
          ;; Open file for input
          (let ((stream (maiko-lisp.io:open-file test-file :input)))
            (assert stream nil "File should be opened for reading")
            (let ((byte (read-byte stream)))
              (assert (= byte 65) nil "Should read byte 65"))
            (maiko-lisp.io:close-file stream))
          ;; Cleanup
          (delete-file test-file))
      (file-error (err)
        ;; Cleanup on error
        (ignore-errors (delete-file test-file))
        (error err)))))

(defun run-filesystem-tests ()
  "Run all filesystem tests"
  (format t "Running filesystem tests...~%")
  (test-translate-pathname-unix)
  (format t "  ✓ Translate pathname test passed~%")
  (test-open-close-file)
  (format t "  ✓ Open/close file test passed~%")
  (format t "All filesystem tests passed!~%"))