(in-package :laiko-tests)

;; Sysout loading tests
;; Per task T029: Test cases for loading and validating sysout files

(defun test-validate-sysout ()
  "Test sysout validation"
  (let ((ifpage (laiko.data:make-ifpage
                 :keyval #x12345678
                 :lversion 1
                 :minbversion 1
                 :process-size 16
                 :nactivepages 10
                 :fptovpstart 0
                 :storagefullstate 0
                 :stackbase 0
                 :endofstack 0
                 :currentfxp 0)))
    (assert (laiko.data:validate-sysout ifpage) nil "Valid sysout should pass validation")))

(defun test-validate-sysout-invalid-keyval ()
  "Test sysout validation with invalid keyval"
  (let ((ifpage (laiko.data:make-ifpage
                 :keyval #xDEADBEEF ; Invalid keyval
                 :lversion 1
                 :minbversion 1
                 :process-size 16
                 :nactivepages 10
                 :fptovpstart 0
                 :storagefullstate 0
                 :stackbase 0
                 :endofstack 0
                 :currentfxp 0)))
    (assert (not (laiko.data:validate-sysout ifpage)) nil "Invalid keyval should fail validation")))

(defun test-load-sysout-file-not-found ()
  "Test loading non-existent sysout file"
  (handler-case
      (progn
        (laiko.data:load-sysout "/nonexistent/path/to/sysout")
        (assert nil nil "Should have raised sysout-load-failed error"))
    (laiko.utils:sysout-load-failed (err)
      (assert t nil "Correctly raised sysout-load-failed error"))))

(defun run-sysout-tests ()
  "Run all sysout tests"
  (format t "Running sysout tests...~%")
  (test-validate-sysout)
  (format t "  ✓ Validate sysout test passed~%")
  (test-validate-sysout-invalid-keyval)
  (format t "  ✓ Validate sysout invalid keyval test passed~%")
  (test-load-sysout-file-not-found)
  (format t "  ✓ Load sysout file not found test passed~%")
  (format t "All sysout tests passed!~%"))
