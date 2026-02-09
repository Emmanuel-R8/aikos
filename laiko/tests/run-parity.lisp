;;; Parity testing between C and Common Lisp emulators
;;; Compares execution traces to verify correctness

(in-package :maiko-lisp-tests)

;;; Configuration
(defparameter *c-emulator-path*
  #+linux "/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/maiko/build/c/linux.x86_64/lde"
  #-linux nil
  "Path to C emulator executable")

(defparameter *sysout-path*
  "/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/medley/internal/loadups/starter.sysout"
  "Path to sysout file for testing")

(defparameter *c-trace-file* "c_emulator_unified_trace.txt"
  "Temporary file for C emulator trace output")

(defparameter *lisp-trace-file* "lisp_emulator_unified_trace.txt"
  "Temporary file for Lisp emulator trace output")

(defparameter *max-test-steps* 1000
  "Maximum number of steps to compare during testing")

(defun c-emulator-available-p ()
  "Check if C emulator is available"
  (when *c-emulator-path*
    (probe-file *c-emulator-path*)))

(defun run-c-emulator-trace (&optional (max-steps *max-test-steps*))
  "Run C emulator and capture trace output"
  (when (c-emulator-available-p)
    (let ((env-str (format nil "EMULATOR_MAX_STEPS=~D" max-steps)))
      (format t "Running C emulator...~%")
      #+sbcl
      (sb-ext:run-program
       *c-emulator-path*
       (list *sysout-path* "-info")
       :environment (list env-str)
       :output :interactive
       :error :output)
      #-sbcl
      (format t "C emulator run not supported on this Lisp~%"))
    (format t "C emulator trace complete~%")))

(defun run-lisp-emulator-trace (&optional (max-steps *max-test-steps*))
  "Run Lisp emulator and save trace output"
  (format t "Running Lisp emulator...~%")
  #+sbcl
  (let ((sbcl-code
         `(progn
            (require :asdf)
            (load "/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/laiko/maiko-lisp.asd")
            (asdf:load-system :maiko-lisp)
            (setf maiko-lisp.vm:*max-trace-steps* ,max-steps)
            (maiko-lisp:run-emulator
             ,*sysout-path*
             (list "-trace" ,*lisp-trace-file*)))))
    (sb-ext:run-program
     "sbcl"
     (list "--non-interactive" "--eval" (format nil "~S" sbcl-code))
     :input nil
     :output :interactive
     :error :output))
  #-sbcl
  (format t "Lisp emulator run not supported on this Lisp~%")
  (format t "Lisp emulator trace complete~%"))

(defun read-file-lines (path)
  "Read all lines from a file into a vector"
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line into lines
          finally (return (coerce lines 'vector)))))

(defun compare-trace-files (file1 file2)
  "Compare two trace files and report differences"
  (format t "~%Comparing traces: ~A vs ~A~%" file1 file2)
  (let ((lines1 (read-file-lines file1))
        (lines2 (read-file-lines file2))
        (differences nil)
        (line-num 0))
    (loop while (and (< line-num (length lines1))
                     (< line-num (length lines2)))
          do (incf line-num)
             (let ((line1 (aref lines1 (1- line-num)))
                   (line2 (aref lines2 (1- line-num))))
               (unless (string= line1 line2)
                 (push (list line-num line1 line2) differences))))
    (if differences
        (progn
          (format t "~D differences found:~%" (length differences))
          (dolist (diff (reverse differences))
            (format t "Line ~D:~%" (first diff))
            (format t "  C:      ~A~%" (second diff))
            (format t "  Lisp:   ~A~%" (third diff))))
        (format t "No differences found in first ~D lines~%" line-num))
    differences))

(defun run-parity-test (&optional (max-steps *max-test-steps*))
  "Run full parity test between C and Lisp emulators"
  (format t "=== Parity Test ===~%")
  (format t "Sysout: ~A~%" *sysout-path*)
  (format t "Max steps: ~D~%" max-steps)
  
  (cond
    ((not (c-emulator-available-p))
     (format t "WARNING: C emulator not found at ~A~%" *c-emulator-path*)
     (format t "Running only Lisp emulator test...~%")
     (run-lisp-emulator-trace max-steps)
     (format t "Trace saved to: ~A~%" *lisp-trace-file*))
    
    (t
     ;; Run C emulator
     (run-c-emulator-trace max-steps)
     
     ;; Run Lisp emulator
     (run-lisp-emulator-trace max-steps)
     
     ;; Compare traces
     (let ((diffs (compare-trace-files *c-trace-file* *lisp-trace-file*)))
       (format t "~%=== Test Complete ===~%")
       (if diffs
           (format t "FAILED: ~D differences found~%" (length diffs))
           (format t "PASSED: No differences found~%"))
       diffs))))

(defun quick-parity-test ()
  "Run a quick parity test with default settings"
  (run-parity-test 100))

;; Run tests when loaded
(format t "Parity test module loaded. Use (run-parity-test) or (quick-parity-test)~%")
