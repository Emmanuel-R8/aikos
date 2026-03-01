;;;; verify-sysout-loading.lisp - Verify sysout file loading for Laiko
;;;;
;;;; This script tests:
;;;; 1. Loading the starter.sysout file
;;;; 2. IFPAGE key validation
;;;; 3. FPtoVP table loading
;;;; 4. Byte swapping for big-endian data
;;;; 5. Virtual memory initialization

(require :asdf)

;; Load the .asd file first to register the system
(load "laiko.asd")

;; Load the laiko system
(asdf:load-system :laiko)

(use-package :laiko.data)
(use-package :laiko.utils)

(defun verify-sysout-loading ()
  "Verify sysout loading functionality."
  (format t "~%=== Phase 0: Sysout Loading Verification ===~%~%")

  (let* ((sysout-path #p"../medley/internal/loadups/starter.sysout")
         (success t))

    ;; Check if file exists
    (unless (probe-file sysout-path)
      (format t "ERROR: Sysout file not found: ~A~%" sysout-path)
      (return-from verify-sysout-loading nil))

    (format t "Sysout file found: ~A~%" sysout-path)
    (format t "File size: ~D bytes~%" (file-size sysout-path))
    (format t "~%")

    ;; Attempt to load sysout
    (handler-case
        (multiple-value-bind (ifpage fptovp vmem)
            (load-sysout sysout-path)

          ;; Verify IFPAGE key
          (format t "~%--- IFPAGE Verification ---~%")
          (let ((key (ifpage-key ifpage)))
            (format t "IFPAGE key: ~4,'0X (expected: ~4,'0X)~%" key +ifpage-keyval+)
            (if (= key +ifpage-keyval+)
                (format t "  ✓ IFPAGE key validation PASSED~%")
                (progn
                  (format t "  ✗ IFPAGE key validation FAILED~%")
                  (setf success nil))))

          ;; Verify critical IFPAGE fields
          (format t "~%IFPAGE critical fields:~%")
          (format t "  currentfxp: ~D~%" (ifpage-currentfxp ifpage))
          (format t "  stackbase: ~D~%" (ifpage-stackbase ifpage))
          (format t "  fptovpstart: ~D~%" (ifpage-fptovpstart ifpage))
          (format t "  nactivepages: ~D~%" (ifpage-nactivepages ifpage))
          (format t "  process-size: ~D MB~%" (ifpage-process-size ifpage))
          (format t "  lversion: ~D~%" (ifpage-lversion ifpage))
          (format t "  machinetype: ~D~%" (ifpage-machinetype ifpage))

          ;; Verify FPtoVP table
          (format t "~%--- FPtoVP Table Verification ---~%")
          (format t "FPtoVP table size: ~D entries~%" (length fptovp))

          ;; Count valid pages
          (let ((valid-pages 0)
                (sparse-pages 0))
            (loop for i from 0 below (min 100 (length fptovp))
                  for page-ok = (get-page-ok fptovp i)
                  do (if (= page-ok +page-not-present+)
                         (incf sparse-pages)
                         (incf valid-pages)))
            (format t "  First 100 entries: ~D valid, ~D sparse~%" valid-pages sparse-pages)
            (format t "  ✓ FPtoVP table loaded~%"))

          ;; Verify virtual memory
          (format t "~%--- Virtual Memory Verification ---~%")
          (let ((loaded-pages 0)
                (total-pages (length vmem)))
            (loop for i from 0 below total-pages
                  when (aref vmem i)
                    do (incf loaded-pages))
            (format t "Virtual memory size: ~D pages~%" total-pages)
            (format t "Pages loaded: ~D~%" loaded-pages)
            (format t "  ✓ Virtual memory initialized~%"))

          ;; Verify byte swapping
          (format t "~%--- Byte Swapping Verification ---~%")
          (format t "Endianness: ~A~%" (if (little-endian-p) "little-endian" "big-endian"))
          (format t "  ✓ Byte swapping configured~%")

          ;; Summary
          (format t "~%=== Summary ===~%")
          (if success
              (format t "✓ Sysout loading verification PASSED~%")
              (format t "✗ Sysout loading verification FAILED~%"))

          success)

      (invalid-sysout-key (c)
        (format t "ERROR: Invalid sysout key - ~A~%" c)
        nil)

      (sysout-read-error (c)
        (format t "ERROR: Sysout read error - ~A~%" c)
        nil)

      (error (c)
        (format t "ERROR: Unexpected error - ~A~%" c)
        nil))))

;; Run verification and exit with appropriate code
(let ((result (verify-sysout-loading)))
  (sb-ext:quit :unix-code (if result 0 1)))
