;;;; verify-ifpage-accessors.lisp - Verify IFPAGE accessor functions for Laiko
;;;;
;;;; This script verifies that IFPAGE accessor functions are properly exported
;;;; and work correctly with the IFPAGE structure.

(in-package :laiko.data)

(defun verify-ifpage-accessors ()
  "Verify IFPAGE accessor functions are exported and working."
  (format t "~%=== IFPAGE Accessor Verification ===~%")

  ;; Test 1: Check IFPAGE structure can be created
  (format t "~%Test 1: Creating IFPAGE structure...~%")
  (let ((ifp (make-ifpage-raw)))
    (format t "  ✓ IFPAGE structure created~%")

    ;; Test 2: Set and read key fields
    (format t "~%Test 2: Setting and reading key fields...~%")
    (setf (ifpage-key ifp) #x15e3)
    (format t "  Key: ~4,'0X (expected: 15E3)~%" (ifpage-key ifp))
    (assert (= (ifpage-key ifp) #x15e3))
    (format t "  ✓ Key field works~%")

    (setf (ifpage-process-size ifp) 64)
    (format t "  Process size: ~D MB~%" (ifpage-process-size ifp))
    (assert (= (ifpage-process-size ifp) 64))
    (format t "  ✓ Process size field works~%")

    (setf (ifpage-stackbase ifp) #x1000)
    (format t "  Stack base: ~4,'0X~%" (ifpage-stackbase ifp))
    (assert (= (ifpage-stackbase ifp) #x1000))
    (format t "  ✓ Stack base field works~%")

    (setf (ifpage-currentfxp ifp) #x2000)
    (format t "  Current FXP: ~4,'0X~%" (ifpage-currentfxp ifp))
    (assert (= (ifpage-currentfxp ifp) #x2000))
    (format t "  ✓ Current FXP field works~%")

    (setf (ifpage-fptovpstart ifp) 100)
    (format t "  FPtoVP start: ~D~%" (ifpage-fptovpstart ifp))
    (assert (= (ifpage-fptovpstart ifp) 100))
    (format t "  ✓ FPtoVP start field works~%")

    (setf (ifpage-nactivepages ifp) 5000)
    (format t "  Nactivepages: ~D~%" (ifpage-nactivepages ifp))
    (assert (= (ifpage-nactivepages ifp) 5000))
    (format t "  ✓ Nactivepages field works~%")

    (setf (ifpage-lversion ifp) 10)
    (format t "  LVersion: ~D~%" (ifpage-lversion ifp))
    (assert (= (ifpage-lversion ifp) 10))
    (format t "  ✓ LVersion field works~%")

    (setf (ifpage-minbversion ifp) 5)
    (format t "  MinBVersion: ~D~%" (ifpage-minbversion ifp))
    (assert (= (ifpage-minbversion ifp) 5))
    (format t "  ✓ MinBVersion field works~%")

    (setf (ifpage-machinetype ifp) 1)
    (format t "  Machine type: ~D~%" (ifpage-machinetype ifp))
    (assert (= (ifpage-machinetype ifp) 1))
    (format t "  ✓ Machine type field works~%")

    (setf (ifpage-storagefullstate ifp) 0)
    (format t "  Storage full state: ~D~%" (ifpage-storagefullstate ifp))
    (assert (= (ifpage-storagefullstate ifp) 0))
    (format t "  ✓ Storage full state field works~%")

    ;; Test 3: Verify constants
    (format t "~%Test 3: Verifying constants...~%")
    (format t "  +ifpage-address+: ~D~%" +ifpage-address+)
    (assert (= +ifpage-address+ #x200))
    (format t "  ✓ IFPAGE address is #x200~%")

    (format t "  +ifpage-keyval+: ~4,'0X~%" +ifpage-keyval+)
    (assert (= +ifpage-keyval+ #x15e3))
    (format t "  ✓ IFPAGE key value is 0x15E3~%")

    (format t "  +bytesper-page+: ~D~%" +bytesper-page+)
    (assert (= +bytesper-page+ #x200))
    (format t "  ✓ Bytes per page is #x200~%")

    (format t "~%=== All IFPAGE accessor tests passed ===~%")
    t))

;; Auto-run when loaded
(handler-case
    (if (verify-ifpage-accessors)
        (sb-ext:exit :code 0)
        (sb-ext:exit :code 1))
  (error (c)
    (format t "~%ERROR: ~A~%" c)
    (sb-ext:exit :code 1)))