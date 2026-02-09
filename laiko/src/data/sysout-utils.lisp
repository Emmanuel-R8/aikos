(in-package :maiko-lisp.data)

;; Sysout utility functions
;; Per maiko/src/ldsout.c

(defconstant +ifpage-address+ 512)

(defconstant +bytesper-page+ 512)

(defconstant +sysout-keyval+ #x15e3)

(defconstant +page-not-present+ #xFFFF)

(defconstant +lversion+ 21000)

(defconstant +minbversion+ 21001)

(defun check-version-compatibility (ifpage)
  "Check if sysout version is compatible with this emulator."
  (declare (type ifpage ifpage))
  (let ((lversion (ifpage-lversion ifpage))
        (minbversion (ifpage-minbversion ifpage)))
    (cond
      ((< lversion +lversion+)
       (error 'maiko-lisp.utils:sysout-load-failed
              :message (format nil "Lisp version ~D is too old (minimum: ~D)"
                               lversion +lversion+)))
      ((> minbversion +minbversion+)
       (error 'maiko-lisp.utils:sysout-load-failed
              :message (format nil "Emulator too old (needs ~D, have ~D)"
                               minbversion +minbversion+))))
    t))

(defun validate-sysout (ifpage)
  "Validate sysout by checking keyval."
  (declare (type ifpage ifpage))
  (= (ifpage-key ifpage) +sysout-keyval+))

(defun calculate-fptovp-offset (fptovp-start)
  "Calculate file offset for FPtoVP table."
  (declare (type (unsigned-byte 16) fptovp-start))
  (- (* (1- fptovp-start) +bytesper-page+) 2))

(defun get-fptovp (fptovp file-page)
  "Get virtual page number for file page."
  (declare (type (simple-array (unsigned-byte 16) (*)) fptovp)
           (type (unsigned-byte 32) file-page))
  (aref fptovp file-page))

(defun get-page-ok (fptovp file-page)
  "Check if page is present (non-zero high byte)."
  (declare (type (simple-array (unsigned-byte 16) (*)) fptovp)
           (type (unsigned-byte 32) file-page))
  (let ((entry (aref fptovp file-page)))
    (logand #xFF00 entry)))
