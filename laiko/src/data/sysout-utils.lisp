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
  "Calculate file offset for FPtoVP table.
   Per C code (maiko/src/ldsout.c:865-869):
   - BIGVM: (fptovpstart - 1) * BYTESPER_PAGE + 4
   - Non-BIGVM: (fptovpstart - 1) * BYTESPER_PAGE + 2

   Zig implementation uses BIGVM format (32-bit entries) with +4 offset.
   Laiko should also use BIGVM format per Zig implementation.

   If fptovpstart is 0, this indicates a special case - the FPtoVP table
   might be at a different location or the sysout format is different."
  (declare (type (unsigned-byte 16) fptovp-start))
  (if (zerop fptovp-start)
      ;; Special case: fptovpstart = 0 might mean table is at end of file or different format
      ;; For now, try the standard calculation anyway (will give -510, which we'll handle)
      (- (* (1- fptovp-start) +bytesper-page+) 4)
      ;; BIGVM format: (fptovpstart - 1) * BYTESPER_PAGE + 4
      (+ (* (1- fptovp-start) +bytesper-page+) 4)))

(defun get-fptovp (fptovp file-page)
  "Get virtual page number for file page (BIGVM format).
   Per Zig implementation: GETFPTOVP returns low 16 bits of 32-bit entry.
   C: #define GETFPTOVP(b, o) ((b)[o]) - for BIGVM, this is the full u32 entry.
   But we extract low 16 bits as the virtual page number."
  (declare (type (simple-array (unsigned-byte 32) (*)) fptovp)
           (type (unsigned-byte 32) file-page))
  (logand (aref fptovp file-page) #xFFFF))

(defun get-page-ok (fptovp file-page)
  "Check if page is present (BIGVM format).
   Per Zig implementation: GETPAGEOK returns high 16 bits of 32-bit entry.
   C: #define GETPAGEOK(b, o) ((b)[o] >> 16) - for BIGVM.
   Returns page OK flag: 0xFFFF = sparse (not present), other values = valid."
  (declare (type (simple-array (unsigned-byte 32) (*)) fptovp)
           (type (unsigned-byte 32) file-page))
  (ash (aref fptovp file-page) -16))
