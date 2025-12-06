(in-package :maiko-lisp.data)

;; Sysout file loading
;; Per data-model.md and rewrite-spec/data-structures/sysout-format.md

;; Sysout validation key (matches C IFPAGE_KEYVAL)
(defconstant +sysout-keyval+ #x12345678)

(defstruct (ifpage (:conc-name ifpage-))
  "Interface page structure (matches C IFPAGE)"
  (keyval 0 :type (unsigned-byte 32))
  (lversion 0 :type (unsigned-byte 32))
  (minbversion 0 :type (unsigned-byte 32))
  (process-size 0 :type (unsigned-byte 32))
  (nactivepages 0 :type (unsigned-byte 32))
  (fptovpstart 0 :type (unsigned-byte 32))
  (storagefullstate 0 :type (unsigned-byte 32))
  (stackbase 0 :type maiko-lisp.utils:lisp-ptr)
  (endofstack 0 :type maiko-lisp.utils:lisp-ptr)
  (currentfxp 0 :type maiko-lisp.utils:lisp-ptr))

(defun validate-sysout (ifpage)
  "Validate sysout file by checking keyval"
  (declare (type ifpage ifpage))
  (= (ifpage-keyval ifpage) +sysout-keyval+))

(defun read-dlword (stream)
  "Read a DLword (16-bit little-endian) from stream.
   Handles endianness conversion if needed per task T073."
  (declare (type stream stream))
  (let ((byte1 (read-byte stream))
        (byte2 (read-byte stream)))
    (if (maiko-lisp.utils:little-endian-p)
        (logior byte1 (ash byte2 8))
        (logior (ash byte1 8) byte2))))

(defun read-lisp-ptr (stream)
  "Read a LispPTR (32-bit little-endian) from stream.
   Handles endianness conversion if needed per task T073."
  (declare (type stream stream))
  (let ((byte1 (read-byte stream))
        (byte2 (read-byte stream))
        (byte3 (read-byte stream))
        (byte4 (read-byte stream)))
    (if (maiko-lisp.utils:little-endian-p)
        (logior byte1
                (ash byte2 8)
                (ash byte3 16)
                (ash byte4 24))
        (logior (ash byte1 24)
                (ash byte2 16)
                (ash byte3 8)
                byte4))))

(defun read-ifpage (stream)
  "Read IFPAGE structure from stream"
  (declare (type stream stream))
  (make-ifpage
   :keyval (read-lisp-ptr stream)
   :lversion (read-lisp-ptr stream)
   :minbversion (read-lisp-ptr stream)
   :process-size (read-lisp-ptr stream)
   :nactivepages (read-lisp-ptr stream)
   :fptovpstart (read-lisp-ptr stream)
   :storagefullstate (read-lisp-ptr stream)
   :stackbase (read-lisp-ptr stream)
   :endofstack (read-lisp-ptr stream)
   :currentfxp (read-lisp-ptr stream)))

(defun load-sysout (path)
  "Load sysout file from path.
   Returns IFPAGE structure and file contents as byte array."
  (declare (type string path))
  (handler-case
      (with-open-file (stream path
                             :element-type '(unsigned-byte 8)
                             :direction :input)
        ;; Read IFPAGE from beginning of file
        ;; In actual sysout format, IFPAGE is at a specific offset
        ;; For now, assume it's at the start
        (let ((ifpage (read-ifpage stream)))
          ;; Validate sysout
          (unless (validate-sysout ifpage)
            ;; Edge case: sysout version mismatch per task T072
            (let ((expected-keyval +sysout-keyval+)
                  (actual-keyval (ifpage-keyval ifpage))
                  (version (ifpage-lversion ifpage)))
              (error 'maiko-lisp.utils:sysout-load-failed
                     :message (format nil "Invalid sysout keyval: expected ~X, got ~X (version: ~A)"
                                     expected-keyval actual-keyval version))))

          ;; Edge case: Check version compatibility per task T072
          (let ((min-version 1)
                (max-version 999))
            (when (or (< (ifpage-lversion ifpage) min-version)
                     (> (ifpage-minbversion ifpage) max-version))
              (error 'maiko-lisp.utils:sysout-load-failed
                     :message (format nil "Sysout version incompatible: lversion=~A, minbversion=~A"
                                     (ifpage-lversion ifpage)
                                     (ifpage-minbversion ifpage)))))

          ;; Read remaining file contents
          (file-position stream :start)
          (let ((file-size (file-length stream))
                (file-contents (make-array file-size
                                           :element-type '(unsigned-byte 8))))
            (read-sequence file-contents stream)
            (values ifpage file-contents))))
    (file-error (err)
      (error 'maiko-lisp.utils:sysout-load-failed
             :message (format nil "Failed to open sysout file ~A: ~A" path err)))
    (end-of-file (err)
      (error 'maiko-lisp.utils:sysout-load-failed
             :message (format nil "Unexpected end of file while reading sysout: ~A" err)))))