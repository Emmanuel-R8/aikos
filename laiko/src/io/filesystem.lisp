(in-package :laiko.io)

;; File system operations
;; Per rewrite documentation io/file-system.md
;; Per contracts/io-interface.lisp

(defun laiko-translate-pathname (lisp-pathname)
  "Translate Lisp pathname to platform path per contracts/io-interface.lisp"
  (declare (type (or string pathname) lisp-pathname))
  (let* ((raw (if (stringp lisp-pathname)
                  lisp-pathname
                  (namestring lisp-pathname)))
         ;; Lisp pathnames use forward slashes; on Unix-like systems we can use
         ;; RAW directly, on Windows we translate "/" to "\".
         (path #+win32 (substitute #\\ #\/ raw)
               #-win32 raw))
    path))

(defun open-file (pathname direction)
  "Open file for I/O per contracts/io-interface.lisp"
  (declare (type (or string pathname) pathname)
           (type (member :input :output :io) direction))
  (handler-case
      (let ((platform-path (laiko-translate-pathname pathname)))
        (ecase direction
          (:input
           (open platform-path :direction :input :element-type '(unsigned-byte 8)))
          (:output
           (open platform-path :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create))
          (:io
           (open platform-path :direction :io :element-type '(unsigned-byte 8)
                               :if-exists :overwrite :if-does-not-exist :create))))
    (file-error (err)
      (error 'laiko.utils:io-error
             :message (format nil "Failed to open file ~A: ~A" pathname err)))))

(defun close-file (stream)
  "Close file stream per contracts/io-interface.lisp"
  (declare (type stream stream))
  (close stream)
  nil)
