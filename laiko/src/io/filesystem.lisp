(in-package :maiko-lisp.io)

;; File system operations
;; Per rewrite documentation io/file-system.md
;; Per contracts/io-interface.lisp

(defun maiko-translate-pathname (lisp-pathname)
  "Translate Lisp pathname to platform path per contracts/io-interface.lisp"
  (declare (type (or string pathname) lisp-pathname))
  (let ((path (if (stringp lisp-pathname)
                  lisp-pathname
                  (namestring lisp-pathname
                              ;; Lisp pathnames use forward slashes, convert to platform-specific
                              ;; For Unix/Linux/macOS: no conversion needed
                              ;; For Windows: would convert / to \
                              #+win32
                              (substitute #\\ #\/ path)
                              #-win32
                              path))

              (defun open-file (pathname direction)
                "Open file for I/O per contracts/io-interface.lisp"
                (declare (type (or string pathname) pathname)
                         (type (member :input :output :io) direction))
                (handler-case
                    (let ((platform-path (maiko-translate-pathname pathname)))
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
                    (error 'maiko-lisp.utils:io-error
                           :message (format nil "Failed to open file ~A: ~A" pathname err)))))

              (defun close-file (stream)
                "Close file stream per contracts/io-interface.lisp"
                (declare (type stream stream))
                (close stream)
                nil)
