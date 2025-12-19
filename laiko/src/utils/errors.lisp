(in-package :maiko-lisp.utils)

(define-condition vm-error (error)
  ((message :initarg :message :reader vm-error-message))
  (:documentation "VM execution error"))

(define-condition memory-error (error)
  ((message :initarg :message :reader memory-error-message))
  (:documentation "Memory management error"))

(define-condition display-error (error)
  ((message :initarg :message :reader display-error-message))
  (:documentation "Display subsystem error"))

(define-condition io-error (error)
  ((message :initarg :message :reader io-error-message))
  (:documentation "I/O subsystem error"))

;; Specific error types
(define-condition stack-overflow (vm-error)
  ((message :initarg :message :reader stack-overflow-message :initform "Stack overflow"))
  (:documentation "Stack overflow error"))

(define-condition invalid-address (memory-error)
  ()
  (:documentation "Invalid memory address"))

(define-condition division-by-zero (vm-error)
  ()
  (:documentation "Division by zero"))

(define-condition sysout-load-failed (memory-error)
  ((message :initarg :message :reader sysout-load-failed-message))
  (:documentation "Failed to load sysout file"))
