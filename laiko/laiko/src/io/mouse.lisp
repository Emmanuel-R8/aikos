(in-package :maiko-lisp.io)

;; Mouse handling
;; Per rewrite documentation io/mouse-protocol.md
;; Per contracts/io-interface.lisp

(defstruct (mouse-event (:conc-name mouse-))
  "Mouse event structure per data-model.md"
  (x 0 :type (integer 0 *))
  (y 0 :type (integer 0 *))
  (buttons 0 :type (unsigned-byte 8))
  (pressed-p nil :type boolean))

;; Current mouse position
(defvar *mouse-x* 0
  "Current mouse X coordinate")
(defvar *mouse-y* 0
  "Current mouse Y coordinate")

(defun translate-mouse-event (os-event)
  "Translate OS mouse event to Lisp mouse event per contracts/io-interface.lisp"
  (declare (type t os-event))
  ;; TODO: When SDL3 is available, translate SDL mouse events
  ;; For now, create a stub mouse event
  (make-mouse-event
   :x *mouse-x*
   :y *mouse-y*
   :buttons 0
   :pressed-p nil))

(defun update-mouse-position (x y)
  "Update mouse position per contracts/io-interface.lisp"
  (declare (type (integer 0 *) x y))
  (setf *mouse-x* x
        *mouse-y* y)
  nil)

(defun get-mouse-position ()
  "Get current mouse position per contracts/io-interface.lisp"
  (values *mouse-x* *mouse-y*))