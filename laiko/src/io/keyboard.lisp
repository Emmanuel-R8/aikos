(in-package :laiko.io)

;; Keyboard handling
;; Per rewrite documentation io/keyboard-protocol.md
;; Per contracts/io-interface.lisp

(defstruct (keyboard-event (:conc-name kbd-))
  "Keyboard event structure per data-model.md"
  (keycode 0 :type (unsigned-byte 32))
  (modifiers 0 :type (unsigned-byte 16))
  (pressed-p nil :type boolean))

;; Keyboard event queue
(defvar *keyboard-event-queue* nil
  "Queue of keyboard events waiting to be processed")

(defun translate-keycode (os-keycode)
  "Translate OS keycode to Lisp keycode per contracts/io-interface.lisp"
  (declare (type (integer 0 *) os-keycode))
  ;; Simple translation: for now, pass through OS keycode
  ;; In full implementation, would map OS-specific keycodes to Lisp keycodes
  ;; Common mappings:
  ;; - ASCII keys: direct mapping
  ;; - Special keys: function keys, arrows, etc. mapped to Lisp keycode space
  (cond
    ((< os-keycode 128)
     ;; ASCII range: direct mapping
     os-keycode)
    (t
     ;; Extended keys: map to Lisp keycode space (0x80-0xFF)
     (logior #x80 (logand os-keycode #x7F)))))

(defun enqueue-key-event (keyboard-event)
  "Enqueue keyboard event for processing per contracts/io-interface.lisp"
  (declare (type keyboard-event keyboard-event))
  (push keyboard-event *keyboard-event-queue*)
  nil)

(defun dequeue-key-event ()
  "Dequeue next keyboard event per contracts/io-interface.lisp"
  (if *keyboard-event-queue*
      (pop *keyboard-event-queue*)
      nil))
