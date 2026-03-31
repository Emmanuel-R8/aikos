(in-package :laiko.display)

;; Display event handling
;; Per rewrite documentation display/event-protocols.md
;; Per contracts/display-interface.lisp

(defun poll-events (display)
  "Poll for display events per contracts/display-interface.lisp"
  (declare (type display-interface display))
  ;; TODO: When SDL3 is available, poll SDL events and convert to Lisp events
  ;; For now, return empty list
  (let ((events nil))
    #+cl-sdl3
    (progn
      ;; TODO: Poll SDL3 events
      )
    #-cl-sdl3
    ;; No events available without SDL3
    nil
    events))

(defun wait-for-event (display timeout)
  "Wait for event with timeout per contracts/display-interface.lisp"
  (declare (type display-interface display)
           (type (or null (integer 0 *)) timeout))
  ;; TODO: When SDL3 is available, wait for SDL events
  ;; For now, return nil (no events)
  nil)
