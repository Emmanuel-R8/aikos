(in-package :maiko-lisp.display)

;; SDL3 display backend
;; Per rewrite documentation display/interface-abstraction.md
;; Per contracts/display-interface.lisp

(defstruct (display-interface (:conc-name display-))
  "Display interface structure per data-model.md"
  (window nil :type (or null t))
  (renderer nil :type (or null t))
  (width 0 :type (integer 1 *))
  (height 0 :type (integer 1 *))
  (buffer nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defun init-display (width height)
  "Initialize SDL display per contracts/display-interface.lisp"
  (declare (type (integer 1 *) width height))
  (handler-case
      ;; Try to use cl-sdl3 if available
      #+cl-sdl3
      (progn
        ;; TODO: Initialize SDL3 when cl-sdl3 is available
        ;; For now, create display structure without actual SDL window
        (make-display-interface
         :window nil
         :renderer nil
         :width width
         :height height
         :buffer (make-array (* width height 4) ; RGBA buffer
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
      #-cl-sdl3
      ;; Fallback: Create display structure without SDL
      (make-display-interface
       :window nil
       :renderer nil
       :width width
       :height height
       :buffer (make-array (* width height 4) ; RGBA buffer
                          :element-type '(unsigned-byte 8)
                          :initial-element 0))
    (error (err)
      ;; Edge case: SDL initialization failure per task T072
      (error 'maiko-lisp.utils:display-error
             :message (format nil "Failed to initialize display: ~A. SDL3 may not be available. Install cl-sdl3 or use without display." err)))))

(defun destroy-display (display)
  "Destroy display and cleanup resources per contracts/display-interface.lisp"
  (declare (type display-interface display))
  ;; TODO: Cleanup SDL3 resources when cl-sdl3 is available
  (setf (display-window display) nil)
  (setf (display-renderer display) nil)
  (setf (display-buffer display) nil)
  nil)