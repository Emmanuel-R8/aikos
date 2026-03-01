(in-package :laiko.display)

;; Graphics operations
;; Per rewrite documentation display/graphics-operations.md
;; Per contracts/display-interface.lisp

(defun render-region (display x y width height buffer)
  "Render region to display per contracts/display-interface.lisp"
  (declare (type display-interface display)
           (type (integer 0 *) x y width height)
           (type (simple-array (unsigned-byte 8) (*)) buffer))
  (let ((display-buffer (display-buffer display))
        (display-width (display-width display))
        (display-height (display-height display)))
    ;; Validate bounds
    (when (or (>= x display-width)
              (>= y display-height)
              (> (+ x width) display-width)
              (> (+ y height) display-height))
      (error 'laiko.utils:display-error
             :message (format nil "Render region out of bounds: (~A,~A) ~Ax~A in ~Ax~A"
                              x y width height display-width display-height)))
    ;; Copy buffer to display buffer
    ;; For now, simple copy (will be optimized with SDL3)
    (let ((buffer-size (* width height 4))) ; RGBA
      (when (> buffer-size (length buffer))
        (error 'laiko.utils:display-error
               :message "Buffer too small for region"))
      (loop for row from 0 below height
            for src-offset = (* row width 4)
            for dst-offset = (+ (* (+ y row) display-width 4) (* x 4))
            do (replace display-buffer buffer
                        :start1 dst-offset
                        :start2 src-offset
                        :end2 (+ src-offset (* width 4))))))
  nil)

(defun bitblt (display src-x src-y width height dst-x dst-y operation)
  "Perform BitBLT operation per contracts/display-interface.lisp
   Per documentation/rewrite-spec/display/graphics-operations.md"
  (declare (type display-interface display)
           (type (integer 0 *) src-x src-y width height dst-x dst-y)
           (type (unsigned-byte 8) operation))
  (let ((buffer (display-buffer display))
        (display-width (display-width display))
        (display-height (display-height display)))
    ;; Validate bounds
    (when (or (>= src-x display-width)
              (>= src-y display-height)
              (>= dst-x display-width)
              (>= dst-y display-height)
              (> (+ src-x width) display-width)
              (> (+ src-y height) display-height)
              (> (+ dst-x width) display-width)
              (> (+ dst-y height) display-height))
      (error 'laiko.utils:display-error
             :message "BitBLT source or destination out of bounds"))
    ;; Perform blit operation
    ;; Operation codes: 0=COPY, 1=XOR, 2=AND, 3=OR, etc.
    (case operation
      (0 ; COPY
       (loop for row from 0 below height
             for src-offset = (+ (* (+ src-y row) display-width 4) (* src-x 4))
             for dst-offset = (+ (* (+ dst-y row) display-width 4) (* dst-x 4))
             do (replace buffer buffer
                         :start1 dst-offset
                         :start2 src-offset
                         :end2 (+ src-offset (* width 4)))))
      (1 ; XOR
       (loop for row from 0 below height
             for src-offset = (+ (* (+ src-y row) display-width 4) (* src-x 4))
             for dst-offset = (+ (* (+ dst-y row) display-width 4) (* dst-x 4))
             do (loop for i from 0 below (* width 4)
                      do (setf (aref buffer (+ dst-offset i))
                               (logxor (aref buffer (+ dst-offset i))
                                       (aref buffer (+ src-offset i)))))))
      (t
       (error 'laiko.utils:display-error
              :message (format nil "Unsupported BitBLT operation: ~A" operation)))))
  nil)

(defun flush-display-region (display x y width height)
  "Flush display region to screen per contracts/display-interface.lisp"
  (declare (type display-interface display)
           (type (integer 0 *) x y width height))
  ;; TODO: When SDL3 is available, update SDL texture/surface and present
  ;; For now, just mark region as dirty (would trigger SDL update)
  nil)
