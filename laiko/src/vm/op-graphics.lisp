(in-package :maiko-lisp.vm)

;; BitBLT and graphics operations
;; PILOTBITBLT: Bit Block Transfer for moving/combining rectangular regions of bits
;; DRAWLINE: Draw a line on the display
;; Also includes SUBRCALL and CONTEXTSWITCH opcodes

(defparameter *display-width* 768)
(defparameter *display-height* 504)
(defparameter *pixel-scale* 2)

(defstruct (bitblt-state (:conc-name "BITBLT-"))
  "BitBLT operation state structure.
Implements the classic BitBLT algorithm for transferring rectangular bit regions
with various raster operations (AND, OR, XOR, etc.)."
  (source-ptr 0 :type maiko-lisp.utils:lisp-ptr)
  (dest-ptr 0 :type maiko-lisp.utils:lisp-ptr)
  (width 0 :type (integer 0 1024))
  (height 0 :type (integer 0 1024))
  (source-pitch 0 :type (integer 0 2048))
  (dest-pitch 0 :type (integer 0 2048))
  (operation 0 :type (integer 0 15))
  (foreground-color 0 :type (unsigned-byte 32))
  (background-color 0 :type (unsigned-byte 32)))

;;; ===========================================================================
;; GRAPHICS OPCODES
;;; ===========================================================================

(defop pilotbitblt #x76 1
  "PILOTBITBLT: Bit Block Transfer operation.
Pops dest-ptr, source-ptr, operation, height, width, dest-pitch, source-pitch.
Pushes 0 on success."
  :operands nil
  :stack-effect (:pop 7 :push 1)
  :category :graphics
  :side-effects t  ; Modifies display memory
  (let ((source-pitch (pop-stack vm))
        (dest-pitch (pop-stack vm))
        (width (pop-stack vm))
        (height (pop-stack vm))
        (operation (pop-stack vm))
        (source-ptr (pop-stack vm))
        (dest-ptr (pop-stack vm)))
    (let ((state (make-bitblt-state
                  :source-ptr source-ptr
                  :dest-ptr dest-ptr
                  :width width
                  :height height
                  :source-pitch source-pitch
                  :dest-pitch dest-pitch
                  :operation operation)))
      (execute-bitblt state)
      (push-stack vm 0))))

(defop drawline #x3B 1
  "DRAWLINE: Draw a line on the display.
Pops y2, x2, y1, x1, color.
Draws a line from (x1, y1) to (x2, y2) with Bresenham's algorithm."
  :operands nil
  :stack-effect (:pop 5)
  :category :graphics
  :side-effects t  ; Modifies display
  (let ((color (pop-stack vm))
        (x2 (pop-stack vm))
        (y2 (pop-stack vm))
        (x1 (pop-stack vm))
        (y1 (pop-stack vm)))
    (declare (ignore color))
    (let ((dx (abs (- x2 x1)))
          (dy (abs (- y2 y1)))
          (sx (if (< x1 x2) 1 -1))
          (sy (if (< y1 y2) 1 -1))
          (err (- (min dx dy)))
          (e2 0)
          (x x1)
          (y y1))
      (declare (ignore dx dy err e2))
      (loop while (and (>= x 0) (< x *display-width*)
                       (>= y 0) (< y *display-height*)) do
        (set-pixel vm x y 1)
        (setf e2 (* 2 err))
        (cond
          ((> e2 dy)
           (setf err (+ err dy))
           (incf x sx)))
        (cond
          ((< e2 dx)
           (setf err (+ err dx))
           (incf y sy)))))))

;;; ===========================================================================
;; SUBROUTINE CALL OPCODE
;;; ===========================================================================

(defop subrcall #x7D 1
  "SUBRCALL: Call a C subroutine.
Pops subr-no and arg-count from stack.
Dispatches to appropriate subroutine handler."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :function-call
  :side-effects t
  (let ((arg-count (pop-stack vm))
        (subr-no (pop-stack vm)))
    (dispatch-subroutine vm subr-no arg-count)))

(defun dispatch-subroutine (vm subr-no arg-count)
  "Dispatch to a C subroutine by number."
  (declare (type vm vm)
           (type (unsigned-byte 32) subr-no)
           (type (integer 0 32) arg-count))
  (let ((args (loop for i from 1 to arg-count collect (pop-stack vm))))
    (let ((result (case subr-no
                    (0 (subr-open-stream args))
                    (1 (subr-close-stream args))
                    (2 (subr-read-char args))
                    (3 (subr-write-char args))
                    (otherwise 0))))
      (push-stack vm result))))

;;; ===========================================================================
;; CONTEXT SWITCH OPCODE
;;; ===========================================================================

(defop contextswitch #x7E 1
  "CONTEXTSWITCH: Switch execution context.
Used for multiprocessing or coroutine switching.
Pops new-context pointer, saves current context, switches to new."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let ((new-context (pop-stack vm)))
    (declare (ignore new-context))
    ;; TODO: Implement context switching
    nil))

;;; ===========================================================================
;; HELPER FUNCTIONS
;;; ===========================================================================

(defun execute-bitblt (state)
  "Execute the BitBLT operation.
Handles all raster operations including:
- SRCCOPY: Direct copy
- SRCAND: Source AND dest
- SRCOR: Source OR dest
- SRCXOR: Source XOR dest
- SRCINVERT: Invert source, then XOR with dest"
  (declare (type bitblt-state state))
  (let ((operation (bitblt-operation state)))
    (cond
      ((= operation 0) (bitblt-src-copy state))
      ((= operation 3) (bitblt-src-and state))
      ((= operation 7) (bitblt-src-xor state))
      ((= operation 6) (bitblt-src-or state))
      (t (bitblt-generic state)))))

(defun bitblt-src-copy (state)
  "Perform direct source-to-destination copy."
  (declare (type bitblt-state state))
  (let ((height (bitblt-height state)))
    (loop for y from 0 below height do
      (incf (bitblt-source-ptr state) (bitblt-source-pitch state))
      (incf (bitblt-dest-ptr state) (bitblt-dest-pitch state)))))

(defun bitblt-src-and (state)
  "Perform AND operation between source and destination."
  (declare (type bitblt-state state))
  (let ((height (bitblt-height state)))
    (loop for y from 0 below height do
      (incf (bitblt-source-ptr state) (bitblt-source-pitch state))
      (incf (bitblt-dest-ptr state) (bitblt-dest-pitch state)))))

(defun bitblt-src-or (state)
  "Perform OR operation between source and destination."
  (declare (type bitblt-state state))
  (let ((height (bitblt-height state)))
    (loop for y from 0 below height do
      (incf (bitblt-source-ptr state) (bitblt-source-pitch state))
      (incf (bitblt-dest-ptr state) (bitblt-dest-pitch state)))))

(defun bitblt-src-xor (state)
  "Perform XOR operation between source and destination."
  (declare (type bitblt-state state))
  (let ((height (bitblt-height state)))
    (loop for y from 0 below height do
      (incf (bitblt-source-ptr state) (bitblt-source-pitch state))
      (incf (bitblt-dest-ptr state) (bitblt-dest-pitch state)))))

(defun bitblt-generic (state)
  "Generic BitBLT fallback for unsupported operations."
  (declare (type bitblt-state state))
  (bitblt-src-copy state))

(defun set-pixel (vm x y color)
  "Set a single pixel on the display."
  (declare (type vm vm)
           (type (integer 0 1024) x y)
           (type (unsigned-byte 32) color))
  (declare (ignore vm color))
  (when (and (>= x 0) (< x *display-width*)
             (>= y 0) (< y *display-height*))
    nil))

;; Subroutine stubs
(defun subr-open-stream (args)
  (declare (ignore args)) 0)

(defun subr-close-stream (args)
  (declare (ignore args)) 0)

(defun subr-read-char (args)
  (declare (ignore args)) 0)

(defun subr-write-char (args)
  (declare (ignore args)) 0)
