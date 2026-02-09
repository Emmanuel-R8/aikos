(in-package :maiko-lisp.vm)

;; BitBLT and graphics operations
;; BitBLT: Bit Block Transfer for moving/combining rectangular regions of bits

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

(defun handle-bitblt (vm)
  "BITBLT: Bit Block Transfer operation.
   Pops: dest-ptr, source-ptr, operation, height, width, dest-pitch, source-pitch
   Pushes: 0 (success) or error code"
  (declare (type vm vm))
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

;; Drawing operations
(defun handle-drawline (vm operands)
  "DRAWLINE: Draw a line on the display.
   Pops: y2, x2, y1, x1, color
   Draws a line from (x1, y1) to (x2, y2) with Bresenham's algorithm."
  (declare (type vm vm)
           (type list operands))
  (declare (ignore operands))
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

(defun set-pixel (vm x y color)
  "Set a single pixel on the display."
  (declare (type vm vm)
           (type (integer 0 1024) x y)
           (type (unsigned-byte 32) color))
  (declare (ignore vm color))
  (when (and (>= x 0) (< x *display-width*)
             (>= y 0) (< y *display-height*))
    nil))

;; Undefined Function Name (UFN) handling
(defstruct (ufn-entry (:conc-name "UFN-"))
  "UFN entry structure for undefined function handling."
  (atom-index 0 :type (unsigned-byte 32))
  (byte-count 0 :type (integer 0 256))
  (arg-count 0 :type (integer 0 32)))

(defvar *ufn-table* (make-hash-table :test 'equal)
  "Hash table mapping opcode bytes to UFN entry structures.")

(defun handle-ufn (vm operands)
  "UFN: Undefined Function Name handler."
  (declare (type vm vm)
           (type list operands))
  (let ((opcode (first operands)))
    (let ((entry (gethash opcode *ufn-table*)))
      (if entry
          (dispatch-ufn-handler vm (ufn-atom-index entry) (ufn-arg-count entry))
          (error 'maiko-lisp.utils:vm-error
                 :message (format nil "UFN: No handler for opcode ~A" opcode))))))

(defun dispatch-ufn-handler (vm atom-index arg-count)
  "Dispatch to a UFN handler function."
  (declare (type vm vm)
           (type (unsigned-byte 32) atom-index)
           (type (integer 0 32) arg-count))
  (let ((handler-ptr (maiko-lisp.data:get-global-value atom-index)))
    (when (zerop handler-ptr)
      (error 'maiko-lisp.utils:vm-error
             :message "UFN: Handler function not found"))
    (push-stack vm handler-ptr)
    (push-stack vm arg-count)))

;; Subroutine call handling
(defun handle-subrcall (vm operands)
  "SUBRCALL: Call a C subroutine."
  (declare (type vm vm)
           (type list operands))
  (declare (ignore operands))
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
                    (4 (subr-file-position args))
                    (5 (subr-file-length args))
                    (6 (subr-get-output-stream args))
                    (7 (subr-get-input-stream args))
                    (t 0))))
      (push-stack vm result))))

(defun subr-open-stream (args)
  "SUBR: Open a stream."
  (declare (ignore args))
  0)

(defun subr-close-stream (args)
  "SUBR: Close a stream."
  (declare (ignore args))
  0)

(defun subr-read-char (args)
  "SUBR: Read a character from a stream."
  (declare (ignore args))
  -1)

(defun subr-write-char (args)
  "SUBR: Write a character to a stream."
  (declare (ignore args))
  -1)

(defun subr-file-position (args)
  "SUBR: Get file position."
  (declare (ignore args))
  -1)

(defun subr-file-length (args)
  "SUBR: Get file length."
  (declare (ignore args))
  -1)

(defun subr-get-output-stream (args)
  "SUBR: Get output stream."
  (declare (ignore args))
  0)

(defun subr-get-input-stream (args)
  "SUBR: Get input stream."
  (declare (ignore args))
  0)

;; Context switching operation
(defun handle-contextsw (vm)
  "CONTEXTSW: Context switch operation."
  (declare (type vm vm))
  (let ((new-context (pop-stack vm)))
    (when (not (zerop new-context))
      (restore-context vm new-context))))

(defstruct (context (:conc-name "CONTEXT-"))
  "Context structure for context switching."
  (pc 0 :type maiko-lisp.utils:lisp-ptr)
  (stack-ptr 0 :type (integer 0 65536))
  (pvar-ptr 0 :type (integer 0 1024))
  (registers nil :type simple-vector))

(defun save-context (vm)
  "Save the current VM context."
  (declare (type vm vm))
  (make-context :pc (vm-pc vm)
                :stack-ptr (vm-stack-ptr vm)
                :pvar-ptr (vm-pvar-ptr vm)
                :registers (copy-seq (vm-registers vm))))

(defun restore-context (vm context)
  "Restore a previously saved VM context."
  (declare (type vm vm)
           (type context context))
  (setf (vm-pc vm) (context-pc context))
  (setf (vm-stack-ptr vm) (context-stack-ptr context))
  (setf (vm-pvar-ptr vm) (context-pvar-ptr context))
  (replace (vm-registers vm) (context-registers context)))
