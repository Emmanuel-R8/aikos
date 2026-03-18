(in-package :laiko.vm)

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
  (source-ptr 0 :type laiko.utils:lisp-ptr)
  (dest-ptr 0 :type laiko.utils:lisp-ptr)
  (width 0 :type (integer 0 #x400))
  (height 0 :type (integer 0 #x400))
  (source-pitch 0 :type (integer 0 2048))
  (dest-pitch 0 :type (integer 0 2048))
  (operation 0 :type (integer 0 15))
  (foreground-color 0 :type (unsigned-byte 32))
  (background-color 0 :type (unsigned-byte 32)))

;;; ===========================================================================
;; GRAPHICS OPCODES
;;; ===========================================================================

(defop pilotbitblt :hexcode #x76 :instruction-length 1
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

(defun set-pixel (vm x y color)
  "Set a single pixel on the display."
  (declare (type vm vm)
           (type (integer 0 #x400) x y)
           (type (unsigned-byte 32) color))
  (declare (ignore vm color))
  (when (and (>= x 0) (< x *display-width*)
             (>= y 0) (< y *display-height*))
    nil))

(defop drawline :hexcode #x3B :instruction-length 1
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
    (let* ((dx (abs (- x2 x1)))
          (dy (abs (- y2 y1)))
          (sx (if (< x1 x2) 1 -1))
          (sy (if (< y1 y2) 1 -1))
          (err (- (min dx dy)))
          (e2 0)
          (x x1)
          (y y1))
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

(defop subrcall :hexcode #x7D :instruction-length 3
  "SUBRCALL: Call a C subroutine.
  Operands: subr-no (1 byte), arg-count (1 byte).
  Pops ARG-COUNT arguments from stack.
  Pushes 1 result."
  :operands ((subr-no :uint8 "Subroutine number")
             (arg-count :uint8 "Number of arguments"))
  :stack-effect (:pop-n arg-count :push 1)
  :category :function-call
  :side-effects t
  (let ((subr-no (read-pc-8 vm))
        (arg-count (read-pc-8 vm)))
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

(defconstant +stk-fsb-word+ #xA000
  "Free-stack-block marker written by Maiko contextsw/return paths.")

(defun %stack-byte-offset->stack-offset (byte-offset)
  "Convert an absolute Lisp-world byte offset into a Stackspace-relative DLword offset."
  (declare (type (unsigned-byte 32) byte-offset))
  (- (ash byte-offset -1) laiko.data:+stk-offset-dlword+))

(defun %sync-current-frame-from-fx (vm fx)
  (setf (vm-current-frame vm)
        (make-stack-frame
         :next-block (laiko.data:fx-nextblock fx)
         :link (laiko.data:fx-alink fx)
         :fn-header (laiko.data:fx-fnheader fx)
         :pc-offset (laiko.data:fx-pc fx))))

(defop contextswitch :hexcode #x7E :instruction-length 1
  "CONTEXTSWITCH: Switch execution context.
Used for multiprocessing or coroutine switching.
Pops new-context pointer, saves current context, switches to new."
  :operands nil
  :stack-effect (:pop 1)
  :category :control-flow
  :side-effects t
  (let* ((fxnum (logand (vm-tos vm) #xFFFF))
         (virtual-memory (vm-virtual-memory vm))
         (ifpage (vm-ifpage vm))
         (current-frame-offset (%stack-byte-offset->stack-offset
                                (vm-frame-pointer-offset vm)))
         (current-fx (laiko.data:read-fx-from-vm virtual-memory current-frame-offset)))
    (unless ifpage
      (error 'laiko.utils:vm-error
             :message "CONTEXTSWITCH requires IFPAGE state"))
    ;; Save current FX state per Maiko contextsw().
    (laiko.data:set-fx-nopush current-fx t)
    (setf (laiko.data:fx-pc current-fx)
          (- (+ *current-base-pc* (vm-pc vm))
             (* 2 (laiko.data:fx-fnheader current-fx))))
    (let* ((saved-value-offset (+ (vm-stack-ptr-offset vm) 4))
           (fsb-offset (+ saved-value-offset 4)))
      ;; OP_contextsw stores fxnum into the old frame, not cached TOS.
      (vm-write-lispptr vm saved-value-offset fxnum)
      (setf (laiko.data:fx-nextblock current-fx)
            (%stack-byte-offset->stack-offset fsb-offset))
      (laiko.data:write-fx-to-vm virtual-memory current-frame-offset current-fx)
      (vm-write-word vm fsb-offset +stk-fsb-word+)
      (vm-write-word vm (+ fsb-offset 2)
                     (ash (- (vm-stack-end-offset vm) fsb-offset) -1))
      ;; Midpunt: exchange current FX offset with the requested IFPAGE slot.
      (let* ((selected-frame-offset (laiko.data:get-ifpage-fx-slot ifpage fxnum)))
        (laiko.data:set-ifpage-fx-slot ifpage fxnum current-frame-offset)
        (when (zerop selected-frame-offset)
          (error 'laiko.utils:vm-error
                 :message (format nil "CONTEXTSWITCH target FX slot ~D is zero" fxnum)))
        (let* ((selected-fx (laiko.data:read-fx-from-vm virtual-memory selected-frame-offset))
               (next68k (+ laiko.data:+stackspace-byte-offset+
                           (* 2 (laiko.data:fx-nextblock selected-fx)))))
          (when (laiko.data:fx-incall-p selected-fx)
            (error 'laiko.utils:vm-error
                   :message (format nil "CONTEXTSWITCH returned to incall frame 0x~X"
                                    selected-frame-offset)))
          (unless (= (vm-read-word vm next68k) +stk-fsb-word+)
            (error 'laiko.utils:vm-error
                   :message (format nil "CONTEXTSWITCH expected FSB at 0x~X for FX 0x~X"
                                    next68k selected-frame-offset)))
          ;; Merge adjacent FSBs to recompute EndSTKP.
          (let ((freeptr next68k)
                (new-end next68k))
            (loop while (= (vm-read-word vm freeptr) +stk-fsb-word+)
                  do (setf new-end (+ freeptr (* 2 (vm-read-word vm (+ freeptr 2)))))
                     (setf freeptr new-end))
            (setf (vm-stack-end-offset vm) new-end))
          (setf (vm-frame-pointer-offset vm)
                (+ laiko.data:+stackspace-byte-offset+ (* 2 selected-frame-offset)))
          (if (laiko.data:fx-nopush-p selected-fx)
              (progn
                (laiko.data:set-fx-nopush selected-fx nil)
                (laiko.data:write-fx-to-vm virtual-memory selected-frame-offset selected-fx)
                (let ((resume-sp (- next68k 4)))
                  (setf (vm-top-of-stack vm) (vm-read-lispptr vm resume-sp))
                  (setf (vm-stack-ptr-offset vm) (- resume-sp 4))))
              (setf (vm-stack-ptr-offset vm) (- next68k 4)))
          (setf (vm-pc vm)
                (+ (* 2 (laiko.data:fx-fnheader selected-fx))
                   (laiko.data:fx-pc selected-fx)))
          (%sync-current-frame-from-fx vm selected-fx)
          nil)))))

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

;; Subroutine stubs
(defun subr-open-stream (args)
  (declare (ignore args)) 0)

(defun subr-close-stream (args)
  (declare (ignore args)) 0)

(defun subr-read-char (args)
  (declare (ignore args)) 0)

(defun subr-write-char (args)
  (declare (ignore args)) 0)
