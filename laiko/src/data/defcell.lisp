;;;; defcell.lisp - Definition Cell Structure for Laiko
;;;;
;;;; Implements definition cell structure matching C DefCell from maiko/inc/cell.h
;;;; Per zaiko/src/data/defcell.zig

(in-package :laiko.data)

;;;============================================================================
;;; DefCell Structure
;;;============================================================================

(defstruct (defcell (:constructor make-defcell-raw))
  "Definition cell structure for function definitions.

   For BIGVM: 28-bit defpointer, for non-BIGVM: 24-bit defpointer
   Matches C DefCell from maiko/inc/cell.h"
  (ccodep 0 :type (unsigned-byte 1))
  (fastp 0 :type (unsigned-byte 1))
  (argtype 0 :type (unsigned-byte 2))
  (defpointer 0 :type (unsigned-byte 32)))

;;;============================================================================
;;; DefCell Reading
;;;============================================================================

(defun %read-vm-lispptr (virtual-memory byte-offset)
  "Read a 32-bit LispPTR from VM storage at BYTE-OFFSET.

   LispPTR fields remain sequential in the word-swapped VM image, so this uses
   the same little-endian byte order as atom value reads."
  (declare (type (unsigned-byte 32) byte-offset))
  (let ((b0 (or (get-vm-byte virtual-memory byte-offset) 0))
        (b1 (or (get-vm-byte virtual-memory (1+ byte-offset)) 0))
        (b2 (or (get-vm-byte virtual-memory (+ byte-offset 2)) 0))
        (b3 (or (get-vm-byte virtual-memory (+ byte-offset 3)) 0)))
    (logior b0
            (ash b1 8)
            (ash b2 16)
            (ash b3 24))))

(defun %read-vm-logical-word (virtual-memory byte-offset)
  "Read a logical 16-bit word from VM storage at BYTE-OFFSET.

   Function-header fields are addressed through Maiko GETWORD semantics on
   BYTESWAP builds, so apply the logical address XOR before using GET-VM-WORD."
  (declare (type (unsigned-byte 32) byte-offset))
  (get-vm-word virtual-memory
               (if (laiko.utils:little-endian-p)
                   (logxor byte-offset 2)
                   byte-offset)))

(defun read-defcell (virtual-memory atom-index)
  "Read DefCell from atom's definition cell.

   Per C: GetDEFCELL68k(atom_index) then read DefCell structure.

   Returns a defcell structure."
  (declare (type (unsigned-byte 32) atom-index))
  (let ((vmem virtual-memory))
    (unless vmem
      (return-from read-defcell (make-defcell-raw)))

    (let ((defcell-offset (get-defcell virtual-memory atom-index)))
      (when (zerop defcell-offset)
        (return-from read-defcell (make-defcell-raw)))
      (let* ((first-ptr (%read-vm-lispptr virtual-memory defcell-offset))
             (ccodep (logand (ash first-ptr -31) 1))
             (fastp (logand (ash first-ptr -30) 1))
             (argtype (logand (ash first-ptr -28) 3))
             (defpointer (logand first-ptr #x0FFFFFFF)))
        (make-defcell-raw
         :ccodep ccodep
         :fastp fastp
         :argtype argtype
         :defpointer defpointer)))))

;;;============================================================================
;;; DefCell Accessors
;;;============================================================================

(defun is-c-code (defcell)
  "Check if DefCell points to C code function."
  (declare (type defcell defcell))
  (= (defcell-ccodep defcell) 1))

(defun get-function-header (defcell)
  "Get function header pointer from DefCell.

   Per C: LOCFNCELL = (struct fnhead *)NativeAligned4FromLAddr(defpointer)"
  (declare (type defcell defcell))
  ;; For non-BIGVM, mask to 24 bits
  ;; For BIGVM, use full 28 bits
  (let ((ptr (defcell-defpointer defcell)))
    ;; Convert LispPTR to byte offset: ptr * 2 (DLword offset)
    (* ptr 2)))

;;;============================================================================
;;; Function Header Reading
;;;============================================================================

(defun read-function-header (virtual-memory fnheader-offset)
  "Read function header from virtual memory.

   Per C: struct fnhead at offset.
   Returns a function-header structure."
  (declare (type (unsigned-byte 32) fnheader-offset))
  (let ((vmem virtual-memory))
    (unless vmem
      (return-from read-function-header nil))

    (let ((page-num (ash fnheader-offset -9)))
      (when (>= page-num (length vmem))
        (return-from read-function-header nil))

      (let ((page (aref vmem page-num)))
        (when (null page)
          (return-from read-function-header nil)))

    ;; Follow the same logical-word offsets that the working FVAR scanner
    ;; already uses for byte-swapped BIGVM function headers.
    (let* ((stkmin (%read-vm-logical-word virtual-memory fnheader-offset))
           (na (%read-vm-logical-word virtual-memory (+ fnheader-offset 2)))
           (pv (%read-vm-logical-word virtual-memory (+ fnheader-offset 4)))
           (startpc (%read-vm-logical-word virtual-memory (+ fnheader-offset 6)))
           (framename/flags (%read-vm-lispptr virtual-memory (+ fnheader-offset 8)))
           (ntsize (%read-vm-logical-word virtual-memory (+ fnheader-offset 12)))
           (locals/fvars (%read-vm-logical-word virtual-memory (+ fnheader-offset 14))))
      (make-function-header
       :stkmin stkmin
       :framename (logand framename/flags #x0FFFFFFF)
       :startpc startpc
       :nv pv
       :na na
       :ntsize ntsize
       :nlocals (ash locals/fvars -8)
       :fvaroffset (logand locals/fvars #xFF))))))
