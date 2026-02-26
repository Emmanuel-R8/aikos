;;;; atom.lisp - Atom Table Access for Laiko
;;;;
;;;; Implements atom table access matching C implementation in maiko/inc/cell.h
;;;; Per zaiko/src/data/atom.zig

(in-package :maiko-lisp.data)

;;;============================================================================
;;; Constants
;;;============================================================================

;; Segment mask for NEWATOM detection
(defconstant +segmask+ #x0F000000
  "Segment mask for NEWATOM detection")

;; Atom offsets (in DLwords, multiply by 2 for bytes)
(defconstant +newatom-value-offset+ 4
  "Offset to value cell in NEWATOM (DLwords)")
(defconstant +newatom-defn-offset+ 8
  "Offset to definition cell in NEWATOM (DLwords)")
(defconstant +newatom-pname-offset+ 0
  "Offset to pname in NEWATOM (DLwords)")
(defconstant +newatom-plist-offset+ 12
  "Offset to plist in NEWATOM (DLwords)")

;; For BIGVM BIGATOMS: AtomSpace layout (5 LispPTRs per atom)
(defconstant +newatom-value-ptroff+ 1
  "Value cell index in AtomSpace")
(defconstant +newatom-defn-ptroff+ 2
  "Definition cell index in AtomSpace")
(defconstant +newatom-pname-ptroff+ 0
  "Pname cell index in AtomSpace")
(defconstant +newatom-plist-ptroff+ 3
  "Plist cell index in AtomSpace")

;; Memory layout constants from maiko/inc/lispmap.h
;; For BIGVM: ATOMS_OFFSET = 0x2c0000 (byte offset)
(defconstant +atoms-offset+ #x2c0000
  "Byte offset for AtomSpace (BIGVM)")

;; For non-BIGATOMS: Valspace/Defspace offsets (DLword offsets)
(defconstant +defs-offset-dlwords+ #xA0000
  "Defspace offset in DLwords")
(defconstant +vals-offset-dlwords+ #xC0000
  "Valspace offset in DLwords")

;; Build configuration
;; For Medley with BIGVM, uses 32-bit atom indices (BIGATOMS)
;; Per C introspection: bigvm=1, bigatoms=1
(defparameter *bigatoms* t
  "Whether using BIGATOMS mode (32-bit atom indices)")

;;;============================================================================
;;; Atom Table Access Functions
;;;============================================================================

(defun get-valcell (vm atom-index)
  "Get value cell byte offset for an atom.

   Per C: GetVALCELL68k(atom_index)

   For non-BIGATOMS: GetLongWord(Valspace + ((x) << 1))
   Returns byte offset in virtual memory."
  (declare (type (unsigned-byte 32) atom-index))
  (if (not *bigatoms*)
      ;; Non-BIGATOMS: Valspace indexing
      ;; Byte offset = (VALS_OFFSET + (atom_index << 1)) * 2
      (let ((laddr-dlwords (+ +vals-offset-dlwords+ (ash atom-index 1))))
        (* laddr-dlwords 2))

      ;; BIGATOMS: Check NEWATOM vs LITATOM
      (if (/= (logand atom-index +segmask+) 0)
          ;; NEWATOM
          (let ((value-offset-bytes (* +newatom-value-offset+ 2)))
            (+ atom-index value-offset-bytes))

          ;; LITATOM (BIGVM)
          (let* ((cells (+ (* atom-index 5) +newatom-value-ptroff+))
                 (laddr-dlwords (+ +atoms-offset+ (* cells 2))))
            (* laddr-dlwords 2)))))

(defun get-defcell (vm atom-index)
  "Get definition cell byte offset for an atom.

   Per C: GetDEFCELL68k(atom_index)

   Returns byte offset in virtual memory."
  (declare (type (unsigned-byte 32) atom-index))
  (let ((vmem (maiko-lisp.vm:vm-virtual-memory vm)))
    (unless vmem
      (return-from get-defcell 0))

    (if (not *bigatoms*)
        ;; Non-BIGATOMS: Defspace indexing
        ;; Byte offset = (DEFS_OFFSET + (atom_index << 1)) * 2
        (let* ((laddr-dlwords (+ +defs-offset-dlwords+ (ash atom-index 1)))
               (byte-off (* laddr-dlwords 2)))
          (if (>= (+ byte-off 4) (length vmem)) 0 byte-off))

        ;; BIGATOMS
        (if (/= (logand atom-index +segmask+) 0)
            ;; NEWATOM
            (let ((defn-offset-bytes (* +newatom-defn-offset+ 2))
                  (defn-addr (+ atom-index (* +newatom-defn-offset+ 2))))
              (if (>= (+ defn-addr 4) (length vmem)) 0 defn-addr))

            ;; LITATOM (BIGVM)
            (let* ((cells (+ (* atom-index 5) +newatom-defn-ptroff+))
                   (laddr-dlwords (+ +atoms-offset+ (* cells 2)))
                   (defn-offset (* laddr-dlwords 2)))
              (if (>= (+ defn-offset 4) (length vmem)) 0 defn-offset))))))

(defun read-atom-value (vm atom-index)
  "Read value from atom's value cell.

   Per C: GVAR macro - reads value from value cell."
  (declare (type (unsigned-byte 32) atom-index))
  (let ((vmem (maiko-lisp.vm:vm-virtual-memory vm)))
    (unless vmem
      (return-from read-atom-value 0))
    (let ((value-cell-offset (get-valcell vm atom-index)))
      (when (>= (+ value-cell-offset 4) (length vmem))
        (return-from read-atom-value 0))
      ;; Read LispPTR from virtual memory (little-endian, already swapped)
      (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
        ;; Read with XOR addressing for bytes
        (loop for i from 0 below 4
              for xor-addr = (logxor (+ value-cell-offset i) 3)
              for page-num = (ash xor-addr -9)
              for page-offset = (logand xor-addr #x1FF))
        ;; For now, use simple read without XOR for atoms
        (let ((page-num (ash value-cell-offset -9))
              (page-offset (logand value-cell-offset #x1FF)))
          (when (>= page-num (length vmem))
            (return-from read-atom-value 0))
          (let ((page (aref vmem page-num)))
            (if (null page)
                0
                (logior (ash (aref page page-offset) 24)
                        (ash (aref page (1+ page-offset)) 16)
                        (ash (aref page (+ page-offset 2)) 8)
                        (aref page (+ page-offset 3))))))))))

(defun write-atom-value (vm atom-index value)
  "Write value to atom's value cell.

   Per C: GVAR_ opcode - writes value to value cell."
  (declare (type (unsigned-byte 32) atom-index value))
  (let ((vmem (maiko-lisp.vm:vm-virtual-memory vm)))
    (unless vmem
      (return-from write-atom-value nil))
    (let ((value-cell-offset (get-valcell vm atom-index)))
      (when (>= (+ value-cell-offset 4) (length vmem))
        (return-from write-atom-value nil))
      (let ((page-num (ash value-cell-offset -9))
            (page-offset (logand value-cell-offset #x1FF)))
        (when (>= page-num (length vmem))
          (return-from write-atom-value nil))
        (let ((page (aref vmem page-num)))
          (when page
            ;; Write with XOR byte addressing
            (setf (aref page (logxor page-offset 3)) (logand (ash value -24) #xFF)
                  (aref page (logxor (1+ page-offset) 3)) (logand (ash value -16) #xFF)
                  (aref page (logxor (+ page-offset 2) 3)) (logand (ash value -8) #xFF)
                  (aref page (logxor (+ page-offset 3) 3)) (logand value #xFF)))))))
  value)
