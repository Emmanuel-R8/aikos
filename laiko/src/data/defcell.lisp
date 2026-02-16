;;;; defcell.lisp - Definition Cell Structure for Laiko
;;;;
;;;; Implements definition cell structure matching C DefCell from maiko/inc/cell.h
;;;; Per zaiko/src/data/defcell.zig

(in-package :maiko-lisp.data)

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

(defun read-defcell (vm atom-index)
  "Read DefCell from atom's definition cell.
   
   Per C: GetDEFCELL68k(atom_index) then read DefCell structure.
   
   Returns a defcell structure."
  (declare (type (unsigned-byte 32) atom-index))
  (let ((vmem (maiko-lisp.vm:vm-virtual-memory vm)))
    (unless vmem
      (return-from read-defcell (make-defcell-raw)))
    
    (let ((defcell-offset (get-defcell vm atom-index)))
      (when (zerop defcell-offset)
        (return-from read-defcell (make-defcell-raw)))
      
      (when (>= (+ defcell-offset 4) (length vmem))
        (return-from read-defcell (make-defcell-raw)))
      
      ;; Read first LispPTR from virtual memory
      ;; Use vm-read-lispptr from stack.lisp
      (let* ((first-ptr (vm-read-lispptr vm defcell-offset))
             ;; Extract fields from first LispPTR
             (ccodep (logand (ash first-ptr -31) 1))
             (fastp (logand (ash first-ptr -30) 1))
             (argtype (logand (ash first-ptr -28) 3))
             ;; For BIGVM: 28-bit defpointer (low 28 bits)
             ;; For non-BIGVM: 24-bit defpointer (low 24 bits)
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

(defun read-function-header (vm fnheader-offset)
  "Read function header from virtual memory.
   
   Per C: struct fnhead at offset.
   Returns a function-header structure."
  (declare (type (unsigned-byte 32) fnheader-offset))
  (let ((vmem (maiko-lisp.vm:vm-virtual-memory vm)))
    (unless vmem
      (return-from read-function-header nil))
    
    (let ((page-num (ash fnheader-offset -9))
          (page-offset (logand fnheader-offset #x1FF)))
      (when (>= page-num (length vmem))
        (return-from read-function-header nil))
      
      (let ((page (aref vmem page-num)))
        (when (null page)
          (return-from read-function-header nil))
        
        ;; Read function header fields
        ;; startpc is at bytes 0-1
        ;; nv is at bytes 2-3 (number of variables)
        ;; See maiko/inc/stack.h struct fnhead
        (let ((startpc (logior (ash (aref page page-offset) 8)
                               (aref page (1+ page-offset))))
              (nv (logior (ash (aref page (+ page-offset 2)) 8)
                          (aref page (+ page-offset 3))))
              (na (logior (ash (aref page (+ page-offset 4)) 8)
                          (aref page (+ page-offset 5)))))
          (make-function-header
           :startpc startpc
           :nv nv
           :na na))))))
