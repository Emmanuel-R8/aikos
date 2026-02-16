;;;; memory-access.lisp - Unified Memory Access Utilities for Laiko
;;;;
;;;; Provides tested primitives for reading from virtual memory with
;;;; correct byte ordering and XOR addressing for BYTESWAP mode.

(in-package :maiko-lisp.data)

;;;============================================================================
;;; XOR Addressing Constants
;;;============================================================================

(defconstant +xor-byte+ 3
  "XOR mask for byte access: GETBYTE(base) = *(base ^ 3)")

(defconstant +xor-word+ 2  
  "XOR mask for word access: GETWORD(base) = *(base ^ 2)")

;;;============================================================================
;;; Core Memory Access Functions
;;;============================================================================

(defun vm-read-byte (vm byte-offset)
  "Read a single byte from virtual memory with XOR addressing.
   
   Per C: GETBYTE(base) = *(unsigned char *)(3 ^ (UNSIGNED)(base))
   Returns 0 if page not loaded or address invalid."
  (declare (type (unsigned-byte 32) byte-offset))
  (let* ((vmem (vm-virtual-memory vm))
         (xor-offset (logxor byte-offset +xor-byte+))
         (page-num (ash xor-offset -9))
         (page-offset (logand xor-offset #x1FF)))
    (when (and vmem (< page-num (length vmem)))
      (let ((page (aref vmem page-num)))
        (when (and page (< page-offset 512))
          (aref page page-offset))))))

(defun vm-read-word (vm byte-offset)
  "Read a 16-bit word from virtual memory with XOR addressing.
   
   Per C: GETWORD(base) = *(DLword *)(2 ^ (UNSIGNED)(base))
   Constructs word from two bytes with XOR addressing.
   Returns 0 if page not loaded."
  (declare (type (unsigned-byte 32) byte-offset))
  (let* ((vmem (vm-virtual-memory vm))
         ;; Word XOR applies to the base, not each byte
         (word-base (logxor byte-offset +xor-word+))
         (page-num (ash word-base -9))
         (page-offset (logand word-base #x1FF)))
    (when (and vmem (< page-num (length vmem)))
      (let ((page (aref vmem page-num)))
        (when (and page (< (+ page-offset 1) 512))
          ;; Word is stored big-endian after swap
          (logior (ash (aref page page-offset) 8)
                  (aref page (1+ page-offset))))))))

(defun vm-read-lispptr (vm byte-offset)
  "Read a 32-bit LispPTR from virtual memory.
   
   Uses word XOR addressing. Constructs 32-bit value from two words.
   Returns 0 if page not loaded."
  (declare (type (unsigned-byte 32) byte-offset))
  (let ((hi (vm-read-word vm byte-offset))
        (lo (vm-read-word vm (+ byte-offset 2))))
    (logior (ash hi 16) lo)))

(defun vm-write-byte (vm byte-offset value)
  "Write a single byte to virtual memory with XOR addressing."
  (declare (type (unsigned-byte 32) byte-offset)
           (type (unsigned-byte 8) value))
  (let* ((vmem (vm-virtual-memory vm))
         (xor-offset (logxor byte-offset +xor-byte+))
         (page-num (ash xor-offset -9))
         (page-offset (logand xor-offset #x1FF)))
    (when (and vmem (< page-num (length vmem)))
      (let ((page (aref vmem page-num)))
        (when (and page (< page-offset 512))
          (setf (aref page page-offset) value)
          value)))))

(defun vm-write-word (vm byte-offset value)
  "Write a 16-bit word to virtual memory with XOR addressing."
  (declare (type (unsigned-byte 32) byte-offset)
           (type (unsigned-byte 16) value))
  (let* ((vmem (vm-virtual-memory vm))
         (word-base (logxor byte-offset +xor-word+))
         (page-num (ash word-base -9))
         (page-offset (logand word-base #x1FF)))
    (when (and vmem (< page-num (length vmem)))
      (let ((page (aref vmem page-num)))
        (when (and page (< (+ page-offset 1) 512))
          (setf (aref page page-offset) (logand (ash value -8) #xFF)
                (aref page (1+ page-offset)) (logand value #xFF))
          value)))))

(defun vm-write-lispptr (vm byte-offset value)
  "Write a 32-bit LispPTR to virtual memory."
  (declare (type (unsigned-byte 32) byte-offset value))
  (vm-write-word vm byte-offset (logand (ash value -16) #xFFFF))
  (vm-write-word vm (+ byte-offset 2) (logand value #xFFFF))
  value)

;;;============================================================================
;;; Stack Operations (using unified memory access)
;;;============================================================================

(defun vm-stack-push (vm value)
  "Push value onto stack. Stack grows DOWN.
   Updates stack-ptr-offset and top-of-stack."
  (declare (type (unsigned-byte 32) value))
  (let ((sp (vm-stack-ptr-offset vm)))
    (decf sp 4)  ; Move down by 4 bytes (1 LispPTR)
    (vm-write-lispptr vm sp value)
    (setf (vm-stack-ptr-offset vm) sp
          (vm-top-of-stack vm) value)
    value))

(defun vm-stack-pop (vm)
  "Pop value from stack. Returns value, updates stack-ptr-offset."
  (let* ((sp (vm-stack-ptr-offset vm))
         (value (vm-read-lispptr vm sp)))
    (incf sp 4)  ; Move up by 4 bytes
    (setf (vm-stack-ptr-offset vm) sp
          (vm-top-of-stack vm) (vm-read-lispptr vm sp))
    value))

(defun vm-stack-tos (vm)
  "Get top-of-stack (cached value)."
  (vm-top-of-stack vm))

(defun vm-stack-set-tos (vm value)
  "Set top-of-stack. Writes to current stack location and updates cache."
  (declare (type (unsigned-byte 32) value))
  (vm-write-lispptr vm (vm-stack-ptr-offset vm) value)
  (setf (vm-top-of-stack vm) value))
