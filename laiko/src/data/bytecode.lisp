(in-package :maiko-lisp.data)

;; Bytecode extraction from virtual memory
;; Per maiko/src/xc.c (bytecode extraction)

;;;============================================================================
;;; Constants
;;;============================================================================

(defconstant +xor-byte-mask+ 3
  "XOR mask for byte access in BYTESWAP mode. Per C: GETBYTE(base) = *(base ^ 3)")

(defconstant +xor-word-mask+ 2
  "XOR mask for word access in BYTESWAP mode. Per C: GETWORD(base) = *(base ^ 2)")

;;;============================================================================
;;; XOR Addressing Utilities
;;;============================================================================

(defun apply-xor-addressing-byte (address)
  "Apply XOR addressing for byte access (BYTESWAP mode).
   Per maiko/src/adr68k.h and Zig memory_access.zig:
   GETBYTE(base) = *(unsigned char *)(3 ^ (UNSIGNED)(base))
   
   For address 0x60F130, reads from 0x60F130 ^ 3 = 0x60F133"
  (declare (type (unsigned-byte 32) address))
  (logxor address +xor-byte-mask+))

(defun apply-xor-addressing-word (address)
  "Apply XOR addressing for word access (BYTESWAP mode).
   Per C: GETWORD(base) = *(DLword *)(2 ^ (UNSIGNED)(base))"
  (declare (type (unsigned-byte 32) address))
  (logxor address +xor-word-mask+))

;;;============================================================================
;;; Bytecode Extraction
;;;============================================================================

(defun extract-bytecode-from-vm (virtual-memory start-address &optional (max-bytes 4096))
  "Extract bytecode from virtual memory starting at address.

   Parameters:
   - virtual-memory: Array of page arrays from load-sysout
   - start-address: LispPTR where bytecode starts
   - max-bytes: Maximum bytes to extract (default 4KB)

   Returns: Simple array of bytecode bytes

   CRITICAL: Pages were 32-bit word-swapped during load, so bytecode
   access requires XOR addressing. Per C: GETBYTE(base) = *(base ^ 3)
   This compensates for the word swap."
  (declare (type (unsigned-byte 32) start-address)
           (type (integer 1 *) max-bytes)
           (optimize (speed 3)))
  (unless (and virtual-memory (arrayp virtual-memory))
    (return-from extract-bytecode-from-vm
      (make-array max-bytes :element-type '(unsigned-byte 8) :initial-element 0)))
  (let* ((num-pages (length virtual-memory))
         (bytecode (make-array max-bytes :element-type '(unsigned-byte 8)))
         (offset 0))
    (loop while (< offset max-bytes)
          do (let* ((address (+ start-address offset))
                    ;; XOR addressing needed for bytecode after word swap
                    (xor-address (logxor address +xor-byte-mask+))
                    (page-num (ash xor-address -9))
                    (page-offset (logand xor-address #x1FF)))
               (when (>= page-num num-pages)
                 (return bytecode))
               (let ((page (aref virtual-memory page-num)))
                 (if (null page)
                     (setf (aref bytecode offset) 0)
                     (progn
                       (when (>= page-offset 512)
                         (return bytecode))
                       (setf (aref bytecode offset)
                             (aref page page-offset)))))
               (incf offset)))
    bytecode))

(defun get-vm-byte (virtual-memory address)
  "Get a single byte from virtual memory at address.

   Returns the byte value, or 0 if address is invalid or page not loaded."
  (declare (type (simple-array (or null (simple-array (unsigned-byte 8) (*))) (*)) virtual-memory)
           (type (unsigned-byte 32) address)
           (optimize (speed 3)))
  (let* ((page-num (ash address -9))
         (page-offset (logand address #x1FF)))
    (when (and (< page-num (length virtual-memory))
               (< page-offset 512))
      (let ((page (aref virtual-memory page-num)))
        (when page
          (aref page page-offset))))))

(defun get-vm-word (virtual-memory address)
  "Get a 16-bit word from virtual memory at address.

   Per maiko/src/adr68k.h: NativeAligned2FromLAddr(address) for word access.
   Words are stored big-endian in sysout, may need byte-swapping.
   Returns 0 if page not loaded or address invalid."
  (declare (type (simple-array (or null (simple-array (unsigned-byte 8) (*))) (*)) virtual-memory)
           (type (unsigned-byte 32) address)
           (optimize (speed 3)))
  (let* ((page-num (ash address -9))
         (page-offset (logand address #x1FF)))
    (let ((aligned-offset (logand page-offset #xFFFE)))
      (if (and (< page-num (length virtual-memory))
               (< (+ aligned-offset 1) 512))
          (let ((page (aref virtual-memory page-num)))
            (if page
                (let ((low-byte (aref page aligned-offset))
                      (high-byte (aref page (1+ aligned-offset))))
                  (if (maiko-lisp.utils:little-endian-p)
                      (logior (ash high-byte 8) low-byte)
                      (logior (ash low-byte 8) high-byte)))
                0))
          0))))

(defun read-bytecode-word (virtual-memory pc)
  "Read a 16-bit word from bytecode stream at PC.

   Used for reading opcode operands that span multiple bytes.
   Returns (values word new-pc)."
  (declare (type (simple-array (or null (simple-array (unsigned-byte 8) (*))) (*)) virtual-memory)
           (type (unsigned-byte 32) pc)
           (optimize (speed 3)))
  (let ((word (get-vm-word virtual-memory pc)))
    (values word (+ pc 2))))
