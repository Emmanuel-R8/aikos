(in-package :maiko-lisp.data)

;; Bytecode extraction from virtual memory
;; Per maiko/src/xc.c (bytecode extraction)

(defun extract-bytecode-from-vm (virtual-memory start-address &optional (max-bytes 4096))
  "Extract bytecode from virtual memory starting at address.

   Parameters:
   - virtual-memory: Array of page arrays from load-sysout
   - start-address: LispPTR where bytecode starts
   - max-bytes: Maximum bytes to extract (default 4KB)

   Returns: Simple array of bytecode bytes

   Per maiko/src/xc.c: Bytecode is accessed via PC-relative addressing
   through the virtual memory system."
  (declare (type (simple-array (simple-array (unsigned-byte 8) (*)) (*)) virtual-memory)
           (type (unsigned-byte 32) start-address)
           (type (integer 1 *) max-bytes)
           (optimize (speed 3)))
  (let* ((num-pages (length virtual-memory))
         (bytecode (make-array max-bytes :element-type '(unsigned-byte 8)))
         (offset 0))
    (loop while (< offset max-bytes)
          do (let* ((address (+ start-address offset))
                    (page-num (ash address -9))
                    (page-offset (logand address #x1FF))
                    (byte-index page-offset))
               (when (>= page-num num-pages)
                 (return bytecode))
               (let ((page (aref virtual-memory page-num)))
                 (when (null page)
                   (return bytecode))
                 (when (>= byte-index 512)
                   (return bytecode))
                 (setf (aref bytecode offset)
                       (aref page byte-index))))
               (incf offset))
    bytecode))

(defun get-vm-byte (virtual-memory address)
  "Get a single byte from virtual memory at address.

   Returns the byte value, or 0 if address is invalid."
  (declare (type (simple-array (simple-array (unsigned-byte 8) (*)) (*)) virtual-memory)
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
   Words are stored big-endian in sysout, may need byte-swapping."
  (declare (type (simple-array (simple-array (unsigned-byte 8) (*)) (*)) virtual-memory)
           (type (unsigned-byte 32) address)
           (optimize (speed 3)))
  (let* ((page-num (ash address -9))
         (page-offset (logand address #x1FF)))
    (let ((aligned-offset (logand page-offset #xFFFE)))
      (when (and (< page-num (length virtual-memory))
                 (< (+ aligned-offset 1) 512))
        (let ((page (aref virtual-memory page-num)))
          (when page
            (let ((low-byte (aref page aligned-offset))
                  (high-byte (aref page (1+ aligned-offset))))
              (if (maiko-lisp.utils:little-endian-p)
                  (logior (ash high-byte 8) low-byte)
                  (logior (ash low-byte 8) high-byte)))))))))

(defun read-bytecode-word (virtual-memory pc)
  "Read a 16-bit word from bytecode stream at PC.

   Used for reading opcode operands that span multiple bytes.
   Returns (values word new-pc)."
  (declare (type (simple-array (simple-array (unsigned-byte 8) (*)) (*)) virtual-memory)
           (type (unsigned-byte 32) pc)
           (optimize (speed 3)))
  (let ((word (get-vm-word virtual-memory pc)))
    (values word (+ pc 2))))
