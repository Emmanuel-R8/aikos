(in-package :maiko-lisp.data)

;; Frame Extension structure (matches C frameex1 for BIGVM)
;; Per maiko/inc/stack.h

(defconstant +framesize+ 10 "Size of frameex1 in DLwords")

;; CRITICAL: STK_OFFSET is a DLword offset, not a byte offset!
;; Per maiko/inc/lispmap.h: STK_OFFSET = 0x00010000 (DLword offset)
;; So stackspace byte offset = STK_OFFSET * 2 = 0x00020000
(defconstant +stk-offset-dlword+ #x00010000 "Stack offset in DLwords")
(defconstant +stackspace-byte-offset+ #x00020000 "Stack offset in bytes (STK_OFFSET * 2)")

(defstruct (frame-extension (:conc-name fx-))
  "Frame Extension structure (C FX from stack.h)"
  (flags 0 :type (unsigned-byte 8))
  (usecount 0 :type (unsigned-byte 8))
  (alink 0 :type (unsigned-byte 16))
  (fnheader 0 :type (unsigned-byte 32))
  (nextblock 0 :type (unsigned-byte 16))
  (pc 0 :type (unsigned-byte 16))
  (nametable 0 :type (unsigned-byte 32))
  (blink 0 :type (unsigned-byte 16))
  (clink 0 :type (unsigned-byte 16)))

(defun read-fx-from-vm (virtual-memory stack-offset)
  "Read Frame Extension from virtual memory at stack offset.

   stack-offset is a DLword offset from Stackspace (NOT from Lisp_world!).
   Returns a frame-extension structure.

   Per C code (maiko/src/main.c and zaiko/src/vm/vm_initialization.zig):
   - Stackspace = NativeAligned2FromLAddr(STK_OFFSET)
   - STK_OFFSET = 0x00010000 (DLword offset from Lisp_world)
   - Stackspace byte offset = STK_OFFSET * 2 = 0x20000
   - Frame byte offset = Stackspace + (stack-offset * 2)
   - FX is 10 DLwords (20 bytes)"
  (declare (type (unsigned-byte 32) stack-offset))
  ;; CRITICAL FIX: Use stackspace byte offset, not STK_OFFSET directly
  ;; Per Zig: frame_offset = stackspace_byte_offset + (currentfxp * 2)
  ;; Where stackspace_byte_offset = STK_OFFSET * 2 = 0x20000
  (let ((byte-offset (+ +stackspace-byte-offset+ (* stack-offset 2))))
    (let* ((word0 (or (get-vm-word virtual-memory byte-offset) 0))
           (word1 (or (get-vm-word virtual-memory (+ byte-offset 2)) 0))
           ;; fnheader is at bytes 4-7 (after flags+usecount and alink)
           ;; For non-BIGVM: fnheader = hi2fnheader << 16 | lofnheader (24-bit)
           ;; For BIGVM: fnheader is full 32-bit LispPTR
           ;; Per Zig vm_initialization.zig:100-108
           ;; After 32-bit byte-swap: bytes[4,5]=lofnheader, bytes[6,7]=[hi1,hi2]
           (lofnheader (or (get-vm-word virtual-memory (+ byte-offset 4)) 0))
           (hi1-hi2 (or (get-vm-word virtual-memory (+ byte-offset 6)) 0))
           (hi2fnheader (logand hi1-hi2 #xFF))
           (fnheader (logior (ash hi2fnheader 16) lofnheader))
           ;; nextblock at bytes 10-11, pc at bytes 8-9
           (pc (or (get-vm-word virtual-memory (+ byte-offset 8)) 0))
           (nextblock (or (get-vm-word virtual-memory (+ byte-offset 10)) 0))
           (nametable-high (or (get-vm-word virtual-memory (+ byte-offset 12)) 0))
           (nametable-low (or (get-vm-word virtual-memory (+ byte-offset 14)) 0))
           (nametable (logior (ash nametable-high 16) nametable-low))
           (blink (or (get-vm-word virtual-memory (+ byte-offset 16)) 0))
           (clink (or (get-vm-word virtual-memory (+ byte-offset 18)) 0)))
      (make-frame-extension
       :flags (logand word0 #xFF)
       :usecount (ash word0 -8)
       :alink word1
       :fnheader fnheader
       :nextblock nextblock
       :pc pc
       :nametable nametable
       :blink blink
       :clink clink))))
