(in-package :maiko-lisp.utils)

;; Core types for Maiko VM implementation
;; All types must maintain exact compatibility with C implementation

(deftype lisp-ptr ()
  "32-bit virtual address (matches C LispPTR)"
  '(unsigned-byte 32))

(deftype dlword ()
  "16-bit word (matches C DLword)"
  '(unsigned-byte 16))

(deftype bytecode ()
  "Bytecode instruction byte"
  '(unsigned-byte 8))

;; Platform detection
(defun little-endian-p ()
  "Returns T if running on a little-endian host.
   
   DISCUSSION:
   Sysout files are stored in big-endian format. On little-endian hosts
   (x86/x64), byte-swapping is required. This function detects the host
   endianness at runtime.
   
   Per maiko/src/ldsout.c: BYTESWAP is defined on little-endian hosts."
  (let ((test-32 #x12345678))
    (declare (type (unsigned-byte 32) test-32))
    ;; On little-endian, low byte is at lowest address
    (= (logand test-32 #xFF) #x78)))

;; Address component extraction (matches C macros)
(defun hiloc (ptr)
  "Extract high 16 bits from LispPTR"
  (declare (type lisp-ptr ptr))
  (ldb (byte 16 16) ptr))

(defun loloc (ptr)
  "Extract low 16 bits from LispPTR"
  (declare (type lisp-ptr ptr))
  (ldb (byte 16 0) ptr))

;; Constants
(defconstant +nil-ptr+ 0
  "NIL pointer value")

(defconstant +t-ptr+ 1
  "T pointer value (fixnum encoding)")
