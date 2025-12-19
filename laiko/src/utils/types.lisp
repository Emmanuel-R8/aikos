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
