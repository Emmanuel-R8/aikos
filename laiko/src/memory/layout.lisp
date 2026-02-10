(in-package :maiko-lisp.memory)

;; Memory layout
;; Per rewrite documentation memory/memory-layout.md

;; Memory region offsets (matches C defines)
(defconstant +ifpage-offset+ #x00000000)
(defconstant +stk-offset+ #x00010000)
(defconstant +atmht-offset+ #x00020000)
(defconstant +atoms-offset+ #x00030000)
;; NOTE: Use DEFVAR/DEFPARAMETER here to avoid DEFCONSTANT redefinition
;; issues when reloading during development; treat it as constant in practice.
(defparameter +mds-offset+ #x00100000)
(defconstant +ds-offset+ #x00200000)
(defconstant +vs-offset+ #x00300000)
(defconstant +cs-offset+ #x00400000)
(defconstant +ss-offset+ #x00500000)
(defconstant +ts-offset+ #x00600000)
(defconstant +ps-offset+ #x00700000)
(defconstant +es-offset+ #x00800000)
(defconstant +fs-offset+ #x00900000)
(defconstant +gs-offset+ #x00A00000)
(defconstant +hs-offset+ #x00B00000)
(defconstant +is-offset+ #x00C00000)
(defconstant +js-offset+ #x00D00000)
(defconstant +ks-offset+ #x00E00000)
(defconstant +ls-offset+ #x00F00000)

;; Page size constants
(defconstant +page-size+ #x10000) ; 64KB
(defconstant +page-mask+ #xFFFF)

(defun get-page-number (addr)
  "Get page number from LispPTR address"
  (declare (type maiko-lisp.utils:lisp-ptr addr))
  (ash addr -16)) ; Divide by PAGE_SIZE (2^16)

(defun get-page-offset (addr)
  "Get offset within page from LispPTR address"
  (declare (type maiko-lisp.utils:lisp-ptr addr))
  (logand addr +page-mask+))
