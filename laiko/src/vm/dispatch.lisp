(in-package :maiko-lisp.vm)

;; dispatch loop
;; Per rewrite documentation vm-core/execution-model.md

(defun fetch-instruction-byte (pc code)
  "Fetch instruction byte at PC"
  (declare (type maiko-lisp.utils:lisp-ptr pc)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code))
  (if (< pc (length code))
      (aref code pc)
      0))

(defun fetch-operands (pc code length)
  "Fetch operands for instruction starting at PC.
   Returns list of operand bytes (excluding opcode byte)."
  (declare (type maiko-lisp.utils:lisp-ptr pc)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code)
           (type (integer 1 *) length))
  (let ((operand-count (1- length)))
    (if (<= (+ pc length) (length code))
        (loop for i from 1 to operand-count
              collect (aref code (+ pc i)))
        nil)))

(defun decode-opcode (byte)
  "Decode opcode from byte. Returns opcode value."
  (declare (type maiko-lisp.utils:bytecode byte))
  byte)

(defun get-instruction-length (opcode)
  "Get instruction length for opcode based on opcode value.
   Returns total instruction length in bytes (opcode + operands)."
  (declare (type (unsigned-byte 8) opcode))
  (case opcode
    ;; 1-byte opcodes
    (#x67 3) ; ACONST
    (#x68 1) ; NIL
    (#x69 1) ; T
    (#x6A 1) ; CONST_0
    (#x6B 1) ; CONST_1
    (#x6C 2) ; SIC
    (#x6D 2) ; SNIC
    (#x6E 3) ; SICX
    (#x6F 3) ; GCONST
    (#x01 1) ; CAR
    (#x02 1) ; CDR
    (#x1A 1) ; CONS
    (#x18 1) ; RPLACA
    (#x19 1) ; RPLACD
    (#x1F 1) ; CREATECELL
    (#x26 1) ; RPLCONS
    (#x27 1) ; NTH
    (#x28 1) ; NTHCDR
    (#x29 1) ; LAST
    (#x2A 1) ; LISTLENGTH
    (#xD8 1) ; IPLUS2
    (#xD9 1) ; IDIFFERENCE
    (#xDA 1) ; ITIMES2
    (#xDB 1) ; IQUO
    (#xDC 1) ; IREM
    (#x3A 1) ; EQ
    (#x3B 1) ; EQL
    (#xBF 1) ; POP
    (#xD0 1) ; PUSH/ADDBASE
    (#x10 1) ; RETURN
    (#x40 1) ; IVAR0
    (#x41 1) ; IVAR1
    (#x42 1) ; IVAR2
    (#x43 1) ; IVAR3
    (#x44 1) ; IVAR4
    (#x45 1) ; IVAR5
    (#x46 1) ; IVAR6
    (#x48 1) ; PVAR0
    (#x49 1) ; PVAR1
    (#x4A 1) ; PVAR2
    (#x4B 1) ; PVAR3
    (#x4C 1) ; PVAR4
    (#x4D 1) ; PVAR5
    (#x4E 1) ; PVAR6
    (#x50 1) ; FVAR0
    (#x51 1) ; FVAR1
    (#x52 1) ; FVAR2
    (#x53 1) ; FVAR3
    (#x54 1) ; FVAR4
    (#x55 1) ; FVAR5
    (#x56 1) ; FVAR6
    (#xE0 1) ; LOGAND
    (#xE1 1) ; LOGIOR
    (#xE2 1) ; LOGXOR
    (#xE3 1) ; LOGNOT
    (#xE4 1) ; LOGOR2
    (#xE5 1) ; LOGAND2
    (#xE6 1) ; LOGXOR2
    (#xEC 1) ; LSH
    (#xD4 1) ; PLUS2
    (#xD5 1) ; DIFFERENCE
    (#xD6 1) ; TIMES2
    (#xD7 1) ; QUOTIENT
    (#xE8 1) ; FPLUS2
    (#xE9 1) ; FDIFFERENCE
    (#xEA 1) ; FTIMES2
    (#xEB 1) ; FQUOTIENT
    (#x9A 1) ; IMINUS
    (#x9B 1) ; IDIVIDE
    (#x9C 1) ; IMOD
    (#x12 1) ; UNBIND
    (#x64 1) ; COPY
    (#x2F 1) ; STKSCAN
    (#x65 1) ; MYARGCOUNT
    (#x61 1) ; ARG0
    (#x3E 1) ; LEQ
    (#x3F 1) ; GEQ
    (#x0E 1) ; APPLYFN
    (#x07 1) ; UNWIND
    (#x27 1) ; LISTGET
    (#x16 1) ; ASSOC
    (#x1C 1) ; FMEMB
    ;; 2-byte opcodes
    (#x60 3) ; GVAR
    (#x05 2) ; TYPEP
    (#x08 2) ; FN0
    (#x09 2) ; FN1
    (#x0A 2) ; FN2
    (#x0B 2) ; FN3
    (#x0C 2) ; FN4
    (#x11 2) ; BIND
    (#x0D 4) ; FNX
    (#xEE 2) ; GETAEL1
    (#xEF 3) ; GETAEL2
    (#xB6 1) ; AREF1
    (#xB7 1) ; ASET1
    (#xC2 1) ; GETBASEBYTE
    (#xC7 1) ; PUTBASEBYTE
    (#xC8 2) ; GETBASE_N
    (#xC9 2) ; GETBASEPTR_N
    (#xCD 2) ; PUTBASE_N
    (#xCE 2) ; PUTBASEPTR_N
    (#xD2 1) ; HILOC
    (#xD3 1) ; LOLOC
    (#xF0 1) ; EQ (alt)
    (#xF3 1) ; GREATERP (alt)
    (#xF4 1) ; EQUAL (alt)
    (#xF5 1) ; MAKENUMBER
    (#xFF 1) ; CL_EQUAL
    (#xB8 1) ; PVARSETPOP0
    (#xB9 1) ; PVARSETPOP1
    (#xBA 1) ; PVARSETPOP2
    (#xBB 1) ; PVARSETPOP3
    (#xBC 1) ; PVARSETPOP4
    (#xBD 1) ; PVARSETPOP5
    (#xBE 1) ; PVARSETPOP6
    (#xC0 2) ; POP_N
    (#x15 2) ; GCREF
    (#x13 1) ; DUNBIND
    (#xF1 1) ; IGREATERP
    ;; Default: assume 1 byte
    (otherwise 1)))

;; Byte-to-symbol opcode map for dispatch
(defvar *byte-opcode-map* (make-hash-table :test 'eql))

;; Handler registry - maps opcode names to handler functions
(defvar *opcode-handlers* (make-hash-table :test 'eq))

;; Register an opcode handler
(defun register-opcode-handler (opcode handler)
  "Register a handler function for an opcode"
  (setf (gethash opcode *opcode-handlers*) handler))

;; Get an opcode handler
(defun get-opcode-handler (opcode)
  "Get the handler function for an opcode"
  (gethash opcode *opcode-handlers*))

;; Initialize the byte-to-symbol map
(defun initialize-byte-opcode-map ()
  "Initialize the mapping from byte opcodes to symbol names"
  (clrhash *byte-opcode-map*)
  ;; Constants
  (setf (gethash #x67 *byte-opcode-map*) 'aconst)
  (setf (gethash #x68 *byte-opcode-map*) 'nil)
  (setf (gethash #x69 *byte-opcode-map*) 't)
  (setf (gethash #x6A *byte-opcode-map*) 'const-0)
  (setf (gethash #x6B *byte-opcode-map*) 'const-1)
  (setf (gethash #x6C *byte-opcode-map*) 'sic)
  (setf (gethash #x6D *byte-opcode-map*) 'snic)
  (setf (gethash #x6E *byte-opcode-map*) 'sicx)
  (setf (gethash #x6F *byte-opcode-map*) 'gconst)
  ;; List operations
  (setf (gethash #x01 *byte-opcode-map*) 'car)
  (setf (gethash #x02 *byte-opcode-map*) 'cdr)
  (setf (gethash #x1A *byte-opcode-map*) 'cons)
  (setf (gethash #x18 *byte-opcode-map*) 'rplaca)
  (setf (gethash #x19 *byte-opcode-map*) 'rplacd)
  (setf (gethash #x1F *byte-opcode-map*) 'createcell)
  (setf (gethash #x26 *byte-opcode-map*) 'rplcons)
  ;; Integer arithmetic
  (setf (gethash #xD8 *byte-opcode-map*) 'iplus2)
  (setf (gethash #xD9 *byte-opcode-map*) 'idifference)
  (setf (gethash #xDA *byte-opcode-map*) 'itimes2)
  (setf (gethash #xDB *byte-opcode-map*) 'iquo)
  (setf (gethash #xDC *byte-opcode-map*) 'irem)
  ;; General arithmetic
  (setf (gethash #xD4 *byte-opcode-map*) 'plus2)
  (setf (gethash #xD5 *byte-opcode-map*) 'difference)
  (setf (gethash #xD6 *byte-opcode-map*) 'times2)
  (setf (gethash #xD7 *byte-opcode-map*) 'quotient)
  ;; Comparison
  (setf (gethash #x3A *byte-opcode-map*) 'eq)
  (setf (gethash #x3B *byte-opcode-map*) 'eql)
  (setf (gethash #x3E *byte-opcode-map*) 'leq)
  (setf (gethash #x3F *byte-opcode-map*) 'geq)
  (setf (gethash #xF0 *byte-opcode-map*) 'eq-alt)
  (setf (gethash #xF1 *byte-opcode-map*) 'igreaterp)
  (setf (gethash #xF3 *byte-opcode-map*) 'greaterp-alt)
  (setf (gethash #xF4 *byte-opcode-map*) 'equal-alt)
  (setf (gethash #xF5 *byte-opcode-map*) 'makenumber)
  (setf (gethash #xFF *byte-opcode-map*) 'cl-equal)
  ;; Variable access
  (setf (gethash #x40 *byte-opcode-map*) 'ivar0)
  (setf (gethash #x41 *byte-opcode-map*) 'ivar1)
  (setf (gethash #x42 *byte-opcode-map*) 'ivar2)
  (setf (gethash #x43 *byte-opcode-map*) 'ivar3)
  (setf (gethash #x44 *byte-opcode-map*) 'ivar4)
  (setf (gethash #x45 *byte-opcode-map*) 'ivar5)
  (setf (gethash #x46 *byte-opcode-map*) 'ivar6)
  (setf (gethash #x48 *byte-opcode-map*) 'pvar0)
  (setf (gethash #x49 *byte-opcode-map*) 'pvar1)
  (setf (gethash #x4A *byte-opcode-map*) 'pvar2)
  (setf (gethash #x4B *byte-opcode-map*) 'pvar3)
  (setf (gethash #x4C *byte-opcode-map*) 'pvar4)
  (setf (gethash #x4D *byte-opcode-map*) 'pvar5)
  (setf (gethash #x4E *byte-opcode-map*) 'pvar6)
  (setf (gethash #x50 *byte-opcode-map*) 'fvar0)
  (setf (gethash #x51 *byte-opcode-map*) 'fvar1)
  (setf (gethash #x52 *byte-opcode-map*) 'fvar2)
  (setf (gethash #x53 *byte-opcode-map*) 'fvar3)
  (setf (gethash #x54 *byte-opcode-map*) 'fvar4)
  (setf (gethash #x55 *byte-opcode-map*) 'fvar5)
  (setf (gethash #x56 *byte-opcode-map*) 'fvar6)
  ;; Control flow
  (setf (gethash #x10 *byte-opcode-map*) 'return)
  (setf (gethash #x80 *byte-opcode-map*) 'jump0)
  (setf (gethash #x81 *byte-opcode-map*) 'jump1)
  (setf (gethash #x82 *byte-opcode-map*) 'jump2)
  (setf (gethash #x83 *byte-opcode-map*) 'jump3)
  (setf (gethash #x84 *byte-opcode-map*) 'jump4)
  (setf (gethash #x85 *byte-opcode-map*) 'jump5)
  (setf (gethash #x86 *byte-opcode-map*) 'jump6)
  (setf (gethash #x87 *byte-opcode-map*) 'jump7)
  (setf (gethash #x88 *byte-opcode-map*) 'jump8)
  (setf (gethash #x89 *byte-opcode-map*) 'jump9)
  (setf (gethash #x8A *byte-opcode-map*) 'jump10)
  (setf (gethash #x8B *byte-opcode-map*) 'jump11)
  (setf (gethash #x8C *byte-opcode-map*) 'jump12)
  (setf (gethash #x8D *byte-opcode-map*) 'jump13)
  (setf (gethash #x8E *byte-opcode-map*) 'jump14)
  (setf (gethash #x8F *byte-opcode-map*) 'jump15)
  (setf (gethash #x90 *byte-opcode-map*) 'fjump0)
  (setf (gethash #x91 *byte-opcode-map*) 'fjump1)
  (setf (gethash #x92 *byte-opcode-map*) 'fjump2)
  (setf (gethash #x93 *byte-opcode-map*) 'fjump3)
  (setf (gethash #x94 *byte-opcode-map*) 'fjump4)
  (setf (gethash #x95 *byte-opcode-map*) 'fjump5)
  (setf (gethash #x96 *byte-opcode-map*) 'fjump6)
  (setf (gethash #x97 *byte-opcode-map*) 'fjump7)
  (setf (gethash #x98 *byte-opcode-map*) 'fjump8)
  (setf (gethash #x99 *byte-opcode-map*) 'fjump9)
  (setf (gethash #x9A *byte-opcode-map*) 'fjump10)
  (setf (gethash #x9B *byte-opcode-map*) 'fjump11)
  (setf (gethash #x9C *byte-opcode-map*) 'fjump12)
  (setf (gethash #x9D *byte-opcode-map*) 'fjump13)
  (setf (gethash #x9E *byte-opcode-map*) 'fjump14)
  (setf (gethash #x9F *byte-opcode-map*) 'fjump15)
  (setf (gethash #xA0 *byte-opcode-map*) 'tjump0)
  (setf (gethash #xA1 *byte-opcode-map*) 'tjump1)
  (setf (gethash #xA2 *byte-opcode-map*) 'tjump2)
  (setf (gethash #xA3 *byte-opcode-map*) 'tjump3)
  (setf (gethash #xA4 *byte-opcode-map*) 'tjump4)
  (setf (gethash #xA5 *byte-opcode-map*) 'tjump5)
  (setf (gethash #xA6 *byte-opcode-map*) 'tjump6)
  (setf (gethash #xA7 *byte-opcode-map*) 'tjump7)
  (setf (gethash #xA8 *byte-opcode-map*) 'tjump8)
  (setf (gethash #xA9 *byte-opcode-map*) 'tjump9)
  (setf (gethash #xAA *byte-opcode-map*) 'tjump10)
  (setf (gethash #xAB *byte-opcode-map*) 'tjump11)
  (setf (gethash #xAC *byte-opcode-map*) 'tjump12)
  (setf (gethash #xAD *byte-opcode-map*) 'tjump13)
  (setf (gethash #xAE *byte-opcode-map*) 'tjump14)
  (setf (gethash #xAF *byte-opcode-map*) 'tjump15)
  (setf (gethash #xB0 *byte-opcode-map*) 'jumpx)
  (setf (gethash #xB2 *byte-opcode-map*) 'fjumpx)
  (setf (gethash #xB3 *byte-opcode-map*) 'tjumpx)
  (setf (gethash #x08 *byte-opcode-map*) 'fn0)
  (setf (gethash #x09 *byte-opcode-map*) 'fn1)
  (setf (gethash #x0A *byte-opcode-map*) 'fn2)
  (setf (gethash #x0B *byte-opcode-map*) 'fn3)
  (setf (gethash #x0C *byte-opcode-map*) 'fn4)
  (setf (gethash #x0D *byte-opcode-map*) 'fnx)
  (setf (gethash #x0E *byte-opcode-map*) 'applyfn)
  ;; Memory operations
  (setf (gethash #xEE *byte-opcode-map*) 'getael1)
  (setf (gethash #xEF *byte-opcode-map*) 'getael2)
  (setf (gethash #xB6 *byte-opcode-map*) 'aref1)
  (setf (gethash #xB7 *byte-opcode-map*) 'aset1)
  (setf (gethash #xC2 *byte-opcode-map*) 'getbasebyte)
  (setf (gethash #xC7 *byte-opcode-map*) 'putbasebyte)
  (setf (gethash #xC8 *byte-opcode-map*) 'getbase-n)
  (setf (gethash #xC9 *byte-opcode-map*) 'getbaseptr-n)
  (setf (gethash #xCD *byte-opcode-map*) 'putbase-n)
  (setf (gethash #xCE *byte-opcode-map*) 'putbaseptr-n)
  (setf (gethash #xD2 *byte-opcode-map*) 'hiloc)
  (setf (gethash #xD3 *byte-opcode-map*) 'loloc)
  (setf (gethash #xD0 *byte-opcode-map*) 'addbase)
  ;; Bitwise operations
  (setf (gethash #xE0 *byte-opcode-map*) 'logand)
  (setf (gethash #xE1 *byte-opcode-map*) 'logior)
  (setf (gethash #xE2 *byte-opcode-map*) 'logxor)
  (setf (gethash #xE3 *byte-opcode-map*) 'lognot)
  (setf (gethash #xE4 *byte-opcode-map*) 'logor2)
  (setf (gethash #xE5 *byte-opcode-map*) 'logand2)
  (setf (gethash #xE6 *byte-opcode-map*) 'logxor2)
  (setf (gethash #xE7 *byte-opcode-map*) 'llsh1)
  (setf (gethash #xE8 *byte-opcode-map*) 'llsh8)
  (setf (gethash #xE9 *byte-opcode-map*) 'lrsh1)
  (setf (gethash #xEA *byte-opcode-map*) 'lrsh8)
  (setf (gethash #xEC *byte-opcode-map*) 'lsh)
  ;; Floating point
  (setf (gethash #xEB *byte-opcode-map*) 'fplus2)
  (setf (gethash #xED *byte-opcode-map*) 'fdifference)
  (setf (gethash #xEE *byte-opcode-map*) 'ftimes2)
  (setf (gethash #xEF *byte-opcode-map*) 'fquotient)
  ;; Miscellaneous
  (setf (gethash #x05 *byte-opcode-map*) 'typep)
  (setf (gethash #x11 *byte-opcode-map*) 'bind)
  (setf (gethash #x12 *byte-opcode-map*) 'unbind)
  (setf (gethash #x13 *byte-opcode-map*) 'dunbind)
  (setf (gethash #x60 *byte-opcode-map*) 'gvar)
  (setf (gethash #x61 *byte-opcode-map*) 'arg0)
  (setf (gethash #x64 *byte-opcode-map*) 'copy)
  (setf (gethash #x65 *byte-opcode-map*) 'myargcount)
  (setf (gethash #xBF *byte-opcode-map*) 'pop)
  (setf (gethash #xC0 *byte-opcode-map*) 'pop-n)
  (setf (gethash #x07 *byte-opcode-map*) 'unwind)
  (setf (gethash #x15 *byte-opcode-map*) 'gcref)
  (setf (gethash #x16 *byte-opcode-map*) 'assoc)
  (setf (gethash #x1C *byte-opcode-map*) 'fmemb)
  (setf (gethash #x27 *byte-opcode-map*) 'listget)
  (setf (gethash #x28 *byte-opcode-map*) 'nth)
  (setf (gethash #x29 *byte-opcode-map*) 'nthcdr)
  (setf (gethash #x2A *byte-opcode-map*) 'last)
  (setf (gethash #x2B *byte-opcode-map*) 'listlength)
  (setf (gethash #x2C *byte-opcode-map*) 'append)
  (setf (gethash #x2D *byte-opcode-map*) 'reverse)
  (setf (gethash #x2F *byte-opcode-map*) 'stkscan)
  (setf (gethash #xFD *byte-opcode-map*) 'swap)
  (setf (gethash #xFE *byte-opcode-map*) 'nop)
  (setf (gethash #xB8 *byte-opcode-map*) 'pvarsetpop0)
  (setf (gethash #xB9 *byte-opcode-map*) 'pvarsetpop1)
  (setf (gethash #xBA *byte-opcode-map*) 'pvarsetpop2)
  (setf (gethash #xBB *byte-opcode-map*) 'pvarsetpop3)
  (setf (gethash #xBC *byte-opcode-map*) 'pvarsetpop4)
  (setf (gethash #xBD *byte-opcode-map*) 'pvarsetpop5)
  (setf (gethash #xBE *byte-opcode-map*) 'pvarsetpop6)
  )

(defun dispatch (vm code)
  "Main dispatch loop"
  (declare (type vm vm)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code))
  ;; Initialize tracing
  (trace-begin)
  (let ((pc (vm-pc vm)))
    (loop while (< pc (length code))
          do
          ;; Check interrupts before execution
          (when (check-interrupts vm)
            ;; Handle pending interrupts
            (let ((int-state (vm-interrupt-state vm)))
              (when int-state
                (cond
                  ((int-storage-full int-state)
                   (handle-interrupt vm :storage-full))
                  ((int-stack-overflow int-state)
                   (handle-interrupt vm :stack-overflow))
                  ((int-io-interrupt int-state)
                   (handle-interrupt vm :io))
                  ((int-log-file-io int-state)
                   (handle-interrupt vm :log-file-io))
                  ((int-ether-interrupt int-state)
                   (handle-interrupt vm :ether-interrupt))
                  (t
                   (handle-interrupt vm :timer))))))

          ;; Fetch instruction
          (let ((opcode-byte (fetch-instruction-byte pc code)))
            (when (zerop opcode-byte)
              (return)) ; End of code or invalid

            ;; Decode opcode
            (let* ((opcode (decode-opcode opcode-byte))
                   (instruction-len (get-instruction-length opcode-byte))
                   (operands (fetch-operands pc code instruction-len)))
              ;; Log trace before execution
              (multiple-value-bind (opcode-name found) (gethash opcode *byte-opcode-map*)
                (when found
                  (trace-log vm pc opcode operands :instruction-name opcode-name)))

              ;; Execute opcode handler (pass operands)
              (handler-case
                  (execute-opcode vm opcode operands)
                (maiko-lisp.utils:vm-error (err)
                  (error err))
                (error (err)
                  (error 'maiko-lisp.utils:vm-error
                         :message (format nil "Error executing opcode ~A: ~A" opcode err))))

              ;; Update program counter (unless opcode modified it)
              ;; Control flow opcodes (JUMP, RETURN) modify PC themselves
              (let ((current-pc (vm-pc vm)))
                (if (= current-pc pc)
                    ;; PC wasn't modified, advance normally
                    (progn
                      (setf pc (+ pc instruction-len))
                      (setf (vm-pc vm) pc))
                    ;; PC was modified by opcode (e.g., JUMP, RETURN)
                    (setf pc current-pc)))))

          ;; Check interrupts after execution
          (when (check-interrupts vm)
            ;; Handle pending interrupts
            (let ((int-state (vm-interrupt-state vm)))
              (when int-state
                (cond
                  ((int-storage-full int-state)
                   (handle-interrupt vm :storage-full))
                  ((int-stack-overflow int-state)
                   (handle-interrupt vm :stack-overflow))
                  ((int-io-interrupt int-state)
                   (handle-interrupt vm :io))
                  ((int-log-file-io int-state)
                   (handle-interrupt vm :log-file-io))
                  ((int-ether-interrupt int-state)
                   (handle-interrupt vm :ether-interrupt))
                  (t
                   (handle-interrupt vm :timer)))))))))

(defun execute-opcode (vm opcode operands)
  "Execute opcode with operands using handler registry"
  (declare (type vm vm)
           (type (unsigned-byte 8) opcode)
           (type list operands))
  ;; Look up handler symbol from byte-to-symbol map
  (multiple-value-bind (opcode-symbol found) (gethash opcode *byte-opcode-map*)
    (if found
        ;; Look up handler function from symbol registry
        (let ((handler (gethash opcode-symbol *opcode-handlers*)))
          (if handler
              ;; Call handler - handlers that need operands get them, others ignore
              (if (or (null operands) (zerop (length operands)))
                  (funcall handler vm)
                  (funcall handler vm operands))
              (error 'maiko-lisp.utils:vm-error
                     :message (format nil "No handler for opcode: ~A (0x~2,'0X)"
                                     opcode-symbol opcode))))
        ;; Unknown opcode
        (error 'maiko-lisp.utils:vm-error
               :message (format nil "Unknown opcode: 0x~2,'0X" opcode)))))

;; Auto-initialize byte-opcode-map when this file is loaded
(initialize-byte-opcode-map)
