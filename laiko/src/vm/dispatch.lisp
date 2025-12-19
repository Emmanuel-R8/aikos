(in-package :maiko-lisp.vm)

;; Dispatch loop
;; Per rewrite documentation vm-core/execution-model.md

(defun fetch-instruction-byte (pc code)
  "Fetch instruction byte at PC"
  (declare (type maiko-lisp.utils:lisp-ptr pc)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code))
  (if (< pc (length code))
      (aref code pc)
      0)) ; Invalid/end of code

(defun fetch-operands (pc code length)
  "Fetch operands for instruction starting at PC.
   Returns list of operand bytes (excluding opcode byte)."
  (declare (type maiko-lisp.utils:lisp-ptr pc)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code)
           (type (integer 1 *) length))
  (let ((operand-count (1- length))) ; Subtract 1 for opcode byte
    (if (<= (+ pc length) (length code))
        (loop for i from 1 to operand-count
              collect (aref code (+ pc i)))
        nil))) ; Not enough bytes

(defun decode-opcode (byte)
  "Decode opcode from byte. Returns opcode value."
  (declare (type maiko-lisp.utils:bytecode byte))
  byte)

(defun get-instruction-length (opcode)
  "Get instruction length for opcode based on opcode value.
   Returns total instruction length in bytes (opcode + operands)."
  (declare (type (unsigned-byte 8) opcode))
  ;; Most opcodes are 1 byte (opcode only)
  ;; Some opcodes have operands:
  ;; - TYPEP (0x05): 2 bytes (opcode + 1 byte type)
  ;; - BIND (0x0A): 2 bytes (opcode + 1 byte count)
  ;; - JUMP variants: 2 bytes (opcode + 1 byte signed offset)
  ;; - Atom references: 2-3 bytes depending on BIGATOMS
  (case opcode
    ;; 1-byte opcodes (most common)
    (#x67 3) ; ACONST (opcode + 2 byte atom index)
    (#x68 1) ; NIL
    (#x69 1) ; T
    (#x6A 1) ; CONST_0
    (#x6B 1) ; CONST_1
    (#x6C 2) ; SIC (opcode + 1 byte value)
    (#x6D 2) ; SNIC (opcode + 1 byte value)
    (#x6E 3) ; SICX (opcode + 2 byte value)
    (#x6F 3) ; GCONST (opcode + 2 byte index)
    (#x01 1) ; CAR
    (#x02 1) ; CDR
    (#x1A 1) ; CONS
    (#x18 1) ; RPLACA
    (#x19 1) ; RPLACD
    (#x1F 1) ; CREATECELL
    (#x26 1) ; RPLCONS
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
    (#xE4 1) ; IPLUS
    (#xE5 1) ; IDIFFERENCE1
    (#x3C 1) ; EQUAL
    (#x3D 1) ; NUMEQUAL
    (#xE0 1) ; LLSH1
    (#xE1 1) ; LLSH8
    (#xE2 1) ; LRSH1
    (#xE3 1) ; LRSH8
    (#xE4 1) ; LOGOR2
    (#xE5 1) ; LOGAND2
    (#xE6 1) ; LOGXOR2
    (#xE7 1) ; LOGNOT (approximate)
    (#xEC 1) ; LSH (approximate)
    (#xD4 1) ; PLUS2
    (#xD5 1) ; DIFFERENCE
    (#xD6 1) ; TIMES2
    (#xD7 1) ; QUOTIENT
    (#xE8 1) ; FPLUS2 (floating-point addition)
    (#xE9 1) ; FDIFFERENCE (floating-point subtraction)
    (#xEA 1) ; FTIMES2 (floating-point multiplication)
    (#xEB 1) ; FQUOTIENT (floating-point division)
    (#xE9 1) ; IMINUS
    (#xEA 1) ; IDIVIDE
    (#xEB 1) ; IMOD
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
    (#x60 3) ; GVAR (opcode + 2 byte atom index)
    (#x05 2) ; TYPEP (opcode + 1 byte type)
    (#x08 2) ; FN0 (2 bytes)
    (#x09 2) ; FN1 (2 bytes)
    (#x0A 2) ; FN2 (2 bytes) - Note: 0x0A is FN2, not BIND in Maiko
    (#x0B 2) ; FN3 (2 bytes)
    (#x0C 2) ; FN4 (2 bytes)
    (#x11 2) ; BIND (opcode + 1 byte count, 0x11 = 17)
    (#x0D 4) ; FNX (4 bytes with atom index + arg count)
    (#xEE 2) ; GETAEL1 (opcode + 1 byte index) or AREF2 (1 byte)
    (#xEF 3) ; GETAEL2 (opcode + 2 byte index) or ASET2 (1 byte)
    (#xB6 1) ; AREF1
    (#xB7 1) ; ASET1
    (#xC2 1) ; GETBASEBYTE
    (#xC7 1) ; PUTBASEBYTE
    (#xC8 2) ; GETBASE_N (opcode + 1 byte index)
    (#xC9 2) ; GETBASEPTR_N (opcode + 1 byte index)
    (#xCD 2) ; PUTBASE_N (opcode + 1 byte index)
    (#xCE 2) ; PUTBASEPTR_N (opcode + 1 byte index)
    (#xD2 1) ; HILOC
    (#xD3 1) ; LOLOC
    (#xF0 1) ; EQ (alternative)
    (#xF3 1) ; GREATERP (alternative)
    (#xF4 1) ; EQUAL (alternative)
    (#xF5 1) ; MAKENUMBER
    (#xFF 1) ; CL_EQUAL
    (#xB8 1) ; PVARSETPOP0
    (#xB9 1) ; PVARSETPOP1
    (#xBA 1) ; PVARSETPOP2
    (#xBB 1) ; PVARSETPOP3
    (#xBC 1) ; PVARSETPOP4
    (#xBD 1) ; PVARSETPOP5
    (#xBE 1) ; PVARSETPOP6
    (#xC0 2) ; POP_N (opcode + 1 byte count)
    (#x15 2) ; GCREF (opcode + 1 byte ref type)
    (#x13 1) ; DUNBIND
    (#xF1 1) ; IGREATERP
    ;; Default: assume 1 byte
    (otherwise 1)))

(defun dispatch (vm code)
  "Main dispatch loop"
  (declare (type vm vm)
           (type (simple-array maiko-lisp.utils:bytecode (*)) code))
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
            (let ((opcode (decode-opcode opcode-byte))
                  (instruction-length (get-instruction-length opcode-byte))
                  (operands (fetch-operands pc code instruction-length)))

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
                      (setf pc (+ pc instruction-length))
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
  "Execute opcode with operands"
  (declare (type vm vm)
           (type (unsigned-byte 8) opcode)
           (type list operands))
  (case opcode
    ;; Constants
    (#x67 (handle-aconst vm operands)) ; ACONST (0x67 = 103)
    (#x68 (handle-nil vm))          ; NIL (0x68 = 104)
    (#x69 (handle-t vm))            ; T (0x69 = 105)
    (#x6A (handle-const-0 vm))      ; CONST_0 (0x6A = 106)
    (#x6B (handle-const-1 vm))      ; CONST_1 (0x6B = 107)
    (#x6C (handle-sic vm operands)) ; SIC (0x6C = 108)
    (#x6D (handle-snic vm operands)) ; SNIC (0x6D = 109)
    (#x6E (handle-sicx vm operands)) ; SICX (0x6E = 110)
    (#x6F (handle-gconst vm operands)) ; GCONST (0x6F = 111)
    ;; List operations
    (#x01 (handle-car vm))          ; CAR
    (#x02 (handle-cdr vm))          ; CDR
    (#x1A (handle-cons vm))          ; CONS (0x1A = 26)
    (#x18 (handle-rplaca vm))       ; RPLACA (0x18 = 24)
    (#x19 (handle-rplacd vm))       ; RPLACD (0x19 = 25)
    (#x1F (handle-createcell vm))   ; CREATECELL (31 = 0x1F)
    (#x26 (handle-rplcons vm))      ; RPLCONS (38 = 0x26)
    ;; Integer arithmetic
    (#xD8 (handle-iplus2 vm))        ; IPLUS2 (216 = 0xD8)
    (#xD9 (handle-idifference vm))   ; IDIFFERENCE (217 = 0xD9)
    (#xDA (handle-itimes2 vm))       ; ITIMES2 (218 = 0xDA)
    (#xDB (handle-iquo vm))          ; IQUO (219 = 0xDB)
    (#xDC (handle-irem vm))          ; IREM (220 = 0xDC)
    ;; General arithmetic (handles integers and floats)
    (#xD4 (handle-plus2 vm))         ; PLUS2 (212 = 0xD4)
    (#xD5 (handle-difference vm))   ; DIFFERENCE (213 = 0xD5)
    (#xD6 (handle-times2 vm))        ; TIMES2 (214 = 0xD6)
    (#xD7 (handle-quotient vm))      ; QUOTIENT (215 = 0xD7)
    ;; Comparison
    (#x3A (handle-eq vm))            ; EQ (0x3A = 58)
    (#x3B (handle-eql vm))           ; EQL (0x3B = 59, approximate)
    (#x3E (handle-leq vm))           ; LEQ (less than or equal, approximate)
    (#x3F (handle-geq vm))           ; GEQ (greater than or equal, approximate)
    (#xF0 (handle-eq-alt vm))        ; EQ (alternative, 240 = 0xF0)
    (#xF1 (handle-igreaterp vm))      ; IGREATERP (241 = 0xF1)
    (#xF3 (handle-greaterp-alt vm))   ; GREATERP (alternative, 243 = 0xF3)
    (#xF4 (handle-equal-alt vm))      ; EQUAL (alternative, 244 = 0xF4)
    (#xF5 (handle-makenumber vm))    ; MAKENUMBER (245 = 0xF5)
    (#xFF (handle-cl-equal vm))      ; CL_EQUAL (255 = 0xFF)
    ;; Variable access - IVAR (local variables)
    (#x40 (handle-ivar0 vm))         ; IVAR0 (0x40 = 64)
    (#x41 (handle-ivar1 vm))         ; IVAR1 (0x41 = 65)
    (#x42 (handle-ivar2 vm))         ; IVAR2 (0x42 = 66)
    (#x43 (handle-ivar3 vm))         ; IVAR3 (0x43 = 67)
    (#x44 (handle-ivar4 vm))         ; IVAR4 (0x44 = 68)
    (#x45 (handle-ivar5 vm))         ; IVAR5 (0x45 = 69)
    (#x46 (handle-ivar6 vm))         ; IVAR6 (0x46 = 70)
    ;; Variable access - PVAR (parameters)
    (#x48 (handle-pvar0 vm))         ; PVAR0 (0x48 = 72)
    (#x49 (handle-pvar1 vm))         ; PVAR1 (0x49 = 73)
    (#x4A (handle-pvar2 vm))         ; PVAR2 (0x4A = 74)
    (#x4B (handle-pvar3 vm))         ; PVAR3 (0x4B = 75)
    (#x4C (handle-pvar4 vm))         ; PVAR4 (0x4C = 76)
    (#x4D (handle-pvar5 vm))         ; PVAR5 (0x4D = 77)
    (#x4E (handle-pvar6 vm))         ; PVAR6 (0x4E = 78)
    ;; Variable access - FVAR (free variables)
    (#x50 (handle-fvar0 vm))         ; FVAR0 (0x50 = 80)
    (#x51 (handle-fvar1 vm))         ; FVAR1 (0x51 = 81)
    (#x52 (handle-fvar2 vm))         ; FVAR2 (0x52 = 82)
    (#x53 (handle-fvar3 vm))         ; FVAR3 (0x53 = 83)
    (#x54 (handle-fvar4 vm))         ; FVAR4 (0x54 = 84)
    (#x55 (handle-fvar5 vm))         ; FVAR5 (0x55 = 85)
    (#x56 (handle-fvar6 vm))         ; FVAR6 (0x56 = 86)
    ;; Control flow
    (#x10 (handle-return vm))        ; RETURN (0x10 = 16)
    ;; Optimized jumps (offset encoded in opcode)
    (#x80 (handle-jump0 vm))         ; JUMP0 (0x80 = 128)
    (#x81 (handle-jump1 vm))         ; JUMP1
    (#x82 (handle-jump2 vm))         ; JUMP2
    (#x83 (handle-jump3 vm))         ; JUMP3
    (#x84 (handle-jump4 vm))         ; JUMP4
    (#x85 (handle-jump5 vm))         ; JUMP5
    (#x86 (handle-jump6 vm))         ; JUMP6
    (#x87 (handle-jump7 vm))         ; JUMP7
    (#x88 (handle-jump8 vm))         ; JUMP8
    (#x89 (handle-jump9 vm))         ; JUMP9
    (#x8A (handle-jump10 vm))        ; JUMP10
    (#x8B (handle-jump11 vm))        ; JUMP11
    (#x8C (handle-jump12 vm))        ; JUMP12
    (#x8D (handle-jump13 vm))        ; JUMP13
    (#x8E (handle-jump14 vm))        ; JUMP14
    (#x8F (handle-jump15 vm))       ; JUMP15
    (#x90 (handle-fjump0 vm))       ; FJUMP0 (0x90 = 144)
    (#x91 (handle-fjump1 vm))        ; FJUMP1
    (#x92 (handle-fjump2 vm))        ; FJUMP2
    (#x93 (handle-fjump3 vm))        ; FJUMP3
    (#x94 (handle-fjump4 vm))        ; FJUMP4
    (#x95 (handle-fjump5 vm))        ; FJUMP5
    (#x96 (handle-fjump6 vm))        ; FJUMP6
    (#x97 (handle-fjump7 vm))        ; FJUMP7
    (#x98 (handle-fjump8 vm))        ; FJUMP8
    (#x99 (handle-fjump9 vm))        ; FJUMP9
    (#x9A (handle-fjump10 vm))       ; FJUMP10
    (#x9B (handle-fjump11 vm))       ; FJUMP11
    (#x9C (handle-fjump12 vm))       ; FJUMP12
    (#x9D (handle-fjump13 vm))       ; FJUMP13
    (#x9E (handle-fjump14 vm))       ; FJUMP14
    (#x9F (handle-fjump15 vm))       ; FJUMP15
    (#xA0 (handle-tjump0 vm))        ; TJUMP0 (0xA0 = 160)
    (#xA1 (handle-tjump1 vm))        ; TJUMP1
    (#xA2 (handle-tjump2 vm))       ; TJUMP2
    (#xA3 (handle-tjump3 vm))       ; TJUMP3
    (#xA4 (handle-tjump4 vm))       ; TJUMP4
    (#xA5 (handle-tjump5 vm))       ; TJUMP5
    (#xA6 (handle-tjump6 vm))       ; TJUMP6
    (#xA7 (handle-tjump7 vm))       ; TJUMP7
    (#xA8 (handle-tjump8 vm))       ; TJUMP8
    (#xA9 (handle-tjump9 vm))       ; TJUMP9
    (#xAA (handle-tjump10 vm))       ; TJUMP10
    (#xAB (handle-tjump11 vm))       ; TJUMP11
    (#xAC (handle-tjump12 vm))       ; TJUMP12
    (#xAD (handle-tjump13 vm))       ; TJUMP13
    (#xAE (handle-tjump14 vm))      ; TJUMP14
    (#xAF (handle-tjump15 vm))       ; TJUMP15
    ;; Extended jumps
    (#xB0 (handle-jumpx vm operands)) ; JUMPX (0xB0 = 176)
    (#xB2 (handle-fjumpx vm operands)) ; FJUMPX (0xB2 = 178)
    (#xB3 (handle-tjumpx vm operands)) ; TJUMPX (0xB3 = 179)
    ;; Function calls
    (#x08 (handle-fn0 vm))           ; FN0 (0x08 = 8)
    (#x09 (handle-fn1 vm))           ; FN1 (0x09 = 9)
    (#x0A (handle-fn2 vm))           ; FN2 (0x0A = 10)
    (#x0B (handle-fn3 vm))           ; FN3 (0x0B = 11)
    (#x0C (handle-fn4 vm))           ; FN4 (0x0C = 12)
    (#x0D (handle-fnx vm operands))  ; FNX (0x0D = 13)
    ;; Array access
    (#xEE (handle-getael1 vm operands)) ; GETAEL1 (0xEE = 238)
    (#xEF (handle-getael2 vm operands)) ; GETAEL2 (0xEF = 239)
    (#xB6 (handle-aref1 vm))           ; AREF1 (182 = 0xB6)
    (#xB7 (handle-aset1 vm))           ; ASET1 (183 = 0xB7)
    (#xEE (handle-aref2 vm))           ; AREF2 (238 = 0xEE, conflicts with GETAEL1)
    (#xEF (handle-aset2 vm))           ; ASET2 (239 = 0xEF, conflicts with GETAEL2)
    ;; Type checking
    (#x05 (handle-typep vm operands)) ; TYPEP (0x05 = 5)
    ;; Bitwise operations
    (#xE0 (handle-llsh1 vm))          ; LLSH1 (224 = 0xE0)
    (#xE1 (handle-llsh8 vm))          ; LLSH8 (225 = 0xE1)
    (#xE2 (handle-lrsh1 vm))          ; LRSH1 (226 = 0xE2)
    (#xE3 (handle-lrsh8 vm))          ; LRSH8 (227 = 0xE3)
    ;; Bitwise logical operations
    (#xE4 (handle-logor2 vm))        ; LOGOR2 (228 = 0xE4)
    (#xE5 (handle-logand2 vm))        ; LOGAND2 (229 = 0xE5)
    (#xE6 (handle-logxor2 vm))        ; LOGXOR2 (230 = 0xE6)
    ;; Note: LOGNOT and LSH are at different opcodes
    (#xE7 (handle-lognot vm))        ; LOGNOT (231 = 0xE7)
    (#xEC (handle-lsh vm))           ; LSH (using 0xEC as placeholder)
    ;; Floating-point arithmetic (decimal opcodes: 232-235 = 0xE8-0xEB)
    (#xE8 (handle-fplus2 vm))        ; FPLUS2 (232 = 0xE8)
    (#xE9 (handle-fdifference vm))   ; FDIFFERENCE (233 = 0xE9)
    (#xEA (handle-ftimes2 vm))       ; FTIMES2 (234 = 0xEA)
    (#xEB (handle-fquotient vm))     ; FQUOTIENT (235 = 0xEB)
    ;; Stack operations
    (#x2F (handle-stkscan vm))       ; STKSCAN (0x2F = 47)
    ;; Function metadata
    (#x65 (handle-myargcount vm))   ; MYARGCOUNT (0x65 = 101)
    (#x61 (handle-arg0 vm))          ; ARG0 (0x61 = 97)
    ;; Additional comparison
    (#x3C (handle-equal vm))         ; EQUAL (approximate)
    (#x3D (handle-numequal vm))      ; NUMEQUAL (approximate)
    ;; Character operations (stubs, opcodes TBD)
    ;; (#xE7 (handle-charcode vm))      ; CHARCODE (conflicts with LOGNOT)
    ;; (#xE8 (handle-charn vm))         ; CHARN (conflicts with FPLUS2)
    ;; Binding
    (#x11 (handle-bind vm operands)) ; BIND (0x11 = 17)
    (#x12 (handle-unbind vm))       ; UNBIND (0x12 = 18)
    ;; Copy
    (#x64 (handle-copy vm))          ; COPY (0x64 = 100)
    ;; Global variables
    (#x60 (handle-gvar vm operands)) ; GVAR (0x60 = 96)
    ;; Store (note: STORE_N is at 0x3C, but we use different opcode)
    ;; (#x3C already used for EQUAL)
    ;; Apply
    (#x0E (handle-applyfn vm))       ; APPLYFN (0x0E = 14)
    ;; Unwind
    (#x07 (handle-unwind vm))        ; UNWIND (0x07 = 7)
    ;; List operations
    (#x27 (handle-listget vm))       ; LISTGET (0x27 = 39)
    ;; Atom operations
    (#x16 (handle-assoc vm))         ; ASSOC (0x16 = 22)
    (#x1C (handle-fmemb vm))         ; FMEMB (0x1C = 28)
    ;; Stack operations
    (#xD0 (handle-push vm))          ; PUSH/ADDBASE (208 = 0xD0)
    (#xBF (handle-pop vm))           ; POP (0xBF = 191)
    (#xC0 (handle-pop-n vm operands)) ; POP_N (192 = 0xC0)
    (#xC2 (handle-getbasebyte vm))   ; GETBASEBYTE (194 = 0xC2)
    (#xC7 (handle-putbasebyte vm))   ; PUTBASEBYTE (199 = 0xC7)
    (#xC8 (handle-getbase-n vm operands)) ; GETBASE_N (200 = 0xC8)
    (#xC9 (handle-getbaseptr-n vm operands)) ; GETBASEPTR_N (201 = 0xC9)
    (#xCD (handle-putbase-n vm operands)) ; PUTBASE_N (205 = 0xCD)
    (#xCE (handle-putbaseptr-n vm operands)) ; PUTBASEPTR_N (206 = 0xCE)
    (#xD2 (handle-hiloc vm))         ; HILOC (210 = 0xD2)
    (#xD3 (handle-loloc vm))         ; LOLOC (211 = 0xD3)
    (#xFD (handle-swap vm))          ; SWAP (0xFD = 253)
    (#xFE (handle-nop vm))           ; NOP (0xFE = 254)
    ;; Variable setting
    (#xB8 (handle-pvarsetpop0 vm))   ; PVARSETPOP0 (184 = 0xB8)
    (#xB9 (handle-pvarsetpop1 vm))   ; PVARSETPOP1 (185 = 0xB9)
    (#xBA (handle-pvarsetpop2 vm))   ; PVARSETPOP2 (186 = 0xBA)
    (#xBB (handle-pvarsetpop3 vm))   ; PVARSETPOP3 (187 = 0xBB)
    (#xBC (handle-pvarsetpop4 vm))   ; PVARSETPOP4 (188 = 0xBC)
    (#xBD (handle-pvarsetpop5 vm))   ; PVARSETPOP5 (189 = 0xBD)
    (#xBE (handle-pvarsetpop6 vm))   ; PVARSETPOP6 (190 = 0xBE)
    ;; GC operations
    (#x15 (handle-gcref vm operands)) ; GCREF (21 = 0x15)
    ;; Binding
    (#x13 (handle-dunbind vm))       ; DUNBIND (19 = 0x13)
    (otherwise
     (error 'maiko-lisp.utils:vm-error
            :message (format nil "Unknown opcode: ~A (0x~2,'0X)" opcode opcode)))))
