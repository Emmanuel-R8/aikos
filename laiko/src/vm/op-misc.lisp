(in-package :maiko-lisp.vm)

;; Miscellaneous operations
;; bind, unbind, typep, fixp, smallp, charcode, charn, etc.

(defun handle-nop (vm)
  "NOP: No operation (opcode 0x00 - opc_unused_0).
   Per maiko/inc/opcodes.h, opcode 0x00 is opc_unused_0.
   Per maiko/src/codetbl.c:418, it's handled via name table.
   For now, this is a no-op that just advances PC."
  (declare (type vm vm))
  nil)

(defun get-pvar-slot (vm index)
  "Get value from PVAR slot at given index."
  (declare (type vm vm)
           (type (integer 0 *) index))
  (let ((pvar (vm-pvar vm))
        (pvar-ptr (vm-pvar-ptr vm)))
    (when (and (> pvar-ptr 0) (< index pvar-ptr))
      (aref pvar index))))

(defun set-pvar-slot (vm index value)
  "Set value in PVAR slot at given index."
  (declare (type vm vm)
           (type (integer 0 *) index)
           (type maiko-lisp.utils:lisp-ptr value))
  (let ((pvar (vm-pvar vm))
        (pvar-ptr (vm-pvar-ptr vm)))
    (when (and (> pvar-ptr 0) (< index pvar-ptr))
      (setf (aref pvar index) value))))

(defun allocate-pvar-slots (vm count)
  "Allocate COUNT slots in PVAR area. Returns the base offset."
  (declare (type vm vm)
           (type (integer 0 *) count))
  (let ((old-ptr (vm-pvar-ptr vm)))
    (setf (vm-pvar-ptr vm) (+ old-ptr count))
    old-ptr))

(defun handle-bind (vm operands)
  "BIND: Create variable bindings.
    Operand byte format: [n1:4][n2:4]
    - n1: number of variables to bind to NIL
    - n2: number of variables to bind to stack values
    Binds n1+n2 variables in PVAR area and pushes a binding marker."
  (declare (type vm vm)
           (type list operands))
   (let* ((operand (if operands (first operands) 0))
          (stack-ptr (vm-stack-ptr vm))
          (stack (vm-stack vm))
          (n1 (ash operand -4))
          (n2 (logand operand #xF))
          (total (+ n1 n2)))
     ;; Allocate PVAR slots
     (let ((pvar-offset (allocate-pvar-slots vm total)))
       ;; DEBUG: Report values
       (format t "[BIND DEBUG] operand=~X n1=~A n2=~A total=~A~%" operand n1 n2 total)
       (format t "[BIND DEBUG] pvar-offset=~A stack-ptr=~A stack[0]=~A~%" pvar-offset stack-ptr (aref stack 0))
       ;; Bind n1 variables to NIL
       (loop for i from 0 below n1 do
         (let ((slot-index (+ pvar-offset i)))
           (format t "[BIND DEBUG] Set PVAR~A (idx ~A) to 0~%" i slot-index)
           (set-pvar-slot vm slot-index 0)))
       ;; Bind n2 variables from stack values
       (loop for i from 0 below n2 do
         (let ((value (aref stack (- stack-ptr 1 i))))
           (format t "[BIND DEBUG] i=~A: stack[-~A]=stack[~A]=~A~%" i (1+ i) (- stack-ptr 1 i) value)
           (let ((slot-index (+ pvar-offset n1 i)))
             (format t "[BIND DEBUG] Set PVAR~A (idx ~A) to ~A~%" (+ n1 i) slot-index value)
             (set-pvar-slot vm slot-index value))))
       ;; Push binding marker
       ;; Marker format: [MSB:1][~count:15][offset:15]
       ;;   - MSB (bit 31): Always set to 1 to identify marker
       ;;   - count (bits 16-30): Ones-complement of slot count (15 bits)
       ;;   - offset (bits 1-15): PVAR slot offset << 1 (to match C encoding)
       (let* ((count-encoded (logand (logxor total #x7FFF) #x7FFF))
              (offset-encoded (logand (ash pvar-offset 1) #x7FFF))
              (marker (logior #x80000000
                             (ash count-encoded 16)  ;; Count at bits 16-30
                             offset-encoded)))       ;; Offset at bits 1-15 (shifted)
        (setf (aref stack stack-ptr) marker)
        (setf (vm-stack-ptr vm) (1+ stack-ptr))))))

(defun find-binding-marker (vm start-ptr)
  "Find the nearest binding marker by scanning stack downward.
    Returns (count pvar-offset) or nil if not found."
  (declare (type vm vm)
           (type (integer 0 *) start-ptr))
  (block find-marker
    (let ((stack (vm-stack vm))
          (ptr start-ptr))
      (format t "[FIND-MARKER DEBUG] start-ptr=~A scanning downward~%" ptr)
      (loop while (> ptr 0) do
        (let ((value (aref stack (1- ptr))))
          (format t "[FIND-MARKER DEBUG] Check stack[~A]=~X MSB=~A~%" (1- ptr) value (logtest value #x80000000))
          ;; Check if MSB is set (binding marker)
          (when (logtest value #x80000000)
           ;; Extract marker info: [MSB:1][~count:15][offset:15]
             ;; Use LDB for logical bit extraction (not ash which sign-extends negatives)
             ;; Note: offset is encoded as (pvar_offset << 1), so decode by shifting right
             (let* ((raw-count-bits (ldb (byte 15 16) value))  ;; Count at bits 16-30
                    (count (logxor raw-count-bits #x7FFF))
                    (pvar-offset (ash (ldb (byte 15 1) value) -1))) ;; Offset at bits 1-15, shift right
              (format t "[FIND-MARKER DEBUG] Found marker! count=~A offset=~A~%" count pvar-offset)
              (return-from find-marker (values count pvar-offset))))
          (decf ptr)))
      (format t "[FIND-MARKER DEBUG] No marker found~%")
      (values nil nil))))

(defun handle-unbind (vm)
  "UNBIND: Restore variable bindings.
    For now, this is a no-op since we're using virtual memory stack.
    Per C: scans stack for binding marker."
  (declare (type vm vm))
  ;; TODO: Implement proper UNBIND with virtual memory stack
  ;; For now, just continue execution
  (format t "[UNBIND] skipped (vmem stack)~%")
  nil)

(defun handle-dunbind (vm)
  "DUNBIND: Dynamic unbind.
    If TOS is unbound marker, unbind directly.
    If TOS is bound, scan stack for marker first."
  (declare (type vm vm))
  (let ((tos (get-top-of-stack vm))
        (stack-ptr (vm-stack-ptr vm))
        (stack (vm-stack vm)))
    (cond
      ;; Check if TOS has MSB set (unbound marker)
      ((logtest tos #x80000000)
       ;; tos is unbound - extract count from ones-complement in bits 16-30
       ;; Note: offset is encoded as (pvar_offset << 1), so decode by shifting right
       (let* ((count (logxor (ldb (byte 15 16) tos) #x7FFF))
              (pvar-offset (ash (ldb (byte 15 1) tos) -1)))
         (when (> count 0)
           (let ((pvar (vm-pvar vm)))
             (loop for i from 0 below count do
               (setf (aref pvar (+ pvar-offset i)) +unbound-marker+))))))
      ;; tos is bound - scan stack for marker
      (t
       (multiple-value-bind (count pvar-offset) (find-binding-marker vm stack-ptr)
         (when count
           (let ((pvar (vm-pvar vm)))
             (loop for i from 0 below count do
               (setf (aref pvar (+ pvar-offset i)) +unbound-marker+)))))))))

(defun handle-typep (vm operands)
  "TYPEP: Type check.
   Checks if top of stack has the given type code.
   Type codes:
     0 = NIL
     1 = Fixnum
     2 = Float
     3 = List (consp)
     4 = Symbol
     5 = Array
     6 = Vector"
  (declare (type vm vm)
           (type list operands))
  (let ((type-code (if operands (first operands) 0))
        (value (get-top-of-stack vm)))
    (let ((result
           (case type-code
             (0 (if (zerop value) 1 0))  ; NIL is 0
             (1 (if (logtest value #x2) 1 0))  ; Fixnum has bit 1 set
             (2 (if (and (logtest value #x2) (not (logtest value #x1))) 1 0))  ; Float
             (3 (if (not (zerop value)) 1 0))  ; Non-zero could be list pointer
             (t 0))))
      (set-top-of-stack vm result))))

(defun handle-fixp (vm)
  "FIXP: Check if value is a fixnum.
   Returns 1 if fixnum (type code 1), 0 otherwise."
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (if (logtest value #x2) 1 0))))

(defun handle-smallp (vm)
   "SMALLP: Check if value is a small positive integer.
    Returns 1 if small positive, 0 otherwise."
   (declare (type vm vm))
   (let ((value (get-top-of-stack vm)))
     (set-top-of-stack vm (if (and (> value 0)
                                   (not (logtest value #x80000000)))
                              1
                              0))))

(defun handle-charcode (vm)
  "CHARCODE: Get character code from character pointer.
   Extracts the character code from a character pointer."
  (declare (type vm vm))
  (let ((char-ptr (get-top-of-stack vm)))
    (let ((char-code (if (zerop char-ptr)
                        0
                        (logand char-ptr #xFF))))
      (set-top-of-stack vm char-code))))

(defun handle-charn (vm)
  "CHARN: Get Nth character from string.
   Pops n and string-ptr, pushes nth character code."
  (declare (type vm vm))
  (let ((n (pop-stack vm))
        (string-ptr (pop-stack vm)))
    (if (or (zerop string-ptr) (>= n 256))
        (push-stack vm 0)
        (let ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) string-ptr)))
          (let ((char-code (maiko-lisp.data:get-car cell)))
            (push-stack vm (logand char-code #xFF)))))))

(defun handle-copy (vm)
  "COPY: Copy top of stack"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (push-stack vm value)))

(defun handle-store (vm)
  "STORE: Store value to variable"
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (var-ptr (pop-stack vm)))
    nil))

(defun handle-unwind (vm)
  "UNWIND: Unwind stack to catch point"
  (declare (type vm vm))
  (let ((catch-tag (pop-stack vm)))
    nil))

(defun handle-listget (vm)
  "LISTGET: Get element from list by index"
  (declare (type vm vm))
  (let ((index (pop-stack vm))
        (list-ptr (pop-stack vm)))
    (push-stack vm 0)))

(defun handle-assoc (vm)
  "ASSOC: Association list lookup"
  (declare (type vm vm))
  (let ((key (pop-stack vm))
        (alist (pop-stack vm)))
    (push-stack vm 0)))

(defun handle-fmemb (vm)
  "FMEMB: Fast member test"
  (declare (type vm vm))
  (let ((item (pop-stack vm))
        (list-ptr (pop-stack vm)))
    (push-stack vm 0)))

(defun handle-stkscan (vm)
  "STKSCAN: Scan stack for value"
  (declare (type vm vm))
  (let ((target (pop-stack vm))
        (stack (vm-stack vm))
        (stack-ptr (vm-stack-ptr vm)))
    (let ((found nil))
      (loop for i from (1- stack-ptr) downto 0
            when (= (aref stack i) target)
            do (setf found t) (return))
      (push-stack vm (if found 1 0)))))

(defun handle-pop-n (vm operands)
  "POP_N: Pop N values from stack"
  (declare (type vm vm)
           (type list operands))
  (let ((count (if operands (first operands) 0)))
    (loop for i from 1 to count do
      (pop-stack vm))))

(defun handle-gcref (vm operands)
  "GCREF: GC reference operation"
  (declare (type vm vm)
           (type list operands))
  (let ((ref-type (if operands (first operands) 0))
        (object-ptr (get-top-of-stack vm)))
    nil))

(defun handle-fplus2 (vm)
  "FDIFFERENCE: Floating-point subtraction.
   Pops b, a; computes a - b, pushes result."
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (if (and (> a 1) (logtest a #x2))
                  (decode-float-pointer a)
                  0.0d0))
          (fb (if (and (> b 1) (logtest b #x2))
                  (decode-float-pointer b)
                  0.0d0)))
      (let ((result (- fa fb)))
        (push-stack vm (encode-float-pointer result))))))

(defun handle-ftimes2 (vm)
  "FTIMES2: Floating-point multiplication.
   Pops two values, multiplies as floats, pushes result."
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (if (and (> a 1) (logtest a #x2))
                  (decode-float-pointer a)
                  0.0d0))
          (fb (if (and (> b 1) (logtest b #x2))
                  (decode-float-pointer b)
                  0.0d0)))
      (let ((result (* fa fb)))
        (push-stack vm (encode-float-pointer result))))))

(defun handle-fquotient (vm)
  "FQUOTIENT: Floating-point division.
   Pops b, a; computes a / b, pushes result."
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((fa (if (and (> a 1) (logtest a #x2))
                  (decode-float-pointer a)
                  0.0d0))
          (fb (if (and (> b 1) (logtest b #x2))
                  (decode-float-pointer b)
                  1.0d0)))
      (if (= fb 0.0d0)
          (error 'maiko-lisp.utils:vm-error
                 :message "Floating-point division by zero")
          (let ((result (/ fa fb)))
            (push-stack vm (encode-float-pointer result)))))))

(defun decode-float-pointer (ptr)
  "Decode a LispPTR to a float value"
  (declare (type maiko-lisp.utils:lisp-ptr ptr))
  (let ((type-code (ldb (byte 3 24) ptr)))
    (cond
      ;; Type 2: Float stored inline (24-bit mantissa)
      ((= type-code 2)
       (let ((mantissa (logand ptr #xFFFFFF)))
         (+ (* (ash mantissa -12) 1.0d0)
            (* (logand mantissa #xFFF) 1.0d-12))))
      ;; Type 1: Small integer (fixnum)
      ((= type-code 1)
       (coerce (ash ptr -3) 'double-float))
      ;; Default: treat as 0
      (t 0.0d0))))

(defun encode-float-pointer (value)
  "Encode a float value to a LispPTR"
  (declare (type double-float value))
  (let ((int-val (truncate value)))
    (if (and (>= int-val 0) (< int-val #x200000))
        ;; Small float fits in inline format
        (logior #x200000 int-val)
        ;; Large float - for now, encode as type 2 with mantissa
         (logior #x200000 (logand (round value) #xFFFFFF)))))

;; General arithmetic operations
(defun handle-plus2 (vm)
  "PLUS2: Addition"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (+ a b))))

(defun handle-difference (vm)
  "DIFFERENCE: Subtraction"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (- a b))))

(defun handle-times2 (vm)
  "TIMES2: Multiplication"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (* a b))))

(defun handle-quotient (vm)
  "QUOTIENT: Division"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (when (zerop b)
      (error 'maiko-lisp.utils:vm-arithmetic-error :message "Division by zero"))
    (push-stack vm (truncate a b))))
