(in-package :maiko-lisp.vm)

;; Opcode handlers
;; Per rewrite documentation instruction-set/opcodes.md

(defun handle-iplus2 (vm)
  "Handle IPLUS2 opcode: Integer addition of two operands"
  (declare (type vm vm))
  ;; Pop two values from stack
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; Add them (treating as signed 32-bit integers)
    ;; Convert unsigned 32-bit to signed
    (let ((a-signed (if (>= a #x80000000)
                        (- a #x100000000)
                        a))
          (b-signed (if (>= b #x80000000)
                        (- b #x100000000)
                        b)))
      (let ((result (+ a-signed b-signed)))
        ;; Convert back to unsigned 32-bit representation
        (let ((result-unsigned (if (minusp result)
                                    (logand (+ result #x100000000) #xFFFFFFFF)
                                    (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defun handle-idifference (vm)
  "Handle IDIFFERENCE opcode: Integer subtraction (a - b)"
  (declare (type vm vm))
  ;; Pop two values from stack (b first, then a)
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; Subtract them (treating as signed 32-bit integers)
    (let ((a-signed (if (>= a #x80000000)
                        (- a #x100000000)
                        a))
          (b-signed (if (>= b #x80000000)
                        (- b #x100000000)
                        b)))
      (let ((result (- a-signed b-signed)))
        ;; Convert back to unsigned 32-bit representation
        (let ((result-unsigned (if (minusp result)
                                    (logand (+ result #x100000000) #xFFFFFFFF)
                                    (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defun handle-push (vm)
  "Handle PUSH opcode: Push value onto stack.
   Note: PUSH (0xD0) is actually ADDBASE in Maiko, but can be used to push values.
   For now, we'll push a constant value from operands if present."
  (declare (type vm vm))
  ;; If operands are provided, push the first operand as a value
  ;; Otherwise, this might be pushing from a register or constant
  ;; For now, push 0 as placeholder - actual implementation needs operand handling
  (push-stack vm 0))

(defun handle-push-constant (vm value)
  "Handle PUSH with constant value"
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr value))
  (push-stack vm value))

(defun handle-pop (vm)
  "Handle POP opcode: Pop value from stack"
  (declare (type vm vm))
  ;; POP removes top of stack (value is discarded)
  (pop-stack vm)
  nil)

(defun handle-nil (vm)
  "Handle NIL opcode: Push NIL (0) onto stack"
  (declare (type vm vm))
  (push-stack vm 0))

(defun handle-t (vm)
  "Handle T opcode: Push T (1) onto stack"
  (declare (type vm vm))
  (push-stack vm 1))

(defun handle-itimes2 (vm)
  "Handle ITIMES2 opcode: Integer multiplication of two operands"
  (declare (type vm vm))
  ;; Pop two values from stack
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; Multiply them (treating as signed 32-bit integers)
    (let ((a-signed (if (>= a #x80000000)
                        (- a #x100000000)
                        a))
          (b-signed (if (>= b #x80000000)
                        (- b #x100000000)
                        b)))
      (let ((result (* a-signed b-signed)))
        ;; Convert back to unsigned 32-bit representation
        (let ((result-unsigned (if (minusp result)
                                  (logand (+ result #x100000000) #xFFFFFFFF)
                                  (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defun handle-iquo (vm)
  "Handle IQUO opcode: Integer division (quotient)"
  (declare (type vm vm))
  ;; Pop two values from stack (b first, then a)
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (when (zerop b)
      (error 'maiko-lisp.utils:division-by-zero))
    ;; Divide them (treating as signed 32-bit integers)
    (let ((a-signed (if (>= a #x80000000)
                        (- a #x100000000)
                        a))
          (b-signed (if (>= b #x80000000)
                        (- b #x100000000)
                        b)))
      (let ((result (truncate a-signed b-signed)))
        ;; Convert back to unsigned 32-bit representation
        (let ((result-unsigned (if (minusp result)
                                  (logand (+ result #x100000000) #xFFFFFFFF)
                                  (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

(defun handle-irem (vm)
  "Handle IREM opcode: Integer remainder (modulo)"
  (declare (type vm vm))
  ;; Pop two values from stack (b first, then a)
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (when (zerop b)
      (error 'maiko-lisp.utils:division-by-zero))
    ;; Modulo them (treating as signed 32-bit integers)
    (let ((a-signed (if (>= a #x80000000)
                        (- a #x100000000)
                        a))
          (b-signed (if (>= b #x80000000)
                        (- b #x100000000)
                        b)))
      (let ((result (rem a-signed b-signed)))
        ;; Convert back to unsigned 32-bit representation
        (let ((result-unsigned (if (minusp result)
                                  (logand (+ result #x100000000) #xFFFFFFFF)
                                  (logand result #xFFFFFFFF))))
          (push-stack vm result-unsigned))))))

;; List manipulation opcodes
(defun handle-car (vm)
  "Handle CAR opcode: Get CAR of cons cell"
  (declare (type vm vm))
  (let ((list-ptr (get-top-of-stack vm))
        (storage (vm-storage vm)))
    (cond
      ((zerop list-ptr) ; NIL
       (set-top-of-stack vm 0))
      ((not storage)
       (error 'maiko-lisp.utils:vm-error
              :message "CAR: No storage available"))
      (t
       ;; TODO: Translate list-ptr to storage offset
       ;; TODO: Get cons cell from memory and extract CAR
       ;; For now, return 0 as placeholder
       (set-top-of-stack vm 0)))))

(defun handle-cdr (vm)
  "Handle CDR opcode: Get CDR of cons cell (with CDR coding)"
  (declare (type vm vm))
  (let ((list-ptr (get-top-of-stack vm))
        (storage (vm-storage vm)))
    (cond
      ((zerop list-ptr) ; NIL
       (set-top-of-stack vm 0))
      ((not storage)
       (error 'maiko-lisp.utils:vm-error
              :message "CDR: No storage available"))
      (t
       ;; TODO: Translate list-ptr to storage offset
       ;; TODO: Get cons cell from memory, decode CDR coding, and extract CDR
       ;; For now, return 0 (NIL) as placeholder
       (set-top-of-stack vm 0)))))

(defun handle-cons (vm)
  "Handle CONS opcode: Create new cons cell"
  (declare (type vm vm))
  (let ((cdr-value (pop-stack vm))
        (car-value (pop-stack vm))
        (storage (vm-storage vm)))
    (unless storage
      (error 'maiko-lisp.utils:vm-error
             :message "CONS: No storage available"))
      ;; Allocate cons cell from storage
      (let ((cons-offset (maiko-lisp.memory:allocate-cons-cell storage)))
        ;; Create cons cell structure
        (let ((cons-cell (maiko-lisp.data:make-cons-cell
                          :car-field car-value
                          :cdr-code (if (zerop cdr-value)
                                        maiko-lisp.data:+cdr-nil+
                                        maiko-lisp.data:+cdr-indirect+))))
          ;; TODO: Write cons cell to memory at offset
          ;; For now, use offset as pointer (will need proper address translation)
          (let ((cons-ptr (+ maiko-lisp.memory:+mds-offset+ cons-offset)))
            ;; Update GC reference counts
            (when (vm-storage vm)
              ;; TODO: Call ADDREF for car-value and cdr-value if they're pointers
              )
            (push-stack vm cons-ptr))))))

(defun handle-rplaca (vm)
  "Handle RPLACA opcode: Replace CAR of cons cell"
  (declare (type vm vm))
  (let ((new-car (pop-stack vm))
        (cons-ptr (pop-stack vm)))
    ;; TODO: Get cons cell from memory
    ;; TODO: Handle CDR_INDIRECT case
    ;; TODO: Update CAR field
    ;; TODO: Push cons-ptr back onto stack
    (error 'maiko-lisp.utils:vm-error
           :message "RPLACA implementation needs memory access")))

(defun handle-rplacd (vm)
  "Handle RPLACD opcode: Replace CDR of cons cell"
  (declare (type vm vm))
  (let ((new-cdr (pop-stack vm))
        (cons-ptr (pop-stack vm)))
    ;; TODO: Get cons cell from memory
    ;; TODO: Update CDR with CDR coding
    ;; TODO: Push cons-ptr back onto stack
    (error 'maiko-lisp.utils:vm-error
           :message "RPLACD implementation needs memory access and CDR encoding")))

;; Comparison opcodes
(defun handle-eq (vm)
  "Handle EQ opcode: Pointer equality test"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (if (= a b) 1 0))))

(defun handle-eql (vm)
  "Handle EQL opcode: Equality test (numbers and pointers)"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; EQL is same as EQ for now (full implementation needs type checking)
    (push-stack vm (if (= a b) 1 0))))

(defun handle-lessp (vm)
  "Handle LESSP opcode: Less than comparison"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; Treat as signed integers
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (< a-signed b-signed) 1 0)))))

(defun handle-greaterp (vm)
  "Handle GREATERP opcode: Greater than comparison"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; Treat as signed integers
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (> a-signed b-signed) 1 0)))))

;; Variable access opcodes
(defun get-ivar (vm index)
  "Get local variable at index from current frame"
  (declare (type vm vm)
           (type (integer 0 *) index))
  (let ((frame (vm-current-frame vm))
        (stack (vm-stack vm))
        (stack-ptr (vm-stack-ptr vm)))
    (unless frame
      (error 'maiko-lisp.utils:vm-error :message "No current frame for IVAR access"))
    ;; Local variables are stored in the stack frame
    ;; For now, return 0 as placeholder
    ;; TODO: Calculate correct offset based on frame layout
    0))

(defun handle-ivar0 (vm)
  "Handle IVAR0: Access local variable 0"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 0)))

(defun handle-ivar1 (vm)
  "Handle IVAR1: Access local variable 1"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 1)))

(defun handle-ivar2 (vm)
  "Handle IVAR2: Access local variable 2"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 2)))

(defun handle-ivar3 (vm)
  "Handle IVAR3: Access local variable 3"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 3)))

(defun handle-ivar4 (vm)
  "Handle IVAR4: Access local variable 4"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 4)))

(defun handle-ivar5 (vm)
  "Handle IVAR5: Access local variable 5"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 5)))

(defun handle-ivar6 (vm)
  "Handle IVAR6: Access local variable 6"
  (declare (type vm vm))
  (push-stack vm (get-ivar vm 6)))

(defun get-pvar (vm index)
  "Get parameter variable at index from current frame"
  (declare (type vm vm)
           (type (integer 0 *) index))
  ;; Parameters are stored before local variables in frame
  ;; For now, return 0 as placeholder
  0)

(defun handle-pvar0 (vm)
  "Handle PVAR0: Access parameter 0"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 0)))

(defun handle-pvar1 (vm)
  "Handle PVAR1: Access parameter 1"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 1)))

(defun handle-pvar2 (vm)
  "Handle PVAR2: Access parameter 2"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 2)))

(defun handle-pvar3 (vm)
  "Handle PVAR3: Access parameter 3"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 3)))

(defun handle-pvar4 (vm)
  "Handle PVAR4: Access parameter 4"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 4)))

(defun handle-pvar5 (vm)
  "Handle PVAR5: Access parameter 5"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 5)))

(defun handle-pvar6 (vm)
  "Handle PVAR6: Access parameter 6"
  (declare (type vm vm))
  (push-stack vm (get-pvar vm 6)))

(defun get-fvar (vm index)
  "Get free variable at index from closure"
  (declare (type vm vm)
           (type (integer 0 *) index))
  ;; Free variables come from closure environment
  ;; For now, return 0 as placeholder
  0)

(defun handle-fvar0 (vm)
  "Handle FVAR0: Access free variable 0"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 0)))

(defun handle-fvar1 (vm)
  "Handle FVAR1: Access free variable 1"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 1)))

(defun handle-fvar2 (vm)
  "Handle FVAR2: Access free variable 2"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 2)))

(defun handle-fvar3 (vm)
  "Handle FVAR3: Access free variable 3"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 3)))

(defun handle-fvar4 (vm)
  "Handle FVAR4: Access free variable 4"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 4)))

(defun handle-fvar5 (vm)
  "Handle FVAR5: Access free variable 5"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 5)))

(defun handle-fvar6 (vm)
  "Handle FVAR6: Access free variable 6"
  (declare (type vm vm))
  (push-stack vm (get-fvar vm 6)))

;; Control flow opcodes
(defun handle-return (vm)
  "Handle RETURN opcode: Return from function"
  (declare (type vm vm))
  ;; Return value should be on stack
  (let ((return-value (maiko-lisp.vm:return-from-function vm)))
    ;; PC is updated by return-from-function
    return-value))

(defun handle-jump (vm operands)
  "Handle JUMP opcode: Unconditional jump"
  (declare (type vm vm)
           (type list operands))
  (if operands
      (let ((offset (first operands)))
        ;; Signed byte offset
        (let ((signed-offset (if (>= offset #x80)
                                 (- offset #x100)
                                 offset)))
          (incf (vm-pc vm) signed-offset)))
      (error 'maiko-lisp.utils:vm-error :message "JUMP needs offset operand")))

(defun handle-jumpif (vm operands)
  "Handle JUMPIF opcode: Conditional jump if TOS is non-NIL"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (get-top-of-stack vm)))
    (when (not (zerop tos)) ; Non-NIL
      (handle-jump vm operands))))

(defun handle-jumpifnil (vm operands)
  "Handle JUMPIFNIL opcode: Conditional jump if TOS is NIL"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (get-top-of-stack vm)))
    (when (zerop tos) ; NIL
      (handle-jump vm operands))))

;; Type checking opcodes
(defun handle-typep (vm operands)
  "Handle TYPEP opcode: Type check"
  (declare (type vm vm)
           (type list operands))
  (let ((type-code (if operands (first operands) 0))
        (value (get-top-of-stack vm)))
    ;; TODO: Implement proper type checking
    ;; For now, always return T (1)
    (set-top-of-stack vm 1)))

(defun handle-fixp (vm)
  "Handle FIXP opcode: Check if value is a fixnum"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    ;; Fixnums are even numbers (low bit = 0)
    (set-top-of-stack vm (if (evenp value) 1 0))))

(defun handle-smallp (vm)
  "Handle SMALLP opcode: Check if value is a small integer"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    ;; Small integers are in a specific range
    ;; For now, check if it's a small fixnum
    (set-top-of-stack vm (if (and (evenp value) (< value #x10000)) 1 0))))

;; Array access opcodes
(defun handle-getael1 (vm operands)
  "Handle GETAEL1 opcode: Get array element with 1-byte index"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "GETAEL1: NIL is not an array"))
    ;; TODO: Get array header from memory, access element at index
    ;; For now, push 0 as placeholder
    (error 'maiko-lisp.utils:vm-error :message "GETAEL1 needs memory access")))

(defun handle-getael2 (vm operands)
  "Handle GETAEL2 opcode: Get array element with 2-byte index"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if (>= (length operands) 2)
                   (logior (first operands)
                           (ash (second operands) 8))
                   0))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "GETAEL2: NIL is not an array"))
    ;; TODO: Get array header from memory, access element at index
    (error 'maiko-lisp.utils:vm-error :message "GETAEL2 needs memory access")))

(defun handle-setael1 (vm operands)
  "Handle SETAEL1 opcode: Set array element with 1-byte index"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "SETAEL1: NIL is not an array"))
    ;; TODO: Get array header from memory, set element at index
    ;; Push array pointer back
    (push-stack vm array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "SETAEL1 needs memory access")))

(defun handle-setael2 (vm operands)
  "Handle SETAEL2 opcode: Set array element with 2-byte index"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if (>= (length operands) 2)
                   (logior (first operands)
                           (ash (second operands) 8))
                   0))
        (value (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "SETAEL2: NIL is not an array"))
    ;; TODO: Get array header from memory, set element at index
    (push-stack vm array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "SETAEL2 needs memory access")))

;; Function call opcodes
(defun handle-fn0 (vm)
  "Handle FN0 opcode: Call function with 0 arguments"
  (declare (type vm vm))
  (let ((func-obj (pop-stack vm)))
    (when (zerop func-obj)
      (error 'maiko-lisp.utils:vm-error :message "FN0: Cannot call NIL"))
    ;; TODO: Get function header, validate, call with 0 args
    (error 'maiko-lisp.utils:vm-error :message "FN0 needs function call implementation")))

(defun handle-fn1 (vm)
  "Handle FN1 opcode: Call function with 1 argument"
  (declare (type vm vm))
  (let ((arg1 (pop-stack vm))
        (func-obj (pop-stack vm)))
    (when (zerop func-obj)
      (error 'maiko-lisp.utils:vm-error :message "FN1: Cannot call NIL"))
    ;; TODO: Get function header, validate, call with 1 arg
    (error 'maiko-lisp.utils:vm-error :message "FN1 needs function call implementation")))

(defun handle-fn2 (vm)
  "Handle FN2 opcode: Call function with 2 arguments"
  (declare (type vm vm))
  (let ((arg2 (pop-stack vm))
        (arg1 (pop-stack vm))
        (func-obj (pop-stack vm)))
    (when (zerop func-obj)
      (error 'maiko-lisp.utils:vm-error :message "FN2: Cannot call NIL"))
    ;; TODO: Get function header, validate, call with 2 args
    (error 'maiko-lisp.utils:vm-error :message "FN2 needs function call implementation")))

(defun handle-fn3 (vm)
  "Handle FN3 opcode: Call function with 3 arguments"
  (declare (type vm vm))
  (let ((arg3 (pop-stack vm))
        (arg2 (pop-stack vm))
        (arg1 (pop-stack vm))
        (func-obj (pop-stack vm)))
    (when (zerop func-obj)
      (error 'maiko-lisp.utils:vm-error :message "FN3: Cannot call NIL"))
    ;; TODO: Get function header, validate, call with 3 args
    (error 'maiko-lisp.utils:vm-error :message "FN3 needs function call implementation")))

(defun handle-fn4 (vm)
  "Handle FN4 opcode: Call function with 4 arguments"
  (declare (type vm vm))
  (let ((arg4 (pop-stack vm))
        (arg3 (pop-stack vm))
        (arg2 (pop-stack vm))
        (arg1 (pop-stack vm))
        (func-obj (pop-stack vm)))
    (when (zerop func-obj)
      (error 'maiko-lisp.utils:vm-error :message "FN4: Cannot call NIL"))
    ;; TODO: Get function header, validate, call with 4 args
    (error 'maiko-lisp.utils:vm-error :message "FN4 needs function call implementation")))

(defun handle-fnx (vm operands)
  "Handle FNX opcode: Call function with variable arguments"
  (declare (type vm vm)
           (type list operands))
  ;; FNX has atom index and argument count in operands
  (let ((atom-index (if (>= (length operands) 2)
                        (logior (first operands)
                                (ash (second operands) 8))
                        0))
        (arg-count (if (>= (length operands) 3)
                       (third operands)
                       0)))
    ;; TODO: Get function from atom, validate, call with N args
    (error 'maiko-lisp.utils:vm-error :message "FNX needs atom and function call implementation")))

;; Bitwise operations
(defun handle-logand (vm)
  "Handle LOGAND opcode: Bitwise AND"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (logand a b))))

(defun handle-logior (vm)
  "Handle LOGIOR opcode: Bitwise OR"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (logior a b))))

(defun handle-logxor (vm)
  "Handle LOGXOR opcode: Bitwise XOR"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (push-stack vm (logxor a b))))

(defun handle-lognot (vm)
  "Handle LOGNOT opcode: Bitwise NOT"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (lognot value) #xFFFFFFFF))))

;; Additional arithmetic
(defun handle-iplus (vm)
  "Handle IPLUS opcode: Integer addition (unary, adds 1)"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (let ((value-signed (if (>= value #x80000000)
                           (- value #x100000000)
                           value)))
      (let ((result (+ value-signed 1)))
        (let ((result-unsigned (if (minusp result)
                                  (logand (+ result #x100000000) #xFFFFFFFF)
                                  (logand result #xFFFFFFFF))))
          (set-top-of-stack vm result-unsigned))))))

(defun handle-idifference1 (vm)
  "Handle IDIFFERENCE1 opcode: Integer subtraction (unary, subtracts 1)"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (let ((value-signed (if (>= value #x80000000)
                              (- value #x100000000)
                              value)))
      (let ((result (- value-signed 1)))
        (let ((result-unsigned (if (minusp result)
                                  (logand (+ result #x100000000) #xFFFFFFFF)
                                  (logand result #xFFFFFFFF))))
          (set-top-of-stack vm result-unsigned))))))

;; Additional comparison opcodes
(defun handle-equal (vm)
  "Handle EQUAL opcode: Deep equality test"
  (declare (type vm vm))
  ;; For now, EQUAL is same as EQ
  ;; Full implementation needs recursive comparison
  (handle-eq vm))

(defun handle-numequal (vm)
  "Handle NUMEQUAL opcode: Numeric equality"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    ;; Treat as signed integers
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (= a-signed b-signed) 1 0)))))

;; Shift operations
(defun handle-lsh (vm)
  "Handle LSH opcode: Logical shift"
  (declare (type vm vm))
  (let ((shift-amount (pop-stack vm))
        (value (pop-stack vm)))
    ;; Shift amount is signed
    (let ((shift-signed (if (>= shift-amount #x80000000)
                            (- shift-amount #x100000000)
                            shift-amount)))
      (if (minusp shift-signed)
          ;; Right shift
          (push-stack vm (ash value shift-signed))
          ;; Left shift
          (push-stack vm (logand (ash value shift-signed) #xFFFFFFFF))))))

;; Character operations
(defun handle-charcode (vm)
  "Handle CHARCODE opcode: Get character code"
  (declare (type vm vm))
  (let ((char-ptr (get-top-of-stack vm)))
    ;; TODO: Extract character code from character object
    ;; For now, return 0
    (set-top-of-stack vm 0)))

(defun handle-charn (vm)
  "Handle CHARN opcode: Get Nth character from string"
  (declare (type vm vm))
  (let ((n (pop-stack vm))
        (string-ptr (pop-stack vm)))
    ;; TODO: Get character from string at index N
    (push-stack vm 0)))

;; Binding operations
(defun handle-bind (vm operands)
  "Handle BIND opcode: Bind variables"
  (declare (type vm vm)
           (type list operands))
  (let ((count (if operands (first operands) 0)))
    ;; TODO: Bind N variables from stack
    (error 'maiko-lisp.utils:vm-error
           :message "BIND needs variable binding implementation")))

(defun handle-unbind (vm)
  "Handle UNBIND opcode: Unbind variables"
  (declare (type vm vm))
  ;; TODO: Unbind variables
  (error 'maiko-lisp.utils:vm-error
         :message "UNBIND needs variable unbinding implementation"))

;; Copy operations
(defun handle-copy (vm)
  "Handle COPY opcode: Copy value"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (push-stack vm value)))

;; Global variable access
(defun handle-gvar (vm operands)
  "Handle GVAR opcode: Access global variable"
  (declare (type vm vm)
           (type list operands))
  (let ((atom-index (if (>= (length operands) 2)
                        (logior (first operands)
                                (ash (second operands) 8))
                        0)))
    ;; TODO: Get global variable value from atom table
    (error 'maiko-lisp.utils:vm-error
           :message "GVAR needs atom table access")))

;; Store operations
(defun handle-store (vm)
  "Handle STORE opcode: Store value to variable"
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (var-ptr (pop-stack vm)))
    ;; TODO: Store value to variable location
    (error 'maiko-lisp.utils:vm-error
           :message "STORE needs variable storage implementation")))

;; Apply operations
(defun handle-applyfn (vm)
  "Handle APPLYFN opcode: Apply function to argument list"
  (declare (type vm vm))
  (let ((arg-list (pop-stack vm))
        (func-obj (pop-stack vm)))
    ;; TODO: Apply function to argument list
    (error 'maiko-lisp.utils:vm-error
           :message "APPLYFN needs apply implementation")))

;; Unwind operations
(defun handle-unwind (vm)
  "Handle UNWIND opcode: Unwind stack"
  (declare (type vm vm))
  ;; TODO: Unwind stack to catch point
  (error 'maiko-lisp.utils:vm-error
         :message "UNWIND needs unwind implementation"))

;; List operations
(defun handle-listget (vm)
  "Handle LISTGET opcode: Get element from list"
  (declare (type vm vm))
  (let ((index (pop-stack vm))
        (list-ptr (pop-stack vm)))
    ;; TODO: Get Nth element from list
    (push-stack vm 0)))

;; Atom operations
(defun handle-assoc (vm)
  "Handle ASSOC opcode: Association list lookup"
  (declare (type vm vm))
  (let ((key (pop-stack vm))
        (alist (pop-stack vm)))
    ;; TODO: Look up key in association list
    (push-stack vm 0))) ; Return NIL if not found

(defun handle-fmemb (vm)
  "Handle FMEMB opcode: Fast member test"
  (declare (type vm vm))
  (let ((item (pop-stack vm))
        (list-ptr (pop-stack vm)))
    ;; TODO: Test if item is member of list
    (push-stack vm 0))) ; Return NIL if not member

;; Additional comparison opcodes
(defun handle-leq (vm)
  "Handle LEQ opcode: Less than or equal"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (<= a-signed b-signed) 1 0)))))

(defun handle-geq (vm)
  "Handle GEQ opcode: Greater than or equal"
  (declare (type vm vm))
  (let ((b (pop-stack vm))
        (a (pop-stack vm)))
    (let ((a-signed (if (>= a #x80000000) (- a #x100000000) a))
          (b-signed (if (>= b #x80000000) (- b #x100000000) b)))
      (push-stack vm (if (>= a-signed b-signed) 1 0)))))

;; Additional arithmetic opcodes
(defun handle-iminus (vm)
  "Handle IMINUS opcode: Unary minus (negate)"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (let ((value-signed (if (>= value #x80000000)
                           (- value #x100000000)
                           value)))
      (let ((result (- value-signed)))
        (let ((result-unsigned (if (minusp result)
                                  (logand (+ result #x100000000) #xFFFFFFFF)
                                  (logand result #xFFFFFFFF))))
          (set-top-of-stack vm result-unsigned))))))

(defun handle-idivide (vm)
  "Handle IDIVIDE opcode: Integer division (alternative to IQUO)"
  (declare (type vm vm))
  (handle-iquo vm)) ; Same as IQUO

(defun handle-imod (vm)
  "Handle IMOD opcode: Integer modulo (alternative to IREM)"
  (declare (type vm vm))
  (handle-irem vm)) ; Same as IREM

;; Stack scan operations
(defun handle-stkscan (vm)
  "Handle STKSCAN opcode: Scan stack for value"
  (declare (type vm vm))
  (let ((target (pop-stack vm))
        (stack (vm-stack vm))
        (stack-ptr (vm-stack-ptr vm)))
    ;; Scan stack from top to bottom
    (let ((found nil))
      (loop for i from (1- stack-ptr) downto 0
            when (= (aref stack i) target)
            do (setf found t) (return))
      (push-stack vm (if found 1 0)))))

;; Argument count operations
(defun handle-myargcount (vm)
  "Handle MYARGCOUNT opcode: Get number of arguments for current function"
  (declare (type vm vm))
  (let ((frame (vm-current-frame vm)))
    (if frame
        ;; TODO: Get actual argument count from function header
        (push-stack vm 0)
        (push-stack vm 0))))

;; Name table operations
(defun handle-arg0 (vm)
  "Handle ARG0 opcode: Access argument 0"
  (declare (type vm vm))
  ;; ARG0 is same as PVAR0
  (handle-pvar0 vm))

;; Constant opcodes
(defun handle-const-0 (vm)
  "Handle CONST_0 opcode: Push integer constant 0"
  (declare (type vm vm))
  (push-stack vm 0))

(defun handle-const-1 (vm)
  "Handle CONST_1 opcode: Push integer constant 1"
  (declare (type vm vm))
  (push-stack vm 2)) ; Small integer 1 is encoded as 2 (low bit = 0 for fixnums)

(defun handle-aconst (vm operands)
  "Handle ACONST opcode: Push atom constant"
  (declare (type vm vm)
           (type list operands))
  (let ((atom-index (if (>= (length operands) 2)
                        (logior (first operands)
                                (ash (second operands) 8))
                        0)))
    ;; TODO: Get atom from atom table and push
    (error 'maiko-lisp.utils:vm-error
           :message "ACONST needs atom table access")))

(defun handle-sic (vm operands)
  "Handle SIC opcode: Small integer constant (positive)"
  (declare (type vm vm)
           (type list operands))
  (let ((value (if operands (first operands) 0)))
    ;; SIC encodes small positive integers
    ;; Format: S_POSITIVE | value
    (push-stack vm (logior #xE0000 value))))

(defun handle-snic (vm operands)
  "Handle SNIC opcode: Small integer constant (negative)"
  (declare (type vm vm)
           (type list operands))
  (let ((value (if operands (first operands) 0)))
    ;; SNIC encodes small negative integers
    ;; Format: S_NEGATIVE | 0xff00 | value
    (push-stack vm (logior #xFF00 value))))

(defun handle-sicx (vm operands)
  "Handle SICX opcode: Small integer constant (extended)"
  (declare (type vm vm)
           (type list operands))
  (let ((value (if (>= (length operands) 2)
                   (logior (first operands)
                           (ash (second operands) 8))
                   0)))
    ;; SICX encodes larger small integers
    (push-stack vm (logior #xE0000 value))))

(defun handle-gconst (vm operands)
  "Handle GCONST opcode: Global constant"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if (>= (length operands) 2)
                   (logior (first operands)
                           (ash (second operands) 8))
                   0)))
    ;; TODO: Get global constant from constant table
    (error 'maiko-lisp.utils:vm-error
           :message "GCONST needs constant table access")))

;; Optimized jump opcodes (offset encoded in opcode)
(defun handle-jump-n (vm offset)
  "Handle JUMP with offset N (0-15)"
  (declare (type vm vm)
           (type (integer 0 15) offset))
  ;; Unconditional jump - offset is already encoded in opcode
  (incf (vm-pc vm) offset))

(defun handle-fjump-n (vm offset)
  "Handle FJUMP with offset N (0-15): Jump if false"
  (declare (type vm vm)
           (type (integer 0 15) offset))
  (let ((tos (pop-stack vm)))
    (when (zerop tos) ; NIL (false)
      (incf (vm-pc vm) offset))))

(defun handle-tjump-n (vm offset)
  "Handle TJUMP with offset N (0-15): Jump if true"
  (declare (type vm vm)
           (type (integer 0 15) offset))
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos)) ; Non-NIL (true)
      (incf (vm-pc vm) offset))))

;; Generate handlers for JUMP0-15, FJUMP0-15, TJUMP0-15
;; JUMP0-15 (unconditional jumps)
(defun handle-jump0 (vm) (handle-jump-n vm 0))
(defun handle-jump1 (vm) (handle-jump-n vm 1))
(defun handle-jump2 (vm) (handle-jump-n vm 2))
(defun handle-jump3 (vm) (handle-jump-n vm 3))
(defun handle-jump4 (vm) (handle-jump-n vm 4))
(defun handle-jump5 (vm) (handle-jump-n vm 5))
(defun handle-jump6 (vm) (handle-jump-n vm 6))
(defun handle-jump7 (vm) (handle-jump-n vm 7))
(defun handle-jump8 (vm) (handle-jump-n vm 8))
(defun handle-jump9 (vm) (handle-jump-n vm 9))
(defun handle-jump10 (vm) (handle-jump-n vm 10))
(defun handle-jump11 (vm) (handle-jump-n vm 11))
(defun handle-jump12 (vm) (handle-jump-n vm 12))
(defun handle-jump13 (vm) (handle-jump-n vm 13))
(defun handle-jump14 (vm) (handle-jump-n vm 14))
(defun handle-jump15 (vm) (handle-jump-n vm 15))

;; FJUMP0-15 (jump if false)
(defun handle-fjump0 (vm) (handle-fjump-n vm 0))
(defun handle-fjump1 (vm) (handle-fjump-n vm 1))
(defun handle-fjump2 (vm) (handle-fjump-n vm 2))
(defun handle-fjump3 (vm) (handle-fjump-n vm 3))
(defun handle-fjump4 (vm) (handle-fjump-n vm 4))
(defun handle-fjump5 (vm) (handle-fjump-n vm 5))
(defun handle-fjump6 (vm) (handle-fjump-n vm 6))
(defun handle-fjump7 (vm) (handle-fjump-n vm 7))
(defun handle-fjump8 (vm) (handle-fjump-n vm 8))
(defun handle-fjump9 (vm) (handle-fjump-n vm 9))
(defun handle-fjump10 (vm) (handle-fjump-n vm 10))
(defun handle-fjump11 (vm) (handle-fjump-n vm 11))
(defun handle-fjump12 (vm) (handle-fjump-n vm 12))
(defun handle-fjump13 (vm) (handle-fjump-n vm 13))
(defun handle-fjump14 (vm) (handle-fjump-n vm 14))
(defun handle-fjump15 (vm) (handle-fjump-n vm 15))

;; TJUMP0-15 (jump if true)
(defun handle-tjump0 (vm) (handle-tjump-n vm 0))
(defun handle-tjump1 (vm) (handle-tjump-n vm 1))
(defun handle-tjump2 (vm) (handle-tjump-n vm 2))
(defun handle-tjump3 (vm) (handle-tjump-n vm 3))
(defun handle-tjump4 (vm) (handle-tjump-n vm 4))
(defun handle-tjump5 (vm) (handle-tjump-n vm 5))
(defun handle-tjump6 (vm) (handle-tjump-n vm 6))
(defun handle-tjump7 (vm) (handle-tjump-n vm 7))
(defun handle-tjump8 (vm) (handle-tjump-n vm 8))
(defun handle-tjump9 (vm) (handle-tjump-n vm 9))
(defun handle-tjump10 (vm) (handle-tjump-n vm 10))
(defun handle-tjump11 (vm) (handle-tjump-n vm 11))
(defun handle-tjump12 (vm) (handle-tjump-n vm 12))
(defun handle-tjump13 (vm) (handle-tjump-n vm 13))
(defun handle-tjump14 (vm) (handle-tjump-n vm 14))
(defun handle-tjump15 (vm) (handle-tjump-n vm 15))

;; Extended jump opcodes
(defun handle-jumpx (vm operands)
  "Handle JUMPX opcode: Unconditional jump with 16-bit offset"
  (declare (type vm vm)
           (type list operands))
  (let ((offset (if (>= (length operands) 2)
                     (let ((low (first operands))
                           (high (second operands)))
                       (let ((unsigned (logior low (ash high 8))))
                         ;; Sign extend 16-bit to 32-bit
                         (if (>= unsigned #x8000)
                             (- unsigned #x10000)
                             unsigned)))
                     0)))
    (incf (vm-pc vm) offset)))

(defun handle-fjumpx (vm operands)
  "Handle FJUMPX opcode: Conditional jump if false with 16-bit offset"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (pop-stack vm)))
    (when (zerop tos) ; NIL (false)
      (handle-jumpx vm operands))))

(defun handle-tjumpx (vm operands)
  "Handle TJUMPX opcode: Conditional jump if true with 16-bit offset"
  (declare (type vm vm)
           (type list operands))
  (let ((tos (pop-stack vm)))
    (when (not (zerop tos)) ; Non-NIL (true)
      (handle-jumpx vm operands))))

;; Array access opcodes (improved implementations)
(defun handle-aref1 (vm)
  "Handle AREF1 opcode: 1-dimensional array access"
  (declare (type vm vm))
  (let ((index (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "AREF1: NIL is not an array"))
    ;; TODO: Get array header from memory, validate index, access element
    ;; For now, push 0 as placeholder
    (error 'maiko-lisp.utils:vm-error :message "AREF1 needs memory access")))

(defun handle-aset1 (vm)
  "Handle ASET1 opcode: 1-dimensional array set"
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (index (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "ASET1: NIL is not an array"))
    ;; TODO: Get array header from memory, validate index, set element
    ;; Push array pointer back
    (push-stack vm array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "ASET1 needs memory access")))

(defun handle-aref2 (vm)
  "Handle AREF2 opcode: 2-dimensional array access"
  (declare (type vm vm))
  (let ((index1 (pop-stack vm))
        (index0 (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "AREF2: NIL is not an array"))
    ;; TODO: Get array header from memory, calculate linear index, access element
    (error 'maiko-lisp.utils:vm-error :message "AREF2 needs memory access")))

(defun handle-aset2 (vm)
  "Handle ASET2 opcode: 2-dimensional array set"
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (index1 (pop-stack vm))
        (index0 (pop-stack vm))
        (array-ptr (pop-stack vm)))
    (when (zerop array-ptr)
      (error 'maiko-lisp.utils:vm-error :message "ASET2: NIL is not an array"))
    ;; TODO: Get array header from memory, calculate linear index, set element
    (push-stack vm array-ptr)
    (error 'maiko-lisp.utils:vm-error :message "ASET2 needs memory access")))

;; Variable setting opcodes
(defun set-pvar (vm index)
  "Set parameter variable at index from stack"
  (declare (type vm vm)
           (type (integer 0 6) index))
  (let ((value (pop-stack vm))
        (frame (vm-current-frame vm)))
    (unless frame
      (error 'maiko-lisp.utils:vm-error :message "No current frame for PVAR set"))
    ;; TODO: Set parameter variable in frame
    ;; For now, just pop the value
    value))

(defun handle-pvarsetpop0 (vm)
  "Handle PVARSETPOP0 opcode: Set parameter 0 and pop"
  (declare (type vm vm))
  (set-pvar vm 0))

(defun handle-pvarsetpop1 (vm)
  "Handle PVARSETPOP1 opcode: Set parameter 1 and pop"
  (declare (type vm vm))
  (set-pvar vm 1))

(defun handle-pvarsetpop2 (vm)
  "Handle PVARSETPOP2 opcode: Set parameter 2 and pop"
  (declare (type vm vm))
  (set-pvar vm 2))

(defun handle-pvarsetpop3 (vm)
  "Handle PVARSETPOP3 opcode: Set parameter 3 and pop"
  (declare (type vm vm))
  (set-pvar vm 3))

(defun handle-pvarsetpop4 (vm)
  "Handle PVARSETPOP4 opcode: Set parameter 4 and pop"
  (declare (type vm vm))
  (set-pvar vm 4))

(defun handle-pvarsetpop5 (vm)
  "Handle PVARSETPOP5 opcode: Set parameter 5 and pop"
  (declare (type vm vm))
  (set-pvar vm 5))

(defun handle-pvarsetpop6 (vm)
  "Handle PVARSETPOP6 opcode: Set parameter 6 and pop"
  (declare (type vm vm))
  (set-pvar vm 6))

;; Additional comparison opcodes
(defun handle-igreaterp (vm)
  "Handle IGREATERP opcode: Integer greater-than comparison"
  (declare (type vm vm))
  (handle-greaterp vm)) ; Same as GREATERP for integers

;; Additional utility opcodes
(defun handle-pop-n (vm operands)
  "Handle POP_N opcode: Pop N values from stack"
  (declare (type vm vm)
           (type list operands))
  (let ((count (if operands (first operands) 0)))
    (loop for i from 1 to count do
      (pop-stack vm))))

(defun handle-dunbind (vm)
  "Handle DUNBIND opcode: Dynamic unbind"
  (declare (type vm vm))
  ;; TODO: Implement dynamic unbinding
  (error 'maiko-lisp.utils:vm-error
         :message "DUNBIND needs dynamic unbinding implementation"))

(defun handle-gcref (vm operands)
  "Handle GCREF opcode: GC reference operation"
  (declare (type vm vm)
           (type list operands))
  (let ((ref-type (if operands (first operands) 0))
        (object-ptr (get-top-of-stack vm)))
    ;; TODO: Implement GC reference operations (ADDREF, DELREF, STKREF)
    (error 'maiko-lisp.utils:vm-error
           :message "GCREF needs GC reference implementation")))

;; Shift opcodes (optimized)
(defun handle-llsh1 (vm)
  "Handle LLSH1 opcode: Logical left shift by 1"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (ash value 1) #xFFFFFFFF))))

(defun handle-llsh8 (vm)
  "Handle LLSH8 opcode: Logical left shift by 8"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (logand (ash value 8) #xFFFFFFFF))))

(defun handle-lrsh1 (vm)
  "Handle LRSH1 opcode: Logical right shift by 1"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -1))))

(defun handle-lrsh8 (vm)
  "Handle LRSH8 opcode: Logical right shift by 8"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (ash value -8))))

;; General arithmetic opcodes (handle both integers and floats)
(defun handle-plus2 (vm)
  "Handle PLUS2 opcode: General addition (integers or floats)"
  (declare (type vm vm))
  ;; For now, same as IPLUS2 (will be extended for floats)
  (handle-iplus2 vm))

(defun handle-difference (vm)
  "Handle DIFFERENCE opcode: General subtraction (integers or floats)"
  (declare (type vm vm))
  ;; For now, same as IDIFFERENCE (will be extended for floats)
  (handle-idifference vm))

(defun handle-times2 (vm)
  "Handle TIMES2 opcode: General multiplication (integers or floats)"
  (declare (type vm vm))
  ;; For now, same as ITIMES2 (will be extended for floats)
  (handle-itimes2 vm))

(defun handle-quotient (vm)
  "Handle QUOTIENT opcode: General division (integers or floats)"
  (declare (type vm vm))
  ;; For now, same as IQUO (will be extended for floats)
  (handle-iquo vm))

;; Bitwise opcodes (LOGOR2, LOGAND2, LOGXOR2)
(defun handle-logor2 (vm)
  "Handle LOGOR2 opcode: Bitwise OR (alternative name)"
  (declare (type vm vm))
  (handle-logior vm))

(defun handle-logand2 (vm)
  "Handle LOGAND2 opcode: Bitwise AND (alternative name)"
  (declare (type vm vm))
  (handle-logand vm))

(defun handle-logxor2 (vm)
  "Handle LOGXOR2 opcode: Bitwise XOR (alternative name)"
  (declare (type vm vm))
  (handle-logxor vm))

;; Cell creation opcodes
(defun handle-createcell (vm)
  "Handle CREATECELL opcode: Create a new cell of specified type"
  (declare (type vm vm))
  (let ((type-code (pop-stack vm)))
    ;; TODO: Allocate cell of given type from storage
    ;; For now, return NIL as placeholder
    (error 'maiko-lisp.utils:vm-error
           :message "CREATECELL needs type-based cell allocation")))

(defun handle-rplcons (vm)
  "Handle RPLCONS opcode: Replace cons (CONS + RPLACA)"
  (declare (type vm vm))
  ;; RPLCONS is like CONS but modifies existing cons cell
  ;; For now, same as CONS
  (handle-cons vm))

;; Base address operations
(defun handle-getbasebyte (vm)
  "Handle GETBASEBYTE opcode: Get byte from base address"
  (declare (type vm vm))
  (let ((offset (pop-stack vm))
        (base-addr (pop-stack vm)))
    ;; TODO: Read byte from base address + offset
    ;; For now, return 0
    (push-stack vm 0)))

(defun handle-putbasebyte (vm)
  "Handle PUTBASEBYTE opcode: Put byte to base address"
  (declare (type vm vm))
  (let ((value (pop-stack vm))
        (offset (pop-stack vm))
        (base-addr (pop-stack vm)))
    ;; TODO: Write byte to base address + offset
    ;; Push value back
    (push-stack vm value)))

(defun handle-getbase-n (vm operands)
  "Handle GETBASE_N opcode: Get value from base address with index N"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (base-addr (pop-stack vm)))
    ;; TODO: Read value from base address + index
    (push-stack vm 0)))

(defun handle-getbaseptr-n (vm operands)
  "Handle GETBASEPTR_N opcode: Get pointer from base address with index N"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (base-addr (pop-stack vm)))
    ;; TODO: Read pointer from base address + index
    (push-stack vm 0)))

(defun handle-putbase-n (vm operands)
  "Handle PUTBASE_N opcode: Put value to base address with index N"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (value (pop-stack vm))
        (base-addr (pop-stack vm)))
    ;; TODO: Write value to base address + index
    (push-stack vm base-addr)))

(defun handle-putbaseptr-n (vm operands)
  "Handle PUTBASEPTR_N opcode: Put pointer to base address with index N"
  (declare (type vm vm)
           (type list operands))
  (let ((index (if operands (first operands) 0))
        (value (pop-stack vm))
        (base-addr (pop-stack vm)))
    ;; TODO: Write pointer to base address + index
    (push-stack vm base-addr)))

;; Address manipulation
(defun handle-addbase (vm)
  "Handle ADDBASE opcode: Add base address (same as PUSH)"
  (declare (type vm vm))
  ;; ADDBASE is same as PUSH
  (handle-push vm))

(defun handle-hiloc (vm)
  "Handle HILOC opcode: Get high 16 bits of address"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (maiko-lisp.utils:hiloc value))))

(defun handle-loloc (vm)
  "Handle LOLOC opcode: Get low 16 bits of address"
  (declare (type vm vm))
  (let ((value (get-top-of-stack vm)))
    (set-top-of-stack vm (maiko-lisp.utils:loloc value))))

;; Additional comparison opcodes (alternative opcodes)
(defun handle-eq-alt (vm)
  "Handle EQ opcode (alternative opcode 0xF0)"
  (declare (type vm vm))
  (handle-eq vm))

(defun handle-greaterp-alt (vm)
  "Handle GREATERP opcode (alternative opcode 0xF3)"
  (declare (type vm vm))
  (handle-greaterp vm))

(defun handle-equal-alt (vm)
  "Handle EQUAL opcode (alternative opcode 0xF4)"
  (declare (type vm vm))
  (handle-equal vm))

(defun handle-makenumber (vm)
  "Handle MAKENUMBER opcode: Make number from value"
  (declare (type vm vm))
  ;; For now, just pass through the value
  ;; TODO: Convert to proper number representation
  nil)

(defun handle-cl-equal (vm)
  "Handle CL_EQUAL opcode: Case-insensitive equal"
  (declare (type vm vm))
  ;; For now, same as EQUAL
  (handle-equal vm))
