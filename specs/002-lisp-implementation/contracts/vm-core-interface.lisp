;;; VM Core Interface Contract
;;; Feature: 002-lisp-implementation
;;; Date: 2025-12-04

(in-package :maiko-lisp.vm)

;;; ============================================================================
;;; VM Initialization
;;; ============================================================================

(defun make-vm (stack-size)
  "Create and initialize VM with given stack size.

   Args:
     stack-size: Stack size in DLwords (must be positive)

   Returns:
     vm: Initialized VM structure

   Errors:
     vm-error: If stack-size is invalid"
  (declare (type (integer 1 *) stack-size))
  ...)

;;; ============================================================================
;;; Stack Operations
;;; ============================================================================

(defun push-stack (vm value)
  "Push value onto VM stack.

   Args:
     vm: VM structure
     value: Value to push (LispPTR)

   Returns:
     nil

   Errors:
     stack-overflow: If stack is full"
  (declare (type vm vm)
           (type lisp-ptr value))
  ...)

(defun pop-stack (vm)
  "Pop value from VM stack.

   Args:
     vm: VM structure

   Returns:
     value: Popped value (LispPTR)

   Errors:
     stack-underflow: If stack is empty"
  (declare (type vm vm))
  ...)

(defun get-top-of-stack (vm)
  "Get top of stack without popping.

   Args:
     vm: VM structure

   Returns:
     value: Top of stack (LispPTR)"
  (declare (type vm vm))
  ...)

(defun set-top-of-stack (vm value)
  "Set top of stack value.

   Args:
     vm: VM structure
     value: New top value (LispPTR)

   Returns:
     nil"
  (declare (type vm vm)
           (type lisp-ptr value))
  ...)

;;; ============================================================================
;;; Stack Frame Management
;;; ============================================================================

(defun allocate-stack-frame (vm frame-size)
  "Allocate new stack frame.

   Args:
     vm: VM structure
     frame-size: Frame size in DLwords

   Returns:
     frame: New stack frame

   Errors:
     stack-overflow: If insufficient stack space"
  (declare (type vm vm)
           (type (integer 1 *) frame-size))
  ...)

(defun free-stack-frame (vm frame)
  "Free stack frame.

   Args:
     vm: VM structure
     frame: Stack frame to free

   Returns:
     nil"
  (declare (type vm vm)
           (type stack-frame frame))
  ...)

;;; ============================================================================
;;; Dispatch Loop
;;; ============================================================================

(defun dispatch (vm code)
  "Main dispatch loop - execute bytecode instructions.

   Args:
     vm: VM structure
     code: Bytecode array (simple-array bytecode (*))

   Returns:
     nil (runs until halt or error)

   Errors:
     vm-error: On invalid opcode or execution error"
  (declare (type vm vm)
           (type (simple-array bytecode (*)) code))
  ...)

(defun fetch-instruction-byte (vm code)
  "Fetch next instruction byte.

   Args:
     vm: VM structure
     code: Bytecode array

   Returns:
     byte: Instruction byte (bytecode)"
  (declare (type vm vm)
           (type (simple-array bytecode (*)) code))
  ...)

(defun decode-opcode (byte)
  "Decode opcode from instruction byte.

   Args:
     byte: Instruction byte

   Returns:
     opcode: Opcode value (0-255)"
  (declare (type bytecode byte))
  ...)

(defun execute-opcode (vm opcode operands)
  "Execute opcode with operands.

   Args:
     vm: VM structure
     opcode: Opcode value (0-255)
     operands: List of operand values

   Returns:
     nil

   Errors:
     vm-error: On invalid opcode or execution error"
  (declare (type vm vm)
           (type (unsigned-byte 8) opcode)
           (type list operands))
  ...)

;;; ============================================================================
;;; Function Calls
;;; ============================================================================

(defun call-function (vm func-header args)
  "Call function with arguments.

   Args:
     vm: VM structure
     func-header: Function header structure
     args: List of argument values (LispPTR)

   Returns:
     frame: New stack frame

   Errors:
     stack-overflow: If insufficient stack space"
  (declare (type vm vm)
           (type function-header func-header)
           (type list args))
  ...)

(defun return-from-function (vm)
  "Return from current function.

   Args:
     vm: VM structure

   Returns:
     return-value: Return value (LispPTR)"
  (declare (type vm vm))
  ...)

;;; ============================================================================
;;; Interrupt Handling
;;; ============================================================================

(defun check-interrupts (vm)
  "Check for pending interrupts.

   Args:
     vm: VM structure

   Returns:
     interrupt-type: Interrupt type (symbol) or nil if none"
  (declare (type vm vm))
  ...)

(defun handle-interrupt (vm interrupt-type)
  "Handle interrupt.

   Args:
     vm: VM structure
     interrupt-type: Interrupt type (symbol)

   Returns:
     nil"
  (declare (type vm vm)
           (type symbol interrupt-type))
  ...)

(defun set-interrupt-flag (vm flag)
  "Set interrupt flag.

   Args:
     vm: VM structure
     flag: Flag name (keyword)

   Returns:
     nil"
  (declare (type vm vm)
           (type keyword flag))
  ...)

(defun clear-interrupt-flag (vm flag)
  "Clear interrupt flag.

   Args:
     vm: VM structure
     flag: Flag name (keyword)

   Returns:
     nil"
  (declare (type vm vm)
           (type keyword flag))
  ...)

;;; ============================================================================
;;; Address Translation
;;; ============================================================================

(defun translate-address-2 (vm addr)
  "Translate LispPTR to native 16-bit aligned address.

   Args:
     vm: VM structure
     addr: LispPTR virtual address

   Returns:
     native-addr: Native address (pointer or offset)"
  (declare (type vm vm)
           (type lisp-ptr addr))
  ...)

(defun translate-address-4 (vm addr)
  "Translate LispPTR to native 32-bit aligned address.

   Args:
     vm: VM structure
     addr: LispPTR virtual address

   Returns:
     native-addr: Native address (pointer or offset)"
  (declare (type vm vm)
           (type lisp-ptr addr))
  ...)
