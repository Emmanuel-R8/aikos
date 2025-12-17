= Execution Semantics


Complete specification of instruction execution semantics, including execution rules, stack effects, and side effects.

== Execution Model

=== Instruction Execution Cycle

#figure(
  caption: [Sequence Diagram],
  [Diagram: See original documentation for visual representation.],
)

  )
)

=== Execution Steps

1. *Fetch*: Read opcode byte from program counter
2. *Decode*: Determine instruction length, extract operands
3. *Execute*: Call opcode handler with operands
4. *Update*: Advance program counter by instruction length
5. *Check*: Check for pending interrupts

== Execution Rules

=== Stack Operations pointerPush Operation: [`function PushStack(value`]:
    CurrentStackPTR = CurrentStackPTR - 2  // Move stack down
    CurrentStackPTR[0] = value
    TopOfStack = value)

*Pop Operation*: [`function PopStack(`]:
    value = CurrentStackPTR[0]
    CurrentStackPTR = CurrentStackPTR + 2  // Move stack up
    TopOfStack = (CurrentStackPTR - 2)[0]  // New top
    return value)

=== Stack Effects Notation
- *+N*: Pushes N values onto stack
- *-N*: Pops N values from stack
- *0*: No stack change
- *→value*: Replaces TOS with value
- *value→*: Pops value, result on TOS

=== Memory Access pointerRead Operation: [`function ReadMemory(lisp_address, size`]:
    native_ptr = TranslateAddress(lisp_address)
    return ReadFromNative(native_ptr, size))

*Write Operation*: [`function WriteMemory(lisp_address, value, size`]:
    native_ptr = TranslateAddress(lisp_address)
    WriteToNative(native_ptr, value, size)
    UpdateGCReferences(lisp_address, value))

=== Address Translation

All memory accesses use address translation:

[`function TranslateAddress(lisp_address, alignment`]:
    // Use FPtoVP mapping table
    page_base = GetPageBase(lisp_address)
    offset = GetPageOffset(lisp_address)
    native_page = FPtoVP[page_base]
    return native_page + offset)

== Opcode Execution Patterns

=== Pattern 1: Stack-Based Operations pointerExample: Arithmetic operations

[`function ExecuteArithmeticOp(operation`]:
    arg2 = PopStack()
    arg1 = PopStack()
    result = operation(arg1, arg2)
    PushStack(result)
    PC = PC + instruction_length)

=== Pattern 2: Memory Modification pointerExample: RPLACA, RPLACD

[`function ExecuteMemoryModifyOp(`]:
    new_value = PopStack()
    target = PopStack()
    old_value = ReadMemory(target)
    WriteMemory(target, new_value)
    UpdateGCReferences(target, old_value, new_value)
    PushStack(target)
    PC = PC + instruction_length)

=== Pattern 3: Control Flow pointerExample: JUMP, FJUMP, TJUMP

[`function ExecuteJumpOp(condition_check`]:
    if condition_check():
        offset = DecodeOffset(operands)
        PC = PC + offset
    else:
        PC = PC + instruction_length)

=== Pattern 4: Function Calls pointerExample: FN0-FN4, FNX

[`function ExecuteFunctionCall(arg_count`]:
    // Save current frame state
    SavePC()
    SaveStackPointer()

    // Get function object
    function_obj = GetFunctionFromStack()

    // Allocate new frame
    new_frame = AllocateStackFrame(function_obj)

    // Set up arguments
    SetupArguments(new_frame, arg_count)

    // Enter function
    SetCurrentFrame(new_frame)
    PC = function_obj.code_start
    // Continue dispatch loop)

== Error Handling

=== Error Conditions pointerType Errors: [`if not HasExpectedType(value, expected_type`]:
    ERROR_EXIT(value)
    // Triggers error handler, may unwind stack)

*Overflow Errors*: [`if arithmetic_overflow_detected(result`]:
    ERROR_EXIT(operands)
    // Triggers error handler)

*Memory Errors*: [`if invalid_address(address`]:
    ERROR_EXIT(address)
    // Triggers error handler)

=== Error Propagation

Errors propagate through:

1. Opcode handler detects error
2. Calls ERROR_EXIT with error value
3. Error handler processes error
4. May unwind stack frames
5. May signal interrupt

== Side Effects

=== GC Side Effects

Operations that affect GC:
- *CONS*: Allocates memory, may trigger GC
- *RPLACA/RPLACD*: Updates reference counts
- *GCREF*: Explicit reference counting

=== I/O Side Effects

Operations that perform I/O: - *BIN/BOUT*: Byte input/output - *Subroutine calls*: May perform I/O

=== State Side Effects

Operations that modify VM state:
- *BIND/UNBIND*: Modifies variable bindings
- *CONTEXTSW*: Changes execution context
- *Interrupts*: Modify interrupt state

== Execution Ordering

=== Sequential Execution

Instructions execute sequentially:

- One instruction completes before next starts
- Program counter advances after each instruction
- Stack state consistent between instructions

=== Interrupt Points

Interrupts checked between instructions:

- After PC update
- Before next instruction fetch
- Interrupts may modify execution flow

=== Atomicity

Each instruction is atomic:

- Either completes fully or triggers error
- No partial execution
- State consistent after execution

== Performance Considerations

=== Dispatch Optimization
- *Computed Goto*: Fastest dispatch method
- *Switch Statement*: Portable fallback
- *Opcode Table*: Pre-computed handler addresses

=== Stack Management
- *Efficient Push/Pop*: Minimal manipulation
- *Stack Overflow Check*: Before frame allocation
- *Frame Reuse*: Where possible

=== Memory Access
- *Address Translation Cache*: Cache recent translations
- *Alignment*: Proper alignment for performance
- *GC Coordination*: Minimize GC overhead

== Related Documentation

- Opcodes Reference - Complete opcode list
- Instruction Format - Encoding details
- VM Core Execution Model - Dispatch implementation
- Stack Management - Stack operations
