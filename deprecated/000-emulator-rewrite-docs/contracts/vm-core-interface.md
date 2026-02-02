# VM Core Interface Contract

**Version**: 1.0
**Date**: 2025-12-04
**Type**: Interface Specification

## Overview

This contract specifies the interfaces between VM core subsystems and external components. These interfaces must be maintained for compatibility.

## Memory Interface

### Address Translation

**Operation**: `translate_address(lisp_address, alignment) -> native_pointer`

**Preconditions**:

- `lisp_address` is valid LispPTR
- `alignment` is 2 or 4 (16-bit or 32-bit alignment)

**Postconditions**: Returns native pointer to memory location

**Semantics**: Convert Lisp virtual address to native memory pointer using FPtoVP mapping.

### Memory Allocation

**Operation**: `allocate_cons_cell() -> LispPTR`

**Operation**: `allocate_array(size, type) -> LispPTR`

**Operation**: `allocate_page() -> LispPTR`

**Semantics**: Allocate memory objects in Lisp heap.

## Garbage Collection Interface

### Reference Operations

**Operation**: `add_reference(object_ptr, ref_type)`

**Operation**: `remove_reference(object_ptr, ref_type)`

**Operation**: `mark_stack_reference(object_ptr)`

**Semantics**: Track object references for garbage collection.

**Reference Types**:

- `ADDREF`: Normal reference increment
- `DELREF`: Normal reference decrement
- `STKREF`: Stack reference (special handling)

### GC Trigger

**Operation**: `check_storage_full(pages_needed) -> trigger_gc`

**Semantics**: Check if storage is full and trigger GC if needed.

## Interrupt Interface

### Interrupt State

**Structure**:

```
InterruptState {
    LogFileIO: boolean
    ETHERInterrupt: boolean
    IOInterrupt: boolean
    gcdisabled: boolean
    vmemfull: boolean
    stackoverflow: boolean
    storagefull: boolean
    waitinginterrupt: boolean
    intcharcode: integer
}
```

### Interrupt Handling

**Operation**: `check_interrupts() -> InterruptState`

**Operation**: `handle_interrupt(interrupt_type)`

**Semantics**: Check for pending interrupts between bytecode instructions and handle them.

## Stack Interface

### Stack Frame Operations

**Operation**: `allocate_stack_frame(size) -> FramePointer`

**Operation**: `free_stack_frame(frame_ptr)`

**Operation**: `get_activation_link(frame_ptr) -> FramePointer`

**Semantics**: Manage stack frames for function calls.

### Stack Frame Structure

Must include:

- Activation link (previous frame pointer)
- Function header reference
- Program counter offset
- Local variables
- Name table (if present)

## Function Call Interface

### Function Invocation

**Operation**: `call_function(function_ptr, args[], arg_count) -> return_value`

**Semantics**: Invoke Lisp function with arguments.

**Process**:

1. Allocate stack frame
2. Set up activation link
3. Push arguments
4. Set program counter
5. Enter dispatch loop

### Function Return

**Operation**: `return_from_function(return_value)`

**Semantics**: Return from function call, restoring previous frame.

## Bytecode Execution Interface

### Instruction Fetch

**Operation**: `fetch_instruction(pc) -> Instruction`

**Semantics**: Fetch bytecode instruction from program counter.

### Instruction Decode

**Operation**: `decode_opcode(instruction) -> opcode_value`

**Operation**: `decode_operands(instruction, opcode) -> operands[]`

**Semantics**: Decode instruction into opcode and operands.

### Instruction Execute

**Operation**: `execute_opcode(opcode, operands[]) -> result`

**Semantics**: Execute opcode handler with operands.

## Platform Abstraction

### Required Behaviors (Must Match)

- Address translation algorithm
- Stack frame layout
- Function call/return mechanism
- Interrupt handling semantics
- Bytecode execution semantics

### Implementation Choices (May Differ)

- Dispatch mechanism (computed goto vs switch)
- Stack allocation strategy
- Interrupt checking frequency
- Instruction caching (if any)

## Error Handling

- Invalid address: Return error, do not crash
- Stack overflow: Trigger stack overflow interrupt
- Invalid opcode: Trigger error handler
- Memory allocation failure: Trigger storage full interrupt

## Compatibility Requirements

- Must execute bytecode identically to Maiko
- Must maintain stack frame compatibility
- Must handle interrupts correctly
- Must preserve function call semantics
