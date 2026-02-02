# VM Core Interface Contract

**Version**: 1.0
**Date**: 2025-12-04
**Type**: Interface Specification
**Language**: Zig

## Overview

This contract specifies the interfaces for VM core components in the Zig implementation. These interfaces must be maintained for compatibility and modularity.

## Address Translation Interface

```zig
// Translate LispPTR to native pointer (2-byte aligned)
pub fn translateAddress2(ptr: LispPTR, fptovp: []LispPTR) ![*]DLword;

// Translate LispPTR to native pointer (4-byte aligned)
pub fn translateAddress4(ptr: LispPTR, fptovp: []LispPTR) ![*]u32;

// Reverse translation: native pointer to LispPTR
pub fn lispAddressFromNative(native: [*]DLword, fptovp: []LispPTR) LispPTR;
```

**Preconditions**:

- `ptr` is valid LispPTR
- `fptovp` table is initialized

**Postconditions**: Returns native pointer to memory location

**Semantics**: Convert Lisp virtual address to native memory pointer using FPtoVP mapping.

## Dispatch Loop Interface

```zig
// Main dispatch loop
pub fn dispatch(vm: *VM) !void;

// Fetch instruction
pub fn fetchInstruction(pc: LispPTR, code: []const ByteCode) ByteCode;

// Decode opcode
pub fn decodeOpcode(byte: ByteCode) Opcode;

// Execute opcode handler
pub fn executeOpcode(vm: *VM, opcode: Opcode) !void;
```

**Preconditions**: VM initialized, code loaded

**Postconditions**: Instructions executed, PC advanced

**Semantics**: Main execution loop that fetches, decodes, and executes bytecode instructions.

## Stack Management Interface

```zig
// Allocate stack frame
pub fn allocateStackFrame(vm: *VM, size: usize) !*FX;

// Free stack frame
pub fn freeStackFrame(vm: *VM, frame: *FX) void;

// Extend stack
pub fn extendStack(vm: *VM) !void;

// Get activation link
pub fn getActivationLink(frame: *FX) LispPTR;
```

**Preconditions**: Stack space available

**Postconditions**: Frame allocated/freed, stack extended if needed

**Semantics**: Manage stack frames for function calls.

## Function Call Interface

```zig
// Call function
pub fn callFunction(vm: *VM, func: LispPTR, args: []LispPTR) !void;

// Return from function
pub fn returnFromFunction(vm: *VM, value: LispPTR) void;

// Setup function frame
pub fn setupFunctionFrame(vm: *VM, func_header: *FunctionHeader, args: []LispPTR) !*FX;
```

**Preconditions**: Function valid, arguments correct

**Postconditions**: Function called, frame set up, or returned

**Semantics**: Invoke Lisp function with arguments, manage call/return.

## Interrupt Handling Interface

```zig
// Check for interrupts
pub fn checkInterrupts(vm: *VM) bool;

// Handle interrupt
pub fn handleInterrupt(vm: *VM, interrupt_type: InterruptType) !void;

// Set interrupt flag
pub fn setInterruptFlag(vm: *VM, interrupt_type: InterruptType) void;
```

**Preconditions**: VM running

**Postconditions**: Interrupts checked/handled, flags set

**Semantics**: Check for pending interrupts between instructions and handle them.

## Error Types

```zig
pub const VMError = error{
    InvalidAddress,
    StackOverflow,
    InvalidOpcode,
    StorageFull,
    SysoutLoadFailed,
    InterruptError,
};
```

## Related Documentation

- `documentation/rewrite-spec/vm-core/` - VM core specifications
- `documentation/rewrite-spec/memory/address-translation.md` - Address translation details
