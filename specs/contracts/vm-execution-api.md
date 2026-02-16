# VM Execution API Contract

**Date**: 2025-12-07
**Feature**: Zig Emulator Completion

## Overview

API contract for VM execution functionality, defining the interface for bytecode execution and VM state management.

## Functions

### `initializeVMState`

**Signature**:

```zig
pub fn initializeVMState(
    vm: *VM,
    ifpage: *const IFPAGE,
    virtual_memory: []u8,
) !void
```

**Purpose**: Initialize VM state from IFPAGE after sysout loading.

**Parameters**:

- `vm`: VM instance to initialize
- `ifpage`: IFPAGE structure with VM state
- `virtual_memory`: Loaded virtual memory space

**Returns**: `void` on success, error on failure

**Errors**:

- `error.InvalidStackPointer` - Stack pointers out of range
- `error.InvalidFramePointer` - Frame pointer invalid

**Preconditions**:

- `ifpage` must be validated
- `virtual_memory` must be loaded

**Postconditions**:

- VM state initialized from IFPAGE
- Stack pointers set
- Frame pointer set
- Program counter initialized

---

### `startDispatchLoop`

**Signature**:

```zig
pub fn startDispatchLoop(vm: *VM) !void
```

**Purpose**: Enter the VM dispatch loop and begin bytecode execution.

**Parameters**:

- `vm`: Initialized VM instance

**Returns**: `void` on success (may not return if loop runs indefinitely)

**Errors**:

- `error.VMNotInitialized` - VM state not initialized
- `error.InvalidProgramCounter` - Program counter invalid

**Preconditions**:

- VM state must be initialized
- Program counter must point to valid code

**Postconditions**:

- Dispatch loop running
- Bytecode execution active

---

### `executeOpcode`

**Signature**:

```zig
pub fn executeOpcode(
    vm: *VM,
    opcode: Opcode,
    args: []const LispPTR,
) !void
```

**Purpose**: Execute a single opcode instruction.

**Parameters**:

- `vm`: VM instance
- `opcode`: Opcode to execute
- `args`: Opcode arguments

**Returns**: `void` on success, error on failure

**Errors**:

- `error.InvalidOpcode` - Opcode not recognized
- `error.StackOverflow` - Stack overflow
- `error.StackUnderflow` - Stack underflow
- `error.InvalidMemoryAccess` - Invalid memory access

**Preconditions**:

- VM state must be valid
- Opcode must be valid
- Arguments must be valid for opcode

**Postconditions**:

- VM state modified according to opcode semantics
- Stack updated if applicable
- Memory updated if applicable

---

### `handleInterrupt`

**Signature**:

```zig
pub fn handleInterrupt(
    vm: *VM,
    interrupt_type: InterruptType,
) !void
```

**Purpose**: Handle an interrupt during VM execution.

**Parameters**:

- `vm`: VM instance
- `interrupt_type`: Type of interrupt (I/O, timer, system)

**Returns**: `void` on success, error on failure

**Errors**:

- `error.InterruptHandlerFailed` - Interrupt handler failed

**Preconditions**:

- VM must be executing
- Interrupt must be pending

**Postconditions**:

- Interrupt handled
- VM state updated
- Execution may resume or halt

---

## Data Structures

### `VM`

**Definition**:

```zig
pub const VM = struct {
    stackbase: LispPTR,
    endofstack: LispPTR,
    currentfxp: LispPTR,
    program_counter: LispPTR,
    virtual_memory: []u8,
    // ... other VM state fields
};
```

**Purpose**: Represents VM execution state

**Fields**:

- `stackbase`: Stack base address
- `endofstack`: End of stack address
- `currentfxp`: Current frame pointer
- `program_counter`: Current bytecode position
- `virtual_memory`: Virtual memory space

**Constraints**:

- Stack pointers must be within valid range
- Program counter must point to valid code

---

### `Opcode`

**Definition**: Enum of all 256 opcodes

**Purpose**: Represents bytecode instruction types

**Categories**:

- Arithmetic (IPLUS2, IDIFFERENCE, etc.)
- Comparison (EQ, EQL, LESSP, etc.)
- Control flow (JUMP, CALL, RETURN, etc.)
- Memory (CAR, CDR, CONS, etc.)
- I/O (BIN, BOUT, etc.)

---

### `InterruptType`

**Definition**:

```zig
pub const InterruptType = enum {
    IO,
    Timer,
    System,
    StorageFull,
};
```

**Purpose**: Types of interrupts that can occur

**Values**:

- `IO`: I/O operation interrupt
- `Timer`: Timer interrupt
- `System`: System interrupt
- `StorageFull`: Storage full interrupt

---

## Constants

### `LVERSION`

**Value**: Lisp version constant (from C implementation)

**Purpose**: Minimum Lisp version required

**Usage**: Version compatibility checking

---

### `MINBVERSION`

**Value**: Minimum bytecode version constant (from C implementation)

**Purpose**: Maximum bytecode version supported

**Usage**: Version compatibility checking

---

## Error Handling

All functions use Zig error unions (`!Type`) for error handling.

**Error Types**:

- `VMNotInitialized` - VM state not initialized
- `InvalidProgramCounter` - Program counter invalid
- `InvalidOpcode` - Opcode not recognized
- `StackOverflow` - Stack overflow
- `StackUnderflow` - Stack underflow
- `InvalidMemoryAccess` - Invalid memory access
- `InterruptHandlerFailed` - Interrupt handler failed

**Error Propagation**: Errors are propagated using `try` keyword

---

## Usage Example

```zig
// Initialize VM from sysout
const sysout_result = try sysout.loadSysout(allocator, "starter.sysout");
var vm = VM{};
try vm.initializeVMState(&sysout_result.ifpage, sysout_result.virtual_memory);

// Start execution
try vm.startDispatchLoop();
```
