= C Emulator COPY Opcode Deep Tracing

*Date*: 2025-01-27
*Status*: ✅ Completed - Verified Understanding
*Location*: `maiko/src/xc.c`

== Purpose

This document tracks exhaustive tracing of the C emulator's COPY opcode to understand:
- Stack duplication mechanism
- TOS value preservation
- Stack pointer manipulation

== Tracing Implementation

=== COPY Opcode Detailed Trace

*Location*: `maiko/src/xc.c:1665-1703`

Added comprehensive tracing for COPY opcode (instruction #5) to understand stack duplication:

```c
/* AUTO: DETAILED TRACING for COPY to understand stack duplication */
static int copy_traced = 0;
if (!copy_traced) {
  // ... detailed tracing ...
}
```

=== Trace Output

The trace captures:
1. **Current State**: PC pointer and offset
2. **Stack State**: TopOfStack, CurrentStackPTR, stack depth before COPY
3. **Value to be Copied**: TOS value that will be duplicated
4. **Expected Stack State**: Stack state after COPY (simulated)

== Findings from Trace Execution

*Date*: 2025-01-27
*Trace File*: `c_copy_detailed_trace.txt`

=== Execution Context

- **PC (byte pointer)**: `0x7f00b060f139`
- **PC offset from Lisp_world**: `0x60f139` bytes (`0x30789c` DLwords)

=== Stack State Before COPY

- **TopOfStack (TOS)**: `0x00000000`
- **CurrentStackPTR**: `0x7f00b0025d10`
- **CurrentStackPTR offset from Lisp_world**: `0x25d10` bytes
- **Stack depth**: 11912 entries

*Note*: TOS is 0, which suggests the previous instruction (GETBASEPTR_N) resulted in a null pointer.

=== Value to be Copied

- **TOS value**: `0x00000000`
- **Action**: This value will be pushed onto stack (duplicated)

=== Expected Stack State After COPY

- **New TOS**: `0x00000000` (same as old TOS)
- **Old TOS**: `0x00000000` (now at TOS-1)
- **Stack depth**: 11913 entries (increased by 1)

== COPY Algorithm Summary

1. **Get TOS value**: `TOPOFSTACK`
2. **Push value**: `HARD_PUSH(TOPOFSTACK)` - pushes TOS onto stack
3. **Advance PC**: `nextop1` - advance by 1 byte (opcode only, no operands)

== Key Understanding

### Stack Duplication

- COPY duplicates the top-of-stack value
- Original TOS remains on stack (becomes TOS-1)
- New TOS is a copy of the original value
- Stack depth increases by 1

### C Macro Definition

```c
#define COPY               \
  do {                     \
    HARD_PUSH(TOPOFSTACK); \
    nextop1;               \
  } while (0)
```

**Key Points**:
- `HARD_PUSH(TOPOFSTACK)`: Pushes current TOS value onto stack
- `nextop1`: Advance PC by 1 byte (opcode only, no operands)
- No value modification - pure duplication

### Stack Pointer Manipulation

- `CurrentStackPTR` is incremented by 2 (for LispPTR push)
- Original TOS value is preserved at new TOS-1 location
- New TOS contains copy of original value

== Zig Implementation Verification

*Status*: ✅ Verified

The Zig implementation in `zaiko/src/vm/opcodes/variable_access.zig` matches the C behavior:

```zig
pub fn handleCOPY(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const value = stack_module.getTopOfStack(vm);
    try stack_module.pushStack(vm, value);
}
```

**Verification**:
- Gets TOS value: `getTopOfStack(vm)`
- Pushes copy: `pushStack(vm, value)`
- Matches C: `HARD_PUSH(TOPOFSTACK)`

== Related Documentation

- `c-emulator-getbaseptr-tracing.typ` - GETBASEPTR_N opcode analysis
- `c-emulator-unbind-tracing.typ` - UNBIND opcode analysis
- `zig-frame-initialization-analysis.typ` - Frame initialization
