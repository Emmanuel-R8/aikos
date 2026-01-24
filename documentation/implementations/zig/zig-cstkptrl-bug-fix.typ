= Zig CSTKPTRL Bug Fix - Stack Pointer Corruption Resolution

*Navigation*: Implementations README | Zig Implementation

*Date*: 2026-01-20 08:01
*Severity*: CRITICAL (caused immediate failure at UNBIND opcode)

== Issue Summary

The Zig emulator was failing at PC=0x60f153 with "unknown opcode 0x78" error immediately after executing the UNBIND opcode. The root cause was **two related bugs in CSTKPTRL (C-stack pointer) handling**.

== Symptoms

- Emulator failure: `ERROR: Failed to decode instruction at PC=0x60f153, opcode=0x78`
- Debug output showed: `translateAddress: FOUND! lisp_addr=0xfffe0002, masked=0xffe0002, byte_offset=0x1ffc0004`
- The marker value `0xfffe0002` was being incorrectly passed to address translation

== Root Cause Analysis

=== Bug 1: UNBIND Loop Not Updating vm.cstkptrl

*Problem*: The original UNBIND implementation used a local variable `p` that didn't update the actual `vm.cstkptrl`:

```zig
// BEFORE (WRONG)
var p = vm_obj.cstkptrl orelse return error.StackUnderflow;
while (true) {
    p -= 1;  // Only updates local 'p', not vm.cstkptrl!
    const v = p[0];
    if (@as(i32, @bitCast(v)) < 0) { ... }
}
```

In C, the for loop `for (; (((int)*--CSTKPTRL) >= 0););` directly modifies CSTKPTRL (a global variable). The Zig code was using a local copy that never propagated back.

*Fix*: Directly update `vm.cstkptrl` to match C behavior:

```zig
// AFTER (CORRECT)
while (true) {
    vm_obj.cstkptrl = vm_obj.cstkptrl.?;
    vm_obj.cstkptrl = vm_obj.cstkptrl.? - 1;
    const v = vm_obj.cstkptrl.?[0];
    if (@as(i32, @bitCast(v)) < 0) { ... }
}
```

=== Bug 2: CSTKPTRL Not Restored Before Each Opcode

*Problem*: CSTKPTRL was only restored once at the start of `dispatch()`, but in C it's restored at the "nextopcode:" label **before each opcode** (via `StackPtrRestore` macro). If opcodes modify CSTKPTRL (like POP, PUSH, UNBIND), subsequent opcodes would have incorrect CSTKPTRL values.

*Location*: `zaiko/src/vm/dispatch.zig`

*Fix*: Moved CSTKPTRL restoration inside the dispatch loop, before each instruction:

```zig
while (true) {
    // CRITICAL: Restore CSTKPTRL from CurrentStackPTR before each opcode
    // C: StackPtrRestore is called at the start of nextopcode: label (before each opcode)
    stack.initCSTKPTRLFromCurrentStackPTR(vm);

    // ... decode and execute instruction
}
```

== Files Modified

1. `zaiko/src/vm/opcodes/binding.zig` - handleUNBIND function (Bug 1 fix)
2. `zaiko/src/vm/dispatch.zig` - dispatch loop (Bug 2 fix)

== Results

| Metric | Before Fix | After Fix |
|--------|-----------|-----------|
| Failure PC | 0x60f153 | 0x60f190 |
| Instructions executed | ~4 | ~30+ |
| Error type | CSTKPTRL/marker corruption | Unimplemented opcode (expected) |

The emulator now successfully executes through POP, GVAR, UNBIND, TJUMP, FJUMP, and many other instructions before hitting unimplemented opcodes (0x17, 0x7f, 0x7e), which is expected progress for a partially-complete emulator.

== Technical Details

=== C Reference Code

The C implementation uses these macros for CSTKPTRL manipulation:

```c
// maiko/inc/inlineC.h - UNBIND macro
#define UNBIND for (; (((int)*--CSTKPTRL) >= 0););

// maiko/inc/tos1defs.h - StackPtrRestore macro
#define StackPtrRestore  CSTKPTRL = (void *)(CurrentStackPTR + 2);
```

Note: `CSTKPTRL` is declared as `LispPTR*` (pointer to 4-byte LispPTR values), NOT `DLword*` (2-byte values). The `--` operator decrements by 4 bytes, not 2.

=== Stack Pointer Initialization

The CSTKPTRL is initialized from CurrentStackPTR:

```zig
// zaiko/src/vm/stack.zig
pub fn initCSTKPTRLFromCurrentStackPTR(vm: *VM) void {
    const addr = @intFromPtr(vm.stack_ptr) + 4; // +2 DLwords = +4 bytes = +1 LispPTR cell
    vm.cstkptrl = @as([*]align(1) LispPTR, @ptrFromInt(addr));
}
```

This matches C: `CSTKPTRL = (void *)(CurrentStackPTR + 2);` where CurrentStackPTR is a DLword pointer.

== Lessons Learned

1. **Pointer semantics matter**: When translating C code that modifies global pointers, ensure the Zig equivalent updates the actual VM state, not local copies.

2. **Dispatch loop timing**: The C dispatch loop restores CSTKPTRL at the "nextopcode:" label BEFORE each opcode. This is critical for correct opcode sequencing.

3. **Debug output is essential**: The detailed debug output showing CSTKPTRL values and virtual memory ranges made this bug traceable and verifiable.

== Verification

To verify the fix is working correctly:

1. Run the Zig emulator with debug output
2. Verify CSTKPTRL is in virtual memory range at dispatch start
3. Verify UNBIND correctly finds the marker and updates CSTKPTRL
4. Check that subsequent opcodes execute without CSTKPTRL corruption

Expected output after fix:
```
DEBUG dispatch: stack_base=0x7ffff2960000, stack_ptr=0x7ffff2965d10, CSTKPTRL=0x7ffff2965d14
DEBUG dispatch: stack_ptr in vmem: true
DEBUG dispatch: CSTKPTRL in vmem: true
...
DEBUG UNBIND: CSTKPTRL=0x7ffff2965d10, v=0xfffe0002
```
