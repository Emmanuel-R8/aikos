= Zig Stack Pointer Initialization Fix

*Date*: 2026-02-03 05:00 *Status*: ✅ Fixed

== Problem

Zig emulator had incorrect stack pointer initialization:
- C emulator: SP=0x02e88, FP=0x307864
- Zig emulator: SP=0x002e88, FP=0x002e72 (wrong)

== Root Cause

Line 198 in `zaiko/src/vm/vm_initialization.zig` set `vm.stack_ptr = pvar_ptr` instead of `current_stack_ptr`.

The issue was that `vm.stack_ptr` should point to `CurrentStackPTR` (the active stack pointer used for push/pop
operations), but the code was setting it to `PVar` (CurrentStackPTR + FRAMESIZE), which is used only for accessing local
variables.

== Solution

Changed `vm.stack_ptr` to point to `current_stack_ptr` (next68k - 2 DLwords) instead of `pvar_ptr`:

```zig
// Before (WRONG):
vm.stack_ptr = pvar_ptr; // PVar = CurrentStackPTR + FRAMESIZE

// After (CORRECT):
vm.stack_ptr = current_stack_ptr; // CurrentStackPTR = next68k - 2 DLwords
```

Also updated debug print statements to reflect the correct behavior.

== Verification

After the fix:
- Zig stack depth: 0x2e88 (5956 DLwords) - matches C
- Zig PC: 0x60f130 - matches C
- Zig frame pointer: 0x307864 - matches C
- Extended test (100 steps): Execution continues correctly
- All existing tests pass

== Related Files

- `zaiko/src/vm/vm_initialization.zig` - VM initialization code
- `zaiko/src/vm/stack.zig` - Stack management structures
- `maiko/src/main.c` - C reference implementation (start_lisp function)
- `reports/WORK_STATE.md` - Project work state documentation

== Cross-References

- C implementation: `maiko/src/main.c:1339` (CurrentStackPTR = next68k - 2)
- C implementation: `maiko/src/main.c:1329` (PVar = NativeAligned2FromStackOffset(currentfxp) + FRAMESIZE)
- C implementation: `maiko/inc/adr68k.h:105-108` (NativeAligned2FromStackOffset macro)
- C implementation: `maiko/inc/lispemul.h:309` (CURRENTFX macro)
- C implementation: `maiko/inc/stack.h:198-231` (frameex1 structure)
