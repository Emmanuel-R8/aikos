= C Emulator PC Advancement Bug Fix

*Date*: 2026-01-12 19:45
*Status*: ✅ Fixed
*Purpose*: Document the critical PC advancement bug fix in C emulator dispatch loop

== Problem Statement

The C emulator was stuck in an infinite loop at PC `0x60f130`, executing the same `POP` instruction repeatedly. All 1000 logged instructions showed the same PC value, indicating that the program counter was not advancing after opcode execution.

== Root Cause Analysis

=== Dispatch Loop Structure

The C emulator's dispatch loop (`maiko/src/xc.c`) uses the following pattern:

```c
nextopcode:
  pccache = PC + 1;  // Initialize pccache from global PC
  // ... logging and opcode execution ...
  switch (Get_BYTE_PCMAC0) {
    case 0xBF: // POP
      POP;
      nextop1;  // Does: pccache += 1; goto nextopcode;
  }
```

=== The Bug

The critical issue was that the global `PC` variable was never updated from `pccache` after opcode execution:

1. At start of `nextopcode:`, `pccache = PC + 1` resets `pccache` to the old `PC` value
2. Opcode executes and increments `pccache` via `nextop1` (e.g., `pccache += 1`)
3. Control returns to `nextopcode:` via `goto nextopcode`
4. `pccache = PC + 1` executes again, resetting `pccache` to the stale `PC` value
5. The increment from step 2 is lost, causing infinite loop

=== Why It Worked Before

The original C emulator code likely had a mechanism to update `PC` from `pccache`, but this was lost or never properly implemented. The `pccache` variable is a local variable in the dispatch function, while `PC` is a global variable that needs to be synchronized.

== Solution

=== Fix Implementation

Added code to update `PC` from `pccache` before resetting `pccache` at the start of each iteration:

*Location*: `maiko/src/xc.c:688-696`

```c
/* AUTO: Update PC from pccache BEFORE resetting pccache, so PC stays in sync */
/* This ensures PC reflects the actual current position after opcode execution */
if (global_debug_instruction_count > 0) {
  PC = PCMAC;  // PCMAC = pccache - 1, so PC = pccache - 1
}

/* Initialize pccache from PC (PCMAC = pccache - 1, so pccache = PC + 1) */
/* CRITICAL: Must initialize BEFORE using PCMAC macro */
pccache = PC + 1;
```

=== How It Works

1. After first instruction (`global_debug_instruction_count > 0`), update `PC = PCMAC`
2. `PCMAC` is defined as `pccache - 1`, so this sets `PC` to the current position
3. Then reset `pccache = PC + 1` for the next iteration
4. This preserves the PC advancement from the previous opcode execution

=== Verification

After the fix:
- C emulator now advances PC correctly: `0x60f130 → 0x60f131 → 0x60f136 → ...`
- Logs show 1000 distinct instructions instead of stuck loop
- First 5 instructions match Zig emulator execution exactly

== Technical Details

=== PC and pccache Relationship

- `PC`: Global variable of type `ByteCode *`, represents current program counter
- `pccache`: Local variable in `dispatch()`, used for efficient bytecode access
- `PCMAC`: Macro defined as `pccache - 1`, represents the actual PC position
- `PCMACL`: Macro defined as `pccache`, used for lvalue operations

=== Opcode PC Advancement

Opcodes advance `pccache` using macros:
- `nextop0`: `goto nextopcode` (no advance)
- `nextop1`: `pccache += 1; goto nextopcode` (advance by 1 byte)
- `nextop2`: `pccache += 2; goto nextopcode` (advance by 2 bytes)
- `nextop3`: `pccache += 3; goto nextopcode` (advance by 3 bytes)
- `nextop5`: `pccache += 5; goto nextopcode` (advance by 5 bytes, for BIGATOMS)

After `pccache` is incremented, `PC` must be updated to reflect the new position.

== Impact

This fix was critical for:
1. Enabling execution comparison between C and Zig emulators
2. Verifying that C emulator executes correctly (source of truth)
3. Identifying that Zig emulator's GVAR implementation needed BIGATOMS mode support

== Related Documentation

- Unified Logging Format - Execution log format for comparison
- Zig GVAR BIGATOMS Implementation - Matching GVAR implementation
- Execution Comparison Analysis - Results of C vs Zig comparison
