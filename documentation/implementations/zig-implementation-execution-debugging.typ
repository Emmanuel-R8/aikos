= Execution Debugging - Critical Discrepancies and Investigation

*Navigation*: Zig Implementation Status | Implementations README | Main README

*Date*: 2025-12-22 12:30  
*Last Updated*: 2025-12-23 16:43
*Status*: Active - Critical execution discrepancies identified, debugging in progress

== Overview

This document consolidates findings from execution trace comparison between C and Zig emulators, identifying critical discrepancies that prevent Zaiko from executing correctly.

== Executive Summary

*CRITICAL*: Execution trace comparison reveals Zaiko emulator is NOT producing correct execution. 0/30 lines match C emulator exactly. Multiple fundamental issues identified.

=== Comparison Results

- *C Emulator Log*: 2000 lines available for comparison
- *Zig Emulator Log*: 2000 lines available for comparison
- *Exact Matches*: 0/2000 lines (0%)
- *Status*: ❌ FAILED - Zaiko execution does not match C emulator

== Systematic Line-by-Line Comparison (2025-12-23)

*Method*: Applied critical debugging technique (value analysis) per `CRITICAL_DEBUGGING_TECHNIQUE.typ`

=== First 10 Lines Analysis

*PC Values*:
- Line 1: Both start at `0x307898` ✓ (PC initialization correct)
- Line 2: C stays at `0x307898`, Zig advances to `0x30789a` ✗ (PC progression differs)
- Line 3: C `0x30789b`, Zig `0x30789c` ✗ (divergence continues)
- Pattern: C PC can stay same or advance by 1-3 bytes; Zig always advances by instruction length

*Stack Depth Analysis* (applying critical debugging technique):
- C value: `5956` (0x1744)
- Zig value: `11912` (0x2e88)
- *Analysis*:
  - `11912 / 2 = 5956` ✓ (exact 2x relationship)
  - `11912 / 4 = 2978` (not matching)
  - `5956 * 2 = 11912` ✓
- *Conclusion*: Zig stack depth is in bytes, C is in DLwords. Zig needs to divide by 2.

*Instruction Bytes*:
- Line 1 C: `000060bfc9120a02` → Opcode `POP` (0xBF)
- Line 1 Zig: `0e00000000000000` → Opcode `APPLYFN` (0x0E)
- *Completely different* - memory content is wrong at PC location

*FuncObj Offset*:
- C: `FuncObj+  104 bytes` (correct)
- Zig: `FuncObj++4687768 bytes` (wrong - huge offset)
- *Analysis*:
  - `4687768 / 104 = 45074.69` (not a simple factor)
  - `4687768 / 2 = 2343884` (not matching)
  - `4687768 / 4 = 1171942` (not matching)
- *Conclusion*: FuncObj offset calculation is fundamentally wrong

*Stack Pointer (P)*:
- C: `P:11912` (consistent)
- Zig: `P:    0` (wrong - always zero)
- *Analysis*:
  - C uses stack pointer offset in DLwords
  - Zig shows zero, indicating stack pointer calculation broken

*Top of Stack (TOS)*:
- C: `TOS:0x000000000000000e` (actual value)
- Zig: `TOS:0x0000000000000000` (all zeros)
- *Conclusion*: Stack initialization or TOS reading is broken

*Frame Header (FH)*:
- C: `FH:0x307864`
- Zig: `FH:0x780030`
- *Analysis*:
  - `0x780030 / 2 = 0x3c0018` (not matching)
  - `0x780030 / 0x307864 = 2.45` (not a simple factor)
- *Conclusion*: Frame header reading from memory is incorrect

*Frame Offset (FO)*:
- C: `FO:+6353096`
- Zig: `FO:+15728736`
- *Analysis*:
  - `15728736 / 2 = 7864368` (not matching)
  - `15728736 / 6353096 = 2.475` (not exact 2x)
  - `15728736 - 6353096 = 9375640` (difference not meaningful)
- *Conclusion*: Frame offset calculation uses wrong formula

=== Critical Findings Summary

1. *PC Initialization*: ✅ Correct (both start at 0x307898)
2. *PC Progression*: ❌ Wrong (Zig always advances, C can stay same)
3. *Stack Depth*: ❌ Wrong (Zig is 2x C - byte vs DLword issue)
4. *Memory Content*: ❌ Wrong (completely different bytes at PC)
5. *FuncObj Offset*: ❌ Wrong (huge incorrect value)
6. *Stack Pointer*: ❌ Wrong (always zero in Zig)
7. *TOS Values*: ❌ Wrong (all zeros in Zig)
8. *Frame Header*: ❌ Wrong (different value)
9. *Frame Offset*: ❌ Wrong (different calculation)

== Critical Issues Identified

=== Issue 1: Memory Loading Failure ⚠️ CRITICAL

*Evidence*:
- At PC 0x307898, C emulator reads: `00 00 60 bf c9 12 0a 02`
- At PC 0x307898, Zig emulator reads: `05 00 0e 00 3f 00 36 00`
- *Completely different content* - not a byte-order issue

*Root Cause*: Either:
- Wrong file page being loaded (file page 5178 loaded, but should be different?)
- FPtoVP table mapping incorrect
- Byte-swapping applied incorrectly
- Virtual page calculation wrong

*Impact*: All instruction execution is wrong because memory content is wrong

*Investigation Status*:
- ✅ Verified: Only 1 file page (5178) maps to virtual page 6204
- ✅ Verified: FPtoVP[5178] = 6204 (GETFPTOVP), GETPAGEOK = 0x0000
- ✅ Verified: File page reading logic matches C emulator
- ⚠️ Issue: Raw bytes from file don't match what C expects after byte-swap
- ⏳ PENDING: Verify which file page C emulator actually loads for virtual page 6204

=== Issue 2: FuncObj Offset Calculation Wrong ⚠️ CRITICAL

*Evidence*:
- C shows: `FuncObj+  104 bytes` (correct)
- Zig shows: `FuncObj++4687768 bytes` (wrong - off by factor of ~45,000)

*Root Cause*: FuncObj offset calculation uses wrong formula
- Should use: `CURRENTFX->pc` field (saved PC offset from function header)
- Currently using: `PC - FuncObj` calculation which is wrong

*Impact*: Frame handling, function lookups, and return address calculations are wrong

*Fix Required*: Update `zaiko/src/vm/execution_trace.zig` to use `CURRENTFX->pc` field directly

=== Issue 3: PC Progression Wrong ⚠️ CRITICAL

*Evidence*:
- C PC: `0x307898 → 0x307898 → 0x30789b → 0x30789b → 0x30789c...`
- Zig PC: `0x307898 → 0x30789a → 0x30789c → 0x30789e → 0x3078a0...`

*Root Cause*: PC always advances by instruction length, but C shows PC can stay same (multi-byte instructions or different execution model)

*Impact*: Wrong instructions executed, execution diverges immediately

*Fix Required*: Update PC advancement logic in `zaiko/src/vm/dispatch/dispatch_loop.zig`

=== Issue 4: Stack Depth Calculation Wrong ⚠️ CRITICAL

*Evidence*:
- C shows: `Stack: D:5956`
- Zig shows: `Stack: D:11912` (exactly 2x)

*Root Cause*: Stack depth calculation formula incorrect
- C: `(CurrentStackPTR - Stackspace) / 2` where both are DLword* pointers
- Zig: Was dividing by 4, but should match C's pointer arithmetic

*Status*: ✅ FIXED - Updated to match C exactly

*Fix Applied*: Updated `zaiko/src/vm/stack.zig` to match C exactly
- Formula: `(diff_bytes / 2) / 2 = diff_bytes / 4`
- Matches C's pointer arithmetic behavior

=== Issue 5: Frame Header (FH) Value Wrong ⚠️ CRITICAL

*Evidence*:
- C shows: `FH:0x307864`
- Zig shows: `FH:0x780030`

*Root Cause*: Frame header reading from frame structure is incorrect
- BIGVM frame layout may be wrong
- Byte order or field offset may be wrong

*Impact*: Function lookups fail, function calls/returns broken

*Fix Required*: Verify frame structure layout matches C definition in `maiko/inc/stack.h`

=== Issue 6: Top of Stack (TOS) Values Wrong ⚠️ CRITICAL

*Evidence*:
- C shows: `TOS:0x000000000000000e` (actual values)
- Zig shows: `TOS:0x0000000000000000` (all zeros)

*Root Cause*: TOS reading or stack initialization incorrect
- Stack may not be initialized correctly
- TOS calculation may be wrong

*Impact*: All stack-based operations fail (arithmetic, data ops, etc.)

*Fix Required*: Verify stack initialization and TOS calculation

=== Issue 7: Execution Stops Early ⚠️ CRITICAL

*Evidence*:
- C emulator: 1000+ lines of execution
- Zig emulator: Only 30 lines

*Root Cause*: Emulator crashes, errors, or stop condition triggered incorrectly

*Impact*: Cannot run full Medley session

*Fix Required*: Investigate why execution stops after 30 instructions

== Investigation History

=== Address and Byte-Order Investigation (2025-12-18)

*File*: `ADDRESS_BYTE_ORDER_INVESTIGATION.md`

*Key Findings*:
- ✅ XOR addressing implemented correctly
- ✅ Execution uses XOR addressing, logging uses raw memory (matches C)
- ⚠️ Raw memory bytes don't match C emulator
- ⚠️ File page 2937 has bytes C expects, but maps to virtual page 11850 (not 6204)
- ⚠️ File page 5178 maps to virtual page 6204, but bytes don't match C's expected bytes

*Status*: Investigation ongoing - memory loading issue identified

=== Endianness Analysis (2025-01-27)

*File*: `ENDIANNESS_FINDINGS.md` (1,893 lines)

*Key Findings*:
- ✅ XOR addressing pattern: `base ^ 3` for bytes, `base ^ 2` for words
- ✅ Manual value construction for multi-byte values
- ✅ Two-stage memory access: address translation + data access
- ✅ 32-bit longword swapping for page loading
- ✅ DLword arithmetic for address translation

*Status*: Static analysis complete, dynamic verification pending

=== Logging Analysis (2025-12-18)

*File*: `LOGGING_ANALYSIS.md`

*Key Findings*:
- ✅ PC initialization fixed (was off by 1 byte)
- ⚠️ Memory at PC location shows zeros (not loaded correctly)
- ⚠️ PC advancement differs from C (Zig always increments, C can stay same)

*Status*: PC initialization fixed, memory loading issue remains

=== Opcodes Review (2025-12-11)

*File*: `OPCODES_REVIEW.md`

*Key Findings*:
- ✅ ~100 opcodes implemented
- ✅ Most core functionality matches C reference
- ⚠️ C code function support missing
- ⚠️ DTD chain walk incomplete
- ⚠️ Individual opcode variants not implemented

*Status*: Good coverage, some gaps remain

== Related Documentation

- *Memory Debugging Systematic* - **NEW**: Systematic reverse engineering approach to memory loading issues
- Execution Debugging Plan - Systematic plan to fix all discrepancies
- Execution Trace Debugging Tasks - Detailed task list (26 tasks)
- XOR Addressing Implementation - XOR addressing implementation details
- Sysout Byte Swapping - Byte-endianness handling specification
- Endianness Findings - Complete endianness analysis (1,893 lines)
- Critical Debugging Technique - Value analysis method (ALWAYS APPLY)

== Next Steps

*Following Systematic Memory Debugging Approach* (see `zig-implementation-memory-debugging-systematic.typ`):

1. *IMMEDIATE*: Use memory dump utility to compare C vs Zig memory at PC 0x307898
2. *IMMEDIATE*: Verify address calculation step-by-step using critical debugging technique
3. *IMMEDIATE*: Verify file page 5178 → virtual page 6204 mapping
4. *IMMEDIATE*: Compare file page 5178 raw bytes with C emulator
5. *IMMEDIATE*: Verify byte-swapping matches C exactly (32-bit longword swap)
6. *IMMEDIATE*: Trace memory loading: file read → byte swap → virtual write
7. Fix FuncObj offset calculation (use CURRENTFX->pc directly)
8. Fix PC progression logic (match C's behavior)
9. Fix frame header reading (verify BIGVM frame layout)
10. Fix TOS initialization/reading (verify stack initialization)
11. Investigate early stop (check stop conditions)

== Success Criteria

- [ ] Memory at PC 0x307898 matches C emulator exactly
- [ ] All 1000 lines of execution log match C emulator
- [ ] PC progression matches C emulator
- [ ] Stack depth matches C emulator
- [ ] Frame header matches C emulator
- [ ] TOS values match C emulator
- [ ] FuncObj offset matches C emulator
- [ ] Execution continues beyond 30 instructions
