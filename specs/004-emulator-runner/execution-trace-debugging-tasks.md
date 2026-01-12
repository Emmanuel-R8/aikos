# Execution Trace Debugging Tasks - Zaiko vs C Emulator

**Date**: 2025-12-22 12:00
**Purpose**: Fix discrepancies found in execution trace comparison between C and Zig emulators

## Comparison Summary

**Logs Compared**: 
- C emulator: `c_emulator_execution_log_1000.txt` (1000 lines)
- Zig emulator: `zig_emulator_execution_log_1000.txt` (30 lines - stopped early)

**Result**: 0/30 lines match exactly - **CRITICAL DISCREPANCIES FOUND**

## Critical Issues Identified

### Issue 1: Memory Loading - Instruction Bytes Don't Match ⚠️ CRITICAL

**Symptom**: At PC 0x307898, C emulator reads `000060bfc9120a02` but Zig reads `0e00000000000000`

**Impact**: Zig emulator is reading wrong memory content, causing all subsequent execution to be incorrect

**Root Cause**: Likely memory loading or byte-swapping issue

**Tasks**:
- [X] D001 [CRITICAL] Verify memory loading at PC 0x307898 in `zaiko/src/data/sysout.zig` - compare loaded bytes with C emulator - **VERIFIED: MISMATCH FOUND** - C shows `00 00 60 bf c9 12 0a 02`, Zig shows `05 00 0e 00 3f 00 36 00`
- [ ] D002 [CRITICAL] Check byte-swapping logic in `zaiko/src/data/sysout.zig` - ensure bytes are swapped correctly for current platform - **ISSUE: Memory content fundamentally different, not just byte-order**
- [ ] D003 [CRITICAL] Verify FPtoVP table mapping for PC page (virtual page containing 0x307898) in `zaiko/src/data/sysout.zig` - **DEBUG shows file page 5178 -> virtual page 6204, but content is wrong**
- [X] D004 [CRITICAL] Add debug output to verify memory content matches C emulator after loading in `zaiko/src/data/sysout.zig` - **COMPLETE: Debug output shows mismatch**

### Issue 2: FuncObj Offset Calculation Wrong ⚠️ CRITICAL

**Symptom**: Zig shows `FuncObj++4687768 bytes` but C shows `FuncObj+  104 bytes` at PC 0x307898

**Impact**: FuncObj offset is used for frame calculations and function header lookups - wrong offset breaks frame handling

**Root Cause**: FuncObj offset calculation in execution trace is incorrect

**Tasks**:
- [ ] D005 [CRITICAL] Fix FuncObj offset calculation in `zaiko/src/vm/execution_trace.zig` - should use CURRENTFX->pc field (saved PC offset), not PC - FuncObj
- [ ] D006 [CRITICAL] Verify frame PC field reading in `zaiko/src/vm/execution_trace.zig` - ensure correct byte offset from frame structure
- [ ] D007 [CRITICAL] Compare FuncObj offset calculation with C emulator logic in `maiko/src/xc.c:568-570`

### Issue 3: PC Progression Wrong ⚠️ CRITICAL

**Symptom**: C emulator PC progresses: 0x307898 → 0x307898 → 0x30789b → 0x30789b → 0x30789c...
Zig emulator PC progresses: 0x307898 → 0x30789a → 0x30789c → 0x30789e → 0x3078a0...

**Impact**: Zig is executing wrong instructions because PC is advancing incorrectly

**Root Cause**: PC increment logic or instruction length calculation is wrong

**Tasks**:
- [ ] D008 [CRITICAL] Verify PC increment logic in `zaiko/src/vm/dispatch/execution.zig` - ensure PC advances by correct instruction length
- [ ] D009 [CRITICAL] Check instruction length calculation in `zaiko/src/vm/dispatch/instruction.zig` - verify all opcodes have correct length
- [ ] D010 [CRITICAL] Compare PC progression with C emulator - C shows PC can stay same (multi-byte instructions), Zig always increments

### Issue 4: Stack Depth Calculation Wrong ⚠️ CRITICAL

**Symptom**: Zig shows `Stack: D:11912` but C shows `Stack: D:5956` at same PC

**Impact**: Stack depth is used for stack operations - wrong depth breaks stack manipulation

**Root Cause**: Stack depth calculation or stack initialization is incorrect

**Tasks**:
- [X] D011 [CRITICAL] Fix stack depth calculation in `zaiko/src/vm/stack.zig` - verify formula matches C: `(CurrentStackPTR - Stackspace) / 2` - **FIXED: Updated calculation to match C exactly (diff_bytes / 4)**
- [ ] D012 [CRITICAL] Verify stack initialization in `zaiko/src/vm/init.zig` - ensure stack_base and stack_ptr are set correctly from IFPAGE
- [ ] D013 [CRITICAL] Compare stack pointer offset calculation with C emulator in `maiko/src/xc.c:745`

### Issue 5: Frame Header (FH) Value Wrong ⚠️ CRITICAL

**Symptom**: Zig shows `FH:0x780030` but C shows `FH:0x307864` at same PC

**Impact**: Frame header is used for function lookups - wrong value breaks function calls and returns

**Root Cause**: Frame header reading from frame structure is incorrect

**Tasks**:
- [ ] D014 [CRITICAL] Fix frame header reading in `zaiko/src/vm/execution_trace.zig` - verify BIGVM frame layout and byte order
- [ ] D015 [CRITICAL] Verify frame structure layout matches C definition in `maiko/inc/stack.h` - check BIGVM vs non-BIGVM
- [ ] D016 [CRITICAL] Compare frame header calculation with C emulator in `maiko/src/xc.c:770-775`

### Issue 6: Top of Stack (TOS) Values Wrong ⚠️ CRITICAL

**Symptom**: Zig shows `TOS:0x0000000000000000` (all zeros) but C shows actual values like `TOS:0x000000000000000e`

**Impact**: Stack operations depend on TOS - wrong values break arithmetic and data operations

**Root Cause**: TOS reading or stack initialization is incorrect

**Tasks**:
- [ ] D017 [CRITICAL] Fix TOS reading in `zaiko/src/vm/stack.zig` - verify TOPOFSTACK calculation matches C
- [ ] D018 [CRITICAL] Verify stack initialization sets correct initial TOS value in `zaiko/src/vm/init.zig`
- [ ] D019 [CRITICAL] Compare TOS calculation with C emulator in `maiko/src/xc.c:746`

### Issue 7: Execution Stops Early ⚠️ CRITICAL

**Symptom**: Zig emulator only produces 30 lines of execution log vs C's 1000+ lines

**Impact**: Emulator crashes or stops execution prematurely - cannot run full Medley session

**Root Cause**: Likely crash, error, or stop condition triggered incorrectly

**Tasks**:
- [ ] D020 [CRITICAL] Investigate why Zig emulator stops after 30 instructions in `zaiko/src/vm/dispatch/execution.zig` - check for crashes or early exit conditions
- [ ] D021 [CRITICAL] Add error handling and logging to identify crash point in `zaiko/src/main.zig`
- [ ] D022 [CRITICAL] Verify stop condition (PC >= 0xf000d5) is not triggered incorrectly in `zaiko/src/vm/execution_trace.zig:78`

### Issue 8: Instruction Bytes Format Mismatch ⚠️ MEDIUM

**Symptom**: C shows instruction bytes as `000060bfc9120a02` (8 bytes, shifted) but Zig shows `0e00000000000000` (8 bytes, different content)

**Impact**: Indicates memory reading is wrong, but format might also differ

**Root Cause**: Memory reading or formatting logic differs

**Tasks**:
- [X] D023 [MEDIUM] Verify instruction byte reading logic in `zaiko/src/vm/execution_trace.zig:114-132` - ensure reading from correct memory location - **FIXED: Changed to read directly from memory (no XOR) to match C log format**
- [ ] D024 [MEDIUM] Compare byte reading with C emulator in `maiko/src/xc.c:583-587` - verify same memory location and format

### Issue 9: Stack Next Values Format ⚠️ LOW

**Symptom**: Stack next values (N:[...]) may differ - need to verify after fixing memory/stack issues

**Impact**: Lower priority, but indicates stack reading logic needs verification

**Tasks**:
- [ ] D025 [LOW] Verify stack next values reading in `zaiko/src/vm/execution_trace.zig:540-563` - ensure correct stack slot reading
- [ ] D026 [LOW] Compare stack value reading with C emulator in `maiko/src/xc.c:750-754`

## Priority Order

1. **CRITICAL (Must fix first)**:
   - D001-D004: Memory loading issues
   - D005-D007: FuncObj offset calculation
   - D008-D010: PC progression
   - D011-D013: Stack depth
   - D014-D016: Frame header
   - D017-D019: TOS values
   - D020-D022: Early stop

2. **MEDIUM (Fix after critical)**:
   - D023-D024: Instruction bytes format

3. **LOW (Verify after fixes)**:
   - D025-D026: Stack next values

## Testing Strategy

After each fix:
1. Regenerate execution logs (up to 1000 lines)
2. Compare logs line-by-line
3. Verify specific field matches C emulator
4. Continue until all 1000 lines match exactly

## Success Criteria

- [ ] All 1000 lines of execution log match C emulator exactly
- [ ] PC progression matches C emulator
- [ ] Instruction bytes match C emulator
- [ ] Stack depth matches C emulator
- [ ] Frame header matches C emulator
- [ ] TOS values match C emulator
- [ ] FuncObj offset matches C emulator
- [ ] Execution continues beyond 30 instructions
