# Execution Trace Debugging - Summary & Findings

**Date**: 2025-12-22 12:15
**Status**: Critical discrepancies found - Zaiko does NOT execute correctly

## Executive Summary

**CRITICAL**: Execution trace comparison reveals Zaiko emulator is NOT producing correct execution. 0/30 lines match C emulator exactly. Multiple fundamental issues identified.

## Comparison Results

- **C Emulator Log**: 1000 lines generated
- **Zig Emulator Log**: 30 lines generated (stops early)
- **Exact Matches**: 0/30 lines (0%)
- **Status**: ❌ **FAILED** - Zaiko execution does not match C emulator

## Critical Issues Found

### 1. Memory Loading Failure ⚠️ CRITICAL

**Evidence**:
- At PC 0x307898, C emulator reads: `00 00 60 bf c9 12 0a 02`
- At PC 0x307898, Zig emulator reads: `05 00 0e 00 3f 00 36 00`
- **Completely different content** - not a byte-order issue

**Root Cause**: Either:
- Wrong file page being loaded (file page 5178 loaded, but should be different?)
- FPtoVP table mapping incorrect
- Byte-swapping applied incorrectly
- Virtual page calculation wrong

**Impact**: All instruction execution is wrong because memory content is wrong

### 2. FuncObj Offset Calculation Wrong ⚠️ CRITICAL

**Evidence**:
- C shows: `FuncObj+  104 bytes` (correct)
- Zig shows: `FuncObj++4687768 bytes` (wrong - off by factor of ~45,000)

**Root Cause**: FuncObj offset calculation uses wrong formula
- Should use: `CURRENTFX->pc` field (saved PC offset from function header)
- Currently using: `PC - FuncObj` calculation which is wrong

**Impact**: Frame handling, function lookups, and return address calculations are wrong

### 3. PC Progression Wrong ⚠️ CRITICAL

**Evidence**:
- C PC: `0x307898 → 0x307898 → 0x30789b → 0x30789b → 0x30789c...`
- Zig PC: `0x307898 → 0x30789a → 0x30789c → 0x30789e → 0x3078a0...`

**Root Cause**: PC always advances by instruction length, but C shows PC can stay same (multi-byte instructions or different execution model)

**Impact**: Wrong instructions executed, execution diverges immediately

### 4. Stack Depth Calculation Wrong ⚠️ CRITICAL

**Evidence**:
- C shows: `Stack: D:5956`
- Zig shows: `Stack: D:11912` (exactly 2x)

**Root Cause**: Stack depth calculation formula incorrect
- C: `(CurrentStackPTR - Stackspace) / 2` where both are DLword* pointers
- Zig: Was dividing by 4, but should match C's pointer arithmetic

**Status**: ✅ **FIXED** - Updated to match C exactly

**Impact**: Stack operations use wrong depth, breaking stack manipulation

### 5. Frame Header (FH) Value Wrong ⚠️ CRITICAL

**Evidence**:
- C shows: `FH:0x307864`
- Zig shows: `FH:0x780030`

**Root Cause**: Frame header reading from frame structure is incorrect
- BIGVM frame layout may be wrong
- Byte order or field offset may be wrong

**Impact**: Function lookups fail, function calls/returns broken

### 6. Top of Stack (TOS) Values Wrong ⚠️ CRITICAL

**Evidence**:
- C shows: `TOS:0x000000000000000e` (actual values)
- Zig shows: `TOS:0x0000000000000000` (all zeros)

**Root Cause**: TOS reading or stack initialization incorrect
- Stack may not be initialized correctly
- TOS calculation may be wrong

**Impact**: All stack-based operations fail (arithmetic, data ops, etc.)

### 7. Execution Stops Early ⚠️ CRITICAL

**Evidence**:
- C emulator: 1000+ lines of execution
- Zig emulator: Only 30 lines

**Root Cause**: Emulator crashes, errors, or stop condition triggered incorrectly

**Impact**: Cannot run full Medley session

## Fixes Applied

1. ✅ **D011**: Fixed stack depth calculation in `zaiko/src/vm/stack.zig`
2. ✅ **D023**: Fixed execution trace byte reading to match C log format (no XOR for display)

## Remaining Critical Tasks

1. **D001-D004**: Memory loading issues - **HIGHEST PRIORITY**
2. **D005-D007**: FuncObj offset calculation
3. **D008-D010**: PC progression logic
4. **D014-D016**: Frame header reading
5. **D017-D019**: TOS values
6. **D020-D022**: Early stop investigation

## Next Steps

1. **IMMEDIATE**: Investigate memory loading - verify which file page should map to virtual page 6204
2. **IMMEDIATE**: Check FPtoVP table - verify file page 5178 is correct mapping
3. **IMMEDIATE**: Verify byte-swapping - ensure 32-bit longword swapping matches C exactly
4. Fix FuncObj offset calculation
5. Fix PC progression logic
6. Fix frame header reading
7. Fix TOS initialization/reading
8. Investigate early stop

## Success Criteria

- [ ] Memory at PC 0x307898 matches C emulator exactly
- [ ] All 1000 lines of execution log match C emulator
- [ ] PC progression matches C emulator
- [ ] Stack depth matches C emulator
- [ ] Frame header matches C emulator
- [ ] TOS values match C emulator
- [ ] FuncObj offset matches C emulator
- [ ] Execution continues beyond 30 instructions
