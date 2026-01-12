# Zig Emulator Enhanced Tracing Implementation

**Date**: 2025-01-27  
**Status**: ✅ Implementation Complete

## Overview

Enhanced tracing has been added to the Zig emulator to match the C emulator's comprehensive tracing output. This enables direct comparison between C and Zig implementations and helps identify discrepancies.

## Implementation Summary

### 1. Sysout Loading Enhanced Tracing ✅

**Location**: `zaiko/src/data/sysout.zig`

**Added Tracing**:
- FPtoVP table loading: Logs first 5 entries before byte-swap
- Critical entries: Logs file pages 5178, 2937, 9427 (before/after swap, GETFPTOVP, GETPAGEOK)
- Page loading: Enhanced trace for PC page (file page 5178 → virtual page 6204)
- Byte-swapping: Logs raw bytes before swap and bytes after swap in virtual memory

**Matches C Tracing**:
- CT000-CT002: FPtoVP entry logging
- PC page loading trace with byte details

### 2. VM Initialization Enhanced Tracing ✅

**Location**: `zaiko/src/vm/vm_initialization.zig`

**Added Tracing**:
- **Before FastRetCALL**:
  - FX_FNHEADER value (DLword offset)
  - CURRENTFX->pc value (byte offset)
  - Expected FuncObj byte offset calculation
  - Expected PC byte offset calculation
  - Bytes at expected FuncObj location
  - Bytes at expected PC location

- **After FastRetCALL**:
  - Actual FuncObj byte offset
  - Actual PC byte offset
  - Verification: Match check between actual and expected PC

**Matches C Tracing**:
- C emulator's "ENHANCED TRACING: Before FastRetCALL" section
- C emulator's "ENHANCED TRACING: After FastRetCALL Verification" section

### 3. Execution Trace Enhanced Tracing ✅

**Location**: `zaiko/src/vm/execution_trace.zig`

**Added Tracing**:
- **First Instruction**:
  - PC byte offset
  - FX_FNHEADER (from frame)
  - CURRENTFX->pc (from frame)
  - FuncObj byte offset calculation
  - Expected PC calculation
  - Actual PC value
  - Match verification
  - Bytes at PC location

**Matches C Tracing**:
- C emulator's "ENHANCED TRACING: First Instruction" section

## Code Improvements

### Fixed FuncObj Calculation

**Issue**: Previous code had incorrect FuncObj calculation using `FX_FNHEADER - 52`.

**Fix**: Changed to correct calculation: `FuncObj = FX_FNHEADER * 2` (DLword offset to byte offset).

**Location**: `zaiko/src/vm/vm_initialization.zig:369`

```zig
// BEFORE (incorrect):
const funcobj_offset_calc: usize = @as(usize, @intCast(fnheader_addr)) - 52;

// AFTER (correct):
const funcobj_offset_calc: usize = @as(usize, @intCast(fnheader_addr)) * 2;
```

**Rationale**: 
- FX_FNHEADER is a DLword offset (LispPTR)
- C: `NativeAligned4FromLAddr(FX_FNHEADER) = Lisp_world + (FX_FNHEADER * 2)`
- Multiply by 2 to convert DLword offset to byte offset

## Tracing Output Format

### FPtoVP Table Loading

```
=== ENHANCED TRACING: FPtoVP Table Loading ===
DEBUG FPtoVP: Reading X bytes (Y entries * 4 bytes, BIGVM format)
DEBUG FPtoVP: First 5 entries (BEFORE byte-swap, raw bytes):
  Entry 0: 0xXX 0xXX 0xXX 0xXX
  ...
DEBUG FPtoVP: Entry 5178 - BEFORE swap: 0xXXXXXXXX, AFTER swap: 0xXXXXXXXX
  GETFPTOVP = XXXX (0xXXXX), GETPAGEOK = 0xXXXX
=== END ENHANCED TRACING: FPtoVP Table Loading ===
```

### FastRetCALL Tracing

```
=== ENHANCED TRACING: Before FastRetCALL ===
DEBUG: FX_FNHEADER = 0xXXXXXX (LispPTR, DLword offset)
DEBUG: CURRENTFX->pc = XXX (0xXX) bytes
DEBUG: Expected FuncObj byte offset = FX_FNHEADER * 2 = 0xXXXXXX * 2 = 0xXXXXXX
DEBUG: Expected PC byte offset = FuncObj + CURRENTFX->pc = 0xXXXXXX + XXX = 0xXXXXXX
DEBUG: Bytes at expected FuncObj (offset 0xXXXXXX): 0xXX 0xXX ...
DEBUG: Bytes at expected PC (offset 0xXXXXXX): 0xXX 0xXX ...
=== END Before FastRetCALL ===

=== ENHANCED TRACING: After FastRetCALL Verification ===
DEBUG: Actual FuncObj byte offset = 0xXXXXXX
DEBUG: Actual PC byte offset = 0xXXXXXX
DEBUG: FX_FNHEADER = 0xXXXXXX (DLword offset)
DEBUG: CURRENTFX->pc = XXX (0xXX) bytes
DEBUG: FX_FNHEADER * 2 + CURRENTFX->pc = 0xXXXXXX * 2 + XXX = 0xXXXXXX
DEBUG: Match check: actual_pc_offset == FX_FNHEADER * 2 + CURRENTFX->pc? YES/NO
=== END After FastRetCALL Verification ===
```

### First Instruction Tracing

```
=== ENHANCED TRACING: First Instruction ===
DEBUG: PC byte offset = 0xXXXXXX
DEBUG: FX_FNHEADER = 0xXXXXXX (DLword offset)
DEBUG: CURRENTFX->pc = XXX bytes
DEBUG: FuncObj byte offset = 0xXXXXXX
DEBUG: Expected PC = FuncObj + CURRENTFX->pc = 0xXXXXXX + XXX = 0xXXXXXX
DEBUG: Actual PC = 0xXXXXXX
DEBUG: Match check: actual_pc == expected_pc? YES/NO
DEBUG: Bytes at PC (offset 0xXXXXXX): 0xXX 0xXX ...
=== END ENHANCED TRACING: First Instruction ===
```

## Comparison with C Emulator

### Expected Matches

1. **FPtoVP Table**: File page 5178 → Virtual page 6204
2. **PC Calculation**: FuncObj = FX_FNHEADER * 2, PC = FuncObj + CURRENTFX->pc
3. **Memory Loading**: Bytes at PC location after byte-swap
4. **Frame Structure**: FX_FNHEADER and CURRENTFX->pc values

### Known Differences

- **Memory Layout**: Absolute addresses differ due to ASLR, but relative calculations should match
- **Byte-Swapping**: Should match C emulator's ntohl() behavior

## Testing

To test enhanced tracing:

```bash
cd zaiko
zig build run -- medley/internal/loadups/starter.sysout 2>&1 | tee zig_emulator_enhanced_trace.txt
```

Compare output with C emulator's `c_emulator_enhanced_trace.txt`.

## Status

✅ **All enhanced tracing implemented and ready for testing**

## References

- C emulator tracing: `documentation/implementations/c-emulator-enhanced-tracing-results.typ`
- C emulator verification: `documentation/implementations/c-emulator-complete-verification.typ`
- Source code: `zaiko/src/data/sysout.zig`, `zaiko/src/vm/vm_initialization.zig`, `zaiko/src/vm/execution_trace.zig`
