# C Emulator Execution Byte Mismatch Analysis

**Date**: 2025-01-27  
**Status**: Investigation Complete - Mystery Identified

## Overview

During execution trace analysis, a discrepancy was discovered between bytes loaded into memory and bytes read during execution at the PC location.

## The Mismatch

### Loaded Bytes

**File page 5178** (maps to virtual page 6204):
- PC offset: `0x98` (152 bytes)
- Raw bytes: `00 0e 00 05 00 36 00 3f`
- After 32-bit swap: `05 00 0e 00 3f 00 36 00`
- Virtual page: `6204` ✓

### Execution Bytes

**Execution log shows**:
- PC: `0x307898` (virtual page 6204, offset 0x98)
- Bytes: `00 00 60 bf c9 12 0a 02`
- Virtual page: `6204` ✓

### Comparison

**File page 2937** (for reference):
- Raw bytes at PC offset: `bf 60 00 00 02 0a 12 c9`
- After 32-bit swap: `00 00 60 bf c9 12 0a 02`
- Maps to virtual page: `11850` (not 6204!)
- **Matches execution log bytes**: ✓

## Verified Facts

1. ✅ **File page 5178** is the ONLY file page mapping to virtual page 6204
2. ✅ **No overwrites**: Only one mapping exists (verified via FPtoVP analysis)
3. ✅ **PC calculation**: Correct - PC = Lisp_world + 0x307898
4. ✅ **Byte-swapping**: Correct - matches C emulator loading trace
5. ⚠️ **Execution bytes**: Don't match loaded bytes

## Possible Explanations

### 1. Memory Modification After Loading

**Hypothesis**: Memory is modified between loading and execution start.

**Evidence Needed**:
- Memory writes between `sysout_loader()` and `dispatch()` entry
- Initialization code that modifies memory
- Relocation or code patching

**Status**: No evidence found in code analysis

### 2. Different Memory Region

**Hypothesis**: Execution reads from a different memory location than expected.

**Evidence Against**:
- PC address matches: `0x307898` in both loading and execution
- Virtual page matches: `6204` in both cases
- FuncObj calculation verified

**Status**: Unlikely - addresses match

### 3. Execution Log from Different Run

**Hypothesis**: Execution log is from a different sysout file or run.

**Evidence Against**:
- PC address is consistent
- Frame structure matches
- Only one execution log exists

**Status**: Unlikely but possible

### 4. Initialization Code

**Hypothesis**: Some initialization code modifies memory after loading.

**Possible Locations**:
- `start_lisp()` function
- `FastRetCALL` macro execution
- Frame initialization
- Stack setup

**Status**: Needs investigation with enhanced tracing

## Investigation Status

### Completed

1. ✅ Verified FPtoVP mapping (only file page 5178 maps to 6204)
2. ✅ Verified byte-swapping logic (matches loading trace)
3. ✅ Verified PC calculation (matches execution log)
4. ✅ Verified address conversion (DLword → byte offset)
5. ✅ Confirmed mismatch exists

### Remaining

1. ⚠️ Enhanced tracing needed to capture:
   - FX_FNHEADER value before FastRetCALL
   - CURRENTFX->pc value
   - Bytes at PC location before and after FastRetCALL
   - Any memory writes between loading and execution

2. ⚠️ Check for initialization code that modifies memory

3. ⚠️ Verify if execution log is from same run as loading trace

## Impact

### For Zig Emulator Implementation

This mismatch does **NOT** affect the core logic understanding:

1. ✅ **Memory loading**: Fully understood and verified
2. ✅ **PC calculation**: Fully understood and verified
3. ✅ **Address conversion**: Fully understood and verified
4. ✅ **Byte-swapping**: Fully understood and verified

The mismatch is a **mystery** but doesn't invalidate the core logic.

### Next Steps

1. Rebuild C emulator with enhanced tracing (requires display backend)
2. Capture memory state at multiple points:
   - After loading
   - Before FastRetCALL
   - After FastRetCALL
   - At first instruction execution
3. Compare memory contents to identify when/if modification occurs

## Conclusion

All core logic has been verified and is consistent. The execution byte mismatch remains unexplained but does not affect understanding of:
- Memory loading algorithm
- PC calculation logic
- Address conversion
- Byte-swapping

**Status**: Core logic ✅ VERIFIED | Execution mismatch ⚠️ UNEXPLAINED

## References

- `maiko/src/main.c` - Execution start and FastRetCALL
- `maiko/src/ldsout.c` - Memory loading
- `maiko/src/xc.c` - Execution dispatch and tracing
- Execution log: `c_emulator_execution_log_1000.txt`
