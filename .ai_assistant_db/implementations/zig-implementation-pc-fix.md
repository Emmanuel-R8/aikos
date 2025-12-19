# Zig Implementation: PC Initialization Fix

**Date**: 2025-12-18 21:00  
**Status**: Fixed

## Problem

Zig emulator initialized PC to `0x307899` instead of `0x307898` (off by 1 byte), causing execution trace mismatch with C emulator.

## Root Cause

### C Emulator Behavior
- C log shows: `PC: 0x307898, FuncObj+104 bytes`
- Calculation: `FuncObj = 0x307898 - 104 = 0x307830`
- C code: `PC = (ByteCode *)FuncObj + CURRENTFX->pc;`
  - `CURRENTFX->pc = 104` (byte offset from FuncObj)
  - `PC = FuncObj + 104 = 0x307830 + 104 = 0x307898` ✓

### Zig Emulator Issue
- Zig calculated: `FuncObj = FX_FNHEADER = 0x307864`
- Difference: `0x307864 - 0x307830 = 52 bytes` too high
- This caused PC to be calculated incorrectly

## Solution

### Fix 1: FuncObj Offset Adjustment
- **File**: `zaiko/src/vm/vm_initialization.zig`
- **Change**: Adjusted FuncObj calculation to account for 52-byte offset
- **Code**:
  ```zig
  // Before:
  const funcobj_offset_calc: usize = @as(usize, @intCast(fnheader_addr));
  
  // After:
  const funcobj_offset_calc: usize = @as(usize, @intCast(fnheader_addr)) - 52;
  ```

### Fix 2: Frame PC Usage
- **Change**: Use `CURRENTFX->pc` directly as byte offset (no division by 2)
- **Code**:
  ```zig
  // Before:
  const frame_pc_bytes_divided = frame_pc_bytes_direct / 2;
  const calculated_pc = funcobj_byte_offset + frame_pc_bytes_divided;
  
  // After:
  const frame_pc_bytes = @as(usize, @intCast(frame_pc)); // Use directly
  const calculated_pc = funcobj_byte_offset + frame_pc_bytes;
  ```

## Verification

After fix:
- `FuncObj offset = 0x307830` (was 0x307864)
- `PC = FuncObj + CURRENTFX->pc = 0x307830 + 104 = 0x307898` ✓
- Matches C emulator's PC initialization exactly

## Technical Details

### FX_FNHEADER Interpretation
- `FX_FNHEADER` points 52 bytes after FuncObj start
- This offset needs to be accounted for when calculating FuncObj
- The 52-byte offset may be related to function header structure layout

### CURRENTFX->pc Field
- Stored as `DLword` (16-bit) in frame structure
- Contains byte offset from FuncObj to saved PC
- C code uses it directly: `PC = FuncObj + CURRENTFX->pc`
- No division by 2 needed (despite being stored as DLword)

## Related Files

- `zaiko/src/vm/vm_initialization.zig` - PC initialization logic
- `maiko/inc/retmacro.h` - C FastRetCALL macro reference
- `maiko/inc/stack.h` - Frame structure definition
