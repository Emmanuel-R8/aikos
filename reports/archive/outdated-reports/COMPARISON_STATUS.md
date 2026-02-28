# Emulator Execution Comparison Status

## Summary

Attempting to run both C and Zig emulators for 1000 execution steps and compare their logs.

## Changes Made

### 1. Modified Execution Limits

**Zig Emulator** (`zaiko/src/vm/dispatch.zig`):
- Changed `MAX_INSTRUCTIONS` from 1000000 to 1000

**Zig Execution Trace** (`zaiko/src/vm/execution_trace.zig`):
- Added check to stop logging after 1000 steps (line ~127)

**C Emulator** (`maiko/src/xc.c`):
- Added check to stop after 1000 steps (line ~585)

### 2. Compilation Issues

The Zig emulator has compilation errors that need to be fixed:

1. **`translateAddress` function signature changed** - now requires `virtual_memory` as first parameter
2. **Multiple files need updates**:
   - `zaiko/src/vm/opcodes/array_ops.zig` - several calls need fixing
   - `zaiko/src/vm/opcodes/base_ops.zig` - needs fixing
   - `zaiko/src/vm/opcodes/base_ops_read.zig` - needs fixing
   - `zaiko/src/vm/opcodes/base_ops_write.zig` - needs fixing

### 3. C Emulator Issues

The C emulator requires:
- Display backend (X11 or SDL2)
- `ldex` (X11) or `ldesdl` (SDL2) binary
- Current build has SDL2 dependency but SDL2 library not available in environment

## Next Steps

1. **Fix Zig compilation errors**: Update all `translateAddress` calls to include `virtual_memory` as first parameter
2. **Build C emulator with X11 backend**: Use `ldex` instead of `ldesdl`, or set up SDL2 environment
3. **Run both emulators**: Execute with 1000-step limit
4. **Compare logs**: Use `diff` to compare `c_emulator_execution_log.txt` and `zig_emulator_execution_log.txt`

## Script Created

`scripts/compare_emulator_execution.sh` - Script to automate the comparison process (needs compilation fixes first)
