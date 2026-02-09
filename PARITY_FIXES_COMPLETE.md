# Parity Fixes Complete - Verification Report

**Date**: 2026-02-04
**Status**: ✅ Both Critical Fixes Complete and Verified

## Fix 1: Line 0 TOS ✅ COMPLETE

### Problem

- C trace: `TOS:0x00000000` at line 0
- Zig trace: `TOS:0x0000000e` at line 0 (incorrect)

### Root Cause

Zig called `readTopOfStackFromMemory(vm)` before the first instruction, overwriting the initial `vm.top_of_stack = 0` with stack memory value `0x0e`. C never reads TOS from memory before the first instruction.

### Solution Implemented

- Modified `zaiko/src/vm/dispatch/dispatch_loop.zig` line 28
- Skip `readTopOfStackFromMemory` for first instruction (`instruction_count >= 2`)
- Pass `instruction_count` from dispatch loop to `executeInstructionInLoop`

### Verification

```bash
$ head -1 c_emulator_execution_log.txt zig_emulator_execution_log.txt | awk -F'|' '{print "TOS:", $9}'
TOS: TOS:0x00000000 N1:0x00000000 N2:0x00000000
TOS: TOS:0x00000000,N1:0x00000000,N2:0x00000000
```

✅ **Both traces show TOS:0x00000000 at line 0**

## Fix 2: Early Exit on Top-Level RETURN ✅ COMPLETE

### Problem

- C trace: 14 lines for `EMULATOR_MAX_STEPS=15`
- Zig trace: ~4-8 lines (stopped early)

### Root Cause

Zig's `returnFromFunction` treated `alink == 0` as "no previous frame" and set `vm.stop_requested = true`. C handles alink 0 by computing a return frame at stack offset 0 and continuing execution.

### Solution Implemented

- Modified `zaiko/src/vm/function.zig` lines 74-98
- When `previous_frame_addr == 0` (alink 0):
  - Compute return frame at `stack_base - FRAMESIZE` (offset 0)
  - Read PC and other state from that frame
  - Set `vm.pc` and `vm.current_frame`
  - **Do NOT** set `stop_requested`
  - Continue execution

### Verification

```bash
$ EMULATOR_MAX_STEPS=15 ./scripts/compare_emulator_execution.sh medley/internal/loadups/starter.sysout
C emulator log: 14 lines
Zig emulator log: 14 lines
```

✅ **Both traces have 14 lines for EMULATOR_MAX_STEPS=15**

## Opcode Name Alignment ✅ COMPLETE

### Problem

Opcode names differed between C and Zig traces (e.g., C "RECLAIMCELL" vs Zig "POP" for 0xbf).

### Solution Implemented

- Added `getOpcodeNameForTrace()` function in `zaiko/src/vm/execution_trace.zig`
- Maps opcode byte values to C trace opcode names (matching xc.c:954-1123)
- Ensures byte-for-byte trace identity for opcode names

### Verification

```bash
$ head -3 c_emulator_execution_log.txt zig_emulator_execution_log.txt | awk -F'|' '{print "Line", $1, "Opcode:", $4, "Instr:", $3}'
Line      0 Opcode: 0xbf Instr: RECLAIMCELL
Line      0 Opcode: 0xbf Instr: RECLAIMCELL
Line      1 Opcode: 0x60 Instr: UNKNOWN
Line      1 Opcode: 0x60 Instr: UNKNOWN
Line      2 Opcode: 0x12 Instr: FN2
Line      2 Opcode: 0x12 Instr: FN2
```

✅ **Opcode names match C trace format**

## Current Status

### ✅ Complete

1. Line 0 TOS matches (0x00000000)
2. Both emulators run to same step count (14 lines for EMULATOR_MAX_STEPS=15)
3. Opcode names match C trace format

### ⏳ Remaining (Expected)

1. **Format differences** (spaces vs commas) - Will resolve when C emulator is rebuilt with new trace format
2. **Some execution value differences** (e.g., line 2 TOS: C=0x00140000 vs Zig=0xa0000374) - Separate issues for future iterations

## Next Steps

1. **Rebuild C Emulator** (required for format parity):

   ```bash
   cd maiko
   ./medley/scripts/build/build-c-emulator.sh --display-backend sdl --build-system cmake --force
   ```

2. **Continue Iterative Parity Workflow**:
   - Run comparison with rebuilt C emulator
   - Identify first execution value divergence (ignoring format)
   - Fix divergence in Zig
   - Re-run from step 0
   - Repeat until 100% parity achieved

## Files Modified

- `zaiko/src/vm/dispatch/dispatch_loop.zig` - Skip TOS sync for first instruction
- `zaiko/src/vm/dispatch.zig` - Pass instruction_count to executeInstructionInLoop
- `zaiko/src/vm/function.zig` - Handle alink 0 like C (no early exit)
- `zaiko/src/vm/execution_trace.zig` - Opcode name mapping to match C

## Verification Commands

```bash
# Verify Fix 1 (Line 0 TOS)
head -1 c_emulator_execution_log.txt zig_emulator_execution_log.txt | awk -F'|' '{print "TOS:", $9}'

# Verify Fix 2 (Line count)
wc -l c_emulator_execution_log.txt zig_emulator_execution_log.txt

# Run comparison
EMULATOR_MAX_STEPS=15 ./scripts/compare_emulator_execution.sh medley/internal/loadups/starter.sysout

# Detailed comparison
python3 scripts/compare_unified_traces.py c_emulator_execution_log.txt zig_emulator_execution_log.txt --max-lines 15
```
