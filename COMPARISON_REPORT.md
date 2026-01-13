# C vs Zig Emulator Execution Log Comparison

**Date**: 2026-01-12 16:10
**Status**: Initial Comparison - Issues Found

## Summary

Attempted to compare 1000-step execution logs from both emulators. Found significant differences in execution start point and Zig emulator crashes early.

## Log Files

- **C Emulator**: `c_emulator_execution_log.txt` - ✅ 1000 lines (complete)
- **Zig Emulator**: `zig_emulator_execution_log.txt` - ⚠️ 32 lines (incomplete - crashes early)

## Key Differences

### 1. Starting PC Address

- **C Emulator**: PC starts at `0x60f130` (Lisp_world+0x60f130)
- **Zig Emulator**: PC starts at `0x022400` (Lisp_world+0x022400)

**Analysis**: The emulators are starting execution from completely different program counter addresses. This suggests:
- Different initialization code paths
- Different frame setup
- Possibly different sysout loading behavior

### 2. First Instructions

**C Emulator (first 3 instructions)**:
```
    1 PC: 0x60f130 ... POP
    2 PC: 0x60f130 ... POP
    3 PC: 0x60f130 ... POP
```

**Zig Emulator (first 3 instructions)**:
```
    1 PC: 0x022400 ... JUMP0
    2 PC: 0x022402 ... UNBIND
    3 PC: 0x022404 ... ATOMCELL_N
```

**Analysis**: Completely different instruction sequences, indicating different execution paths.

### 3. Frame Information

**C Emulator**:
- Frame: `FX:11890 FH:0x307864 PC:104 NB:11914 FO:+6353096`
- FuncObj offset: `+104 bytes`

**Zig Emulator**:
- Frame: `FX:49408 FH:0xffffff PC:65535 NB:65535 FO:+33554430`
- FuncObj offset: `+65535 bytes` (invalid - indicates uninitialized or wrong frame)

**Analysis**: Zig emulator frame information appears invalid (65535 = 0xFFFF suggests uninitialized values).

### 4. Execution Length

- **C Emulator**: Successfully executes 1000 instructions
- **Zig Emulator**: Crashes after 32 instructions with memory leak error

**Error from Zig emulator**:
```
error(gpa): memory address 0x7fa80458f000 leaked
```

## Issues to Address

1. **Zig Emulator Initialization**: PC and frame setup differ from C emulator
2. **Zig Emulator Crash**: Memory leak after 32 instructions prevents full comparison
3. **Frame Setup**: Zig emulator frame values appear uninitialized (0xFFFF values)

## Next Steps

1. Fix Zig emulator initialization to match C emulator starting PC
2. Fix memory leak causing early crash
3. Verify frame setup matches C emulator
4. Regenerate Zig emulator log with 1000 instructions
5. Perform detailed line-by-line comparison

## Technical Details

### C Emulator Log Format
- 1000 lines of execution trace
- Each line: `[count] PC: [address] [bytes] [opcode] [stack] [frame]`
- Format matches `emulator_debug.log` specification

### Zig Emulator Log Format
- 32 lines (incomplete)
- Same format as C emulator
- Stops early due to crash

## Files

- C log: `c_emulator_execution_log.txt` (1000 lines)
- Zig log: `zig_emulator_execution_log.txt` (32 lines)
- This report: `COMPARISON_REPORT.md`
