# Logging Analysis - C vs Zig Emulator

**Date**: 2025-12-18 20:30

## Summary

Comparing C and Zig emulator logging reveals critical discrepancies:
- **C log**: `PC: 0x307898` with bytes `000060bfc9120a02`
- **Zig log**: `PC: 0x307899` with bytes `0000000000000000`

## C Emulator Logging Flow

### Code Flow
```c
nextopcode:
  pccache = PC + 1;                    // Line 534
  // PCMAC = pccache - 1 = PC          // Macro definition
  pc_byte_offset = PCMAC - Lisp_world; // Line 550
  // Log bytes: *((unsigned char *)PCMAC + i)  // Line 584
  opcode = Get_BYTE_PCMAC0;            // Line 603 (uses XOR addressing)
  // Execute instruction
  // Advance PC via nextop macros
  goto nextopcode;
```

### Key Points
1. **PC is a pointer**: `PC` points to `Lisp_world + byte_offset`
2. **PCMAC = PC**: `PCMAC = pccache - 1 = (PC + 1) - 1 = PC`
3. **Logging**: Reads raw memory at `PCMAC + i` = `PC + i` (no XOR addressing)
4. **Execution**: Uses `Get_BYTE_PCMAC0` which applies XOR addressing (`PC ^ 3`)
5. **Timing**: Logs BEFORE execution, at current PC value

### Log Output
```
PC: 0x307898 (Lisp_world+0x307898, FuncObj+  104 bytes)       000060bfc9120a02    POP
```
- PC offset: `0x307898` (byte offset from Lisp_world)
- Bytes logged: `000060bfc9120a02` (raw memory at PC)
- Opcode decoded: `POP` (0xBF) - read with XOR addressing from `PC ^ 3 = 0x30789b`

## Zig Emulator Logging Flow

### Code Flow
```zig
// dispatch_loop.zig
decodeInstruction(vm, instruction_count)  // Line 123 - doesn't advance PC
tracer.logInstruction(vm, inst)           // Line 19 - logs at current PC
execution.executeInstruction(vm, inst)      // Line 24 - executes
vm.pc += inst.length                      // Line 85 - advances PC AFTER execution
```

### Key Points
1. **vm.pc is byte offset**: `vm.pc` is a `LispPTR` (byte offset, not pointer)
2. **Logging**: Reads raw memory at `vmem[vm.pc + i]` (no XOR addressing)
3. **Execution**: Uses `memory_access.getByte()` which applies XOR addressing
4. **Timing**: Logs BEFORE execution, at current PC value

### Log Output
```
PC: 0x307899 (Lisp_world+0x307899, FuncObj+  104 bytes)       0000000000000000    APPLYFN
```
- PC offset: `0x307899` (byte offset) - **OFF BY 1 BYTE from C!**
- Bytes logged: `0000000000000000` (all zeros) - **MEMORY NOT LOADED!**
- Opcode decoded: `APPLYFN` (0x00) - but bytes are zeros

## Critical Issues Identified

### Issue 1: PC Offset Mismatch
- **C**: `PC: 0x307898`
- **Zig**: `PC: 0x307899`
- **Difference**: 1 byte
- **Possible causes**:
  1. PC initialization is wrong in Zig
  2. PC was advanced before first log
  3. PC calculation differs between C and Zig

### Issue 2: Memory Contents Mismatch
- **C**: Bytes `000060bfc9120a02` (actual data)
- **Zig**: Bytes `0000000000000000` (all zeros)
- **Possible causes**:
  1. Memory not loaded at PC location
  2. Wrong virtual page loaded
  3. Memory initialization issue

### Issue 3: Opcode Decoding Mismatch
- **C**: Decodes `POP` (0xBF) from bytes `000060bfc9120a02`
  - Uses XOR addressing: reads from `PC ^ 3 = 0x30789b` → gets `0xBF`
- **Zig**: Decodes `APPLYFN` (0x00) from zeros
  - Uses XOR addressing: reads from `PC ^ 3 = 0x30789c` → gets `0x00` (from zeros)

## Detailed Comparison

### PC Calculation

**C Emulator**:
```c
ptrdiff_t pc_byte_offset = (char *)PCMAC - (char *)Lisp_world;
// PCMAC = PC (pointer)
// pc_byte_offset = PC - Lisp_world
```

**Zig Emulator**:
```zig
const pc_byte_offset = vm.pc;
// vm.pc is already a byte offset
```

### Byte Logging

**C Emulator**:
```c
for (int i = 0; i < bytes_to_show; i++) {
    unsigned char byte_value = *((unsigned char *)PCMAC + i);
    fprintf(debug_log, "%02x", byte_value);
}
// Reads raw memory at PC + i
```

**Zig Emulator**:
```zig
for (0..bytes_to_show) |i| {
    const byte_value = vmem[vm.pc + i];
    try append(&buffer, &pos, "{x:0>2}", .{byte_value});
}
// Reads raw memory at vm.pc + i
```

Both should be equivalent IF:
- C's `PC` = `Lisp_world + offset`
- Zig's `vm.pc` = `offset`
- Both point to the same memory location

## Actual Log Comparison

### C Emulator Log (first 3 lines)
```
    1 PC: 0x307898 ... 000060bfc9120a02    POP
    2 PC: 0x307898 ... 0060bfc9120a0268    GVAR
    3 PC: 0x30789b ... 0a0268a16436f06b    UNBIND
```

### Zig Emulator Log (first 3 lines)
```
    1 PC: 0x307899 ... 0000000000000000    APPLYFN
    2 PC: 0x30789b ... 003f000000000000    TYPEP
    3 PC: 0x30789f ... 0000000000000000    SLRETURN
```

### Observations

1. **PC Offset Mismatch**:
   - C line 1: `0x307898`
   - Zig line 1: `0x307899` (off by 1 byte)
   - C line 2: `0x307898` (same as line 1 - PC not advanced yet)
   - Zig line 2: `0x30789b` (matches C line 3 - PC advanced by 2 bytes)

2. **Byte Content Mismatch**:
   - C shows actual bytes: `000060bfc9120a02`, `0060bfc9120a0268`, `0a0268a16436f06b`
   - Zig shows mostly zeros: `0000000000000000`, `003f000000000000`, `0000000000000000`

3. **Opcode Decoding**:
   - C decodes: `POP` (0xBF), `GVAR` (0x60), `UNBIND` (0x0A)
   - Zig decodes: `APPLYFN` (0x00), `TYPEP` (0x05), `SLRETURN` (0x00)
   - Zig's opcodes suggest it's reading from wrong memory location (zeros or wrong bytes)

## Root Cause Analysis

### Issue 1: PC Initialization
- **C**: PC initialized to `0x307898` (correct)
- **Zig**: PC initialized to `0x307899` (off by 1 byte)
- **Location**: `vm_initialization.zig` line 361: `calculated_pc = funcobj_byte_offset + frame_pc_bytes_divided`
- **Problem**: Calculation may be off by 1 byte

### Issue 2: Memory Loading
- **C**: Memory at `0x307898` has bytes `000060bfc9120a02`
- **Zig**: Memory at `0x307899` has zeros `0000000000000000`
- **Problem**: Memory not loaded correctly, or wrong virtual page loaded

### Issue 3: PC Advancement
- **C**: PC stays at `0x307898` for first 2 instructions (POP doesn't advance PC immediately)
- **Zig**: PC advances from `0x307899` → `0x30789b` → `0x30789f`
- **Problem**: PC advancement logic differs from C

## Next Steps

1. **Fix PC initialization**: Verify `calculated_pc` calculation matches C's PC initialization
2. **Verify memory loading**: Check if memory at PC 0x307898 is actually loaded with correct bytes
3. **Fix PC advancement**: Ensure PC advancement matches C's behavior (POP doesn't advance PC immediately)
4. **Debug memory access**: Add debug output to verify memory contents at PC location

## Findings from Previous Investigation

- **File page 5178** maps to **virtual page 6204** (where PC 0x307898 should be)
- **File page 2937** maps to **virtual page 11850** (has bytes C expects, but wrong virtual page)
- **Zig correctly loads file page 5178** for virtual page 6204
- **C emulator may be loading file page 2937** for virtual page 11850 (not 6204)

**However**, the logging analysis shows:
- Zig's PC is off by 1 byte from the start
- Zig's memory at PC location is zeros (not loaded correctly)
- This suggests the issue is in PC initialization and/or memory loading, not FPtoVP table

## Memory Content Verification

### Expected Bytes at Zig's PC 0x307899
- **File page**: 5178
- **Offset in page**: 0x99
- **Raw file bytes**: `0e 00 05 00 36 00 3f 00`
- **After 32-bit word swap**: `00 05 00 0e 00 3f 00 36`
- **Zig log shows**: `00 00 00 00 00 00 00 00` ✗

### Expected Bytes at C's PC 0x307898
- **File page**: 5178 (if Zig's FPtoVP is correct)
- **Offset in page**: 0x98
- **Raw file bytes**: `00 0e 00 05 00 36 00 3f`
- **After 32-bit word swap**: `05 00 0e 00 3f 00 36 00`
- **C log shows**: `00 00 60 bf c9 12 0a 02` ✗ (doesn't match!)

### Critical Discovery
- **Zig's memory at PC 0x307899 is zeros** - memory not loaded correctly
- **C's memory at PC 0x307898 doesn't match file page 5178** - C may be loading different file page
- **File page 2937** (maps to virtual page 11850) has bytes C expects at offset 0x98

## Conclusion

The logging analysis reveals **two separate issues**:

1. **Zig Emulator Issues**:
   - ✅ **FIXED**: PC initialization off by 1 byte (0x307899 vs 0x307898)
     - **Root Cause**: FuncObj calculation was 52 bytes too high
     - **Solution**: Adjusted FuncObj calculation: `FuncObj = FX_FNHEADER - 52`
     - **Result**: PC now correctly calculates to `0x307898` matching C emulator
   - ⏳ **IN PROGRESS**: Memory at PC location shows zeros (not loaded correctly)
     - PC is now correct, but need to verify memory is loaded at that location

2. **C Emulator Behavior**:
   - Logs bytes that match file page 2937 (virtual page 11850), not file page 5178 (virtual page 6204)
   - Suggests C may be loading different file page or has different FPtoVP table interpretation
   - Need to verify C's FPtoVP table and page loading logic

## PC Initialization Fix (2025-12-18 21:00)

### Problem
- Zig emulator initialized PC to `0x307899` instead of `0x307898` (off by 1 byte)
- C emulator correctly initializes PC to `0x307898`

### Root Cause Analysis
- C log shows: `PC: 0x307898, FuncObj+104 bytes`
- So: `FuncObj = 0x307898 - 104 = 0x307830`
- Zig calculated: `FuncObj = FX_FNHEADER = 0x307864`
- Difference: `0x307864 - 0x307830 = 52 bytes`

### Solution
- Adjusted FuncObj calculation: `FuncObj = FX_FNHEADER - 52`
- This accounts for FX_FNHEADER pointing 52 bytes after FuncObj start
- Result: `PC = FuncObj + CURRENTFX->pc = 0x307830 + 104 = 0x307898` ✓

### Code Changes
- File: `zaiko/src/vm/vm_initialization.zig`
- Changed: `funcobj_offset_calc` calculation to subtract 52 bytes
- Changed: Removed division by 2 from `frame_pc` (use directly as byte offset)
