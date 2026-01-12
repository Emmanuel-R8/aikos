# Zig Emulator Enhanced Tracing Testing

**Date**: 2025-01-27  
**Status**: ⚠️ Ready for Testing (Rebuild Required)

## Overview

Enhanced tracing has been implemented in the Zig emulator. The code is complete but requires rebuilding the binary before testing.

## Code Changes Summary

### Fixed IFPAGE Reading Issue

**Problem**: `std.mem.asBytes(&ifpage)` was not returning exactly 512 bytes due to struct padding.

**Solution**: Changed to read directly into a 512-byte buffer, then swap, then cast to struct:

```zig
// BEFORE (problematic):
var ifpage: IFPAGE = undefined;
const bytes_read = file.read(std.mem.asBytes(&ifpage)) catch |err| { ... };
swapIFPAGEBytes(&ifpage);

// AFTER (fixed):
var ifpage_buffer: [512]u8 = undefined;
const bytes_read = file.read(&ifpage_buffer) catch |err| { ... };
endianness_utils.swapIFPAGE(&ifpage_buffer);
const ifpage: *IFPAGE = @ptrCast(&ifpage_buffer);
```

**Location**: `zaiko/src/data/sysout.zig:107-134`

## Testing Instructions

### 1. Rebuild Zig Emulator

```bash
cd zaiko
zig build
```

### 2. Run with Enhanced Tracing

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
timeout 30 zaiko/zig-out/bin/zaiko medley/internal/loadups/starter.sysout 2>&1 | tee zig_emulator_enhanced_trace.txt
```

### 3. Extract Key Values

```bash
# Extract enhanced tracing sections
cat zig_emulator_enhanced_trace.txt | grep -A30 "ENHANCED TRACING\|Before FastRetCALL\|After FastRetCALL\|First Instruction" | head -100

# Extract key values
cat zig_emulator_enhanced_trace.txt | grep -E "FX_FNHEADER|CURRENTFX->pc|FuncObj|Expected.*PC|Actual.*PC|Bytes at.*PC" | head -40
```

### 4. Compare with C Emulator

Compare `zig_emulator_enhanced_trace.txt` with `c_emulator_enhanced_trace.txt`:

**Expected Matches**:
- FX_FNHEADER: `0x307864` DLwords
- CURRENTFX->pc: `104` bytes
- FuncObj calculation: `FX_FNHEADER * 2 = 0x60f0c8`
- PC calculation: `FuncObj + CURRENTFX->pc = 0x60f130`
- File page 5178 → Virtual page 6204
- Bytes at PC after swap: `05 00 0e 00 3f 00 36 00`

## Expected Output Sections

### FPtoVP Table Loading

```
=== ENHANCED TRACING: FPtoVP Table Loading ===
DEBUG FPtoVP: Reading X bytes (Y entries * 4 bytes, BIGVM format)
DEBUG FPtoVP: First 5 entries (BEFORE byte-swap, raw bytes):
  Entry 0: 0xXX 0xXX 0xXX 0xXX
DEBUG FPtoVP: Entry 5178 - BEFORE swap: 0xXXXXXXXX, AFTER swap: 0xXXXXXXXX
  GETFPTOVP = 6204 (0x183c), GETPAGEOK = 0xXXXX
=== END ENHANCED TRACING: FPtoVP Table Loading ===
```

### FastRetCALL Tracing

```
=== ENHANCED TRACING: Before FastRetCALL ===
DEBUG: FX_FNHEADER = 0x307864 (LispPTR, DLword offset)
DEBUG: CURRENTFX->pc = 104 (0x68) bytes
DEBUG: Expected FuncObj byte offset = FX_FNHEADER * 2 = 0x307864 * 2 = 0x60f0c8
DEBUG: Expected PC byte offset = FuncObj + CURRENTFX->pc = 0x60f0c8 + 104 = 0x60f130
DEBUG: Bytes at expected FuncObj (offset 0x60f0c8): 0xXX 0xXX ...
DEBUG: Bytes at expected PC (offset 0x60f130): 0xXX 0xXX ...
=== END Before FastRetCALL ===

=== ENHANCED TRACING: After FastRetCALL Verification ===
DEBUG: Actual FuncObj byte offset = 0x60f0c8
DEBUG: Actual PC byte offset = 0x60f130
DEBUG: Match check: actual_pc_offset == FX_FNHEADER * 2 + CURRENTFX->pc? YES
=== END After FastRetCALL Verification ===
```

### First Instruction Tracing

```
=== ENHANCED TRACING: First Instruction ===
DEBUG: PC byte offset = 0x60f130
DEBUG: FX_FNHEADER = 0x307864 (DLword offset)
DEBUG: CURRENTFX->pc = 104 bytes
DEBUG: FuncObj byte offset = 0x60f0c8
DEBUG: Expected PC = FuncObj + CURRENTFX->pc = 0x60f0c8 + 104 = 0x60f130
DEBUG: Actual PC = 0x60f130
DEBUG: Match check: actual_pc == expected_pc? YES
DEBUG: Bytes at PC (offset 0x60f130): 0xXX 0xXX ...
=== END ENHANCED TRACING: First Instruction ===
```

## Verification Checklist

- [ ] Binary rebuilt successfully
- [ ] Enhanced tracing output captured
- [ ] FX_FNHEADER matches C emulator (`0x307864`)
- [ ] CURRENTFX->pc matches C emulator (`104` bytes)
- [ ] FuncObj calculation matches (`FX_FNHEADER * 2`)
- [ ] PC calculation matches (`FuncObj + CURRENTFX->pc`)
- [ ] File page 5178 maps to virtual page 6204
- [ ] Bytes at PC match C emulator after swap

## Known Issues

1. **IFPAGE Reading**: Fixed - now reads into 512-byte buffer before casting
2. **Binary Rebuild Required**: Code changes require rebuilding before testing

## Next Steps

1. Rebuild Zig emulator: `cd zaiko && zig build`
2. Run with enhanced tracing and capture output
3. Compare with C emulator results
4. Document any discrepancies found

## References

- Implementation: `documentation/implementations/zig-enhanced-tracing-implementation.typ`
- C emulator results: `documentation/implementations/c-emulator-enhanced-tracing-results.typ`
- C emulator verification: `documentation/implementations/c-emulator-complete-verification.typ`
