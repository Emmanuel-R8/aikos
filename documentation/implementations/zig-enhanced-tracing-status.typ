# Zig Enhanced Tracing Status

**Date**: 2025-01-27  
**Status**: ⚠️ Code Complete, Binary Rebuild Required

## Current Status

### Code Implementation ✅ COMPLETE

All enhanced tracing code has been implemented:
- ✅ Sysout loading enhanced tracing
- ✅ VM initialization enhanced tracing  
- ✅ Execution trace enhanced tracing
- ✅ IFPAGE reading fix (512-byte buffer)

### Binary Status ❌ STALE

The binary (`zaiko/zig-out/bin/zaiko`) is using old code and needs to be rebuilt.

**Error**: Binary still references removed `swapIFPAGEBytes` function, causing assertion failure.

## Required Action

**Rebuild the Zig emulator**:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/zaiko
zig build
```

Then test:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
timeout 30 zaiko/zig-out/bin/zaiko medley/internal/loadups/starter.sysout 2>&1 | tee zig_emulator_enhanced_trace.txt
```

## Code Changes Summary

### Fixed IFPAGE Reading

**File**: `zaiko/src/data/sysout.zig`

**Change**: Read into 512-byte buffer, swap, then cast to struct:

```zig
// Line 107-134
var ifpage_buffer: [512]u8 = undefined;
const bytes_read = file.read(&ifpage_buffer) catch |err| { ... };
if (bytes_read != 512) { ... }

// Swap buffer
const endianness_utils = @import("../utils/endianness.zig");
endianness_utils.swapIFPAGE(&ifpage_buffer);

// Cast to struct
const ifpage: *IFPAGE = @ptrCast(&ifpage_buffer);
```

**Removed**: `swapIFPAGEBytes()` function (no longer needed)

## Expected Output After Rebuild

Once rebuilt, the enhanced tracing should output:

1. **FPtoVP Table Loading**:
   ```
   === ENHANCED TRACING: FPtoVP Table Loading ===
   DEBUG FPtoVP: Entry 5178 - BEFORE swap: 0xXXXXXXXX, AFTER swap: 0xXXXXXXXX
     GETFPTOVP = 6204 (0x183c), GETPAGEOK = 0xXXXX
   ```

2. **FastRetCALL Tracing**:
   ```
   === ENHANCED TRACING: Before FastRetCALL ===
   DEBUG: FX_FNHEADER = 0x307864 (DLword offset)
   DEBUG: CURRENTFX->pc = 104 bytes
   DEBUG: Expected FuncObj byte offset = 0x60f0c8
   DEBUG: Expected PC byte offset = 0x60f130
   ```

3. **First Instruction**:
   ```
   === ENHANCED TRACING: First Instruction ===
   DEBUG: Actual PC = 0x60f130
   DEBUG: Match check: actual_pc == expected_pc? YES
   ```

## Verification Checklist

After rebuild and run:

- [ ] Binary rebuilds without errors
- [ ] No assertion failures
- [ ] Enhanced tracing output appears
- [ ] FX_FNHEADER = 0x307864
- [ ] CURRENTFX->pc = 104
- [ ] FuncObj = 0x60f0c8
- [ ] PC = 0x60f130
- [ ] All calculations match C emulator

## References

- Implementation: `zig-enhanced-tracing-implementation.typ`
- Testing guide: `zig-enhanced-tracing-testing.typ`
- C emulator results: `c-emulator-enhanced-tracing-results.typ`
