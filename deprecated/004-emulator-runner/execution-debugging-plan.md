# Execution Debugging Plan - Zaiko vs C Emulator

**Date**: 2025-12-22 12:30
**Last Updated**: 2025-12-23 16:43
**Purpose**: Systematic plan to fix execution discrepancies between C and Zig emulators
**Status**: Ready for implementation - Phase 0 (C Tracing) added as prerequisite

## Overview

This plan addresses critical discrepancies found in execution trace comparison. The goal is to achieve 100% functional parity with the C emulator, ensuring Zaiko executes bytecode correctly.

## Critical Path Analysis

### Dependency Graph

```
C Emulator Enhanced Tracing (D000-D006) - PREREQUISITE
    ↓
    └─→ Provides ground truth for all debugging work
         ↓
Memory Loading (D001-D014)
    ↓
    ├─→ PC Progression (D015-D020) - depends on correct memory
    ├─→ Instruction Decoding - depends on correct memory
    └─→ All VM operations - depend on correct memory
         ↓
         ├─→ FuncObj Offset (D021-D023) - depends on frame structure
         ├─→ Frame Header (D024-D026) - depends on frame structure
         ├─→ Stack Depth (D027) - ✅ FIXED
         ├─→ TOS Values (D028-D030) - depends on stack initialization
         └─→ Early Stop (D031-D033) - depends on all above
             ↓
             └─→ Verification (D034-D046) - final validation
```

**Key Insights**: 
1. **C Emulator Enhanced Tracing (Phase 0)** is prerequisite - provides ground truth for all debugging
2. **Memory loading** is the root cause blocking all other fixes. Must be fixed first.
3. Enhanced C tracing will enable systematic comparison and faster debugging.

## Phase 1: Memory Loading (CRITICAL - BLOCKING)

**Goal**: Ensure correct memory content is loaded at PC 0x307898

**Priority**: 🔴 **HIGHEST** - Blocks all other fixes

### Phase 1.1: Verify FPtoVP Table Mapping

**Tasks**: D003
**Time Estimate**: 1-2 hours

**Steps**:
1. **Verify virtual page calculation**
   - PC 0x307898 = virtual page 6204 (0x183c)
   - Offset in page: 0x98 (152 bytes)
   - ✅ Verified: Correct

2. **Verify file page mapping**
   - Current: File page 5178 → Virtual page 6204
   - Check C emulator: What file page does C use for this virtual page?
   - Verify FPtoVP table entry: `FPtoVP[5178] = 6204`
   - Check GETPAGEOK value: Should be 0x0000 (valid)

3. **Compare with C emulator**
   - Run C emulator with debug output for file page loading
   - Identify which file page C loads for virtual page 6204
   - If different, investigate FPtoVP table loading

**Verification**:
```bash
# Check FPtoVP table loading
cd zaiko && zig build run -- ../medley/internal/loadups/starter.sysout 2>&1 | grep "FPtoVP\[5178\]"
# Should show: FPtoVP[5178] = 6204

# Compare with C emulator (if debug available)
# C should load same file page for same virtual page
```

**Success Criteria**:
- [ ] File page 5178 correctly maps to virtual page 6204
- [ ] FPtoVP table matches C emulator
- [ ] GETPAGEOK value is correct (0x0000)

### Phase 1.2: Verify File Page Content

**Tasks**: D001
**Time Estimate**: 1-2 hours

**Steps**:
1. **Read raw file page content**
   - File page 5178 starts at offset: `5178 * 512 = 2,651,136 bytes`
   - Read 512 bytes from sysout file at this offset
   - Compare with C emulator's expected bytes

2. **Check byte order in file**
   - Sysout files are big-endian
   - Raw file bytes should match expected after accounting for byte-swapping
   - C expects: `00 00 60 bf c9 12 0a 02` (after byte-swap)
   - Calculate what file should contain: `bf 60 00 00 02 0a 12 c9` (before swap)

3. **Verify file page reading**
   - Check `file.seekTo()` and `file.read()` operations
   - Ensure correct file offset calculation
   - Verify 512 bytes read successfully

**Verification**:
```python
# Read file page 5178 directly from sysout file
import struct
with open('medley/internal/loadups/starter.sysout', 'rb') as f:
    f.seek(5178 * 512)
    page_data = f.read(512)
    # Check bytes at offset 0x98 (152) in page
    pc_offset = 152
    print(f"File bytes at PC offset: {page_data[pc_offset:pc_offset+8].hex()}")
    # Should match expected before byte-swap
```

**Success Criteria**:
- [ ] File page 5178 contains expected bytes at offset 0x98
- [ ] File reading logic is correct
- [ ] File offset calculation is correct

### Phase 1.3: Verify Byte-Swapping Logic

**Tasks**: D002
**Time Estimate**: 2-3 hours

**Steps**:
1. **Understand C byte-swapping**
   - C uses `word_swap_page()` with `ntohl()` (32-bit longword swap)
   - Parameter: 128 longwords (128 * 4 = 512 bytes)
   - Swaps bytes in each 32-bit longword: `[b0,b1,b2,b3] → [b3,b2,b1,b0]`

2. **Verify Zig byte-swapping**
   - Current: Uses `@byteSwap()` on 32-bit longwords
   - Should match C's `ntohl()` behavior
   - Check: Are we swapping correctly?

3. **Test byte-swapping**
   - Create test case with known input/output
   - Verify: `[0x00, 0x00, 0x60, 0xbf]` → `[0xbf, 0x60, 0x00, 0x00]` after swap
   - Check all 128 longwords in page

4. **Compare with C emulator**
   - Run C emulator with byte-swap debug output
   - Compare swapped bytes byte-by-byte
   - Identify any differences

**Verification**:
```zig
// Test byte-swapping
const test_longword: u32 = 0x0060bf00;
const swapped = @byteSwap(test_longword);
// Should be: 0x00bf6000
std.debug.assert(swapped == 0x00bf6000);
```

**Success Criteria**:
- [ ] Byte-swapping matches C emulator exactly
- [ ] All 128 longwords swapped correctly
- [ ] Memory at PC 0x307898 matches C after swap

### Phase 1.4: Verify Virtual Memory Write

**Tasks**: D004
**Time Estimate**: 1 hour

**Steps**:
1. **Verify virtual address calculation**
   - Virtual page 6204 → Virtual address: `6204 * 512 = 3,176,448 bytes (0x307800)`
   - PC 0x307898 = Virtual address 0x307800 + 0x98
   - ✅ Verified: Correct

2. **Verify memory write**
   - Check `@memcpy()` operation
   - Verify bytes written to correct virtual address
   - Check for buffer overflows or underflows

3. **Verify memory after write**
   - Read back from virtual memory at PC 0x307898
   - Compare with expected bytes
   - Should match C emulator exactly

**Verification**:
```zig
// After loading, verify memory content
const verify_pc: usize = 0x307898;
const expected = [_]u8{ 0x00, 0x00, 0x60, 0xbf, 0xc9, 0x12, 0x0a, 0x02 };
for (0..8) |i| {
    std.debug.assert(virtual_memory[verify_pc + i] == expected[i]);
}
```

**Success Criteria**:
- [ ] Virtual address calculation is correct
- [ ] Memory write succeeds
- [ ] Memory content matches C emulator exactly

### Phase 1.5: Integration Test

**Time Estimate**: 1 hour

**Steps**:
1. **Run full memory loading**
2. **Verify PC 0x307898 content**
3. **Compare with C emulator**
4. **If mismatch persists, investigate alternative causes**:
   - Multiple file pages mapping to same virtual page?
   - Page overwriting?
   - Memory initialization issues?

**Success Criteria**:
- [ ] Memory at PC 0x307898 matches C emulator: `00 00 60 bf c9 12 0a 02`
- [ ] All 8 bytes match exactly
- [ ] Ready to proceed to Phase 2

## Phase 2: PC Progression & Instruction Decoding

**Goal**: Ensure PC advances correctly and instructions decode properly

**Priority**: 🟠 **HIGH** - Depends on Phase 1

### Phase 2.1: Fix PC Progression Logic

**Tasks**: D008-D010
**Time Estimate**: 2-3 hours

**Steps**:
1. **Understand C PC progression**
   - C shows: `0x307898 → 0x307898 → 0x30789b → 0x30789b → 0x30789c`
   - PC can stay same (multi-byte instructions or different execution model)
   - Some instructions don't advance PC immediately

2. **Analyze Zig PC progression**
   - Current: Always advances by instruction length
   - Check `dispatch_loop.zig`: PC update logic
   - Identify why PC always increments

3. **Fix PC update logic**
   - Match C behavior: PC stays same for some instructions
   - Check jump instructions: Do they update PC correctly?
   - Verify instruction length calculation

4. **Test PC progression**
   - Run first 10 instructions
   - Compare PC values with C emulator
   - Ensure PC matches exactly

**Verification**:
```zig
// Log PC after each instruction
std.debug.print("PC: 0x{x}\n", .{vm.pc});
// Compare with C emulator log
```

**Success Criteria**:
- [ ] PC progression matches C emulator exactly
- [ ] PC stays same when C shows it stays same
- [ ] PC advances correctly for all instruction types

### Phase 2.2: Verify Instruction Length Calculation

**Tasks**: D009
**Time Estimate**: 1-2 hours

**Steps**:
1. **Verify opcode lengths**
   - Check `length.zig`: All opcodes have correct length
   - Compare with C emulator opcode lengths
   - Verify multi-byte instructions

2. **Test instruction decoding**
   - Decode first 10 instructions
   - Verify opcode and length match C
   - Check operand reading

**Success Criteria**:
- [ ] All opcode lengths match C emulator
- [ ] Instruction decoding produces correct length
- [ ] PC advances by correct amount

## Phase 3: Frame & Stack Fixes

**Goal**: Fix frame structure reading and stack calculations

**Priority**: 🟡 **MEDIUM** - Depends on Phase 2

### Phase 3.1: Fix FuncObj Offset Calculation

**Tasks**: D005-D007
**Time Estimate**: 2-3 hours

**Steps**:
1. **Understand C FuncObj offset**
   - C: `funcobj_byte_offset = (char *)PCMAC - (char *)FuncObj`
   - But for current frame: `funcobj_byte_offset = CURRENTFX->pc`
   - CURRENTFX->pc is saved PC offset from function header

2. **Fix Zig FuncObj offset**
   - Current: Using `PC - FuncObj` calculation (wrong)
   - Should use: `CURRENTFX->pc` field from frame
   - Read frame PC field correctly

3. **Verify frame PC field reading**
   - Frame layout: PC is at bytes [8-9] (BIGVM)
   - Read as DLword with correct byte order
   - Verify byte offset calculation

**Verification**:
```zig
// Read frame PC field
const frame_bytes = vmem[frame_offset..][0..12];
const frame_pc = std.mem.readInt(DLword, frame_bytes[8..10], .little);
// Should match C's CURRENTFX->pc
```

**Success Criteria**:
- [ ] FuncObj offset matches C emulator: `+104 bytes`
- [ ] Frame PC field read correctly
- [ ] Calculation matches C exactly

### Phase 3.2: Fix Frame Header Reading

**Tasks**: D014-D016
**Time Estimate**: 2-3 hours

**Steps**:
1. **Understand C frame header**
   - C: `FX_FNHEADER = (CURRENTFX->hi2fnheader << 16) | CURRENTFX->lofnheader`
   - Frame layout: lofnheader at bytes [4-5], hi2fnheader at byte 6
   - BIGVM vs non-BIGVM differences

2. **Fix Zig frame header reading**
   - Current: Reading wrong value (0x780030 vs 0x307864)
   - Check byte order (big-endian vs little-endian)
   - Verify field offsets

3. **Compare with C frame structure**
   - Check `maiko/inc/stack.h`: Frame structure definition
   - Verify BIGVM frame layout
   - Ensure byte order matches

**Success Criteria**:
- [ ] Frame header matches C emulator: `0x307864`
- [ ] Frame structure layout matches C
- [ ] Byte order correct

### Phase 3.3: Fix Stack Depth (Already Fixed ✅)

**Tasks**: D011-D013
**Status**: ✅ **COMPLETE**

**Fix Applied**: Updated `zaiko/src/vm/stack.zig` to match C exactly
- Formula: `(diff_bytes / 2) / 2 = diff_bytes / 4`
- Matches C's pointer arithmetic behavior

### Phase 3.4: Fix TOS Values

**Tasks**: D017-D019
**Time Estimate**: 2-3 hours

**Steps**:
1. **Understand C TOS**
   - C: `TOPOFSTACK` is cached value
   - Initialized to 0 (NIL)
   - Updated on push/pop operations

2. **Fix Zig TOS reading**
   - Current: Shows all zeros
   - Check stack initialization
   - Verify TOS calculation

3. **Verify stack initialization**
   - Check `init.zig`: Stack initialization
   - Verify initial TOS value
   - Check stack pointer setup

**Success Criteria**:
- [ ] TOS values match C emulator
- [ ] Stack initialization correct
- [ ] TOS updates correctly on push/pop

## Phase 4: Early Stop Investigation

**Goal**: Fix execution stopping prematurely

**Priority**: 🟡 **MEDIUM** - Depends on Phases 1-3

### Phase 4.1: Investigate Early Stop

**Tasks**: D020-D022
**Time Estimate**: 2-3 hours

**Steps**:
1. **Check stop conditions**
   - Verify: `PC >= 0xf000d5` stop condition
   - Check for crashes or errors
   - Add error logging

2. **Add error handling**
   - Log all errors before exit
   - Check for unhandled exceptions
   - Verify error propagation

3. **Debug execution flow**
   - Add detailed logging
   - Track execution path
   - Identify where execution stops

**Success Criteria**:
- [ ] Execution continues beyond 30 instructions
- [ ] No crashes or errors
- [ ] Execution matches C emulator length

## Phase 5: Verification & Testing

**Goal**: Comprehensive verification of all fixes

**Priority**: 🟢 **LOW** - Final validation

### Phase 5.1: Regenerate Execution Logs

**Time Estimate**: 30 minutes

**Steps**:
1. Generate C emulator log (1000 lines)
2. Generate Zig emulator log (1000 lines)
3. Compare line-by-line

### Phase 5.2: Field-by-Field Comparison

**Time Estimate**: 1-2 hours

**Steps**:
1. Compare PC values
2. Compare instruction bytes
3. Compare opcodes
4. Compare stack depth
5. Compare frame header
6. Compare TOS values
7. Compare FuncObj offset

### Phase 5.3: Success Validation

**Success Criteria**:
- [ ] 1000/1000 lines match exactly
- [ ] All fields match C emulator
- [ ] Execution continues to completion
- [ ] No discrepancies found

## Implementation Strategy

### Approach

1. **Fix sequentially by phase** - Don't skip phases
2. **Verify after each fix** - Don't accumulate errors
3. **Test incrementally** - Run after each change
4. **Compare frequently** - Use C emulator as reference

### Testing Protocol

After each fix:
1. Compile Zig emulator
2. Generate execution log (1000 lines)
3. Compare with C emulator log
4. Verify specific field matches
5. Document results

### Rollback Plan

If a fix breaks something:
1. Revert the change
2. Document what broke
3. Investigate root cause
4. Try alternative approach

## Time Estimates

- **Phase 1**: 6-9 hours (Memory Loading)
- **Phase 2**: 3-5 hours (PC Progression)
- **Phase 3**: 6-9 hours (Frame & Stack)
- **Phase 4**: 2-3 hours (Early Stop)
- **Phase 5**: 2-3 hours (Verification)

**Total**: 19-29 hours

## Risk Assessment

### High Risk Areas

1. **Memory Loading** - Root cause, complex dependencies
2. **Byte-Swapping** - Platform-specific, easy to get wrong
3. **Frame Structure** - BIGVM vs non-BIGVM differences

### Mitigation Strategies

1. **Incremental fixes** - Fix one thing at a time
2. **Frequent testing** - Test after each change
3. **C emulator reference** - Always compare with C
4. **Documentation** - Document all findings

## Success Metrics

### Phase 1 Success
- [ ] Memory at PC 0x307898 matches C: `00 00 60 bf c9 12 0a 02`
- [ ] All file pages load correctly
- [ ] Byte-swapping verified

### Phase 2 Success
- [ ] PC progression matches C exactly
- [ ] Instruction decoding correct
- [ ] PC advances correctly

### Phase 3 Success
- [ ] FuncObj offset matches C: `+104 bytes`
- [ ] Frame header matches C: `0x307864`
- [ ] Stack depth matches C: `5956`
- [ ] TOS values match C

### Phase 4 Success
- [ ] Execution continues beyond 30 instructions
- [ ] No crashes or errors
- [ ] Execution length matches C

### Final Success
- [ ] 1000/1000 lines match exactly
- [ ] All fields match C emulator
- [ ] Zaiko executes correctly

## Next Steps

1. **IMMEDIATE - Phase 0**: Implement C emulator enhanced tracing (D000-D006)
   - Provides ground truth for all debugging work
   - Enables systematic comparison with Zig emulator
   - See `c-emulator-enhanced-tracing-proposal.typ` and `c-emulator-tracing-implementation-guide.typ`
2. **After Phase 0**: Run C emulator with enhanced tracing
3. **After Phase 0**: Compare C traces with Zig emulator traces
4. **Then Phase 1**: Use C traces to systematically fix memory loading
5. **Continue sequentially**: Follow plan through all phases

---

## Phase 0: C Emulator Enhanced Tracing (NEW)

**Status**: Added 2025-12-23 16:43

**Purpose**: Add comprehensive tracing to C emulator to provide ground truth for debugging

**Tasks**: D000-D006 (see `execution-debugging-tasks.md`)

**Deliverables**:
- `c_emulator_memory_loading_trace.txt` - Complete memory loading trace
- Enhanced `c_emulator_execution_log.txt` - With file page, virtual address, byte-swap status
- `c_emulator_address_trace.txt` - Step-by-step address calculations

**Benefits**:
- Provides reference for all address calculations
- Enables systematic comparison with Zig emulator
- Applies critical debugging technique at each step
- Identifies exact point where memory loading diverges

**Documentation**:
- Proposal: `documentation/implementations/c-emulator-enhanced-tracing-proposal.typ`
- Implementation Guide: `documentation/implementations/c-emulator-tracing-implementation-guide.typ`

---

**Last Updated**: 2025-12-23 16:43
**Status**: Ready for implementation - Phase 0 (C Tracing) added as prerequisite
