# Address Calculation and Byte-Ordering Investigation

**Date**: 2025-12-18 19:48  
**Purpose**: Systematic investigation of byte mismatch at PC=0x307898  
**Status**: IN PROGRESS - XOR Addressing Implemented, Raw Memory Mismatch Under Investigation

## Problem Statement

**Critical Mismatch**: At PC `0x307898`, the Zig and C emulators read completely different bytes:

- **C Emulator**: Reads bytes `000060bfc9120a02` → decodes `POP` (opcode `0xBF`)
- **Zig Emulator**: Reads bytes `0500000000000000` → decodes `TYPEP` (opcode `0x05`)

This indicates a fundamental issue with:
1. Virtual memory population (sysout loading)
2. Address translation (FPtoVP mapping)
3. Byte-ordering/byte-swapping during page load

## Investigation Plan

### Phase 1: Verify Memory Loading ✅ IN PROGRESS
- [ ] Check if PC 0x307898 is in a mapped virtual page
- [ ] Verify FPtoVP table entry for the file page containing PC
- [ ] Check byte-swapping logic during page load
- [ ] Compare raw bytes from sysout file vs loaded virtual memory

### Phase 2: Verify Address Translation ✅ PENDING
- [ ] Verify PC calculation: `PC = FuncObj + (CURRENTFX->pc / 2)`
- [ ] Check FuncObj address: `FuncObj = FX_FNHEADER` (byte offset)
- [ ] Verify frame field reading (fnheader, pc) with correct byte-ordering

### Phase 3: Verify Byte Access ✅ PENDING
- [ ] Check instruction decoding: how bytes are read from virtual_memory[PC]
- [ ] Verify no additional byte-swapping during instruction fetch
- [ ] Compare instruction decoder logic with C emulator

## Findings Log

### 2025-01-27: Initial Investigation

**Observation**: PC calculation appears correct (0x307898 matches C emulator), but bytes differ.

**Hypothesis 1**: Virtual page containing PC 0x307898 is not loaded correctly
- PC 0x307898 = virtual page 6204 (0x307898 / 512 = 6204)
- Need to verify FPtoVP mapping for file page → virtual page 6204
- **Status**: Added debug output in `loadMemoryPages` to track this page

**Hypothesis 2**: Byte-swapping during page load is incorrect ⚠️ **MOST LIKELY**
- Pages are loaded with `@byteSwap` on 32-bit words (line 512 in sysout.zig)
- C emulator uses `word_swap_page` with `ntohl()` which swaps 32-bit longwords
- **CRITICAL QUESTION**: Does C emulator swap bytes BEFORE or AFTER storing in virtual memory?
- **Analysis**: 
  - If sysout file has: `[00, 00, 60, bf, c9, 12, 0a, 02]` (big-endian)
  - After 32-bit word swap: `[bf, 60, 00, 00, 02, 0a, 12, c9]` (little-endian)
  - But C emulator shows: `[00, 00, 60, bf, c9, 12, 0a, 02]` (unchanged!)
  - **CONCLUSION**: C emulator might NOT be swapping bytes, or swapping differently!

**Hypothesis 3**: Address offset calculation is wrong
- PC is calculated correctly, but accessing `virtual_memory[PC]` might use wrong offset
- Need to verify: is PC a byte offset or DLword offset?
- **Status**: PC is confirmed as byte offset (matches C emulator log format)

### 2025-01-27: Root Cause Identified ⚠️ **CRITICAL**

**Root Cause**: Zig emulator is NOT using XOR addressing for memory access!

**Finding**: From `ENDIANNESS_FINDINGS.md`:
- C emulator uses XOR addressing in BYTESWAP mode:
  - Bytes: `base ^ 3` (e.g., `0x307898 ^ 3 = 0x30789b`)
  - Words: `base ^ 2` (e.g., `0x307898 ^ 2 = 0x30789a`)
- This compensates for byte-swapped memory layout after 32-bit longword swapping
- Zig emulator reads directly from `virtual_memory[PC]` without XOR addressing

**Evidence**:
- C emulator: `Get_BYTE_PCMAC0` → `(PCMAC) ^ 3` → reads from `0x30789b`
- Zig emulator: `virtual_memory[0x307898]` → reads from `0x307898` (wrong!)

**Impact**: 
- All instruction decoding is reading from wrong addresses
- All memory access needs XOR addressing in BYTESWAP mode

**Solution Required**:
1. Implement XOR addressing wrapper functions for memory access
2. Update `fetchInstructionByte` and `decodeInstructionFromMemory` to use XOR addressing
3. Update all memory access throughout the VM to use XOR addressing

**Status**: ✅ **FIX IMPLEMENTED** - Added XOR addressing in `memory_access.zig` and updated `decode.zig`

**Implementation**:
1. ✅ Created `utils/memory_access.zig` with XOR addressing functions:
   - `getByte()`: Applies `address ^ 3` for byte access
   - `getDLword()`: Manually constructs 16-bit value from bytes with XOR addressing
2. ✅ Updated `decode.zig`:
   - `fetchInstructionByte()`: Now uses `memory_access.getByte()` with XOR addressing
   - `decodeInstructionFromMemory()`: Now uses XOR addressing for opcode and operands

**Test Results** (2025-01-27):

✅ **SUCCESS**: XOR addressing is working correctly!
- Debug shows: `address=0x307898, xor_address=0x30789b, byte=0x00`
- First byte read matches C emulator (`0x00` from `0x30789b`)

⚠️ **ISSUE**: Opcode decoding still incorrect
- C emulator: Reads `POP` (0xBF) at PC 0x307898
- Zig emulator: Reads `0x00` (opc_unused_0) at PC 0x307898
- C emulator log bytes: `000060bfc9120a02` - opcode `0xBF` is 4th byte
- **HYPOTHESIS**: The C emulator might be reading the opcode from a different byte position, or the log format shows bytes in a different order

### 2025-12-18 19:48: Critical Discovery - Logging vs Execution

**CRITICAL FINDING**: C emulator uses different memory access for logging vs execution:
- **Logging** (xc.c:584): `*((unsigned char *)PCMAC + i)` - **RAW memory, NO XOR addressing**
- **Execution** (xc.c:603): `Get_BYTE_PCMAC0` - **XOR addressing applied** (`(PCMAC) ^ 3`)

**Zig Implementation Status**:
- ✅ Execution: Uses XOR addressing via `memory_access.getByte()` ✓
- ✅ Logging: Uses raw memory `vmem[vm.pc + i]` to match C format ✓

**Raw Memory Bytes Investigation** (2025-12-18 20:15):

✅ **Verified**: Only 1 file page (5178) maps to virtual page 6204  
✅ **Verified**: FPtoVP[5178] = 6204 (GETFPTOVP), GETPAGEOK = 0x0000  
✅ **Verified**: File page reading logic matches C emulator (file_offset = file_page * BYTESPER_PAGE)
✅ **Verified**: Zig's FPtoVP table reading is CORRECT

**CRITICAL DISCOVERY** (2025-12-18 20:15):
- **File page 2937** has bytes C expects (`bf 60 00 00 02 0a 12 c9` → `00 00 60 bf c9 12 0a 02` after swap) ✓
- **File page 2937 maps to virtual page 11850** (not 6204!) according to FPtoVP table
- **File page 5178 maps to virtual page 6204** (correct for PC 0x307898)
- **Zig correctly loads file page 5178 for virtual page 6204** ✓

**Conclusion**:
- Zig's FPtoVP table reading and page loading are CORRECT
- C emulator may be loading file page 2937 for virtual page 11850 (not 6204)
- C emulator log shows PC 0x307898 (virtual page 6204), but bytes match file page 2937 (virtual page 11850)
- **Possible explanations**:
  1. C emulator's FPtoVP table byte-swapping differs (but Zig matches file format)
  2. C emulator's log shows bytes from a different location than PC
  3. C emulator has a bug in page loading or PC calculation
  4. Virtual page 11850 also has offset 0x98 with those bytes (different virtual address: 0x5c9498 vs 0x307898)

**Next Steps**:
1. ⏳ Verify if virtual page 11850, offset 0x98 also has those bytes
2. ⏳ Check C emulator's FPtoVP table values for file pages 2937 and 5178
3. ⏳ Verify C emulator's PC calculation matches Zig (PC = byte offset from Lisp_world)

**Next Investigation**:
1. ✅ Verified: Only 1 file page (5178) maps to virtual page 6204
2. ✅ Verified: FPtoVP[5178] = 6204 (GETFPTOVP), GETPAGEOK = 0x0000
3. ⏳ **CRITICAL**: Raw bytes from file don't match what C expects after byte-swap
   - C expects after swap: `00 00 60 bf c9 12 0a 02`
   - Zig raw from file: `00 0e 00 05 00 36 00 3f`
   - If we reverse C's expected bytes (undo byte-swap), we get: `bf 60 00 00 02 0a 12 c9`
   - This still doesn't match Zig's raw bytes!
4. ⏳ **HYPOTHESIS**: C emulator might be loading a different file page, or there's an issue with file page reading
5. ⏳ Check if C emulator's log bytes represent something other than raw memory

**Next Steps**:
1. ⏳ Verify FPtoVP mapping: Check C emulator debug output for file page → virtual page 6204
2. ⏳ Compare raw bytes: Check if C emulator's expected bytes match any file page
3. ⏳ Verify byte-swapping: Ensure 32-bit longword swap matches C exactly
4. ⏳ Check frame field access: Verify if frame reading also needs XOR addressing updates

**Important Notes**:
- XOR addressing only affects the low 2 bits of the address (^ 2 for words, ^ 3 for bytes)
- This means addresses stay within the same 4-byte aligned region
- Bounds checking should use the original address, not the XOR address
- The XOR address might be slightly larger than the original (e.g., 0x307898 ^ 3 = 0x30789b), but this is safe as long as we check bounds on the original address + length

## Test Cases

### Test Case 1: Raw Memory Dump at PC
**Action**: Dump 16 bytes from `virtual_memory[0x307898..0x307898+16]`  
**Expected**: Should match C emulator's bytes `000060bfc9120a02...`  
**Status**: PENDING

### Test Case 2: FPtoVP Mapping Verification
**Action**: Find file page that maps to virtual page 6204  
**Expected**: File page should exist and map correctly  
**Status**: PENDING

### Test Case 3: Sysout File Raw Bytes
**Action**: Read raw bytes from sysout file at the location that should map to PC 0x307898  
**Expected**: Should match C emulator's view after accounting for byte-swapping  
**Status**: PENDING

## Success Criteria

1. ✅ PC calculation matches C emulator (DONE: 0x307898)
2. ⏳ Bytes at PC match C emulator (IN PROGRESS)
3. ⏳ Opcode decoding matches C emulator (BLOCKED by #2)
4. ⏳ Execution trace matches C emulator (BLOCKED by #2)

## Documentation Rules

**CRITICAL**: Document every finding, even if it doesn't solve the problem:
- ✅ What was tested
- ✅ What was expected
- ✅ What was observed
- ✅ What hypothesis was confirmed/refuted
- ✅ What next steps are planned

**AVOID**:
- ❌ Reverting fixes without documenting why
- ❌ Making multiple changes without testing each
- ❌ Assuming previous fixes are correct without verification
