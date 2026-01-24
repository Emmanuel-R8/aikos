= Systematic Memory Loading Debugging

*Navigation*: Zig Implementation Status | Implementations README | Main README

*Date*: 2025-12-23 16:43
*Status*: Active - Systematic approach to memory loading issues

== Overview

This document applies reverse engineering debugging techniques to systematically identify and fix memory loading issues in the Zig emulator. Memory loading problems are systemic and likely stem from address calculation and endianness issues.

== Critical Debugging Technique Application

*ALWAYS* apply the critical debugging technique (per `CRITICAL_DEBUGGING_TECHNIQUE.typ`):

For EACH value:
1. Value itself (decimal and hexadecimal)
2. Value / 2 (decimal and hexadecimal)
3. Value * 2 (decimal and hexadecimal)

== Reverse Engineering Debugging Techniques

=== Technique 1: Memory Dump Comparison

*Purpose*: Compare actual memory content byte-by-byte at specific addresses

*Method*:
1. Identify target address (e.g., PC 0x307898)
2. Dump memory from C emulator at that address
3. Dump memory from Zig emulator at that address
4. Compare byte-by-byte
5. Apply critical debugging technique to any differences

*Example*:
```
C emulator at PC 0x307898:  00 00 60 bf c9 12 0a 02
Zig emulator at PC 0x307898: 0e 00 00 00 00 00 00 00

Analysis:
- Completely different content - not a byte-order issue
- Suggests wrong memory page loaded or wrong address calculation
```

=== Technique 2: Address Space Mapping Verification

*Purpose*: Verify that file pages map to correct virtual pages

*Method*:
1. Calculate virtual page from address: `virtual_page = address / BYTESPER_PAGE`
2. Find file page(s) that map to this virtual page using FPtoVP table
3. Verify file page offset calculation: `file_offset = file_page * BYTESPER_PAGE`
4. Check if correct file page is being loaded
5. Verify virtual address calculation: `virtual_address = virtual_page * BYTESPER_PAGE`

*Critical Checks*:
- Virtual page calculation: `PC / 512` (BYTESPER_PAGE)
- File page lookup: `FPtoVP[file_page] == virtual_page`
- Address within page: `offset_in_page = PC % 512`
- File offset: `file_page * 512`
- Virtual address: `virtual_page * 512`

*Example for PC 0x307898*:
```
PC: 0x307898
Virtual page: 0x307898 / 512 = 6204 (0x183c)
Offset in page: 0x307898 % 512 = 0x98 (152 bytes)

Expected file page: FPtoVP[file_page] == 6204
File offset: file_page * 512
Virtual address: 6204 * 512 = 0x307800
PC offset from page start: 0x307898 - 0x307800 = 0x98 (152 bytes)
```

=== Technique 3: Step-by-Step Address Calculation Verification

*Purpose*: Verify each step of address translation

*Steps*:
1. *PC Address*: Verify PC value is correct
2. *Virtual Page Calculation*: `vpage = PC / BYTESPER_PAGE`
3. *Page Offset*: `offset = PC % BYTESPER_PAGE`
4. *FPtoVP Lookup*: Find file_page where `FPtoVP[file_page] == vpage`
5. *File Offset*: `file_offset = file_page * BYTESPER_PAGE`
6. *File Page Read*: Verify correct bytes read from file
7. *Byte Swapping*: Verify byte-swapping matches C exactly
8. *Virtual Address*: `vaddr = vpage * BYTESPER_PAGE`
9. *Memory Write*: Verify bytes written to correct virtual address

*Apply Critical Debugging Technique* at each step:
- If value is wrong, check: value, value/2, value*2
- Look for byte/DLword confusion
- Look for endianness issues

=== Technique 4: Hex Dump Pattern Analysis

*Purpose*: Identify patterns in memory dumps to find known structures

*Method*:
1. Dump memory region around target address
2. Look for known patterns:
   - Function headers (startpc, na, pv fields)
   - Frame structures (FX layout)
   - Opcode sequences
   - String data
3. Compare with C emulator dump
4. Identify where patterns diverge

*Known Patterns*:
- Function header: `startpc` (byte offset), `na` (arg count), `pv` (param var count)
- Frame structure: `fnheader`, `pc`, `al`, `nb`, `fo` fields
- Opcode sequences: Known bytecode patterns

=== Technique 5: Cross-Reference Validation

*Purpose*: Verify calculations using multiple independent methods

*Method*:
1. Calculate address using method A
2. Calculate address using method B
3. Compare results
4. If mismatch, investigate which method is wrong

*Example*:
```
Method A: PC = FuncObj + CURRENTFX->pc
Method B: PC = virtual_page * 512 + offset_in_page
Method C: PC = file_page_offset + offset_in_page

All should yield same PC value.
```

=== Technique 6: Byte Swapping Verification

*Purpose*: Verify byte-swapping matches C emulator exactly

*Method*:
1. Read raw bytes from sysout file
2. Apply byte-swapping per C emulator rules
3. Compare swapped bytes with C emulator memory
4. Verify FPtoVP table byte-swapping separately
5. Verify page byte-swapping separately

*C Emulator Rules* (from code analysis):
- FPtoVP table: `word_swap_page((unsigned short *)fptovp, (sysout_size / 4) + 1)` (BIGVM, incomplete!)
- Page loading: 32-bit longword swapping
- First half of FPtoVP swapped, second half NOT swapped (incomplete implementation!)

=== Technique 7: Offset Verification

*Purpose*: Verify offsets are calculated correctly

*Method*:
1. Calculate offset using formula
2. Verify offset is within bounds
3. Check if offset matches expected value
4. Apply critical debugging technique: offset, offset/2, offset*2

*Common Offset Issues*:
- Byte vs DLword confusion (factor of 2)
- Base address wrong (add/subtract error)
- Alignment issues (not multiple of 2 or 4)

== Systematic Investigation Plan

=== Phase 1: PC Address Verification

*Goal*: Verify PC 0x307898 is correct

*Steps*:
1. ✅ Verify PC initialization sets PC to 0x307898
2. ⏳ Verify PC 0x307898 is valid address
3. ⏳ Calculate virtual page: `0x307898 / 512 = 6204`
4. ⏳ Verify virtual page 6204 exists in virtual memory
5. ⏳ Calculate offset in page: `0x307898 % 512 = 0x98 (152 bytes)`

*Critical Debugging*:
```
PC: 0x307898 (3172504 decimal)
PC / 2: 0x183c4c (1586252 decimal)
PC * 2: 0x60f130 (6345008 decimal)

Virtual page: 6204 (0x183c)
6204 * 512 = 0x307800 (3172352 decimal)
PC - page_start = 0x307898 - 0x307800 = 0x98 (152 bytes)
```

=== Phase 2: Virtual Page to File Page Mapping

*Goal*: Find which file page maps to virtual page 6204

*Steps*:
1. ⏳ Load FPtoVP table
2. ⏳ Search for file_page where `FPtoVP[file_page] == 6204`
3. ⏳ Verify GETPAGEOK flag is valid (not 0xFFFF sparse)
4. ⏳ Check if multiple file pages map to same virtual page
5. ⏳ Compare with C emulator's FPtoVP table

*Expected Result*:
- File page 5178 maps to virtual page 6204 (from previous investigation)
- GETPAGEOK should be 0x0000 (valid page)

*Critical Debugging*:
```
Virtual page: 6204 (0x183c)
File page: 5178 (0x143a)
5178 / 2 = 2589 (not matching)
5178 * 2 = 10356 (not matching)
6204 / 2 = 3102 (not matching)
6204 * 2 = 12408 (not matching)

No simple factor relationship - mapping is correct.
```

=== Phase 3: File Page Offset Calculation

*Goal*: Verify file page offset is calculated correctly

*Steps*:
1. ⏳ Calculate file offset: `file_page * BYTESPER_PAGE = 5178 * 512`
2. ⏳ Verify file offset is within file bounds
3. ⏳ Read raw bytes from file at that offset
4. ⏳ Compare raw bytes with C emulator's file bytes

*Critical Debugging*:
```
File page: 5178
File offset: 5178 * 512 = 2651136 (0x287400)
File offset / 2: 1325568 (0x143a00)
File offset * 2: 5302272 (0x50e800)

Verify: 2651136 < file_size
```

=== Phase 4: Byte Swapping Verification

*Goal*: Verify byte-swapping matches C emulator

*Steps*:
1. ⏳ Read raw bytes from file page 5178
2. ⏳ Apply 32-bit longword swapping (per C emulator)
3. ⏳ Compare swapped bytes with C emulator memory at PC
4. ⏳ Verify byte-swapping matches C exactly

*C Emulator Byte-Swapping*:
- Pages: 32-bit longword swapping
- FPtoVP: `word_swap_page` for first half only (incomplete!)

*Critical Debugging*:
```
Raw file bytes: [read from file]
Swapped bytes: [apply 32-bit longword swap]
C emulator bytes: 00 00 60 bf c9 12 0a 02

Compare byte-by-byte:
- If match: byte-swapping correct
- If off by factor: check byte/DLword confusion
- If completely different: wrong page or wrong offset
```

=== Phase 5: Virtual Address Calculation

*Goal*: Verify virtual address is calculated correctly

*Steps*:
1. ⏳ Calculate virtual address: `virtual_page * BYTESPER_PAGE = 6204 * 512`
2. ⏳ Verify virtual address is within virtual memory bounds
3. ⏳ Verify bytes are written to correct virtual address
4. ⏳ Read bytes from virtual memory at calculated address
5. ⏳ Compare with expected bytes

*Critical Debugging*:
```
Virtual page: 6204
Virtual address: 6204 * 512 = 3172352 (0x307800)
Virtual address / 2: 1586176 (0x183800)
Virtual address * 2: 6344704 (0x60f000)

PC: 0x307898
PC - virtual_address: 0x307898 - 0x307800 = 0x98 (152 bytes)
Verify: 0x98 < 512 (within page)
```

=== Phase 6: Memory Content Verification

*Goal*: Verify memory content matches C emulator

*Steps*:
1. ⏳ Read bytes from virtual memory at PC 0x307898
2. ⏳ Compare byte-by-byte with C emulator
3. ⏳ If mismatch, trace back through loading process
4. ⏳ Verify each step: file read → byte swap → virtual write

*Expected*:
```
C emulator at PC 0x307898: 00 00 60 bf c9 12 0a 02
Zig emulator at PC 0x307898: [should match]
```

== Implementation Tasks

=== Task 1: Add Memory Dump Function

*File*: `zaiko/src/utils/memory_debug.zig` (new file)

*Function*: `dumpMemory(address: LispPTR, length: usize, vmem: []const u8) void`
- Dump memory region as hex
- Show address, hex bytes, ASCII representation
- Format matching C emulator dump format

=== Task 2: Add Address Calculation Verification

*File*: `zaiko/src/data/sysout.zig`

*Function*: `verifyAddressCalculation(pc: LispPTR, fptovp: *const FPtoVPTable) void`
- Calculate virtual page from PC
- Find file page mapping
- Verify file offset calculation
- Verify virtual address calculation
- Print all intermediate values

=== Task 3: Add Byte Swapping Comparison

*File*: `zaiko/src/utils/endianness.zig`

*Function*: `compareByteSwapping(raw_bytes: []const u8, expected_bytes: []const u8) void`
- Compare raw file bytes with expected bytes
- Show before/after byte-swapping
- Identify mismatches
- Apply critical debugging technique to differences

=== Task 4: Add Cross-Reference Validation

*File*: `zaiko/src/vm/vm_initialization.zig`

*Function*: `validatePCAddress(vm: *VM, pc: LispPTR) void`
- Calculate PC using multiple methods
- Compare results
- Report mismatches
- Apply critical debugging technique

== Success Criteria

- [ ] Memory dump at PC 0x307898 matches C emulator exactly
- [ ] Virtual page calculation verified: `PC / 512 = 6204`
- [ ] File page mapping verified: `FPtoVP[5178] == 6204`
- [ ] File offset calculation verified: `5178 * 512 = 0x287400`
- [ ] Byte-swapping verified: swapped bytes match C emulator
- [ ] Virtual address calculation verified: `6204 * 512 = 0x307800`
- [ ] Memory content verified: bytes at PC match C emulator
- [ ] All address calculations verified using critical debugging technique

== Next Steps

1. *IMMEDIATE*: Implement memory dump function
2. *IMMEDIATE*: Add address calculation verification
3. *IMMEDIATE*: Compare file page 5178 raw bytes with C emulator
4. *IMMEDIATE*: Verify byte-swapping matches C exactly
5. *IMMEDIATE*: Trace memory loading step-by-step
6. Fix any identified issues systematically
7. Re-run execution trace comparison
8. Verify all discrepancies resolved

== Related Documentation

- Critical Debugging Technique - Value analysis method
- Execution Debugging - Current discrepancies
- Sysout Byte Swapping - Byte-endianness specification
- Endianness Findings - Complete endianness analysis
