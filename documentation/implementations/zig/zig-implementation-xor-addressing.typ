= XOR Addressing Implementation in Zig

*Date*: 2025-12-18 19:48  
*Status*: ✅ Implemented - XOR Addressing Working, Memory Loading Issue Identified  
*Location*: `zaiko/src/utils/memory_access.zig`

*Related*: See Execution Debugging for memory loading investigation

== Overview

The Zig implementation uses XOR addressing for memory access in BYTESWAP mode, matching the C emulator's behavior. This document tracks the implementation and findings.

== Implementation

=== Core Module

*File*: `zaiko/src/utils/memory_access.zig`

Provides XOR addressing functions matching C emulator macros:
- `getByte()`: Applies `address ^ 3` for byte access (matches `GETBYTE()`)
- `getDLword()`: Manually constructs 16-bit values from bytes with XOR addressing (matches `Get_DLword()`)
- `applyXORAddressingByte()`: Pure function for XOR address calculation
- `applyXORAddressingWord()`: Pure function for word XOR address calculation

=== Integration Points

1. *Instruction Decoding* (`vm/dispatch/decode.zig`):
   - `fetchInstructionByte()`: Uses `memory_access.getByte()` with XOR addressing
   - `decodeInstructionFromMemory()`: Uses XOR addressing for opcode and operand reading

2. *Execution Trace Logging* (`vm/execution_trace.zig`):
   - *CRITICAL*: Logging uses RAW memory (no XOR addressing) to match C emulator log format
   - C emulator logs: `*((unsigned char *)PCMAC + i)` (raw memory)
   - Zig emulator logs: `vmem[vm.pc + i]` (raw memory)
   - Execution uses XOR addressing, but logging shows raw memory bytes

== Key Findings (2025-12-18 19:48)

=== ✅ Success: XOR Addressing Implementation

- XOR addressing correctly implemented and applied during instruction execution
- Debug output confirms: `address=0x307898, xor_address=0x30789b, byte=0x00` ✓
- First byte read matches C emulator expectation (`0x00`)

=== ⚠️ Issue: Raw Memory Bytes Don't Match

*Observation*: Raw memory bytes at PC 0x307898 differ between Zig and C emulators:
- *Zig emulator*: `05 00 0e 00 3f 00 36 00` (after byte-swap)
- *C emulator*: `00 00 60 bf c9 12 0a 02` (expected)

*Raw bytes from file* (before byte-swap): `00 0e 00 05 00 36 00 3f`

*Hypothesis*: Wrong file page may be loaded for virtual page 6204 (PC page)

*Next Steps*:
1. ✅ Verified: FPtoVP mapping is correct (file page 5178 → virtual page 6204)
2. ⚠️ *ISSUE*: Raw bytes from file page 5178 don't match C emulator's expected bytes
3. ⏳ Need to verify: Does C emulator's log show raw memory or bytes after XOR addressing?
4. ⏳ Check if there's an issue with file page reading (offset calculation)

== Testing

=== Test Results

*XOR Addressing Verification*:
- ✅ XOR address calculation: `0x307898 ^ 3 = 0x30789b` ✓
- ✅ Byte read from XOR address: `0x00` (matches C emulator first byte) ✓
- ⚠️ Complete byte sequence: Still investigating

*Memory Access*:
- ✅ Execution uses XOR addressing (via `memory_access.getByte()`) ✓
- ✅ Logging uses raw memory (no XOR) to match C emulator format ✓

== Related Documentation

- ENDIANNESS_FINDINGS.md - Complete C emulator XOR addressing analysis
- ADDRESS_BYTE_ORDER_INVESTIGATION.md - Ongoing investigation
- specifications/data-structures/sysout-byte-swapping.typ - Byte-swapping procedures

== Implementation Notes

=== Why XOR Addressing?

After 32-bit longword byte-swapping during page load, the memory layout requires XOR addressing to read bytes correctly. The XOR operation (`^ 3` for bytes, `^ 2` for words) compensates for the byte-swapped layout.

=== Logging vs Execution

*CRITICAL DISTINCTION*:
- *Execution*: Uses XOR addressing (`Get_BYTE_PCMAC0` → `(PCMAC) ^ 3`)
- *Logging*: Uses raw memory (`*((unsigned char *)PCMAC + i)`) to show actual memory contents

This matches the C emulator's behavior exactly.
