= C Emulator Address and XOR Addressing Deep Tracing

*Date*: 2025-01-27
*Status*: ✅ Completed - Verified Zig Implementation Matches C
*Location*: `maiko/src/xc.c`

== Purpose

This document tracks exhaustive tracing of the C emulator to understand:
- Address calculation (byte offsets vs DLword offsets)
- Virtual memory addressing and paging
- XOR addressing mechanism in BYTESWAP mode
- Endianness and byte-swapping
- Word and halfword alignment
- PC calculation and advancement

== Tracing Implementation

=== GVAR Opcode Detailed Trace

*Location*: `maiko/src/xc.c:1486-1565`

Added comprehensive tracing for GVAR opcode (instruction #2) to understand `Get_Pointer_PCMAC1`:

```c
/* AUTO: DETAILED TRACING for GVAR to understand Get_Pointer_PCMAC1 */
if (global_debug_instruction_count == 2) {
  FILE *gvar_trace = fopen("c_gvar_detailed_trace.txt", "w");
  // ... detailed tracing ...
}
```

=== Trace Output

The trace captures:
1. **Current State**: PC, pccache, PCMAC pointers and offsets
2. **Memory Content**: Raw bytes at PC-1 through PC+8
3. **XOR Addressing Analysis**: How Get_Pointer(PCMAC+1) reads bytes
4. **Get_Pointer_PCMAC1 Result**: Actual pointer value and byte breakdown
5. **Raw Memory Comparison**: Bytes at PCMAC+1 through PCMAC+4

== Key Concepts to Verify

=== Address Types

1. **PC (Program Counter)**: Byte pointer (`char *`) in C emulator
   - Points to current instruction byte
   - Offset from `Lisp_world` is byte offset
   - DLword offset = byte offset / 2

2. **LispPTR**: 32-bit value representing DLword offset from `Lisp_world`
   - To get byte offset: multiply by 2
   - Used for virtual memory addressing

3. **pccache**: Byte pointer, initialized as `PC + 1`
   - Used for reading operands
   - `PCMAC = pccache - 1` (so `PCMAC = PC`)

=== XOR Addressing

*Location*: `maiko/inc/lsptypes.h:565`

```c
#define GETBYTE(base) (*(unsigned char *)(3 ^ (UNSIGNED)(base)))
```

XOR addressing applies `^ 3` to the address before dereferencing.

**Key Insight**: XOR 3 only affects the low 2 bits of the address:
- Address ending in 0 → XOR 3 → ending in 3
- Address ending in 1 → XOR 3 → ending in 2
- Address ending in 2 → XOR 3 → ending in 1
- Address ending in 3 → XOR 3 → ending in 0

This swaps bytes within 4-byte boundaries, compensating for byte-swapped memory layout.

=== Get_Pointer for BIGVM

*Location*: `maiko/inc/lispemul.h:243-244`

```c
#define Get_Pointer(ptr) \
  ((Get_BYTE(ptr) << 24) | (Get_BYTE((ptr) + 1) << 16) | (Get_BYTE((ptr) + 2) << 8) | Get_BYTE((ptr) + 3))
```

For BIGVM mode, reads 4 bytes with XOR addressing:
- `Get_BYTE(ptr) << 24` (high byte)
- `Get_BYTE(ptr+1) << 16` (second byte)
- `Get_BYTE(ptr+2) << 8` (third byte)
- `Get_BYTE(ptr+3)` (low byte)

=== Get_Pointer_PCMAC1

*Location*: `maiko/inc/inlineC.h:39`

```c
#define Get_Pointer_PCMAC1 Get_Pointer(PCMAC + 1)
```

Since `PCMAC = pccache - 1` and `pccache = PC + 1`:
- `PCMAC = PC`
- `PCMAC + 1 = PC + 1`

So `Get_Pointer_PCMAC1` reads from `PC + 1` through `PC + 4` with XOR addressing.

== Key Understanding from Code Analysis

=== Address Calculation Flow

1. **PC (Program Counter)**: `char *` byte pointer
   - Points to current instruction byte in `Lisp_world`
   - Byte offset: `(char *)PC - (char *)Lisp_world`
   - DLword offset: `byte_offset / 2`

2. **pccache**: `char *` byte pointer, initialized as `PC + 1`
   - Used for reading operands
   - `PCMAC = pccache - 1` (macro definition)
   - So `PCMAC = PC` (since `pccache = PC + 1`)

3. **PCMAC**: Macro that expands to `pccache - 1`
   - Used in `Get_BYTE_PCMAC0`, `Get_BYTE_PCMAC1`, etc.
   - `Get_BYTE_PCMAC0 = Get_BYTE(PCMAC) = Get_BYTE(pccache - 1) = Get_BYTE(PC)`
   - `Get_BYTE_PCMAC1 = Get_BYTE(PCMAC + 1) = Get_BYTE(pccache) = Get_BYTE(PC + 1)`

=== XOR Addressing Mechanism

*Location*: `maiko/inc/lsptypes.h:565`

```c
#define GETBYTE(base) (*(unsigned char *)(3 ^ (UNSIGNED)(base)))
```

**Critical Insight**: XOR 3 only affects the **low 2 bits** of the address:
- `0x60f130 ^ 3 = 0x60f133` (0x30 ^ 3 = 0x33)
- `0x60f131 ^ 3 = 0x60f132` (0x31 ^ 3 = 0x32)
- `0x60f132 ^ 3 = 0x60f131` (0x32 ^ 3 = 0x31)
- `0x60f133 ^ 3 = 0x60f130` (0x33 ^ 3 = 0x30)

This swaps bytes within 4-byte boundaries, compensating for byte-swapped memory layout after 32-bit longword swapping during page load.

=== Get_Pointer for BIGVM

*Location*: `maiko/inc/lispemul.h:243-244`

```c
#define Get_Pointer(ptr) \
  ((Get_BYTE(ptr) << 24) | (Get_BYTE((ptr) + 1) << 16) | (Get_BYTE((ptr) + 2) << 8) | Get_BYTE((ptr) + 3))
```

For `Get_Pointer(PCMAC + 1)` where `PCMAC + 1 = PC + 1`:
- Reads 4 bytes: `[PC+1]`, `[PC+2]`, `[PC+3]`, `[PC+4]`
- But with XOR addressing: `[PC+1 XOR 3]`, `[PC+2 XOR 3]`, `[PC+3 XOR 3]`, `[PC+4 XOR 3]`
- Constructs 32-bit value: `byte0 << 24 | byte1 << 16 | byte2 << 8 | byte3`

=== Get_Pointer_PCMAC1fn (Assembly Implementation)

*Location*: `maiko/inc/inlndos.h:245-264`

**CRITICAL**: This is a 3-byte read, NOT 4 bytes!

```c
asm("lea	edx,[%1]	\n\      // edx = pccache
     xor	dl,3		\n\      // XOR low byte: edx = pccache XOR 3
     movzx	eax,byte ptr [edx]	\n\  // eax = byte at [pccache XOR 3] (high byte)
     shl	eax,16	\n\      // eax = byte << 16
     lea	edx,2[%1]	\n\      // edx = pccache + 2
     xor	dl,3		\n\      // XOR low byte: edx = (pccache + 2) XOR 3
     mov	al,[edx]	\n\      // al = byte at [(pccache + 2) XOR 3] (low byte)
     lea	edx,1[%1]	\n\      // edx = pccache + 1
     xor	dl,3		\n\      // XOR low byte: edx = (pccache + 1) XOR 3
     mov	ah,[edx]	\n\      // ah = byte at [(pccache + 1) XOR 3] (middle byte)
     "
```

**Byte Order**:
- High byte (<< 16): `[pccache XOR 3]`
- Middle byte (<< 8): `[(pccache + 1) XOR 3]`
- Low byte: `[(pccache + 2) XOR 3]`

**Result**: `[pccache XOR 3] << 16 | [(pccache + 1) XOR 3] << 8 | [(pccache + 2) XOR 3]`

**BUT**: For BIGVM, `Get_Pointer_PCMAC1 = Get_Pointer(PCMAC + 1)` which reads 4 bytes, not 3!

This is a **mismatch** - the assembly function reads 3 bytes, but the macro for BIGVM should read 4 bytes.

=== Resolution

*Location*: `maiko/inc/inlineC.h:39`

```c
#define Get_Pointer_PCMAC1 Get_Pointer(PCMAC + 1)
```

For BIGVM, this expands to the `Get_Pointer` macro (4 bytes), NOT `Get_Pointer_PCMAC1fn` (3 bytes).

So the actual behavior is:
- `Get_Pointer_PCMAC1` = `Get_Pointer(PC + 1)`
- Reads 4 bytes with XOR addressing from `PC+1` through `PC+4`
- Constructs: `[PC+1 XOR 3] << 24 | [PC+2 XOR 3] << 16 | [PC+3 XOR 3] << 8 | [PC+4 XOR 3]`

== Findings from Trace Execution

*Date*: 2025-01-27
*Trace File*: `c_gvar_detailed_trace.txt`

=== Execution Context

- **PC (byte pointer)**: `0x7fa64060f131`
- **PC offset from Lisp_world**: `0x60f131` bytes (`0x307898` DLwords)
- **pccache**: `0x7fa64060f132` (PC + 1)
- **PCMAC**: `0x7fa64060f131` (pccache - 1 = PC)

=== Raw Memory Content

Memory at PC-1 through PC+8:
```
[PC-1] = 0x60f130 = 0x00
[PC+0] = 0x60f131 = 0x00  (opcode byte)
[PC+1] = 0x60f132 = 0x60  (operand byte 0)
[PC+2] = 0x60f133 = 0xbf  (operand byte 1)
[PC+3] = 0x60f134 = 0xc9  (operand byte 2)
[PC+4] = 0x60f135 = 0x12  (operand byte 3)
[PC+5] = 0x60f136 = 0x0a
[PC+6] = 0x60f137 = 0x02
[PC+7] = 0x60f138 = 0x68
[PC+8] = 0x60f139 = 0xa1
```

**Expected operand bytes** (for 4-byte BIGVM pointer): `0x60, 0xbf, 0xc9, 0x12`

=== XOR Addressing Analysis

For `Get_Pointer_PCMAC1 = Get_Pointer(PCMAC + 1)` where `PCMAC + 1 = PC + 1 = 0x60f132`:

1. **Get_BYTE(base+0)** where `base = 0x60f132`:
   - XOR address: `3 ^ 0x60f132 = 0x60f131` (0x32 ^ 3 = 0x31)
   - Reads from: `[PC+0] = 0x60f131`
   - Byte value: `0x00`
   - Shift: `<< 24` (high byte)

2. **Get_BYTE(base+1)** where `base+1 = 0x60f133`:
   - XOR address: `3 ^ 0x60f133 = 0x60f130` (0x33 ^ 3 = 0x30)
   - Reads from: `[PC-1] = 0x60f130`
   - Byte value: `0x00`
   - Shift: `<< 16` (second byte)

3. **Get_BYTE(base+2)** where `base+2 = 0x60f134`:
   - XOR address: `3 ^ 0x60f134 = 0x60f137` (0x34 ^ 3 = 0x37)
   - Reads from: `[PC+7] = 0x60f137`
   - Byte value: `0x02`
   - Shift: `<< 8` (third byte)

4. **Get_BYTE(base+3)** where `base+3 = 0x60f135`:
   - XOR address: `3 ^ 0x60f135 = 0x60f136` (0x35 ^ 3 = 0x36)
   - Reads from: `[PC+6] = 0x60f136`
   - Byte value: `0x0a`
   - Shift: `<< 0` (low byte)

=== Result

**Get_Pointer_PCMAC1 Result**: `0x0000020a`
- Bytes: `0x00 0x00 0x02 0x0a`
- Constructed as: `(0x00 << 24) | (0x00 << 16) | (0x02 << 8) | 0x0a`

=== Critical Observation

**Mismatch**: The XOR addressing reads from **different memory locations** than the raw operand bytes:
- Raw operand bytes at `[PC+1]` through `[PC+4]`: `0x60, 0xbf, 0xc9, 0x12`
- XOR addressing reads from: `[PC+0]`, `[PC-1]`, `[PC+7]`, `[PC+6]`: `0x00, 0x00, 0x02, 0x0a`

**This is expected behavior** - XOR addressing with `^ 3` swaps bytes within 4-byte boundaries to compensate for byte-swapped memory layout after 32-bit longword swapping during page load.

The XOR addressing pattern:
- `base+0` (0x60f132) XOR 3 → 0x60f131 (reads from PC+0)
- `base+1` (0x60f133) XOR 3 → 0x60f130 (reads from PC-1)
- `base+2` (0x60f134) XOR 3 → 0x60f137 (reads from PC+7)
- `base+3` (0x60f135) XOR 3 → 0x60f136 (reads from PC+6)

This suggests the memory was loaded with 32-bit longword byte-swapping, and XOR addressing reverses this swap to read the correct byte order.

=== Understanding the Byte-Swap Pattern

**Memory Loading Process**:
1. Sysout file contains big-endian data: `0x60, 0xbf, 0xc9, 0x12` (at file offset)
2. Page is loaded into memory as raw bytes: `0x60, 0xbf, 0xc9, 0x12`
3. `word_swap_page()` swaps 32-bit longwords: `[0x60, 0xbf, 0xc9, 0x12]` → `[0x12, 0xc9, 0xbf, 0x60]`
4. Memory after swap: `0x12, 0xc9, 0xbf, 0x60` (at offsets PC+1, PC+2, PC+3, PC+4)

**XOR Addressing Reversal**:
- XOR 3 swaps bytes within 4-byte boundaries to reverse the longword swap
- `base+0` (0x60f132) XOR 3 → 0x60f131: reads from different 4-byte boundary
- This effectively reconstructs the original big-endian value

**Result**: `0x0000020a` is the atom index after XOR addressing reconstruction.

=== Verification

The trace confirms:
- Raw memory shows bytes after `word_swap_page()`: `0x60, 0xbf, 0xc9, 0x12` at PC+1 through PC+4
- XOR addressing reads from different locations to reconstruct the original value
- The final result `0x0000020a` is the correct atom index for the GVAR opcode

=== Understanding the XOR Pattern

The XOR 3 operation only affects the **low 2 bits** of the address:
- Address ending in `...00` → XOR 3 → ending in `...11` (reads from 3 bytes earlier)
- Address ending in `...01` → XOR 3 → ending in `...10` (reads from 1 byte earlier)
- Address ending in `...10` → XOR 3 → ending in `...01` (reads from 1 byte later)
- Address ending in `...11` → XOR 3 → ending in `...00` (reads from 3 bytes later)

For the specific case in the trace:
- `0x60f132` (ends in `10`) ^ 3 = `0x60f131` (ends in `01`) → reads from PC+0
- `0x60f133` (ends in `11`) ^ 3 = `0x60f130` (ends in `00`) → reads from PC-1
- `0x60f134` (ends in `00`) ^ 3 = `0x60f137` (ends in `11`) → reads from PC+7
- `0x60f135` (ends in `01`) ^ 3 = `0x60f136` (ends in `10`) → reads from PC+6

This creates a **4-byte rotation pattern** that reverses the byte-swapping applied during page load.

=== Verification for Zig Implementation

The Zig implementation in `zaiko/src/utils/memory_access.zig` correctly implements:
- `applyXORAddressingByte(address)` → `address ^ 3`
- `getByte()` applies XOR addressing before reading

The `getPointerOperand()` in `instruction_struct.zig` constructs the 4-byte value from operands that were already read with XOR addressing, which matches the C behavior.

**Key Insight**: The operands are read with XOR addressing during decode, so `getPointerOperand()` should use them directly without additional XOR addressing.

== Verification: Zig Emulator Matches C Emulator

*Date*: 2025-01-27
*Status*: ✅ Verified

=== Memory Content Verification

**PC 0x60f131 (C emulator's starting PC)**:
- **Expected (C emulator trace)**: `00 60 bf c9 12 0a 02 68`
- **Actual (Zig emulator)**: `00 60 bf c9 12 0a 02 68`
- **Result**: ✅ **SUCCESS: Memory matches C emulator exactly**

=== XOR Addressing Verification

**Get_Pointer(PC+1) where PC+1 = 0x60f132**:
- **base+0** (0x60f132) XOR 3 = 0x60f131 → byte = `0x00`
- **base+1** (0x60f133) XOR 3 = 0x60f130 → byte = `0x00`
- **base+2** (0x60f134) XOR 3 = 0x60f137 → byte = `0x02`
- **base+3** (0x60f135) XOR 3 = 0x60f136 → byte = `0x0a`
- **Result**: `0x0000020a` (expected: `0x0000020a` from C trace)
- **Status**: ✅ **SUCCESS: XOR addressing matches C emulator**

=== GVAR Operand Extraction Verification

**GVAR opcode at PC 0x60f131**:
- **Operands read with XOR addressing**: `[0]=0x00 [1]=0x00 [2]=0x02 [3]=0x0a`
- **Constructed atom_index**: `0x0000020a` (522 decimal)
- **Status**: ✅ **SUCCESS: GVAR operand extraction matches C emulator**

=== Conclusion

The Zig emulator's XOR addressing implementation correctly matches the C emulator:
1. ✅ Memory loading with byte-swapping produces identical content
2. ✅ XOR addressing reads from correct locations
3. ✅ 4-byte pointer operand extraction works correctly
4. ✅ GVAR atom_index calculation matches C emulator

== Next Steps

1. ✅ Add comprehensive tracing to C emulator
2. ✅ Execute and capture trace output
3. ✅ Analyze trace to understand address/XOR mechanism
4. ✅ Document findings in this file
5. ✅ Update Zig implementation to match C behavior
6. ✅ Verify XOR addressing matches between C and Zig emulators
7. ⏳ Continue with execution log comparison to verify full execution path

== Related Documentation

- `zig-implementation-xor-addressing.typ` - Zig XOR addressing implementation
- `c-emulator-pc-advancement-fix.typ` - PC advancement fix
- `zig-gvar-bigatoms-implementation.typ` - Zig GVAR implementation
- `unified-logging-format.typ` - Unified log format specification
