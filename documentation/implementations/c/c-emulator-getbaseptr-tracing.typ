= C Emulator GETBASEPTR_N Opcode Deep Tracing

*Date*: 2025-01-27
*Status*: ✅ Completed - Verified Understanding
*Location*: `maiko/src/xc.c`

== Purpose

This document tracks exhaustive tracing of the C emulator's GETBASEPTR_N opcode to understand:
- Base pointer access mechanism
- Address calculation and translation
- Memory reading with alignment
- Stack manipulation

== Tracing Implementation

=== GETBASEPTR_N Opcode Detailed Trace

*Location*: `maiko/src/xc.c:1999-2067`

Added comprehensive tracing for GETBASEPTR_N opcode (instruction #4) to understand base pointer access:

```c
/* AUTO: DETAILED TRACING for GETBASEPTR_N to understand base pointer access */
static int getbaseptr_traced = 0;
if (!getbaseptr_traced) {
  // ... detailed tracing ...
}
```

=== Trace Output

The trace captures:
1. **Current State**: PC, pccache, PCMAC pointers and offsets
2. **Operand Byte**: The byte offset parameter
3. **Stack State**: TopOfStack value before operation
4. **Address Calculation**: Base pointer extraction and target address calculation
5. **Native Pointer Translation**: Virtual to native address translation
6. **Pointer Value Read**: The 4-byte pointer value read from memory
7. **Raw Memory**: Bytes at target address
8. **Result**: Final result that replaces TOS

== Findings from Trace Execution

*Date*: 2025-01-27
*Trace File*: `c_getbaseptr_detailed_trace.txt`

=== Execution Context

- **PC (byte pointer)**: `0x7f660460f137`
- **PC offset from Lisp_world**: `0x60f137` bytes (`0x30789b` DLwords)
- **pccache**: `0x7f660460f138`
- **PCMAC = pccache - 1**: `0x7f660460f137`

=== Operand Byte

- **Get_BYTE_PCMAC1 = Get_BYTE(PC + 1)**: `0x36` (54 decimal)
- This is the byte offset from the base pointer

=== Stack State Before GETBASEPTR_N

- **TopOfStack (TOS)**: `0x00000000`
- **TOS as pointer (POINTERMASK & TOS)**: `0x00000000`

*Note*: TOS is 0, which means the base pointer is 0. This is interesting - it suggests we're reading from the beginning of the Lisp address space.

=== Address Calculation

**Formula**: `target_addr = (TOPOFSTACK & POINTERMASK) + operand_byte`

- **base_ptr = TOS & POINTERMASK**: `0x00000000`
- **operand_byte**: `0x36` (54)
- **target_addr = base_ptr + operand_byte**: `0x00000036`

**Interpretation**:
- Base pointer is 0 (start of Lisp address space)
- Adding offset 0x36 (54 bytes) gives target address 0x00000036

=== Native Pointer Translation

**Function**: `NativeAligned4FromLAddr(target_addr)`

- **Input**: `0x00000036` (virtual address)
- **Output**: `0x7f660400006c` (native pointer)
- **Native pointer offset from Lisp_world**: `0x6c` bytes

**Translation Process**:
- Virtual address 0x00000036 is translated to native pointer
- The translation accounts for page mapping and alignment
- Result is 4-byte aligned (required for LispPTR read)

=== Pointer Value Read

**Function**: `GetLongWord(target_native)` or `*target_native`

- **Native pointer**: `0x7f660400006c`
- **Pointer value**: `0x00000000`
- **Bytes**: `0x00 0x00 0x00 0x00`

**Reading Process**:
- Reads 4 bytes (LispPTR) from native pointer
- Uses native byte order (little-endian on x86_64)
- Result is 0x00000000 (null pointer)

=== Raw Memory at target_addr

Memory bytes at target address:
```
[target+0] = [0x6c] = 0x00
[target+1] = [0x6d] = 0x00
[target+2] = [0x6e] = 0x00
[target+3] = [0x6f] = 0x00
```

All bytes are 0, confirming the null pointer read.

=== Result

- **Result = ptr_value & POINTERMASK**: `0x00000000`
- **Action**: This replaces TOS on stack

== GETBASEPTR_N Algorithm Summary

1. **Read operand byte** from PC+1 (byte offset)
2. **Extract base pointer** from TOS: `base_ptr = TOS & POINTERMASK`
3. **Calculate target address**: `target_addr = base_ptr + operand_byte`
4. **Translate to native pointer**: `target_native = NativeAligned4FromLAddr(target_addr)`
5. **Read 4-byte pointer**: `ptr_value = *target_native`
6. **Mask result**: `result = ptr_value & POINTERMASK`
7. **Replace TOS**: `TOPOFSTACK = result`

== Key Understanding

### Address Calculation

- Base pointer comes from TOS (top of stack)
- Operand byte is added to base pointer (byte offset, not DLword offset)
- Result is a virtual address that must be translated

### Native Pointer Translation

- `NativeAligned4FromLAddr()` translates virtual address to native pointer
- Ensures 4-byte alignment (required for LispPTR access)
- Accounts for page mapping via FPtoVP table

### Memory Reading

- Reads 4 bytes (LispPTR) from native pointer
- Uses native byte order (little-endian on x86_64)
- Result is masked with POINTERMASK to extract pointer value

### Stack Manipulation

- TOS is replaced (not pushed/popped)
- Original TOS value is used as base pointer
- Result becomes new TOS value

== C Macro Definition

```c
#define GETBASEPTR_N(N) \
  do { \
    TOPOFSTACK = \
        (POINTERMASK & *((LispPTR *)NativeAligned4FromLAddr((POINTERMASK & TOPOFSTACK) + (N)))); \
    nextop2; \
  } while (0)
```

**Key Points**:
- `(POINTERMASK & TOPOFSTACK)`: Extract base pointer from TOS
- `+ (N)`: Add operand byte offset
- `NativeAligned4FromLAddr(...)`: Translate to native pointer
- `*((LispPTR *)...)`: Read 4-byte pointer value
- `POINTERMASK & ...`: Mask result to extract pointer
- `nextop2`: Advance PC by 2 bytes (opcode + 1-byte operand)

== Related Documentation

- `c-emulator-address-xor-tracing.typ` - XOR addressing mechanism
- `c-emulator-unbind-tracing.typ` - UNBIND opcode analysis
- `zig-frame-initialization-analysis.typ` - Frame initialization
