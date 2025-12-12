# Opcode Reference - Arithmetic & Base Operations

**Navigation**: [Opcode Reference](opcodes.md) | [Instruction Format](instruction-format.md) | [Execution Semantics](execution-semantics.md)

Arithmetic, comparison, bitwise, base address, and miscellaneous opcodes.

## Arithmetic (0xD0-0xFF)

### Integer Arithmetic
- **IPLUS2 (0xD8)** [1] Pop 2, push sum. Signed 32-bit, overflow check.
- **IDIFFERENCE (0xD9)** [1] Pop 2, push diff (arg1 - arg2). Signed 32-bit.
- **ITIMES2 (0xDA)** [1] Pop 2, push product. Overflow check.
- **IQUO (0xDB)** [1] Pop 2, push quotient. Division by zero error.
- **IREM (0xDC)** [1] Pop 2, push remainder. Division by zero error.
- **IPLUS_N (0xDD)** [2] Add constant N to TOS.
  - Stack: `[tos] -> [tos + n]`
  - Operand: n (1B, constant to add)
  - Adds constant n to TOS value
  - Overflow checking matches C implementation
  - C: `N_OP_iplusn` in `maiko/src/arithops.c`
- **IDIFFERENCE_N (0xDE)** [2] Subtract constant N from TOS.
  - Stack: `[tos] -> [tos - n]`
  - Operand: n (1B, constant to subtract)
  - Subtracts constant n from TOS value
  - Overflow checking matches C implementation
  - C: `N_OP_idifferencen` in `maiko/src/arithops.c`
- **BOXIPLUS (0xDF)** [1] Add number to FIXP box in place.
  - Stack: `[number, fixp_box] -> [fixp_box]`
  - Adds number to FIXP box value directly (modifies box in place)
  - Returns box pointer (unchanged)
  - Used to avoid allocating new storage
  - C: `N_OP_boxiplus` in `maiko/src/arithops.c`
- **BOXIDIFFERENCE (0xE0)** [1] Subtract number from FIXP box in place.
  - Stack: `[number, fixp_box] -> [fixp_box]`
  - Subtracts number from FIXP box value directly (modifies box in place)
  - Returns box pointer (unchanged)
  - Used to avoid allocating new storage
  - C: `N_OP_boxidiff` in `maiko/src/arithops.c`

### General Arithmetic (Integer/Float)
- **PLUS2 (0xD4)** [1] Pop 2, push sum. Tries integer, falls back to float.
- **DIFFERENCE (0xD5)** [1] Pop 2, push diff. Integer or float.
- **TIMES2 (0xD6)** [1] Pop 2, push product. Integer or float.
- **QUOTIENT (0xD7)** [1] Pop 2, push quotient. Integer or float.

### Floating-Point Arithmetic
- **FPLUS2 (0xE8)** [1] Pop 2, convert to float, push sum.
- **FDIFFERENCE (0xE9)** [1] Pop 2, convert to float, push diff.
- **FTIMES2 (0xEA)** [1] Pop 2, convert to float, push product.
- **FQUOTIENT (0xEB)** [1] Pop 2, convert to float, push quotient.

### Comparisons
- **EQ (0xF0, also 0x3A)** [1] Pop 2, push (arg1 == arg2) ? T : NIL. Pointer equality.
  - **Stack**: `[a, b] -> [result]`
  - **Algorithm**: Returns T (1) if `a == b`, NIL (0) otherwise
  - **Note**: Atoms are interned, so pointer comparison is correct for atoms
- **EQL (0x3B)** [1] Pop 2, push equality (deep comparison).
  - **Stack**: `[a, b] -> [result]`
  - **Algorithm**: Recursively compares structures
    - Pointer equality check first (fast path)
    - Fixnums: Compare values directly
    - Cons cells: Recursively compare CAR and CDR
    - Atoms: Pointer comparison (atoms are interned)
    - Arrays: Pointer comparison (EQL uses EQ for arrays)
- **EQUAL (0xF4)** [1] Pop 2, push deep equality (recursive).
  - **Stack**: `[a, b] -> [result]`
  - **Algorithm**: Recursive comparison
    - Pointer equality check first
    - Fixnums: Compare values
    - Cons cells: Recursively compare CAR and CDR
    - Atoms: Pointer comparison (atoms are interned)
    - Arrays: Element-by-element comparison (recursive)
      - Compares array lengths first
      - Recursively compares each element using EQUAL
- **LESSP (0x92)** [1] Pop 2, push (arg1 < arg2) ? T : NIL.
  - **Stack**: `[a, b] -> [result]`
  - **Algorithm**: Compares as signed integers, returns T if `a < b`
- **GREATERP (0xF3)** [1] Pop 2, push (arg1 > arg2) ? T : NIL. Integer/float.
  - **Stack**: `[a, b] -> [result]`
  - **Algorithm**: Compares as signed integers, returns T if `a > b`
- **IGREATERP (0xF1)** [1] Pop 2, push (arg1 > arg2) ? T : NIL. Integer only.
  - **Stack**: `[a, b] -> [result]`
  - **Algorithm**: Same as GREATERP (integer comparison)
- **FGREATERP (0xF2)** [1] Pop 2, push (arg1 > arg2) ? T : NIL. Float only.
- **LEQ (0x3E)** [1] Pop 2, push (arg1 <= arg2) ? T : NIL.
- **GEQ (0x3F)** [1] Pop 2, push (arg1 >= arg2) ? T : NIL.
- **NUMEQUAL (0x3D)** [1] Pop 2, push numeric equality.
- **CL_EQUAL (0xFF)** [1] Pop 2, push case-insensitive equality.

### Bitwise Operations
- **LOGOR2 (0xE4)** [1] Pop 2, push bitwise OR.
- **LOGAND2 (0xE5)** [1] Pop 2, push bitwise AND.
- **LOGXOR2 (0xE6)** [1] Pop 2, push bitwise XOR.
- **LOGNOT (0xE7)** [1] TOS = bitwise NOT of TOS.
- **LSH (0xE7)** [1] Pop shift, value. Push shifted result. Left if +, right if -.

### Shift Operations
- **LLSH1 (0xE0)** [1] TOS = TOS << 1.
- **LLSH8 (0xE1)** [1] TOS = TOS << 8.
- **LRSH1 (0xE2)** [1] TOS = (unsigned)TOS >> 1.
- **LRSH8 (0xE3)** [1] TOS = (unsigned)TOS >> 8.

## Constants (0x67-0x6F)

- **ACONST (0x67)** [3] Atom index (2B). Push atom constant.
- **NIL (0x68)** [1] Push NIL.
- **T (0x69)** [1] Push T.
- **CONST_0 (0x6A)** [1] Push integer 0.
- **CONST_1 (0x6B)** [1] Push integer 1.
- **SIC (0x6C)** [2] Value (1B). Push small positive integer (S_POSITIVE | value).
- **SNIC (0x6D)** [2] Value (1B). Push small negative integer (S_NEGATIVE | 0xFF00 | value).
- **SICX (0x6E)** [3] Value (2B). Push extended small integer.
- **GCONST (0x6F)** [3] Atom index (2B). Push global constant.

## Base Address Operations (0xC2-0xCE)

Base operations access memory at a base address plus an offset. All base addresses are masked with `POINTERMASK` (0xfffffff for BIGVM, 0xffffff for non-BIGVM) to extract the pointer portion.

- **GETBASEBYTE (0xC2)** [1] Read byte from memory.
  - Stack: `[byteoffset, base] -> [byte_value]`
  - `byteoffset` must be small integer (S_POSITIVE/S_NEGATIVE) or FIXP object
  - Reads byte at `(POINTERMASK & base) + byteoffset`
  - Returns `S_POSITIVE | (0xFF & byte_value)`
  - If `byteoffset` is not valid, triggers UFN lookup

- **PUTBASEBYTE (0xC7)** [1] Write byte to memory.
  - Stack: `[value, byteoffset, base] -> []`
  - `value` must be `S_POSITIVE` and `< 256`
  - `byteoffset` must be small integer (S_POSITIVE/S_NEGATIVE)
  - Writes `0xFF & value` to `(POINTERMASK & base) + byteoffset`
  - If validation fails, triggers UFN lookup

- **GETBASE_N (0xC8)** [2] Read DLword from memory.
  - Stack: `[base] -> [value]`
  - Index: 1-byte operand
  - Reads DLword (2 bytes, big-endian) from `(POINTERMASK & base) + index`
  - Returns `S_POSITIVE | word_value`

- **GETBASEPTR_N (0xC9)** [2] Read LispPTR from memory.
  - Stack: `[base] -> [pointer]`
  - Index: 1-byte operand
  - Reads LispPTR (4 bytes, big-endian) from `(POINTERMASK & base) + index`
  - Returns `POINTERMASK & pointer_value`

- **PUTBASE_N (0xCD)** [2] Write DLword to memory.
  - Stack: `[value, base] -> [base]`
  - Index: 1-byte operand
  - `value` must be `S_POSITIVE` (small integer)
  - Writes `GetLoWord(value)` as DLword (2 bytes, big-endian) to `(POINTERMASK & base) + index`
  - Pushes `base` back on stack
  - If validation fails, triggers UFN lookup

- **PUTBASEPTR_N (0xCE)** [2] Write LispPTR to memory.
  - Stack: `[pointer, base] -> [base]`
  - Index: 1-byte operand
  - Writes `POINTERMASK & pointer` as LispPTR (4 bytes, big-endian) to `(POINTERMASK & base) + index`
  - Pushes `base` back on stack

- **GETBITS_N_FD (0xCA)** [3] Extract bit field from memory.
  - Stack: `[base] -> [bit_field]`
  - Operands: offset (1B), field descriptor (1B)
  - Field descriptor format: `[shift:4][size:4]` (high 4 bits = shift position, low 4 bits = field size)
  - Reads DLword from `(POINTERMASK & base) + offset`
  - Extracts bit field: `(word >> (16 - shift - size - 1)) & mask`
  - Returns `S_POSITIVE | bit_field`

- **PUTBITS_N_FD (0xCB)** [3] Write bit field to memory.
  - Stack: `[value, base] -> [base]`
  - Operands: offset (1B), field descriptor (1B)
  - `value` must be `S_POSITIVE` (small integer)
  - Field descriptor format: `[shift:4][size:4]`
  - Reads DLword from `(POINTERMASK & base) + offset`
  - Updates bit field: `(word & ~mask) | (value << shift)`
  - Writes updated DLword back
  - Pushes `base` back on stack
  - If validation fails, triggers UFN lookup

## Address Manipulation

- **ADDBASE (0xD0)** [1] Add two base addresses.
  - Stack: `[b, a] -> [a + b]`
  - Applies `POINTERMASK` to both operands before addition
  - Returns `POINTERMASK & (a_ptr + b_ptr)`

- **HILOC (0xD2)** [1] Extract high 16 bits.
  - Stack: `[value] -> [high_word]`
  - Returns `S_POSITIVE | GetHiWord(value)` where `GetHiWord(x) = (x >> 16) & 0xFFFF`

- **LOLOC (0xD3)** [1] Extract low 16 bits.
  - Stack: `[value] -> [low_word]`
  - Returns `S_POSITIVE | GetLoWord(value)` where `GetLoWord(x) = x & 0xFFFF`

- **BASE_LESSTHAN (0xCF)** [1] Compare base addresses.
  - Stack: `[b, a] -> [a < b ? T : NIL]`
  - Applies `POINTERMASK` to both operands before comparison
  - Returns `S_POSITIVE | 1` (T) if `(POINTERMASK & a) < (POINTERMASK & b)`, else `0` (NIL)

## GC Operations

- **GCREF (0x15)** [2] Ref type (1B). TOS = object. ADDREF/DELREF/STKREF based on type.

## Miscellaneous

- **COPY (0x64)** [1] Push copy of TOS.
- **SWAP (0xFD)** [1] Swap top two stack elements.
- **NOP (0xFE)** [1] No operation.
- **MAKENUMBER (0xF5)** [1] Convert TOS to proper number format.
- **MYALINK (0x64)** [1] Push activation link address (previous frame pointer).
  - **Stack**: `[] -> [alink_address]`
  - **C**: `MYALINK: PUSH((((CURRENTFX->alink) & 0xfffe) - FRAMESIZE) | S_POSITIVE);`
  - **Algorithm**:
    1. Get `alink` from current frame (`frame.link`)
    2. Clear LSB: `alink & 0xfffe` (or `alink & 0xFFFFFFFE` for full 32-bit)
    3. Subtract FRAMESIZE: `(alink & 0xfffe) - FRAMESIZE` (FRAMESIZE = 10 DLwords = 20 bytes)
    4. Apply segment mask: `result | S_POSITIVE`
  - **Purpose**: Returns address of previous frame (activation link)
  - **FRAMESIZE**: 10 DLwords = 20 bytes (size of frame header structure)
  - **C Reference**: `MYALINK` macro in `maiko/inc/inlineC.h`

- **MYARGCOUNT (0x65)** [1] Push argument count of current function.
  - **Stack**: `[] -> [arg_count]`
  - **C**: `MYARGCOUNT: PUSH((DLword)((arg_num - (UNSIGNED)IVar) >> 2) | S_POSITIVE);`
  - **Algorithm**:
    1. Check `alink` LSB: `if ((CURRENTFX->alink & 1) == 0)`
    2. Calculate `arg_num`:
       - If LSB is 0: `arg_num = (UNSIGNED)((LispPTR *)(CURRENTFX) - 1)`
       - If LSB is 1: `arg_num = (UNSIGNED)(Stackspace + CURRENTFX->blink)`
    3. Get IVar base: `IVar = frame.nextblock`
    4. Calculate count: `arg_count = (arg_num - IVar) >> 2` (divide by 4 for LispPTR units)
    5. Apply segment mask: `result | S_POSITIVE`
  - **Purpose**: Returns number of arguments passed to current function
  - **C Reference**: `MYARGCOUNT` macro in `maiko/inc/inlineC.h`
- **STKSCAN (0x2F)** [1] Pop target. Push T if found in stack, else NIL.

## Related Documentation

- [Opcode Reference](opcodes.md) - Complete opcode index
- [Control Flow & Memory Operations](opcodes-control-memory.md) - Control flow and memory opcodes
- [Data Operations](opcodes-data.md) - Data operation opcodes
