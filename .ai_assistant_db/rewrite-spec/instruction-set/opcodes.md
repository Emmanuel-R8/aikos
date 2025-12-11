# Opcode Reference

**Navigation**: [README](README.md) | [Instruction Format](instruction-format.md) | [Execution Semantics](execution-semantics.md)

Complete specification of all 256 bytecode opcodes (0x00-0xFF). Format: `Name (0xXX) [Len] [Ops] Stack: [effect] Exec: [brief]`

## Control Flow (0x00-0x3F)

### Function Calls
- **FN0 (0x08)** [3] Calls 0-arg function. Format: [opcode][atom_index:2B]. Atom index is 2 bytes (DLword) for non-BIGATOMS, 3-4 bytes for BIGATOMS. Creates frame.
- **FN1 (0x09)** [3] Calls 1-arg function. Format: [opcode][atom_index:2B]. Creates frame.
- **FN2-FN4 (0x0A-0x0C)** [3] Calls 2-4 arg function. Format: [opcode][atom_index:2B]. Creates frame.
- **FNX (0x0D)** [4-5] Variable argument count. Format: [opcode][atom_index:2-3B][arg_count:1B]. Atom index size depends on BIGATOMS setting.
- **APPLYFN (0x0E)** [1] Apply function to arg list on stack.
- **CHECKAPPLY (0x0F)** [1] Validate apply args before apply.

### Returns
- **RETURN (0x10)** [1] Pop frame, restore PC, return value on TOS.
- **SLRETURN (0x3F)** [1] Soft return (different frame handling).

### Jumps
- **JUMP0-JUMP15 (0x80-0x8F)** [1] Unconditional jump, offset encoded in opcode (0-15). Stack: No effect.
- **FJUMP0-FJUMP15 (0x90-0x9F)** [1] Jump if false (NIL), offset 0-15. Stack: Always pops TOS (pops regardless of condition).
- **TJUMP0-TJUMP15 (0xA0-0xAF)** [1] Jump if true (non-NIL), offset 0-15. Stack: Always pops TOS (pops regardless of condition).
- **JUMPX (0xB0)** [3] Unconditional jump, 16-bit signed offset. Stack: No effect.
- **FJUMPX (0xB2)** [3] Jump if false, 16-bit offset. Stack: Always pops TOS.
- **TJUMPX (0xB3)** [3] Jump if true, 16-bit offset. Stack: Always pops TOS.
- **NFJUMPX (0xB4), NTJUMPX (0xB5)** [3] Negated variants.

**Critical Stack Behavior for Conditional Jumps**:
- FJUMP variants: Always pop TOS before checking condition. If TOS is NIL, jump; otherwise continue.
- TJUMP variants: Always pop TOS before checking condition. If TOS is non-NIL, jump; otherwise continue.
- This matches C implementation: `FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }`
- The stack is popped in both branches of the conditional, ensuring TOS is always consumed.

### Other Control
- **UNWIND (0x07)** [3] Unwind stack to specified frame.
- **BIND (0x11)** [2] Bind variables in PVAR area.
  - **Operands**: byte1 (n1:4, n2:4), byte2 (offset)
  - **Stack**: `[values..., TOS] -> [marker]`
  - **Algorithm**: 
    - Calculates `ppvar = (LispPTR *)PVAR + 1 + offset`
    - Pushes `n1` NIL values backwards from `ppvar`
    - If `n2 == 0`: pushes TOS onto stack
    - Otherwise: stores TOS and `n2-1` more values backwards from `ppvar`
    - Sets TOS to marker: `((~(n1 + n2)) << 16) | (offset << 1)`
  - **Marker format**: High 16 bits = `~(n1 + n2)`, low 16 bits = `offset << 1`
- **UNBIND (0x12)** [1] Unbind variables in reverse bind order.
  - **Stack**: `[marker, ...] -> []`
  - **Algorithm**:
    - Walks backwards through stack until finding negative value (marker)
    - Extracts `num = (~marker) >> 16` and `offset = GetLoWord(marker) >> 1`
    - Calculates `ppvar = (LispPTR *)((DLword *)PVAR + 2 + offset)`
    - Restores `num` values to `0xFFFFFFFF` (unbound marker) backwards from `ppvar`
    - Pops marker from stack
- **DUNBIND (0x13)** [1] Dynamic unbind.
  - **Stack**: `[marker, ...]` or `[TOS] -> []`
  - **Algorithm**: Similar to UNBIND, but checks TOS first
    - If TOS is negative (marker): uses TOS as marker directly
    - Otherwise: walks backwards to find marker (same as UNBIND)
    - Restores variables and pops marker

## Memory Operations (0x40-0x7F)

### Variable Access
- **IVAR0-IVAR6 (0x40-0x46)** [1] Push local variable 0-6.
  - Uses LispPTR offset (index * 4 bytes)
- **IVARX (0x47)** [2] Push indexed local variable.
  - **Stack**: `[] -> [value]`
  - **Operand**: `x` (1B, DLword offset)
  - **CRITICAL**: Uses DLword offset, NOT LispPTR offset
  - **C**: `IVARX(x): PUSH(GetLongWord((DLword *)IVAR + (x)));`
  - **Access**: Reads LispPTR from `(DLword *)IVAR + x` (x is in DLword units)
  - **IVAR Base**: `IVAR` is `frame.nextblock` (LispPTR address, must be translated to native)
  - **Element Size**: Reads 2 DLwords (4 bytes) as LispPTR using `GetLongWord()`
  - **Byte Order**: Handles big-endian byte order from sysout format
- **PVAR0-PVAR6 (0x48-0x4E)** [2] Push parameter 0-6.
  - Uses LispPTR offset (index * 4 bytes)
- **PVARX (0x4F)** [2] Push indexed parameter.
  - **Stack**: `[] -> [value]`
  - **Operand**: `x` (1B, DLword offset)
  - **CRITICAL**: Uses DLword offset, NOT LispPTR offset
  - **C**: `PVARX(x): PUSH(GetLongWord((DLword *)PVAR + (x)));`
  - **Access**: Reads LispPTR from `(DLword *)PVAR + x` (x is in DLword units)
  - **PVAR Base**: `PVAR` starts after frame header (FRAMESIZE bytes)
  - **Element Size**: Reads 2 DLwords (4 bytes) as LispPTR using `GetLongWord()`
  - **Byte Order**: Handles big-endian byte order from sysout format
- **FVAR0-FVAR6 (0x50-0x56)** [1] Push free variable 0-6.
- **FVARX (0x57)** [2] Push indexed free variable.
- **PVAR_0-PVAR_6 (0x58-0x5E)** [1] Alternative PVAR access.
- **PVARX_ (0x5F)** [2] Alternative indexed PVAR.
  - **Stack**: `[value] -> []`
  - **Operand**: `x` (1B, DLword offset)
  - **CRITICAL**: Uses DLword offset, NOT LispPTR offset
  - **C**: `PVARX_(x): *((LispPTR *)((DLword *)PVAR + (x))) = TOPOFSTACK;`
  - **Access**: Writes LispPTR to `(DLword *)PVAR + x` (x is in DLword units)
  - **Byte Order**: Writes in big-endian byte order for sysout format
- **GVAR (0x60)** [3] Atom index (2B). Push global variable value.
- **ARG0 (0x61)** [1] Push argument 0.
- **IVARX_ (0x62)** [2] Set indexed local variable.
  - **Stack**: `[value] -> []`
  - **Operand**: `x` (1B, DLword offset)
  - **CRITICAL**: Uses DLword offset, NOT LispPTR offset
  - **C**: `IVARX_(x): *((LispPTR *)((DLword *)IVAR + (x))) = TOPOFSTACK;`
  - **Access**: Writes LispPTR to `(DLword *)IVAR + x` (x is in DLword units)
  - **IVAR Base**: `IVAR` is `frame.nextblock` (LispPTR address, must be translated to native)
  - **Byte Order**: Writes in big-endian byte order for sysout format
- **FVARX_ (0x63)** [2] Set indexed free variable.

### Variable Setting
- **PVARSETPOP0-PVARSETPOP6 (0xB8-0xBE)** [1] Set param N, pop value.

### Stack Operations
- **POP (0xBF)** [1] Discard TOS.
- **POP_N (0xC0)** [2] Pop N values (count in operand).
- **PUSH/ADDBASE (0xD0)** [1] Push value onto stack.

## Data Operations (0x00-0x3F, 0x80-0xBF)

### Cons Operations
- **CAR (0x01)** [1] TOS = CAR(TOS). Handles CDR_INDIRECT.
  - **Validation**: Must check `Listp(TOS)` before accessing. If not a list, trigger UFN (undefined function name) lookup.
  - **Special case**: CAR of T (ATOM_T = 1) returns T.
  - **CDR_INDIRECT**: If `cdr_code == CDR_INDIRECT`, CAR value points to indirect cell containing actual CAR.
- **CDR (0x02)** [1] TOS = CDR(TOS). Uses CDR coding.
  - **Validation**: Must check `Listp(TOS)` before accessing. If not a list, trigger UFN lookup.
  - **NIL handling**: CDR of NIL returns NIL (no validation needed for NIL).
- **CONS (0x1A)** [1] Pop CDR, CAR. Push new cons cell. Allocates memory.
- **RPLACA (0x18)** [1] Pop new CAR, cons ptr. Modify CAR. Push cons ptr.
- **RPLACD (0x19)** [1] Pop new CDR, cons ptr. Modify CDR (CDR coding). Push cons ptr.
- **CREATECELL (0x1F)** [1] Pop type code. Allocate cell from DTD. Push address.
- **RPLPTR_N (0x24)** [2] Replace pointer at offset N.
  - Stack: `[new_value, base] -> [base]`
  - Operand: offset (1B)
  - Replaces pointer at `base + offset` with new value
  - Updates GC refs: DELREF old value, ADDREF new value (matches C FRPLPTR behavior)
  - Returns base on stack
  - C: `N_OP_rplptr` in `maiko/src/gvar2.c`

### Array Operations
- **AREF1 (0xB6)** [1] Pop index, array. Push element.
  - **Stack**: `[index, array_ptr] -> [element_value]`
  - **Type Check**: Must verify `GetTypeNumber(array_ptr) == TYPE_ONED_ARRAY` (14)
  - **Structure**: Accesses `OneDArray` structure at `array_ptr`
    - `base`: Base address (24 bits for non-BIGVM, 28 bits for BIGVM)
    - `offset`: Offset into array (DLword units)
    - `totalsize`: Total array size (DLword units)
    - `typenumber`: Element type number (determines element size and encoding)
  - **Index Validation**: 
    - Index must be in `S_POSITIVE` segment (non-negative)
    - Index must be `< totalsize`
    - Final index = `index + offset`
  - **Type Dispatch**: Element access depends on `typenumber`:
    - `38` (TYPE_POINTER): 32-bit pointer elements (LispPTR)
    - `20` (TYPE_SIGNED_16): 16-bit signed integers (DLword, sign-extended)
    - `22` (TYPE_SIGNED_32): 32-bit signed integers (LispPTR, may be boxed as FIXP)
    - `67` (TYPE_CHARACTER): 8-bit character elements (with S_CHARACTER tag)
    - `0` (TYPE_UNSIGNED_1BIT): 1-bit per element (bit array)
    - `3` (TYPE_UNSIGNED_8BIT): 8-bit unsigned elements
    - Other types: See C implementation `aref_switch()` in `maiko/src/my.c`
  - **C Reference**: `N_OP_aref1` in `maiko/src/arrayops.c`, `AREF1` macro in `maiko/inc/inlineC.h`

- **AREF2 (0xEE)** [1] Pop index1, index0, array. Push element (2D).
  - Uses `LispArray` structure (multi-dimensional)
  - Calculates linear index: `(index0 * Dim1) + index1`

- **ASET1 (0xB7)** [1] Pop value, index, array. Set element. Push array.
  - **Stack**: `[value, index, array_ptr] -> [array_ptr]`
  - **Type Check**: Must verify `GetTypeNumber(array_ptr) == TYPE_ONED_ARRAY` (14)
  - **Read-only Check**: If `array.readonlyp == 1`, trigger error
  - **Index Validation**: Same as AREF1
  - **Type Dispatch**: Element write depends on `typenumber`:
    - `38` (TYPE_POINTER): Write full LispPTR value
    - `20` (TYPE_SIGNED_16): Write low 16 bits of value
    - `67` (TYPE_CHARACTER): Write low 8 bits of value
    - Other types: See C implementation `aset_switch()` in `maiko/src/my.c`
  - **C Reference**: `N_OP_aset1` in `maiko/src/arrayops.c`, `ASET1` macro in `maiko/inc/inlineC.h`

- **ASET2 (0xEF)** [1] Pop value, index1, index0, array. Set element. Push array.
  - Uses `LispArray` structure (multi-dimensional)

### Type Operations
- **NTYPX (0x04)** [1] TOS = type number of TOS.
  - Returns type number (0-2047) from type table: `GetTypeNumber(TOS) = GetTypeEntry(TOS) & 0x7ff`
  - Type entry read from: `MDStypetbl + (TOS >> 9)`
- **TYPEP (0x05)** [2] Type code (1B). TOS = (GetType(TOS) == code) ? T : NIL.
  - Compares type number of TOS with operand type code.
  - Uses `GetTypeNumber(TOS)` to get type number.
- **DTEST (0x06)** [3] Atom index (2B). TOS = (TOS has type named by atom_index) ? T : NIL.
  - **Implementation**: Walks DTD (Data Type Descriptor) chain starting from `GetDTD(GetTypeNumber(TOS))`.
  - Compares `dtd->dtd_name` with `atom_index` (or `dtd->dtd_namelo + (dtd->dtd_namehi << 16)` for non-BIGVM).
  - If no match found in chain (`dtd->dtd_supertype == 0`), triggers UFN lookup.
  - Returns ATOM_T (1) if match found, NIL_PTR (0) otherwise.
- **STRINGP (0xA3)** [1] TOS = IsString(TOS) ? T : NIL.
- **ARRAYP (0xA4)** [1] TOS = IsArray(TOS) ? T : NIL.
- **CHARACTERP (0xA5)** [1] TOS = IsCharacter(TOS) ? T : NIL.

**Type Checking Functions**:
- `Listp(address)`: Returns true if `GetTypeNumber(address) == TYPE_LISTP` (5)
- `GetTypeNumber(address)`: Returns type number from type table (low 11 bits of type entry)
- `GetTypeEntry(address)`: Returns full 16-bit type entry from `MDStypetbl + (address >> 9)`

**Note**: `FIXP` (0xA0), `SMALLP` (0xA1), and `LISTP` (0xA2) do not exist as separate opcodes. These values are used by `TJUMP0`-`TJUMP2`. Use `TYPEP` with appropriate type codes instead.

### List/Atom Operations
- **ASSOC (0x16)** [1] Association list lookup.
  - Stack: `[key, alist] -> [pair or NIL]`
  - Traverses association list (list of (key . value) pairs)
  - Compares keys using EQ (pointer equality)
  - Returns matching pair if found, NIL otherwise
  - C: `N_OP_assoc` in `maiko/src/vars3.c`

- **FMEMB (0x1C)** [1] Fast member test.
  - Stack: `[item, list] -> [sublist or NIL]`
  - Tests if item is in list using pointer equality (EQ)
  - Returns list starting from item if found, NIL otherwise
  - C: `N_OP_fmemb` in `maiko/src/lsthandl.c`

- **RESTLIST (0x23)** [2] Rest of list.
  - Stack: `[tail] -> [result_list]`
  - Operand: count (1B)
  - Builds list by consing elements (C implementation uses IVar array)
  - Simplified: traverses list count times using CDR
  - C: `N_OP_restlist` in `maiko/src/z2.c`

- **RPLCONS (0x26)** [1] Replace cons CDR.
  - Stack: `[new_cdr, list] -> [list]`
  - Replaces CDR of cons cell with new value
  - Updates GC refs: DELREF old CDR, ADDREF new CDR
  - Returns list (unchanged pointer)
  - C: `N_OP_rplcons` in `maiko/src/rplcons.c`

- **LISTGET (0x27)** [1] Get element from list by index.
  - Stack: `[index, list] -> [element]`
  - Traverses list index times using CDR
  - Returns CAR of current position
  - Returns NIL if index out of bounds
  - C: `N_OP_listget` in `maiko/src/lsthandl.c`

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

## Unused Opcodes

- `0x00` (unused_0)
- `0x25` (unused_37)
- `0x28-0x2B` (unused_40-43)
- `0x70` (unused_112)
- `0xCB` (unused_203)

Unused opcodes trigger UFN (Undefined Function Name) handling.

## Common Misconceptions: Non-existent Opcodes

**CRITICAL**: The following opcodes are commonly assumed but **DO NOT EXIST** in the Maiko VM instruction set. Implementors should use the correct alternatives listed below.

### Generic Jump Opcodes

**Myth**: Generic `JUMP`, `FJUMP`, `TJUMP` opcodes exist.

**Reality**: Only specific variants exist:
- **JUMP variants**: `JUMP0`-`JUMP15` (0x80-0x8F), `JUMPX` (0xB0), `JUMPXX` (0xB1)
- **FJUMP variants**: `FJUMP0`-`FJUMP15` (0x90-0x9F), `FJUMPX` (0xB2)
- **TJUMP variants**: `TJUMP0`-`TJUMP15` (0xA0-0xAF), `TJUMPX` (0xB3)

**Why**: The optimized variants (`JUMP0`-`JUMP15`, etc.) encode small offsets directly in the opcode, reducing instruction size for common cases.

### List Creation Opcodes

**Myth**: `LIST` and `APPEND` opcodes exist for list creation and concatenation.

**Reality**: These opcodes do not exist in the Maiko VM instruction set:
- **LIST opcode**: Does not exist. Lists are created using the `CONS` opcode (0x1A) repeatedly.
- **APPEND opcode**: Does not exist. List concatenation is handled via:
  - `RESTLIST` opcode (0x23) for list traversal
  - `RPLCONS` opcode (0x26) for cons cell manipulation
  - Lisp-level functions implemented in the Lisp runtime

**Why**: List construction is typically done via repeated `CONS` operations, which is more efficient for the VM's execution model. List concatenation is handled at the Lisp level rather than as a primitive VM operation.

**Verification**: Confirmed by examining `maiko/inc/opcodes.h` - no `opc_LIST` or `opc_APPEND` enum values exist.

### Character Opcodes

**Myth**: `CHARCODE` and `CHARN` opcodes exist for character operations.

**Reality**: These opcodes do not exist. Character operations are handled through other mechanisms or type-specific operations.

**Note**: The opcode values 0xB4 and 0xB5 are used by `NFJUMPX` and `NTJUMPX` respectively.

### Array Element Access Opcodes

**Myth**: `GETAEL1`, `GETAEL2`, `SETAEL1`, `SETAEL2` opcodes exist for array element access.

**Reality**: Use the correct array operations:
- **Array read**: `AREF1` (0xB6) for 1D arrays, `AREF2` (0xEE) for 2D arrays
- **Array write**: `ASET1` (0xB7) for 1D arrays, `ASET2` (0xEF) for 2D arrays

**Note**: The opcode values 0x80-0x83 are used by `JUMP0`-`JUMP3` respectively.

### Type Checking Opcodes

**Note**: Type checking predicates exist and use `GetTypeNumber` to check types:
- **LISTP**: Checks if `GetTypeNumber(value) == TYPE_LISTP` (type 5)
  - Returns T if value is a list (cons cell), NIL otherwise
  - C: `LISTP` macro in `maiko/inc/inlineC.h`
- **FIXP**: Checks if `GetTypeNumber(value) == TYPE_FIXP` (type 2)
  - Returns T if value is a FIXP (boxed integer), NIL otherwise
  - Distinguishes FIXP (boxed) from SMALLP (directly encoded)
- **SMALLP**: Checks if value has `S_POSITIVE` or `S_NEGATIVE` segment mask, or `GetTypeNumber(value) == TYPE_SMALLP` (type 1)
  - Returns T if value is a SMALLP (small integer encoded directly), NIL otherwise
  - Small integers are encoded with segment masks: `S_POSITIVE` (0xE0000) or `S_NEGATIVE` (0xF0000)

**Important**: These are type predicates that check the type of a value, not opcodes themselves. They are implemented as part of the type checking system.

### Stack Push Opcode

**Myth**: A generic `PUSH` opcode exists for pushing values onto the stack.

**Reality**: Stack operations are handled implicitly by other opcodes. The opcode value 0xD0 is used by `ADDBASE`, not `PUSH`.

**Alternatives**: Most opcodes that produce values implicitly push them onto the stack. For explicit stack manipulation, use:
- Variable access opcodes (`IVAR`, `PVAR`, `FVAR`, `GVAR`) - push variable values
- Constant opcodes (`NIL`, `T`, `CONST_0`, `CONST_1`, `ACONST`, `GCONST`) - push constants
- Arithmetic/operation opcodes - push results

### Verification Checklist

When implementing opcodes, verify against `maiko/inc/opcodes.h`:
1. ✅ Check that opcode value matches C enum value exactly
2. ✅ Verify opcode name matches C enum name exactly
3. ✅ Confirm opcode exists in C implementation (not just assumed)
4. ✅ Cross-reference instruction length and operand format
5. ✅ Test opcode decoding matches C implementation behavior

## Opcode Length Reference

**Format**: `[Len]` = instruction length in bytes
- `[1]` = opcode only
- `[2]` = opcode + 1 byte operand
- `[3]` = opcode + 2 byte operands
- `[3-4]` = 3 bytes (2B atom index) or 4 bytes (3B atom index with BIGATOMS)

See [Instruction Format](instruction-format.md) for complete length table.

## Common Patterns

**Stack Effects**: `Pop N` = remove N values, `Push` = add value, `TOS` = top of stack
**Error Handling**: Invalid types/values trigger ERROR_EXIT (UFN call)
**Memory**: Allocations may trigger GC
**CDR Coding**: See [Cons Cells](../data-structures/cons-cells.md) for CDR encoding details
