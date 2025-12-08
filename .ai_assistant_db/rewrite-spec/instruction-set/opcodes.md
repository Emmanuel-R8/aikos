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
- **BIND (0x11)** [2] Bind N variables (count in operand).
- **UNBIND (0x12)** [1] Unbind variables in reverse order.
- **DUNBIND (0x13)** [1] Dynamic unbind.

## Memory Operations (0x40-0x7F)

### Variable Access
- **IVAR0-IVAR6 (0x40-0x46)** [1] Push local variable 0-6.
- **IVARX (0x47)** [2] Push indexed local variable.
- **PVAR0-PVAR6 (0x48-0x4E)** [2] Push parameter 0-6.
- **PVARX (0x4F)** [2] Push indexed parameter.
- **FVAR0-FVAR6 (0x50-0x56)** [1] Push free variable 0-6.
- **FVARX (0x57)** [2] Push indexed free variable.
- **PVAR_0-PVAR_6 (0x58-0x5E)** [1] Alternative PVAR access.
- **PVARX_ (0x5F)** [2] Alternative indexed PVAR.
- **GVAR (0x60)** [3] Atom index (2B). Push global variable value.
- **ARG0 (0x61)** [1] Push argument 0.
- **IVARX_ (0x62), FVARX_ (0x63)** [2] Set variants.

### Variable Setting
- **PVARSETPOP0-PVARSETPOP6 (0xB8-0xBE)** [1] Set param N, pop value.

### Stack Operations
- **POP (0xBF)** [1] Discard TOS.
- **POP_N (0xC0)** [2] Pop N values (count in operand).
- **PUSH/ADDBASE (0xD0)** [1] Push value onto stack.

## Data Operations (0x00-0x3F, 0x80-0xBF)

### Cons Operations
- **CAR (0x01)** [1] TOS = CAR(TOS). Handles CDR_INDIRECT.
- **CDR (0x02)** [1] TOS = CDR(TOS). Uses CDR coding.
- **CONS (0x1A)** [1] Pop CDR, CAR. Push new cons cell. Allocates memory.
- **RPLACA (0x18)** [1] Pop new CAR, cons ptr. Modify CAR. Push cons ptr.
- **RPLACD (0x19)** [1] Pop new CDR, cons ptr. Modify CDR (CDR coding). Push cons ptr.
- **CREATECELL (0x1F)** [1] Pop type code. Allocate cell from DTD. Push address.
- **RPLCONS (0x26)** [1] CONS + RPLACA combination.

### Array Operations
- **AREF1 (0xB6)** [1] Pop index, array. Push element.
- **AREF2 (0xEE)** [1] Pop index1, index0, array. Push element (2D).
- **ASET1 (0xB7)** [1] Pop value, index, array. Set element. Push array.
- **ASET2 (0xEF)** [1] Pop value, index1, index0, array. Set element. Push array.

### Type Operations
- **NTYPX (0x04)** [1] TOS = type number of TOS.
- **TYPEP (0x05)** [2] Type code (1B). TOS = (GetType(TOS) == code) ? T : NIL.
- **DTEST (0x06)** [3] Atom index (2B). TOS = (TOS == GetAtom(index)) ? T : NIL.
- **STRINGP (0xA3)** [1] TOS = IsString(TOS) ? T : NIL.
- **ARRAYP (0xA4)** [1] TOS = IsArray(TOS) ? T : NIL.
- **CHARACTERP (0xA5)** [1] TOS = IsCharacter(TOS) ? T : NIL.

**Note**: `FIXP` (0xA0), `SMALLP` (0xA1), and `LISTP` (0xA2) do not exist as separate opcodes. These values are used by `TJUMP0`-`TJUMP2`. Use `TYPEP` with appropriate type codes instead.

### List/Atom Operations
- **ASSOC (0x16)** [1] Pop key, alist. Push (key, value) pair or NIL.
- **FMEMB (0x1C)** [1] Pop item, list. Push T if member, else NIL.
- **LISTGET (0x27)** [1] Pop index, list. Push Nth element.

## Arithmetic (0xD0-0xFF)

### Integer Arithmetic
- **IPLUS2 (0xD8)** [1] Pop 2, push sum. Signed 32-bit, overflow check.
- **IDIFFERENCE (0xD9)** [1] Pop 2, push diff (arg1 - arg2). Signed 32-bit.
- **ITIMES2 (0xDA)** [1] Pop 2, push product. Overflow check.
- **IQUO (0xDB)** [1] Pop 2, push quotient. Division by zero error.
- **IREM (0xDC)** [1] Pop 2, push remainder. Division by zero error.

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
- **EQL (0x3B)** [1] Pop 2, push equality (similar to EQ).
- **EQUAL (0xF4)** [1] Pop 2, push deep equality (recursive).
- **LESSP (0x92)** [1] Pop 2, push (arg1 < arg2) ? T : NIL.
- **GREATERP (0xF3)** [1] Pop 2, push (arg1 > arg2) ? T : NIL. Integer/float.
- **IGREATERP (0xF1)** [1] Pop 2, push (arg1 > arg2) ? T : NIL. Integer only.
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

- **GETBASEBYTE (0xC2)** [1] Pop offset, base. Push byte at base+offset.
- **PUTBASEBYTE (0xC7)** [1] Pop value, offset, base. Write byte. Push value.
- **GETBASE_N (0xC8)** [2] Index (1B). Pop base. Push value at base[index].
- **GETBASEPTR_N (0xC9)** [2] Index (1B). Pop base. Push pointer at base[index].
- **PUTBASE_N (0xCD)** [2] Index (1B). Pop value, base. Write value. Push base.
- **PUTBASEPTR_N (0xCE)** [2] Index (1B). Pop ptr, base. Write pointer. Push base.

## Address Manipulation

- **ADDBASE (0xD0)** [1] Same as PUSH.
- **HILOC (0xD2)** [1] TOS = (TOS >> 16) & 0xFFFF (high 16 bits).
- **LOLOC (0xD3)** [1] TOS = TOS & 0xFFFF (low 16 bits).

## GC Operations

- **GCREF (0x15)** [2] Ref type (1B). TOS = object. ADDREF/DELREF/STKREF based on type.

## Miscellaneous

- **COPY (0x64)** [1] Push copy of TOS.
- **SWAP (0xFD)** [1] Swap top two stack elements.
- **NOP (0xFE)** [1] No operation.
- **MAKENUMBER (0xF5)** [1] Convert TOS to proper number format.
- **MYARGCOUNT (0x65)** [1] Push argument count of current function.
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

**Myth**: Separate `FIXP`, `SMALLP`, and `LISTP` opcodes exist for type checking.

**Reality**: These opcodes do not exist. Use the `TYPEP` opcode (0x05) with appropriate type codes:
- **FIXP**: Use `TYPEP` with fixnum type code, or check if value is a fixnum directly
- **SMALLP**: Use `TYPEP` with small integer type code
- **LISTP**: Use `TYPEP` with list type code

**Note**: The opcode values 0xA0-0xA2 are used by `TJUMP0`-`TJUMP2` respectively. The documentation previously listed `LISTP` (0xA2), `FIXP` (0xA0), and `SMALLP` (0xA1) as valid opcodes, but these conflict with the jump opcodes and do not exist in `maiko/inc/opcodes.h`.

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
