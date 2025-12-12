# Opcode Reference - Reference Information

**Navigation**: [Opcode Reference](opcodes.md) | [Instruction Format](instruction-format.md) | [Execution Semantics](execution-semantics.md)

Reference information for opcodes: unused opcodes, common misconceptions, length reference, and common patterns.

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

## Related Documentation

- [Opcode Reference](opcodes.md) - Complete opcode index
- [Control Flow & Memory Operations](opcodes-control-memory.md) - Control flow and memory opcodes
- [Data Operations](opcodes-data.md) - Data operation opcodes
- [Arithmetic & Base Operations](opcodes-arithmetic.md) - Arithmetic and base operation opcodes
