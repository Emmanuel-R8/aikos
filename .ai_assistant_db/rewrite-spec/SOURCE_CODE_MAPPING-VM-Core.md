# Source Code Mapping - VM Core

**Navigation**: [Source Code Mapping](SOURCE_CODE_MAPPING.md) | [Main README](../README.md)

Mapping of VM Core source code files to documentation sections.

## VM Core - Instruction Set & Execution

### Core Dispatch Loop

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/xc.c` | `.ai_assistant_db/rewrite-spec/vm-core/execution-model.md` | ✅ Complete |
| | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` | ✅ Complete |
| | `.ai_assistant_db/rewrite-spec/vm-core/interrupt-handling.md` | ✅ Complete |
| **Key Functions**: `dispatch()`, opcode handlers (case001-case377) | | |

### Opcode Implementations

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/arithops.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Arithmetic section) | ✅ Complete |
| | **Opcodes**: IPLUS2, IDIFFERENCE, ITIMES2, IQUOTIENT, IREMAINDER, FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT | | |
| `maiko/src/car-cdr.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Data Operations) | ✅ Complete |
| | `.ai_assistant_db/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
| | **Opcodes**: CAR, CDR, CONS, RPLACA, RPLACD | | |
| `maiko/src/arrayops.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Array Operations) | ✅ Complete |
| | `.ai_assistant_db/rewrite-spec/data-structures/arrays.md` | ✅ Complete |
| | **Opcodes**: AREF1, AREF2, ASET1, ASET2, MISC3, MISC4 | | |
| `maiko/src/loopsops.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Control Flow) | ✅ Complete |
| | **Opcodes**: Function call opcodes (FN0-FNX, APPLYFN) | | |
| `maiko/src/return.c` | `.ai_assistant_db/rewrite-spec/vm-core/function-calls.md` | ✅ Complete |
| | `.ai_assistant_db/rewrite-spec/vm-core/stack-management.md` | ✅ Complete |
| | **Functions**: `OP_contextsw()`, `contextsw()` | | |
| `maiko/src/binds.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Binding Operations) | ✅ Complete |
| | **Opcodes**: BIND, UNBIND, DUNBIND | | |
| `maiko/src/gc.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (GC Operations) | ✅ Complete |
| | **Opcodes**: GCREF | | |
| `maiko/src/shift.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` | ⚠️ Needs verification |
| `maiko/src/eqf.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Comparisons) | ✅ Complete |
| | **Opcodes**: EQ, EQUAL, IGREATERP, FGREATERP | | |
| `maiko/src/typeof.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Type Operations) | ✅ Complete |
| | **Opcodes**: TYPEP, NTYPX | | |
| `maiko/src/misc7.c`, `src/miscn.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Miscellaneous) | ✅ Complete |
| `maiko/src/ubf1.c`, `src/ubf2.c`, `src/ubf3.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` | ⚠️ Needs verification |
| `maiko/src/unwind.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (Control Flow) | ✅ Complete |
| | **Opcodes**: UNWIND | | |
| `maiko/src/z2.c` | `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` | ⚠️ Needs verification |

### Stack Management

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/llstk.c` | `.ai_assistant_db/rewrite-spec/vm-core/stack-management.md` | ✅ Complete |
| | **Functions**: `extendstack()`, `moveframe()`, `check_stack_rooms()` | | |
| `maiko/inc/stack.h` | `.ai_assistant_db/rewrite-spec/vm-core/stack-management.md` | ✅ Complete |
| | **Structures**: `FX`, `FNHEAD`, `STK_FSB_WORD` | | |

### Function Calls

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/loopsops.c` | `.ai_assistant_db/rewrite-spec/vm-core/function-calls.md` | ✅ Complete |
| | **Functions**: `lcfuncall()`, function invocation | | |
| `maiko/src/ufn.c` | `.ai_assistant_db/rewrite-spec/vm-core/function-calls.md` | ✅ Complete |
| | **Functions**: `ufn()` - UFN (Undefined Function) handling | | |
| `maiko/src/intcall.c` | `.ai_assistant_db/rewrite-spec/vm-core/interrupt-handling.md` | ✅ Complete |
| | **Functions**: Interrupt call mechanism | | |

### Main Entry Point

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/main.c` | `.ai_assistant_db/rewrite-spec/vm-core/execution-model.md` | ✅ Complete |
| | `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
| | **Functions**: `main()`, `start_lisp()` | | |

## Related Documentation

- [Source Code Mapping](SOURCE_CODE_MAPPING.md) - Complete mapping index
- [VM Core Execution Model](../vm-core/execution-model.md) - Execution model
- [Instruction Set](../instruction-set/) - Opcode specifications
