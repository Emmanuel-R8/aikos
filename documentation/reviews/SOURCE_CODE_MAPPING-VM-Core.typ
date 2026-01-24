 = Source Code Mapping - VM Core
 
 *Navigation*: Source Code Mapping | Main README
 
 Mapping of VM Core source code files to documentation sections.
 
 == VM Core - Instruction Set & Execution
 
 === Core Dispatch Loop
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/xc.c` | `documentation/specifications/vm-core/execution-model.typ` | ✅ Complete |
 // | | `documentation/specifications/instruction-set/opcodes.typ` | ✅ Complete |
 // | | `documentation/specifications/vm-core/interrupt-handling.typ` | ✅ Complete |
 // | **Key Functions**: `dispatch()`, opcode handlers (case001-case377) | | |
 // 
 
 === Opcode Implementations
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/arithops.c` | `documentation/specifications/instruction-set/opcodes.typ` (Arithmetic section) | ✅ Complete |
 // | | **Opcodes**: IPLUS2, IDIFFERENCE, ITIMES2, IQUOTIENT, IREMAINDER, FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT | | |
 // | `maiko/src/car-cdr.c` | `documentation/specifications/instruction-set/opcodes.typ` (Data Operations) | ✅ Complete |
 // | | `documentation/specifications/data-structures/cons-cells.typ` | ✅ Complete |
 // | | **Opcodes**: CAR, CDR, CONS, RPLACA, RPLACD | | |
 // | `maiko/src/arrayops.c` | `documentation/specifications/instruction-set/opcodes.typ` (Array Operations) | ✅ Complete |
 // | | `documentation/specifications/data-structures/arrays.typ` | ✅ Complete |
 // | | **Opcodes**: AREF1, AREF2, ASET1, ASET2, MISC3, MISC4 | | |
 // | `maiko/src/loopsops.c` | `documentation/specifications/instruction-set/opcodes.typ` (Control Flow) | ✅ Complete |
 // | | **Opcodes**: Function call opcodes (FN0-FNX, APPLYFN) | | |
 // | `maiko/src/return.c` | `documentation/specifications/vm-core/function-calls.typ` | ✅ Complete |
 // | | `documentation/specifications/vm-core/stack-management.typ` | ✅ Complete |
 // | | **Functions**: `OP_contextsw()`, `contextsw()` | | |
 // | `maiko/src/binds.c` | `documentation/specifications/instruction-set/opcodes.typ` (Binding Operations) | ✅ Complete |
 // | | **Opcodes**: BIND, UNBIND, DUNBIND | | |
 // | `maiko/src/gc.c` | `documentation/specifications/instruction-set/opcodes.typ` (GC Operations) | ✅ Complete |
 // | | **Opcodes**: GCREF | | |
 // | `maiko/src/shift.c` | `documentation/specifications/instruction-set/opcodes.typ` | ⚠️ Needs verification |
 // | `maiko/src/eqf.c` | `documentation/specifications/instruction-set/opcodes.typ` (Comparisons) | ✅ Complete |
 // | | **Opcodes**: EQ, EQUAL, IGREATERP, FGREATERP | | |
 // | `maiko/src/typeof.c` | `documentation/specifications/instruction-set/opcodes.typ` (Type Operations) | ✅ Complete |
 // | | **Opcodes**: TYPEP, NTYPX | | |
 // | `maiko/src/misc7.c`, `src/miscn.c` | `documentation/specifications/instruction-set/opcodes.typ` (Miscellaneous) | ✅ Complete |
 // | `maiko/src/ubf1.c`, `src/ubf2.c`, `src/ubf3.c` | `documentation/specifications/instruction-set/opcodes.typ` | ⚠️ Needs verification |
 // | `maiko/src/unwind.c` | `documentation/specifications/instruction-set/opcodes.typ` (Control Flow) | ✅ Complete |
 // | | **Opcodes**: UNWIND | | |
 // | `maiko/src/z2.c` | `documentation/specifications/instruction-set/opcodes.typ` | ⚠️ Needs verification |
 // 
 
 === Stack Management
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/llstk.c` | `documentation/specifications/vm-core/stack-management.typ` | ✅ Complete |
 // | | **Functions**: `extendstack()`, `moveframe()`, `check_stack_rooms()` | | |
 // | `maiko/inc/stack.h` | `documentation/specifications/vm-core/stack-management.typ` | ✅ Complete |
 // | | **Structures**: `FX`, `FNHEAD`, `STK_FSB_WORD` | | |
 // 
 
 === Function Calls
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/loopsops.c` | `documentation/specifications/vm-core/function-calls.typ` | ✅ Complete |
 // | | **Functions**: `lcfuncall()`, function invocation | | |
 // | `maiko/src/ufn.c` | `documentation/specifications/vm-core/function-calls.typ` | ✅ Complete |
 // | | **Functions**: `ufn()` - UFN (Undefined Function) handling | | |
 // | `maiko/src/intcall.c` | `documentation/specifications/vm-core/interrupt-handling.typ` | ✅ Complete |
 // | | **Functions**: Interrupt call mechanism | | |
 // 
 
 === Main Entry Point
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/main.c` | `documentation/specifications/vm-core/execution-model.typ` | ✅ Complete |
 // | | `documentation/specifications/data-structures/sysout-format.typ` | ✅ Complete |
 // | | **Functions**: `main()`, `start_lisp()` | | |
 // 
 
 == Related Documentation
 
 - Source Code Mapping - Complete mapping index
 - VM Core Execution Model - Execution model
 - Instruction Set - Opcode specifications
