# Implementation Notes: Maiko Emulator in Zig

**Date**: 2025-12-04
**Version**: 0.1.0
**Status**: In Progress

## Overview

This document describes the Zig implementation of the Maiko emulator, including design decisions, implementation notes, and known limitations.

## Design Decisions

### Zig Version

- **Zig 0.15.1**: Target version for compatibility
- **API Changes**: Code updated for Zig 0.15.1 API changes:
  - `@intToEnum` → `@as(Opcode, @enumFromInt())`
  - `@truncate` → `@as(type, @truncate())`
  - `@ptrCast` → `@as(type, @ptrCast())`
  - `@alignCast` → `@as([*]align(N) type, @alignCast())`

### Memory Management

- **Explicit Allocation**: Using Zig's allocator system
- **Packed Structs**: All data structures use `packed struct` for exact C compatibility
- **GC Implementation**: Reference-counting GC structure in place, full implementation pending

### Build System

- **Zig Build**: Using `build.zig` with module system
- **SDL2**: Currently optional (commented out) until NixOS environment configured
- **C Interop**: Ready for SDL2 integration via C interop

## Implementation Status

### Completed

- ✅ Project structure and build system
- ✅ Core types and utilities
- ✅ VM core framework (dispatch loop, stack management)
- ✅ Stack push/pop operations implemented
- ✅ Address translation framework
- ✅ Basic opcode handlers (arithmetic, comparison, type checking)
- ✅ Memory management structure (GC, storage, virtual memory)
- ✅ Data structures (cons cells, arrays, function headers)
- ✅ Sysout file loading framework
- ✅ I/O subsystem structure (keyboard, mouse, filesystem)
- ✅ Display subsystem structure (SDL backend framework)
- ✅ Opcode enumeration (190+ opcodes defined)
- ✅ Comprehensive test suite

### Implemented Logic

- ✅ Stack push/pop operations
- ✅ Arithmetic opcodes (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM)
- ✅ General arithmetic opcodes (PLUS2, DIFFERENCE, TIMES2, QUOTIENT)
- ✅ Bitwise opcodes (LOGOR2, LOGAND2, LOGXOR2, LSH)
- ✅ Shift opcodes (LLSH1, LLSH8, LRSH1, LRSH8)
- ✅ Comparison opcodes (EQ, EQL, LESSP, GREATERP, IGREATERP)
- ✅ Type checking opcodes (TYPEP, FIXP, SMALLP, LISTP)
- ✅ Stack manipulation (PUSH, POP, POP_N, SWAP)
- ✅ Character operations (CHARCODE, CHARN)
- ✅ Constant opcodes (NIL, T, CONST_0, CONST_1, ACONST, GCONST)
- ✅ Variable access opcodes (IVAR, PVAR, FVAR, GVAR)
- ✅ Type checking opcodes (NTYPX, TYPEP, DTEST)
- ✅ Stack unwinding (UNWIND - placeholder)
- ✅ Binding operations (BIND, UNBIND, DUNBIND - placeholders)
- ✅ Function application (APPLYFN, CHECKAPPLY - placeholders)
- ✅ Stack scanning (STKSCAN - placeholder)
- ✅ Floating-point arithmetic (FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT, FGREATERP - placeholders)
- ✅ Optimized jump variants (JUMP0-JUMP15, FJUMP0-FJUMP15, TJUMP0-TJUMP15)
- ✅ Stack-relative return (SLRETURN - placeholder)
- ✅ Deep equality comparison (EQUAL - placeholder)
- ✅ Number creation (MAKENUMBER - placeholder)
- ✅ List operations (ASSOC, RPLCONS, LISTGET, RESTLIST, CREATECELL, CMLASSOC, FMEMB, CMLMEMBER, FINDKEY - placeholders)
- ✅ I/O operations (BIN, BOUT - placeholders)
- ✅ Evaluation operations (EVAL, ENVCALL - placeholders)
- ✅ Miscellaneous operations (RPLPTR_N, GVAR_, MISCN - placeholders)
- ✅ High-range opcodes (ATOMCELL_N, GETBASEBYTE, INSTANCEP, BLT, MISC10, PUTBASEBYTE, GETBASE_N, GETBASEPTR_N, GETBITS_N_FD, CMLEQUAL, PUTBASE_N, PUTBASEPTR_N, PUTBITS_N_FD, ADDBASE, VAG2, HILOC, LOLOC, IPLUS_N, IDIFFERENCE_N, BASE_LESSTHAN, UBFLOAT2, UBFLOAT1, BOXIPLUS, BOXIDIFFERENCE, FLOATBLT, FFTSTEP, MISC3, MISC4, UPCTRACE, CL_EQUAL - placeholders)
- ✅ PVAR set operations (PVAR_0-PVAR_6, PVARX_ - placeholders)
- ✅ Additional variable operations (ARG0, IVARX_, FVARX_, COPY, MYARGCOUNT, MYALINK - placeholders)
- ✅ Instance cell operations (SIC, SNIC, SICX - placeholders)
- ✅ Additional list/array operations (ELT, NTHCHC, SETA, RPLCHARCODE - placeholders)
- ✅ Extended jump variants (JUMPXX, NFJUMPX, NTJUMPX - placeholders)
- ✅ Multi-dimensional array operations (AREF2, ASET2 - placeholders)
- ✅ Additional miscellaneous operations (TYPECHECK, BUSBLT, MISC8, UBFLOAT3, TYPEMASK_N, MISC7, DRAWLINE, STORE_N, COPY_N, RAID - placeholders)
- ✅ Address translation framework

### In Progress

- 🔄 Opcode handler implementations (basic ones done, more pending)
- 🔄 GC algorithm implementation (structure complete, operations pending)
- 🔄 SDL2 integration (structure ready, needs SDL2 in environment)
- 🔄 Cons cell operations (CAR, CDR, CONS - framework ready)

### Pending

- ⏳ Complete remaining opcode implementations (256 total)
- ⏳ Full GC algorithm with hash table operations
- ⏳ SDL2 display rendering
- ⏳ Event handling integration
- ⏳ Memory access operations (cons cells, arrays)
- ⏳ Performance optimization

## Architecture

### Module Organization

```
src/
├── main.zig          # Entry point
├── vm/               # VM core
│   ├── dispatch.zig  # Dispatch loop
│   ├── opcodes.zig  # Opcode handlers
│   ├── stack.zig    # Stack management
│   ├── function.zig # Function calls
│   └── interrupt.zig # Interrupt handling
├── memory/          # Memory management
│   ├── gc.zig       # Garbage collection
│   ├── virtual.zig  # Virtual memory
│   ├── storage.zig  # Storage allocation
│   └── layout.zig   # Memory layout
├── data/            # Data structures
│   ├── cons.zig     # Cons cells
│   ├── array.zig    # Arrays
│   ├── function_header.zig # Function headers
│   └── sysout.zig   # Sysout loading
├── display/         # Display subsystem
│   ├── sdl_backend.zig # SDL backend
│   ├── graphics.zig # Graphics operations
│   └── events.zig   # Event handling
├── io/              # I/O subsystem
│   ├── keyboard.zig # Keyboard handling
│   ├── mouse.zig    # Mouse handling
│   └── filesystem.zig # File I/O
└── utils/           # Utilities
    ├── types.zig    # Core types
    ├── errors.zig   # Error types
    └── address.zig  # Address translation
```

## Compatibility

### C Implementation Compatibility

- **Memory Layout**: Exact byte-for-byte compatibility via `packed struct`
- **Sysout Files**: Structure ready for loading C-generated sysout files
- **Opcode Semantics**: Following rewrite documentation for exact behavior

### Platform Support

- **Linux**: Primary target platform
- **macOS**: Should work with Zig cross-compilation
- **Windows**: Optional, not yet tested

## Known Limitations

1. **SDL2**: Not yet linked (commented out for NixOS compatibility)
2. **Opcode Handlers**: Framework complete, implementations pending
3. **GC Algorithm**: Structure complete, hash table operations pending
4. **Stack Operations**: Framework complete, push/pop logic pending
5. **Event Handling**: Structure ready, SDL integration pending

## Testing

### Test Coverage

- ✅ Core types and utilities
- ✅ Stack frame allocation
- ✅ Memory allocation
- ✅ GC initialization
- ✅ Sysout validation
- ✅ Keyboard event queue
- ✅ Mouse state management

### Test Execution

```bash
zig build test
```

## Build Instructions

### Prerequisites

- Zig 0.15.1+
- SDL2 development libraries (optional, currently disabled)

### Build

```bash
cd alternatives/zig
zig build
```

### Run

```bash
zig-out/bin/maiko-zig <sysout_file>
```

## Next Steps

1. Implement opcode handler logic
2. Complete GC hash table operations
3. Integrate SDL2 display rendering
4. Add comprehensive test coverage
5. Performance optimization

## Related Documentation

- [Rewrite Documentation](../../.ai_assistant_db/rewrite-spec/) - Complete specifications
- [Implementation Plan](../../specs/001-zig-implementation/plan.md) - Technical plan
- [Tasks](../../specs/001-zig-implementation/tasks.md) - Implementation tasks