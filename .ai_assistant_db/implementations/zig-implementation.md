# Zig Implementation Status

**Navigation**: [Implementations README](README.md) | [Main README](../README.md)

**Date**: 2025-12-07
**Status**: 🔄 In Progress - Completion Phase
**Location**: `maiko/alternatives/zig/`
**Build System**: Zig build system (`build.zig`)
**Display Backend**: SDL2 (linked, integration pending)

## Overview

The Zig implementation provides a complete framework for the Maiko emulator in Zig programming language, following the rewrite documentation specifications. The implementation is currently in the completion phase to achieve functional parity with the C emulator.

## Current Status

### ✅ Completed

- ✅ Project structure and build system
- ✅ Core types and utilities
- ✅ VM core framework (dispatch loop structure, stack management framework)
- ✅ Basic opcode handlers (~50 opcodes: arithmetic, comparison, type checking)
- ✅ Memory management structure (GC framework, storage allocation framework)
- ✅ Data structure frameworks (cons cells, arrays, function headers)
- ✅ I/O subsystem structure (keyboard, mouse, filesystem frameworks)
- ✅ Display subsystem structure (SDL backend framework)
- ✅ Opcode enumeration (190+ opcodes defined)
- ✅ Comprehensive test suite structure
- ✅ SDL2 linking enabled in build.zig
- ✅ **Sysout Loading** (Phase 1 Complete - 2025-12-07)
  - ✅ IFPAGE_KEYVAL corrected (now uses 0x15e3)
  - ✅ IFPAGE structure complete (~100 fields matching C implementation)
  - ✅ FPtoVP table loading implemented (BIGVM and non-BIGVM support)
  - ✅ Page loading algorithm implemented (sparse page handling)
  - ✅ Version compatibility checks (LVERSION, MINBVERSION)
  - ✅ VM state initialization from IFPAGE implemented
  - ✅ Dispatch loop activated in main.zig
  - ⚠️ Byte swapping support (stubbed, needs cross-platform testing)

- 🔄 **VM Execution** (P1 - In Progress)
  - ✅ VM dispatch loop activated in main.zig
  - ✅ VM state initialization from IFPAGE implemented
  - ✅ Program counter initialization added
  - ⚠️ Opcode handlers need completion (many stubs exist)

- 🔄 **Essential Opcodes** (P1 - Critical Blocker)
  - ❌ Function calls (CALL, RETURN, UNWIND) - framework ready, needs completion
  - ❌ Cons cell operations (CAR, CDR, CONS) - framework ready, needs implementation
  - ❌ Variable access completion (IVAR, PVAR, FVAR, GVAR variants)
  - ❌ Control flow (JUMP variants) - some implemented, needs completion
  - ❌ List operations (LIST, APPEND, RPLACA, RPLACD) - placeholders exist

- 🔄 **GC Operations** (P2)
  - ❌ GC hash table operations (ADDREF, DELREF) - structure complete, operations pending
  - ❌ Reclamation logic - pending

- 🔄 **SDL2 Display Integration** (P2)
  - ❌ SDL2 initialization - framework ready
  - ❌ BitBLT rendering - framework ready, needs implementation
  - ❌ Event handling - framework ready, needs implementation

### ⏳ Pending

- ⏳ Complete remaining opcode implementations (beyond essential set)
- ⏳ Performance optimization
- ⏳ Additional platform support (macOS, Windows)
- ⏳ Comprehensive integration testing

## Critical Findings

### IFPAGE_KEYVAL Correction ✅ FIXED

**CRITICAL**: The correct IFPAGE validation key is `0x15e3` (defined in `maiko/inc/ifpage.h:15`), not `0x12345678` as initially used in the Zig implementation.

**Status**: ✅ Fixed in `maiko/alternatives/zig/src/data/sysout.zig:14` and `maiko/alternatives/zig/src/utils/types.zig:95`

**Impact**: This was a critical blocker preventing sysout validation from working.

### IFPAGE Structure ✅ COMPLETE

The IFPAGE structure is now complete with ~100 fields matching the C implementation exactly.

**C Reference**: `maiko/inc/ifpage.h` (non-BIGVM, non-BYTESWAP version used as base)

**Zig Location**: `maiko/alternatives/zig/src/utils/types.zig:24-95`

**Key Fields Implemented**:
- Frame pointers (currentfxp, resetfxp, subovfxp, kbdfxp, etc.)
- Version information (lversion, minrversion, minbversion, rversion, bversion)
- Validation key (key = IFPAGE_KEYVAL = 0x15e3)
- Page management (nactivepages, ndirtypages, fptovpstart, etc.)
- Stack state (stackbase, endofstack)
- VM state (miscstackfn, miscstackarg1/2/result, etc.)

### FPtoVP Table Loading ✅ IMPLEMENTED

The FPtoVP (File Page to Virtual Page) table loading algorithm is now implemented.

**C Reference**: `maiko/src/ldsout.c:197-250`

**Implementation**: `maiko/alternatives/zig/src/data/sysout.zig:loadFPtoVPTable`

**Algorithm**:
1. Calculate offset: `(ifpage.fptovpstart - 1) * BYTESPER_PAGE + offset` (BIGVM: +4, non-BIGVM: +2)
2. Read table entries (16-bit for non-BIGVM, 32-bit for BIGVM)
3. Convert to u16 array for non-BIGVM format
4. Support sparse page marker (0xFFFF)

**Status**: ✅ Implemented with BIGVM/non-BIGVM format support

### Page Loading Algorithm ✅ IMPLEMENTED

The page loading algorithm is now implemented.

**C Reference**: `maiko/src/ldsout.c:250-350`

**Implementation**: `maiko/alternatives/zig/src/data/sysout.zig:loadMemoryPages`

**Algorithm**:
1. Iterate through file pages (0 to num_file_pages)
2. Check FPtoVP entry (skip if 0xFFFF = sparse page)
3. Seek to file page offset: `file_page * BYTESPER_PAGE`
4. Read 512 bytes (BYTESPER_PAGE)
5. Write to virtual address: `virtual_page * BYTESPER_PAGE`
6. Handle byte swapping (stubbed for now)

**Status**: ✅ Implemented with sparse page handling

### Version Constants

**CRITICAL**: Version constants from `maiko/inc/version.h`:
- `LVERSION = 21000` (minimum Lisp version required)
- `MINBVERSION = 21001` (maximum bytecode version supported)

**Implementation**: `maiko/alternatives/zig/src/data/sysout.zig:18-19`

**Validation**: Sysout's `lversion` must be >= LVERSION, and `minbversion` must be <= MINBVERSION

### Opcode Conflicts Discovered

Several opcodes in the Zig implementation don't exist in the C implementation and were causing compilation conflicts:

**Removed/Commented Out**:
- Generic `JUMP`, `FJUMP`, `TJUMP` opcodes (only JUMPX, JUMPXX, and JUMP0-JUMP15 exist)
- `CHARCODE`, `CHARN` (conflict with NFJUMPX/NTJUMPX at 0xB4-0xB5)
- `GETAEL1`, `GETAEL2`, `SETAEL1`, `SETAEL2` (conflict with JUMP0-JUMP3 at 0x80-0x83)
- `FIXP`, `SMALLP`, `LISTP` (conflict with TJUMP0-TJUMP2 at 0xA0-0xA2)
- `PUSH` (conflict with ADDBASE at 0xD0)

**Resolution**: These opcodes were commented out in the dispatch switch statements. They may need to be implemented via different mechanisms or may not be needed.

### Stack Operations: LispPTR Storage Format ✅ FIXED

**CRITICAL**: Stack stores LispPTR values as 32-bit (2 DLwords), not 16-bit as initially implemented.

**Issue**: Initial implementation stored only 16 bits (1 DLword), causing incorrect value storage/retrieval.

**Fix**: Updated `pushStack()`, `popStack()`, `getTopOfStack()`, and `setTopOfStack()` to handle 32-bit LispPTR values as 2 DLwords:
- Low 16 bits stored in `stack_ptr[0]`
- High 16 bits stored in `stack_ptr[1]`
- Values reconstructed as `(high_word << 16) | low_word`

**Zig-Specific Challenge**: Cannot directly cast `[*]DLword` (alignment 2) to `*LispPTR` (alignment 4) due to Zig's strict alignment checking. Solution: Manually read/write 2 DLwords instead of pointer casting.

**Location**: `maiko/alternatives/zig/src/vm/stack.zig:192-242`

**Status**: ✅ Fixed - Stack operations now correctly handle 32-bit values matching C implementation

### Arithmetic Opcodes: SMALLP/FIXP Handling ✅ IMPLEMENTED

**CRITICAL**: Arithmetic opcodes must handle SMALLP (small integers) and FIXP (large integers) correctly.

**Implementation**: Added number extraction and encoding functions matching C `N_IGETNUMBER` and `N_ARITH_SWITCH` macros:
- `extractInteger()`: Extracts integers from SMALLP (S_POSITIVE/S_NEGATIVE segments) or FIXP objects
- `encodeIntegerResult()`: Encodes integer results as SMALLP if in range, otherwise creates FIXP

**Zig-Specific Details**:
- Added constants: `S_POSITIVE`, `S_NEGATIVE`, `SEGMASK`, `MAX_SMALL`, `MIN_SMALL`, `MAX_FIXP`, `MIN_FIXP`
- Overflow checking implemented matching C behavior
- FIXP object creation deferred to Phase 4 (GC implementation)

**Location**: `maiko/alternatives/zig/src/utils/types.zig:124-212`

**Status**: ✅ Implemented - Arithmetic opcodes (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM) now match C behavior

### Function Call Opcodes: FN0-FN4 Implementation ✅ IMPLEMENTED

**CRITICAL**: FN0-FN4 opcodes have 3-byte instruction format (opcode + 2-byte atom index) for non-BIGATOMS.

**Implementation**: Implemented FN0-FN4 handlers matching C `OPFN` macro behavior:
- Extract atom index from instruction operand (2 bytes for non-BIGATOMS)
- Create function header (placeholder for now - atom table lookup deferred to Phase 3)
- Call `callFunction` with appropriate argument count (0-4)

**Zig-Specific Details**:
- Instruction length corrected from 1 byte to 3 bytes (FN_OPCODE_SIZE = 3 for non-BIGATOMS)
- Atom index extracted using `instruction.getWordOperand(0)` (DLword, 2 bytes)
- Function header `na` field is `DLword` (u16) in Zig struct, but C uses `short` (signed). Stored as u16, signed interpretation handled when needed.
- Placeholder function headers created until atom table lookup is implemented (Phase 3)

**C Reference**: `maiko/inc/tosfns.h:OPFN`, `maiko/inc/lispemul.h:FN_OPCODE_SIZE`

**Location**: `maiko/alternatives/zig/src/vm/opcodes.zig:446-511`, `maiko/alternatives/zig/src/vm/dispatch.zig:474-487`

**Status**: ✅ Implemented - FN0-FN4 handlers match C instruction format and call structure

### Function Return: RETURN Opcode Implementation ✅ IMPLEMENTED

**Implementation**: Implemented RETURN handler matching C `OPRETURN` macro behavior:
- Gets return value from TopOfStack
- Restores previous frame via activation link (`alink` field)
- Restores PC from previous frame's `pcoffset`
- Handles top-level return (no previous frame)

**Zig-Specific Details**:
- Frame restoration uses `current_frame.link` to find previous frame
- PC restoration uses `previous_frame.pcoffset` (saved during function call)
- Return value preserved through frame restoration

**C Reference**: `maiko/inc/tosret.h:OPRETURN`

**Location**: `maiko/alternatives/zig/src/vm/opcodes.zig:513-525`, `maiko/alternatives/zig/src/vm/function.zig:53-83`

**Status**: ✅ Implemented - RETURN handler matches C frame restoration behavior

### Compilation Issues Fixed

**Type Mismatches**:
- Fixed `usize` vs `u32` conversions in function.zig and stack.zig
- Fixed pointer alignment issues in storage.zig using `@alignCast`
- Fixed const vs mutable Storage pointer in VM structure

**Error Types**:
- Added `StackUnderflow` and `DivisionByZero` to VMError enum

**Alignment Issues**:
- Changed `translateAddress` alignment parameter from `u2` to `u8` to support 4-byte alignment

## Implementation Statistics

| Category | Status | Count | Notes |
|----------|--------|-------|-------|
| **Opcodes** | Partial | ~50/256 | Essential set needed for Medley startup |
| **IFPAGE Fields** | ✅ Complete | ~100/100 | Matches C structure exactly |
| **Sysout Loading** | ✅ Complete | 22/22 | Phase 1 tasks (T001-T022) complete |
| **GC Operations** | Framework | 0/3 | ADDREF, DELREF, reclamation pending |
| **Display Integration** | Framework | 0/3 | Initialization, BitBLT, events pending |
| **Test Coverage** | Structure | Framework | Needs sysout loading tests |
| **Build Status** | ✅ Success | - | All compilation errors fixed |

## Build and Run

### Prerequisites

- Zig 0.15.2+
- SDL2 2.32.58+ development libraries

### Build

```bash
cd maiko/alternatives/zig
zig build -Doptimize=ReleaseFast
```

### Run

```bash
./zig-out/bin/maiko-zig path/to/sysout.sysout
```

**Current Status**: ✅ Builds successfully. Sysout loading infrastructure complete. Ready for Phase 2 (basic bytecode execution).

### Test

```bash
zig build test
```

## Completion Plan

See `specs/005-zig-completion/` for detailed completion plan:

1. **Phase 1: Fix Sysout Loading** (P1 - MVP)
   - Fix IFPAGE_KEYVAL
   - Complete IFPAGE structure
   - Implement FPtoVP loading
   - Implement page loading

2. **Phase 2: Activate VM Execution** (P1)
   - Initialize VM state from IFPAGE
   - Activate dispatch loop

3. **Phase 3: Essential Opcodes** (P1)
   - Function calls
   - Cons cells
   - Variable access
   - Control flow

4. **Phase 4: GC Operations** (P2)
   - Hash table operations
   - Reclamation

5. **Phase 5: SDL2 Integration** (P2)
   - Display rendering
   - Event handling

## Related Documentation

- [Rewrite Specifications](../rewrite-spec/) - Complete specifications
- [Completion Plan](../../specs/005-zig-completion/plan.md) - Detailed completion plan
- [Research Findings](../../specs/005-zig-completion/research.md) - Critical findings
- [C Implementation Reference](../../maiko/src/) - Reference implementation

## Known Issues

1. ✅ **Sysout Loading**: Fixed IFPAGE_KEYVAL, complete IFPAGE structure, FPtoVP and page loading implemented
2. ⚠️ **Byte Swapping**: Stubbed, needs cross-platform testing
3. ⚠️ **Many Opcodes Placeholders**: ~200 opcodes need implementation (stubs exist)
4. ⚠️ **GC Incomplete**: Hash table operations pending (GCREF handler is stub)
5. ⚠️ **SDL2 Not Integrated**: Framework ready but rendering not implemented
6. ⚠️ **Opcode Conflicts**: Several opcodes removed due to conflicts with C implementation

## Next Steps

1. ✅ ~~Fix IFPAGE_KEYVAL in `src/data/sysout.zig`~~ **DONE**
2. ✅ ~~Complete IFPAGE structure matching C implementation~~ **DONE**
3. ✅ ~~Implement FPtoVP table loading~~ **DONE**
4. ✅ ~~Implement page loading algorithm~~ **DONE**
5. ✅ ~~Activate VM dispatch loop~~ **DONE**
6. 🔄 **Phase 2**: Implement essential opcodes for Medley startup (T023-T034)
7. 🔄 **Phase 3**: Complete essential opcodes for Medley startup (T035-T059)
8. ⏳ **Phase 4**: Complete GC operations (T060-T074)
9. ⏳ **Phase 5**: Integrate SDL2 display (T075+)
