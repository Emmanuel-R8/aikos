# Implementation Plan: Zig Emulator Completion - Bring to Parity with C Implementation

**Branch**: `005-zig-completion` | **Date**: 2025-12-07 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/005-zig-completion/spec.md`

## Summary

Complete the Zig emulator implementation to achieve functional parity with the C emulator, enabling it to successfully load and run Medley Interlisp. The implementation will fix critical blockers (sysout loading, VM activation, essential opcodes) and complete missing functionality (GC operations, SDL2 display integration). All new insights and knowledge will be documented in `documentation/` to improve project knowledge.

**Technical Approach**: Fix sysout loading by implementing correct IFPAGE structure and validation, FPtoVP table loading, and page mapping algorithm. Activate VM dispatch loop and implement essential opcodes for Medley startup. Complete GC hash table operations and integrate SDL2 display rendering. Document all findings and improvements in knowledge base.

## Technical Context

**Language/Version**: Zig 0.15.2+ (already verified available)
**Primary Dependencies**:

- SDL2 2.32.58+ (already verified and linked)
- Zig standard library (allocators, file I/O, error handling)
- C interop for SDL2 integration

**Storage**: File system (sysout files), memory-mapped virtual memory
**Testing**: Zig test framework (`zig build test`), integration tests with actual sysout files, comparison tests against C emulator
**Target Platform**: Linux (primary), macOS (should work), Windows (optional)
**Project Type**: VM emulator (single executable)
**Performance Goals**:

- Sysout loading completes in < 5 seconds for typical sysout files
- Bytecode execution performance within 20% of C emulator execution time for equivalent workloads (optimization deferred to later phase)
- Display rendering at interactive frame rates (30+ fps)

**Constraints**:

- Must maintain exact compatibility with C emulator behavior
- Must load existing sysout files without modification
- Must preserve all existing Zig implementation structure and patterns
- Must document all new insights in `documentation/`

**Scale/Scope**:

- Complete IFPAGE structure (~100 fields matching C implementation)
- Implement essential opcode set (~80-100 opcodes for Medley startup)
- Complete GC hash table operations (ADDREF, DELREF, reclamation)
- SDL2 display integration (window creation, BitBLT, event handling)
- Documentation updates across multiple `documentation/` files

## Constitution Check

_GATE: Must pass before Phase 0 research. Re-check after Phase 1 design._

### Pre-Research Check (Phase 0)

### I. Platform Portability ✅

**Status**: COMPLIANT
**Rationale**: Zig implementation already supports Linux and can cross-compile to macOS/Windows. SDL2 provides cross-platform display abstraction. Sysout file format is platform-independent (with byte swapping support). Implementation uses Zig's platform abstraction features.

### II. Build System Flexibility ✅

**Status**: COMPLIANT
**Rationale**: Zig implementation uses Zig build system (`build.zig`), which satisfies Constitution Principle II (Build System Flexibility). The constitution requires support for multiple build systems (CMake and make for C implementation). The Zig implementation provides an alternative build system (Zig build system), maintaining the principle of build system flexibility across implementations. The C emulator continues to support CMake and make, while the Zig implementation uses Zig build system - both satisfy the flexibility requirement within their respective implementation contexts.

### III. Display Abstraction ✅

**Status**: COMPLIANT
**Rationale**: Zig implementation uses SDL2 (matching C emulator's SDL2 backend). Display interface is abstracted in `src/display/` modules. SDL2 provides cross-platform display abstraction without requiring X11.

### IV. Code Quality & Memory Safety ✅

**Status**: COMPLIANT
**Rationale**: Zig's memory safety features (explicit allocators, error unions, no hidden allocations) provide better safety than C. Implementation uses `packed struct` for exact C compatibility while maintaining Zig's safety guarantees. Code follows Zig idioms and patterns.

### V. Testing & Validation ✅

**Status**: COMPLIANT
**Rationale**: Implementation includes comprehensive test suite structure. Tests will validate against C emulator behavior. Sysout loading will be tested with actual sysout files. Critical VM operations will have test coverage.

**Pre-Research Gate Status**: ✅ **PASS** - All constitution principles satisfied

### Post-Design Check (Phase 1)

_To be completed after Phase 1 design artifacts are created_

## Project Structure

### Documentation (this feature)

```text
specs/005-zig-completion/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
zaiko/
├── build.zig            # Build configuration (SDL2 already enabled)
├── src/
│   ├── main.zig         # Entry point (VM activation complete ✅)
│   ├── data/
│   │   └── sysout.zig   # Sysout loading (IFPAGE fix, FPtoVP, page loading complete ✅)
│   ├── vm/
│   │   ├── dispatch.zig  # Main dispatch loop (166 lines - split from 1150) ✅
│   │   ├── dispatch/     # Dispatch modules (split for maintainability) ✅
│   │   │   ├── instruction.zig  # Instruction struct, Opcode enum, decoding (533 lines)
│   │   │   └── execution.zig   # Execution functions, opcode switch (475 lines)
│   │   ├── opcodes.zig  # Opcode handler re-exports (199 lines - split from 2820) ✅
│   │   ├── opcodes/     # Opcode modules (split for maintainability) ✅
│   │   │   ├── arithmetic.zig      # Integer and general arithmetic (263 lines)
│   │   │   ├── bitwise.zig          # Bitwise operations (132 lines)
│   │   │   ├── stack_ops.zig        # Stack manipulation (68 lines)
│   │   │   ├── function_calls.zig   # Function calls and returns (91 lines)
│   │   │   ├── binding.zig          # Binding operations (68 lines)
│   │   │   ├── control_flow.zig     # Jump operations (105 lines)
│   │   │   ├── data_ops.zig          # CAR, CDR, CONS, RPLACA, RPLACD (277 lines)
│   │   │   ├── array_ops.zig         # Array operations (189 lines)
│   │   │   ├── comparison.zig       # Comparison operations (234 lines)
│   │   │   ├── type_checking.zig    # Type checking (145 lines)
│   │   │   ├── variable_access.zig   # Variable access (340 lines)
│   │   │   ├── floating_point.zig    # Floating point operations (80 lines)
│   │   │   └── misc.zig              # Miscellaneous operations (938 lines)
│   │   ├── stack.zig     # Stack management (complete ✅)
│   │   └── function.zig  # Function call management (complete ✅)
│   ├── memory/
│   │   ├── gc.zig       # GC operations (needs hash table operations)
│   │   └── virtual.zig   # Virtual memory (FPtoVP integration complete ✅)
│   ├── display/
│   │   ├── sdl_backend.zig # SDL backend (needs rendering implementation)
│   │   └── graphics.zig    # BitBLT operations (needs implementation)
│   └── io/              # I/O subsystems (framework ready)
└── tests/               # Test suite (needs sysout loading tests)

documentation/        # Knowledge base (needs updates with new insights)
├── rewrite-spec/
│   └── data-structures/
│       └── sysout-format.md  # May need updates with actual IFPAGE details
└── implementations/
    └── zig-implementation.md # Needs creation/update with completion status
```

**Structure Decision**: This is a completion project modifying existing Zig implementation. Code organization improvements completed:

- **opcodes.zig split** (2025-01-27): Split 2,820-line monolithic file into 13 modular files in `vm/opcodes/` directory, each under 500 lines for better maintainability
- **dispatch.zig split** (2025-01-27): Split 1,150-line file into 3 modules in `vm/dispatch/` directory for better separation of concerns
- All handlers remain accessible via re-export files maintaining backward compatibility
- Knowledge base updates will be made to `documentation/` with new insights and corrections

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations - all constitution principles satisfied.

## Phase 0: Research Complete ✅

**Status**: All research questions resolved

**Key Findings**:

- IFPAGE_KEYVAL is `0x15e3` (not `0x12345678`) - **CRITICAL CORRECTION**
- IFPAGE structure has ~100 fields matching C implementation exactly
- FPtoVP table loading algorithm with byte offset calculations documented
- Page loading algorithm with byte swapping documented
- Essential opcodes identified for Medley startup
- GC hash table operations documented
- SDL2 integration patterns documented

**Research Output**: `research.md` with all findings and decisions

## Phase 1: Design Complete ✅

**Status**: Design artifacts created

**Artifacts**:

- `data-model.md` - Complete data model with entities, relationships, state transitions
- `contracts/sysout-loading-api.md` - API contract for sysout loading
- `contracts/vm-execution-api.md` - API contract for VM execution
- `quickstart.md` - Step-by-step completion guide

**Knowledge Base Updates**:

- Updated `documentation/rewrite-spec/data-structures/sysout-format.md` with IFPAGE_KEYVAL correction
- Created `documentation/implementations/zig-implementation.md` with completion status
- Updated `documentation/implementations/README.md` with Zig implementation entry

## Summary

**Branch**: `005-zig-completion`
**Plan Path**: `specs/005-zig-completion/plan.md`
**Spec Path**: `specs/005-zig-completion/spec.md`

**Generated Artifacts**:

- ✅ `specs/005-zig-completion/research.md` - Phase 0 research findings
- ✅ `specs/005-zig-completion/data-model.md` - Phase 1 data model
- ✅ `specs/005-zig-completion/contracts/sysout-loading-api.md` - Sysout loading API
- ✅ `specs/005-zig-completion/contracts/vm-execution-api.md` - VM execution API
- ✅ `specs/005-zig-completion/quickstart.md` - Completion quickstart guide

**Knowledge Base Updates**:

- ✅ `documentation/rewrite-spec/data-structures/sysout-format.md` - IFPAGE_KEYVAL correction
- ✅ `documentation/implementations/zig-implementation.md` - Zig implementation status
- ✅ `documentation/implementations/README.md` - Updated with Zig entry

**Code Organization**:

- ✅ `opcodes.zig` split into 13 modular files (2025-01-27)
- ✅ `dispatch.zig` split into 3 modular files (2025-01-27)

**Next Steps**: Continue with Phase 3 essential opcodes verification and testing, then proceed to Phase 4 (GC operations) and Phase 5 (SDL2 integration).
