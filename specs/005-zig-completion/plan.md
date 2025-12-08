# Implementation Plan: Zig Emulator Completion - Bring to Parity with C Implementation

**Branch**: `005-zig-completion` | **Date**: 2025-12-07 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/005-zig-completion/spec.md`

## Summary

Complete the Zig emulator implementation to achieve functional parity with the C emulator, enabling it to successfully load and run Medley Interlisp. The implementation will fix critical blockers (sysout loading, VM activation, essential opcodes) and complete missing functionality (GC operations, SDL2 display integration). All new insights and knowledge will be documented in `.ai_assistant_db/` to improve project knowledge.

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
- Bytecode execution performance comparable to C emulator (optimization deferred to later phase)
- Display rendering at interactive frame rates (30+ fps)

**Constraints**:
- Must maintain exact compatibility with C emulator behavior
- Must load existing sysout files without modification
- Must preserve all existing Zig implementation structure and patterns
- Must document all new insights in `.ai_assistant_db/`

**Scale/Scope**:
- Complete IFPAGE structure (~100 fields matching C implementation)
- Implement essential opcode set (~80-100 opcodes for Medley startup)
- Complete GC hash table operations (ADDREF, DELREF, reclamation)
- SDL2 display integration (window creation, BitBLT, event handling)
- Documentation updates across multiple `.ai_assistant_db/` files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Check (Phase 0)

### I. Platform Portability ✅

**Status**: COMPLIANT
**Rationale**: Zig implementation already supports Linux and can cross-compile to macOS/Windows. SDL2 provides cross-platform display abstraction. Sysout file format is platform-independent (with byte swapping support). Implementation uses Zig's platform abstraction features.

### II. Build System Flexibility ✅

**Status**: COMPLIANT
**Rationale**: Zig implementation uses Zig build system (`build.zig`), which is a valid alternative to CMake/Make. The constitution requires support for multiple build systems, and Zig build system is a modern alternative. No conflicts with C emulator build systems.

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

*To be completed after Phase 1 design artifacts are created*

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
maiko/alternatives/zig/
├── build.zig            # Build configuration (SDL2 already enabled)
├── src/
│   ├── main.zig         # Entry point (needs VM activation)
│   ├── data/
│   │   └── sysout.zig   # Sysout loading (needs IFPAGE fix, FPtoVP, page loading)
│   ├── vm/
│   │   ├── dispatch.zig # Dispatch loop (needs activation)
│   │   └── opcodes.zig  # Opcode handlers (needs essential opcodes)
│   ├── memory/
│   │   ├── gc.zig       # GC operations (needs hash table operations)
│   │   └── virtual.zig  # Virtual memory (needs FPtoVP integration)
│   ├── display/
│   │   ├── sdl_backend.zig # SDL backend (needs rendering implementation)
│   │   └── graphics.zig    # BitBLT operations (needs implementation)
│   └── io/              # I/O subsystems (framework ready)
└── tests/               # Test suite (needs sysout loading tests)

.ai_assistant_db/        # Knowledge base (needs updates with new insights)
├── rewrite-spec/
│   └── data-structures/
│       └── sysout-format.md  # May need updates with actual IFPAGE details
└── implementations/
    └── zig-implementation.md # Needs creation/update with completion status
```

**Structure Decision**: This is a completion project modifying existing Zig implementation. No new directory structure needed. Modifications will be made to existing files in `maiko/alternatives/zig/src/`. Knowledge base updates will be made to `.ai_assistant_db/` with new insights and corrections.

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
- Updated `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` with IFPAGE_KEYVAL correction
- Created `.ai_assistant_db/implementations/zig-implementation.md` with completion status
- Updated `.ai_assistant_db/implementations/README.md` with Zig implementation entry

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
- ✅ `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` - IFPAGE_KEYVAL correction
- ✅ `.ai_assistant_db/implementations/zig-implementation.md` - Zig implementation status
- ✅ `.ai_assistant_db/implementations/README.md` - Updated with Zig entry

**Next Steps**: Proceed to Phase 2 (tasks creation) or begin implementation following quickstart guide.
