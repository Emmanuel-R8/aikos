# Implementation Plan: Emulator Runner Scripts for Interlisp

**Branch**: `004-emulator-runner` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/004-emulator-runner/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Create scripts to run the Interlisp system from Medley using the user's choice of emulators (C, Zig, or Lisp). The system will integrate with existing Medley run scripts (run-medley, medley.command, medley_run.sh) to add emulator selection capabilities while preserving all existing functionality. The implementation will leverage the unified build system (spec 003) to locate emulator executables and support automatic emulator building on first run.

**Technical Approach**: Modify existing shell scripts to add `--emulator` argument parsing, emulator selection logic, and lock file mechanism for concurrent run prevention. Leverage existing unified build location checking (from spec 003) and integrate with build-emulator.sh for auto-build functionality. Use PID-based lock files with stale detection, consistent with existing loadup and build system patterns.

## Technical Context

**Language/Version**: Shell scripts (bash/sh), compatible with existing Medley script infrastructure
**Primary Dependencies**:

- Unified build system (spec 003) for emulator location and building
- Existing Medley run scripts (run-medley, medley.command, medley_run.sh)
- Maiko platform detection utilities (osversion, machinetype)
- Emulator executables (lde, ldeinit, ldex, ldesdl for C; maiko-zig for Zig; maiko-lisp for Lisp)
**Storage**: File system (lock files for concurrent run prevention, configuration via environment variables)
**Testing**: Shell script testing, integration tests with actual emulator execution
**Target Platform**: Linux, macOS, FreeBSD (same as existing Medley support)
**Project Type**: Script enhancement (modifying existing shell scripts)
**Performance Goals**: Emulator selection and startup in under 5 seconds (SC-001), error messages within 1 second (SC-004)
**Constraints**:
- Must preserve 100% backward compatibility with existing Medley run script functionality
- Must not modify emulator implementations themselves
- Must work with all existing Medley run scripts
- Lock mechanism must prevent concurrent runs without blocking indefinitely
**Scale/Scope**:
- 3 emulator implementations (C, Zig, Lisp)
- Multiple platforms (Linux, macOS, FreeBSD)
- Integration with 3+ existing run scripts (run-medley, medley.command, medley_run.sh, medley.ps1)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Check (Phase 0)

### I. Platform Portability ✅

**Status**: COMPLIANT
**Rationale**: The emulator runner scripts will leverage existing Medley script infrastructure which already supports multiple platforms (macOS, FreeBSD, Linux, Solaris, Windows). Emulator selection will work across all platforms where Medley runs. Platform detection uses existing Maiko utilities (osversion, machinetype) consistent with existing scripts.

### II. Build System Flexibility ✅

**Status**: COMPLIANT
**Rationale**: The emulator runner scripts do not modify build systems. They use emulators built by the unified build system (spec 003) which orchestrates multiple build systems (CMake/Make for C, Zig build for Zig, ASDF for Lisp) without replacing them. The runner scripts only invoke already-built executables.

### III. Display Abstraction ✅

**Status**: COMPLIANT
**Rationale**: The emulator runner scripts pass display subsystem options to emulators without modifying display abstraction layers. Each emulator (C with X11/SDL2, Zig with SDL2, Lisp with SDL3) handles display independently. The runner scripts only select which emulator to invoke.

### IV. Code Quality & Memory Safety ✅

**Status**: COMPLIANT
**Rationale**: The emulator runner scripts will be implemented as shell scripts following existing Medley script patterns. Shell scripts do not have memory safety concerns. Code quality will follow existing Medley script conventions and include comprehensive error handling.

### V. Testing & Validation ✅

**Status**: COMPLIANT
**Rationale**: The emulator runner scripts will be tested with actual emulator execution on target platforms. Integration tests will verify compatibility with existing Medley run scripts and emulator selection correctness. Script changes will be validated across platforms.

**Pre-Research Gate Status**: ✅ **PASS** - All constitution principles satisfied

### Post-Design Check (Phase 1)

### I. Platform Portability ✅

**Status**: COMPLIANT
**Rationale**: Implementation uses existing Medley script infrastructure which supports multiple platforms. Emulator selection works identically across all platforms. Lock file mechanism uses standard Unix patterns (PID-based) compatible with all target platforms.

### II. Build System Flexibility ✅

**Status**: COMPLIANT
**Rationale**: Runner scripts do not modify build systems. They invoke emulators built by the unified build system which orchestrates multiple build systems without replacing them. Auto-build integration calls existing build-emulator.sh script.

### III. Display Abstraction ✅

**Status**: COMPLIANT
**Rationale**: Runner scripts pass display options to emulators without modifying display layers. Each emulator handles display independently (C with X11/SDL2, Zig with SDL2, Lisp with SDL3).

### IV. Code Quality & Memory Safety ✅

**Status**: COMPLIANT
**Rationale**: Implementation uses shell scripts following existing Medley patterns. Comprehensive error handling, validation, and lock file cleanup ensure robust operation. No memory safety concerns with shell scripts.

### V. Testing & Validation ✅

**Status**: COMPLIANT
**Rationale**: Design includes validation functions (validate_emulator_executable) and comprehensive error handling. Integration with existing test infrastructure. Scripts will be tested with actual emulator execution on target platforms.

**Post-Design Gate Status**: ✅ **PASS** - All constitution principles satisfied

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
medley/
├── run-medley                    # Modified: Add --emulator argument and emulator selection logic
└── scripts/
    ├── medley/
    │   ├── medley.command        # Modified: Add unified build location checking (already partially done in spec 003)
    │   ├── medley_run.sh         # Modified: Add unified build location checking (already partially done in spec 003)
    │   └── medley_main.sh        # May need modification for emulator argument parsing
    └── build/                    # From spec 003: Used for auto-build functionality
        └── build-emulator.sh     # Called when --auto-build is specified

maiko/build/                      # From spec 003: Unified build output location
├── c/<os>.<arch>/               # C emulator executables
├── zig/<os>.<arch>/              # Zig emulator executables
└── lisp/<os>.<arch>/             # Lisp emulator executables
```

**Structure Decision**: This is a script enhancement project that modifies existing Medley run scripts. No new directory structure is needed. Modifications will be made to existing scripts in `medley/` and `medley/scripts/medley/`. The unified build system from spec 003 provides the emulator location structure.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |
