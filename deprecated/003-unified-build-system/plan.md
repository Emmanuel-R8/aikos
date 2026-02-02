# Implementation Plan: Unified Build System for Medley with Multiple Maiko Emulators

**Branch**: `003-unified-build-system` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/003-unified-build-system/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Create a unified build system that orchestrates building all three Maiko emulator implementations (C, Zig, Lisp) using their native build tools, integrates emulator builds into the Medley loadup workflow, and places all executables in a unified location structure (`maiko/build/<emulator>/<os>.<arch>/`). The system will be implemented as shell script wrappers that coordinate the existing build systems without modifying them, ensuring compatibility with existing Medley loadup scripts while adding the ability to select and automatically build emulators.

## Technical Context

**Language/Version**: Shell scripts (bash/sh), compatible with existing Medley build infrastructure
**Primary Dependencies**:
- C emulator: CMake or Make, clang/gcc, X11/SDL2 libraries
- Zig emulator: Zig compiler 0.11+, SDL2 libraries
- Lisp emulator: SBCL, ASDF, SDL3 libraries
- Platform detection: `maiko/bin/osversion`, `maiko/bin/machinetype`
**Storage**: File system (build artifacts, configuration files)
**Testing**: Shell script testing, integration tests with actual builds
**Target Platform**: Linux, macOS, FreeBSD (same as existing Maiko support)
**Project Type**: Build system orchestration (shell scripts)
**Performance Goals**: Build all three emulators in under 10 minutes, single emulator in under 3 minutes on standard development machine
**Constraints**:
- Must not modify existing emulator build systems
- Must preserve existing Medley script compatibility
- Must work with existing Maiko location resolution (PATH, MAIKODIR, MEDLEYDIR/../maiko, MEDLEYDIR/maiko)
- Must support incremental builds
**Scale/Scope**:
- 3 emulator implementations
- Multiple platforms (Linux, macOS, FreeBSD)
- Multiple architectures (x86_64, i386, arm64, arm7l, sparc)
- Integration with existing Medley loadup scripts

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Check (Phase 0)

### I. Platform Portability ✅
**Status**: COMPLIANT
**Rationale**: The unified build system will leverage existing platform detection mechanisms (`osversion`, `machinetype`) from Maiko. It will support the same platforms and architectures as existing Maiko builds (macOS, FreeBSD, Linux, Solaris, Windows). Platform detection will be automatic during build, consistent with existing Maiko build systems.

### II. Build System Flexibility ✅
**Status**: COMPLIANT
**Rationale**: The unified build system will orchestrate multiple build systems (CMake/Make for C, Zig build for Zig, ASDF for Lisp) without replacing them. This maintains flexibility by preserving all existing build system options. The unified system acts as a wrapper that coordinates builds rather than replacing build tools.

### III. Display Abstraction ✅
**Status**: COMPLIANT
**Rationale**: The unified build system will pass display subsystem options (X11, SDL2, SDL3) to the native build tools. It does not modify display abstraction layers, only orchestrates builds with appropriate display backend selections.

### IV. Code Quality & Memory Safety ✅
**Status**: COMPLIANT
**Rationale**: The unified build system will be implemented as shell scripts following existing Medley script patterns. Shell scripts do not have memory safety concerns. Code quality will follow existing Medley script conventions and include error handling.

### V. Testing & Validation ✅
**Status**: COMPLIANT
**Rationale**: The unified build system will be tested with actual builds of all three emulators on target platforms. Integration tests will verify compatibility with existing Medley loadup scripts. Build system changes will be validated across platforms.

**Pre-Research Gate Status**: ✅ **PASS** - All constitution principles satisfied

---

### Post-Design Check (Phase 1)

After completing Phase 1 design and contracts, all constitution principles remain satisfied:

### I. Platform Portability ✅
**Status**: COMPLIANT (No Change)
**Rationale**: Design confirms use of `detect_platform()` function leveraging `osversion`/`machinetype`. Platform detection is automatic and supports all existing Maiko platforms. Unified output structure `maiko/build/<emulator>/<os>.<arch>/` maintains platform-specific organization.

### II. Build System Flexibility ✅
**Status**: COMPLIANT (No Change)
**Rationale**: Design confirms wrapper approach - individual build scripts (`build-c-emulator.sh`, `build-zig-emulator.sh`, `build-lisp-emulator.sh`) call native build tools without modification. All existing build system options preserved and passed through.

### III. Display Abstraction ✅
**Status**: COMPLIANT (No Change)
**Rationale**: Design confirms display backend options are passed to native build tools via unified interface (`--display-backend`). No modification to display abstraction layers.

### IV. Code Quality & Memory Safety ✅
**Status**: COMPLIANT (No Change)
**Rationale**: Design confirms shell script implementation following existing Medley patterns. Error handling, exit codes, and clear error messages specified in contracts.

### V. Testing & Validation ✅
**Status**: COMPLIANT (No Change)
**Rationale**: Design includes integration points with loadup scripts. Contracts specify exit codes and error handling for validation. Quickstart guide provides testing workflows.

**Post-Design Gate Status**: ✅ **PASS** - All constitution principles remain satisfied after design phase

## Project Structure

### Documentation (this feature)

```text
specs/003-unified-build-system/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/          # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
medley/scripts/
├── build/
│   ├── build-all-emulators.sh      # Main orchestrator script
│   ├── build-emulator.sh            # Build individual emulator
│   ├── build-c-emulator.sh          # C emulator build wrapper
│   ├── build-zig-emulator.sh        # Zig emulator build wrapper
│   ├── build-lisp-emulator.sh         # Lisp emulator build wrapper
│   ├── detect-platform.sh            # Platform detection utilities
│   ├── check-prerequisites.sh        # Prerequisite checking
│   └── common.sh                     # Shared functions and constants
│
├── loadups/
│   ├── loadup-all.sh                 # Modified to support emulator selection
│   ├── loadup-init.sh                # Modified to support emulator selection
│   └── [other loadup scripts]        # Modified as needed for integration
│
└── medley/
    └── [existing scripts]            # May need minor modifications for emulator selection

maiko/build/                           # New unified build output directory
├── c/
│   ├── linux.x86_64/
│   │   ├── lde
│   │   ├── ldeinit
│   │   └── ldex
│   ├── darwin.aarch64/
│   └── [other platforms]
├── zig/
│   ├── linux.x86_64/
│   │   └── zaiko
│   └── [other platforms]
└── lisp/
    ├── linux.x86_64/
    │   └── laiko
    └── [other platforms]
```

**Structure Decision**: The build system will be implemented as shell scripts in `medley/scripts/build/` to maintain consistency with existing Medley script infrastructure. The unified build output location `maiko/build/<emulator>/<os>.<arch>/` is separate from existing build outputs to avoid conflicts. Loadup scripts will be modified to support emulator selection and automatic building, but core functionality will be preserved.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations - all constitution principles satisfied.
