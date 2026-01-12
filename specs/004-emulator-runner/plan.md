# Implementation Plan: Emulator Runner Scripts for Interlisp

**Branch**: `004-emulator-runner` | **Date**: 2026-01-12 | **Spec**: specs/004-emulator-runner/spec.md
**Input**: Feature specification from `/specs/004-emulator-runner/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Create scripts to run Interlisp with choice of emulators (C, Zig, Lisp), with locking mechanism, validation, and auto-build capability. Technical approach uses shell scripts integrating with existing Medley infrastructure and unified build system.

## Technical Context

<!--
  ACTION REQUIRED: Replace the content in this section with the technical details
  for the project. The structure here is presented in advisory capacity to guide
  the iteration process.
-->

**Language/Version**: Bash/shell scripts
**Primary Dependencies**: Existing Medley run scripts, unified build system (spec 003)
**Storage**: N/A
**Testing**: Manual testing, shell script validation
**Target Platform**: Linux, macOS (Medley supported platforms)
**Project Type**: Scripts/utilities
**Performance Goals**: Start Interlisp in under 5 seconds after emulator selection
**Constraints**: 100% backward compatibility with existing scripts
**Scale/Scope**: Single user, multiple emulator choices

## Constitution Check

_GATE: Must pass before Phase 0 research. Re-check after Phase 1 design._

Constitution is template - no specific gates defined. Assumed to pass.

## Project Structure

### Documentation (this feature)

```text
specs/004-emulator-runner/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
medley/scripts/
├── run-medley              # Modified to support --emulator flag
├── emulator_utils.sh       # New utility functions for emulator handling
└── medley/                 # Existing directory
    └── emulator_utils.sh   # Existing utility (may be extended)

tests/scripts/
├── test-emulator-selection.sh
├── test-lock-mechanism.sh
├── test-emulator-validation.sh
└── test-backward-compatibility.sh
```

**Structure Decision**: Scripts are placed in medley/scripts/ to integrate with existing Medley infrastructure. Test scripts in tests/scripts/ following project conventions.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation                  | Why Needed         | Simpler Alternative Rejected Because |
| -------------------------- | ------------------ | ------------------------------------ |
| [e.g., 4th project]        | [current need]     | [why 3 projects insufficient]        |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient]  |
