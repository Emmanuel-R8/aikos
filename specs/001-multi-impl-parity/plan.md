# Implementation Plan: Multi-Implementation Parity Workflow

**Branch**: `001-multi-impl-parity` | **Date**: 2026-02-10 | **Spec**: `specs/001-multi-impl-parity/spec.md`
**Input**: Feature specification from `specs/001-multi-impl-parity/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Primary goal: provide a stepwise parity workflow that compares the C reference
emulator against other implementations over small instruction windows, detects
and reports divergences, and records verified windows in persistent state so
long-running parity efforts can be resumed and inspected.

At a high level, the technical approach is:

- Use a unified test harness to run each implementation for a bounded step
  window and produce traces in the unified format.
- Use an execution-window analyzer to derive structured JSON from traces.
- Use an iterative workflow orchestrator to move window-by-window, persisting
  verification state and generating parity status artifacts.

## Technical Context

**Language/Version**: Python 3.12 (for orchestration scripts); existing C, Zig,
Common Lisp, and TypeScript implementations remain the emulator engines.
**Primary Dependencies**: Standard library only for orchestration (subprocess,
argparse, json, pathlib, signal, tempfile, shutil); existing build toolchains
for each emulator; Typst for documentation.
**Storage**: Files on disk in the repository (JSON state, JSONL logs, trace
files, Typst docs); no external database.
**Testing**: Python unit tests and/or smoke tests for orchestration scripts;
existing emulator tests (`zig build test`, C/Laiko/Taiko test flows) reused for
baseline confidence — exact test harness details NEEDS CLARIFICATION if formal
test suite for the workflow itself is required.
**Target Platform**: Linux development environment (project’s standard dev
setup), capable of building and running all emulator implementations.
**Project Type**: Single multi-language project (this repository), with
orchestration logic in `scripts/` and implementation-specific code under
`maiko/`, `zaiko/`, `laiko/`, and `taiko/`.
**Performance Goals**: Parity workflow should be able to verify the first 100
steps across all available implementations in under 10 minutes of wall-clock
time on a typical developer machine; longer runs are acceptable for nightly or
ad-hoc deeper checks.
**Constraints**: No `rm` in scripts; workflow must be robust to interruption
and resume from persisted state; all new source files kept under 500 lines and
preferably under 300 lines by modular design.
**Scale/Scope**: Initially focused on step ranges up to at least 0–95 (first
~100 steps), with design that generalizes to larger ranges as needed.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- **Reference-First (I)**: Plan explicitly treats the C implementation as the
  ground truth when comparing behaviors; all parity judgments are defined
  relative to C traces. ✅
- **Backward Compatibility for Medley Run Scripts (II)**: Workflow works via
  separate orchestration scripts and environment variables; it does not change
  existing Medley run script defaults or semantics. ✅
- **Script Portability and Safety (III)**: New Python scripts avoid interactive
  prompts, use explicit configuration precedence (CLI flags over env over
  defaults), and avoid `rm` by archiving or truncating files safely. ✅
- **Operator-Facing Error Messages (IV)**: Plan requires errors to surface
  clear, user-facing summaries (e.g., “Zig emulator trace missing”) followed by
  technical details (paths, exit codes). ✅
- **Concurrency Discipline (V)**: This workflow is expected to run as a single
  orchestrator process at a time; if a lock mechanism is later added to the
  parity scripts, it must define stale-lock behavior and cleanup explicitly.
  For now, this feature does not introduce new concurrent runners. ✅
- **Traceability: Requirements ↔ Tasks (VI)**: For this feature, each FR in
  `spec.md` will be mapped to at least one task in `tasks.md`, and the plan
  avoids leaving template placeholders in committed artifacts. ✅

At this planning stage there are **no intentional constitution violations** and
no gates are blocking Phase 0 research.

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
<!--
  ACTION REQUIRED: Replace the placeholder tree below with the concrete layout
  for this feature. Delete unused options and expand the chosen structure with
  real paths (e.g., apps/admin, packages/something). The delivered plan must
  not include Option labels.
-->

```text
# [REMOVE IF UNUSED] Option 1: Single project (DEFAULT)
src/
├── models/
├── services/
├── cli/
└── lib/

tests/
├── contract/
├── integration/
└── unit/

# [REMOVE IF UNUSED] Option 2: Web application (when "frontend" + "backend" detected)
backend/
├── src/
│   ├── models/
│   ├── services/
│   └── api/
└── tests/

frontend/
├── src/
│   ├── components/
│   ├── pages/
│   └── services/
└── tests/

# [REMOVE IF UNUSED] Option 3: Mobile + API (when "iOS/Android" detected)
api/
└── [same as backend above]

ios/ or android/
└── [platform-specific structure: feature modules, UI flows, platform tests]
```

**Structure Decision**: [Document the selected structure and reference the real
directories captured above]

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |
