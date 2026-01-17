# Tasks: Runner Scripts + Execution Trace Parity for Interlisp

**Input**: Design documents from `/specs/004-emulator-runner/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/, and execution debugging docs (`execution-*.md`)

**Tests**: No TDD requirement. Include scripted parity workflows (log generation + comparison) as part of US4 so the work stays repeatable.

**Status**: Phase 0 (C Tracing) COMPLETE - All 6 tasks (CT000-CT006) implemented. Parity progress: Zig matches C for first 38 instructions (divergence at instruction 38).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Every task MUST include exact file paths in its description

## Path Conventions

- Runner entrypoint: `medley/run-medley`
- Runner utilities: `medley/scripts/medley/emulator_utils.sh`
- Parity tooling: `scripts/`
- C emulator (reference): `maiko/src/`
- Zig emulator: `zaiko/src/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Ensure the repo has the expected parity and runner tooling layout.

- [x] T001 Verify parity tooling is present and runnable (python3/shebangs) in `scripts/generate_debug_logs.sh`, `scripts/compare_debug_logs.sh`, `scripts/analyze_execution_divergence.py`, `scripts/enhanced_divergence_analysis.py` (FR-019/FR-020)
- [x] T002 Verify runner entrypoints exist and are executable: `medley/run-medley`, `medley/scripts/medley/medley.command`, `medley/scripts/medley/medley_run.sh` (FR-011)
- [x] T003 [P] Verify unified-build path expectations for emulators in `medley/scripts/medley/emulator_utils.sh` (FR-006/FR-007)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before any story can be validated end-to-end.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [x] T004 Ensure lock file behavior matches spec/contracts (path + content + stale threshold) in `medley/scripts/medley/emulator_utils.sh` (FR-016..FR-018)
- [x] T005 Ensure emulator selection precedence and validation are correct in `medley/scripts/medley/emulator_utils.sh` and `medley/run-medley` (FR-001..FR-005)
- [x] T006 Ensure platform-specific emulator availability is handled gracefully (clear message when emulator/build not available for current platform) in `medley/scripts/medley/emulator_utils.sh` and `medley/run-medley` (FR-012)
- [x] T007 Ensure proactive emulator executable validation matches spec (including shebang acceptance) in `medley/scripts/medley/emulator_utils.sh` (FR-015)

**Checkpoint**: Foundation ready.

---

## Phase 3: User Story 4 - Execution Trace Parity: Make Zig Match C (Priority: P0)

**Goal**: Provide a repeatable workflow to generate/compare C vs Zig traces and iteratively fix Zig until it matches C **to completion**, staged: `starter.sysout` first, then `full.sysout`.

**Independent Test**:

- Stage 1: `starter.sysout` traces match to completion
- Stage 2: `full.sysout` traces match to completion (only after Stage 1 is complete)

### Implementation for User Story 4 (Parity Workflow)

- [x] T008 [US4] Define canonical trace line format (required fields + ordering) in `specs/004-emulator-runner/contracts/run-scripts-api.md` (FR-021)
- [x] T009 [US4] Update `scripts/generate_debug_logs.sh` to support staged sysout runs (`starter.sysout` then `full.sysout`) and deterministic log output paths (FR-019, FR-025)
- [x] T010 [US4] Implement “skip already-matching prefix” by adding LCP detection to `scripts/analyze_execution_divergence.py` (FR-024)
- [x] T011 [US4] Add manual resume override (e.g., `--start-line N`) to `scripts/analyze_execution_divergence.py` (FR-024)
- [x] T012 [US4] Update `scripts/enhanced_divergence_analysis.py` to surface LCP length and first-divergence context (PC/bytes/opcode/stack/frame) (FR-020, FR-024)

### Runtime step cap (fast iteration, no source patching)

- [x] T013 [US4] Add shared runtime knob `EMULATOR_MAX_STEPS` to Zig execution loop in `zaiko/src/vm/dispatch.zig` (FR-026)
- [x] T014 [US4] Add shared runtime knob `EMULATOR_MAX_STEPS` to C execution tracing loop in `maiko/src/xc.c` (FR-026)
- [x] T015 [US4] Update parity scripts to use the runtime knob (and stop patching source files) in `scripts/compare_emulator_execution.sh` (FR-022, FR-026)
- [x] T016 [US4] Update parity documentation to mention `EMULATOR_MAX_STEPS` for fast iteration in `specs/004-emulator-runner/quickstart.md` and `specs/004-emulator-runner/contracts/run-scripts-api.md` (FR-022, FR-026)

**Checkpoint**: Parity tooling supports (a) LCP skip, (b) manual resume, and (c) runtime step caps without source modification.

---

## Phase 4: Parity Stage 1 - `starter.sysout` to Completion (Priority: P0)

**Goal**: Achieve completion parity on `medley/internal/loadups/starter.sysout` (SC-009).

- [x] T017 [US4] Run baseline parity workflow for `starter.sysout` and record current first divergence using `scripts/generate_debug_logs.sh` + `scripts/analyze_execution_divergence.py`

### Fix sequence (use C as ground truth; regenerate + compare after each fix)

- [x] T018 [US4] Complete memory-loading integration verification task D014 described in `specs/004-emulator-runner/execution-debugging-tasks.md` (Zig: `zaiko/src/data/sysout.zig`)
- [x] T019 [US4] Fix FuncObj offset parity tasks D021-D023 in `zaiko/src/vm/execution_trace.zig`
- [ ] T020 [US4] Fix frame header parity tasks D024-D026 in `zaiko/src/vm/execution_trace.zig` (reference C layout in `maiko/inc/stack.h`)
- [ ] T021 [US4] Fix TOS parity tasks D028-D030 in `zaiko/src/vm/stack.zig` and `zaiko/src/vm/init.zig`
- [ ] T022 [US4] Fix early-stop parity tasks D031-D033 in `zaiko/src/vm/dispatch/dispatch_loop.zig` and `zaiko/src/main.zig`
- [ ] T023 [US4] Iterate: regenerate logs and advance divergence point until `starter.sysout` reaches completion parity (SC-009) using `scripts/generate_debug_logs.sh` + `scripts/analyze_execution_divergence.py`

**Checkpoint**: `starter.sysout` completion parity achieved (SC-009).

---

## Phase 5: Parity Stage 2 - `full.sysout` to Completion (Priority: P0)

**Goal**: After Stage 1, achieve completion parity on `medley/loadups/full.sysout` (SC-010).

- [ ] T024 [US4] Run parity workflow for `full.sysout` and record current first divergence using `scripts/generate_debug_logs.sh` + `scripts/analyze_execution_divergence.py`
- [ ] T025 [US4] Iterate fixes (same loop as Stage 1): change Zig to match C, regenerate logs, and use LCP skip/resume until completion parity is achieved (SC-010)

**Checkpoint**: `full.sysout` completion parity achieved (SC-010).

---

## Phase 6: User Story 1 - Run Interlisp with Selected Emulator (Priority: P1) 🎯 MVP

**Goal**: Run Interlisp with selected emulator via `--emulator <c|zig|lisp>` while preserving existing run-medley behavior.

**Independent Test**: `cd medley && ./run-medley --emulator zig` starts using Zig emulator (or fails with clear error).

- [ ] T026 [US1] Verify `--emulator` selection works end-to-end for C/Zig/Lisp in `medley/run-medley` (FR-001, FR-014)
- [ ] T027 [US1] Verify `medley/scripts/medley/medley.command` supports the same selection behavior via its component regeneration workflow (FR-011)
- [ ] T028 [US1] Verify `medley/scripts/medley/medley_run.sh` (component script) invokes the selected emulator consistently (FR-011)
- [ ] T029 [US1] Implement and/or verify explicit executable naming mapping (`lde/ldex/ldesdl/zaiko/laiko`) and display-backend rules in `medley/scripts/medley/emulator_utils.sh` (FR-013)

---

## Phase 7: User Story 2 - Set Default Emulator Preference (Priority: P2)

**Goal**: Support `MEDLEY_EMULATOR` as default emulator selection when `--emulator` is not provided.

**Independent Test**: `cd medley && MEDLEY_EMULATOR=zig ./run-medley` uses Zig emulator.

- [ ] T030 [US2] Verify `MEDLEY_EMULATOR` selection and precedence in `medley/run-medley` and `medley/scripts/medley/emulator_utils.sh` (FR-002, FR-003, FR-004)
- [ ] T031 [US2] Verify help/usage text documents env var precedence in `medley/run-medley` (FR-002, FR-003)

---

## Phase 8: User Story 3 - Automatic Emulator Building on First Run (Priority: P3)

**Goal**: Support `--auto-build` to build missing emulator before running.

**Independent Test**: `cd medley && ./run-medley --emulator zig --auto-build` builds and runs (or fails with clear error).

- [ ] T032 [US3] Verify `--auto-build` triggers the right build scripts and propagates failures clearly in `medley/run-medley` and `medley/scripts/medley/emulator_utils.sh` (FR-008, FR-009)

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Clean up, consistency, and validation across runner + parity workflow.

- [ ] T033 [P] Run a consistency pass on docs and contracts for lock file details and parity workflow wording in `specs/004-emulator-runner/contracts/*` and `specs/004-emulator-runner/quickstart.md` (FR-016..FR-018, FR-019..FR-026)
- [ ] T034 Validate `specs/004-emulator-runner/quickstart.md` commands end-to-end on the target platform(s) (SC-006, SC-008, SC-009, SC-010)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup completion; blocks all stories
- **US4 (Phases 3–5)**: Depends on Foundational; staged parity (`starter.sysout` then `full.sysout`)
- **US1/US2/US3 (Phases 6–8)**: Depends on Foundational; can proceed in parallel with US4 once foundational is complete
- **Polish (Phase 9)**: After any subset of stories you want to stabilize/document

### Parallel Opportunities

- Tasks touching different areas can run in parallel (e.g., Python log analyzers vs runner scripts vs emulator code), as long as they don’t fight over the same files:
  - [P] `scripts/analyze_execution_divergence.py` enhancements can proceed alongside emulator runtime knob work.
  - [P] `medley/scripts/medley/medley.command` verification can proceed alongside parity work.

---

## Parallel Example: US4 workflow improvements

```bash
Task: "Add LCP detection to scripts/analyze_execution_divergence.py"
Task: "Add EMULATOR_MAX_STEPS to zaiko/src/vm/dispatch.zig"
Task: "Add EMULATOR_MAX_STEPS to maiko/src/xc.c"
```
