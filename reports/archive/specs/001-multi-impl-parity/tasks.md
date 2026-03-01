# Tasks: Multi-Implementation Parity Workflow

**Input**: Design documents from `/specs/001-multi-impl-parity/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, quickstart.md, contracts/ (optional; see contracts/README.md)

**Tests**: No explicit test-first requirement was specified in the spec; tasks focus on implementing the workflow and its observable behavior. Tests may still be added within implementation work where useful.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Ensure environment and core scripts are discoverable and consistent.

- [ ] T001 Verify C, Zig, Laiko, and Taiko emulators build and run basic smoke tests per plan.md in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
- [x] T002 [P] Document canonical parity window defaults (size 6) and override behavior in specs/001-multi-impl-parity/quickstart.md
- [x] T003 [P] Ensure unified test harness wrapper exists and is executable in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness.py

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be fully implemented.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [x] T004 Ensure unified test harness package structure is in place under /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/ (main.py, runners.py, trace_extractor.py)
- [x] T005 [P] Ensure execution window analysis package exists under /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/execution_window_analysis/ (parser.py, analyzer.py) and wrapper script in scripts/analyze_execution_window.py
- [x] T006 [P] Confirm iterative parity workflow orchestrator script exists and is runnable at /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/iterative_parity_workflow.py
- [x] T007 Define workflow state file schema in specs/001-multi-impl-parity/data-model.md and align it with parity_workflow_state.json expectations
- [x] T008 Configure safe file operations policy for traces and analysis outputs (archive or truncate, no rm) in scripts/unified_test_harness and scripts/iterative_parity_workflow.py

**Checkpoint**: Foundation ready – unified harness, analysis, and workflow orchestration exist and follow safety/constitution rules.

---

## Phase 3: User Story 1 – Run stepwise parity verification (Priority: P1) 🎯 MVP

**Goal**: Provide a workflow that runs parity checks for configurable step windows and reports divergences against the C reference.

**Independent Test**: From quickstart.md, a maintainer can run the workflow for steps 0–5 and see which implementations match or diverge without manual trace comparison.

### Implementation for User Story 1

- [x] T009 [US1] Wire unified test harness to run C emulator and write C trace windows in unified format in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/runners.py
- [x] T010 [P] [US1] Wire unified test harness to run Zig emulator and write Zig trace windows in unified format in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/runners.py
- [x] T011 [P] [US1] Wire unified test harness to run Laiko (Common Lisp) emulator and write trace windows in unified format in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/runners.py
- [x] T012 [P] [US1] Wire unified test harness to integrate TypeScript emulator traces when available, returning a clear diagnostic if not implemented yet in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/runners.py
- [x] T013 [US1] Implement CLI interface and argument parsing in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/main.py to support --implementation, --start-step, --end-step, --sysout, --output
- [x] T014 [US1] Implement trace window extraction and validation helpers in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness/trace_extractor.py
- [x] T015 [US1] Implement unified test harness wrapper script entrypoint in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/unified_test_harness.py
- [x] T016 [US1] Implement unified trace line parsing and sub-field parsing in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/execution_window_analysis/parser.py
- [x] T017 [US1] Implement execution window analyzer to derive per-step opcode, stack, registers, and divergence-relevant data in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/execution_window_analysis/analyzer.py
- [x] T018 [US1] Ensure analyze_execution_window.py CLI writes structured JSON analysis to disk for a given window in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/analyze_execution_window.py
- [x] T019 [US1] Integrate unified harness and analyzer in iterative parity workflow by running C reference for a window and generating analysis in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/iterative_parity_workflow.py
- [x] T020 [US1] Integrate Zig, Laiko, and Taiko runs into iterative parity workflow and compare their traces against the C reference using compare_unified_traces.py in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/iterative_parity_workflow.py
- [x] T021 [US1] Ensure iterative parity workflow stops advancing when a divergence is detected and reports which implementations diverged in scripts/iterative_parity_workflow.py
- [ ] T022 [US1] Validate end-to-end window 0–5 parity run as described in specs/001-multi-impl-parity/quickstart.md using scripts/iterative_parity_workflow.py

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently (single-window parity run, clear divergence reporting).

---

## Phase 4: User Story 2 – Resume long-running parity runs (Priority: P2)

**Goal**: Allow interrupted parity workflows to resume from the last verified window using persisted state.

**Independent Test**: A maintainer can run the workflow for several windows, interrupt it, and restart it, observing that it resumes from the next unverified window without recomputing prior windows.

### Implementation for User Story 2

- [x] T023 [US2] Implement Workflow State structure and JSON serialization in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/iterative_parity_workflow.py consistent with specs/001-multi-impl-parity/data-model.md
- [x] T024 [US2] Implement atomic write logic for parity_workflow_state.json using a temporary file and rename in scripts/iterative_parity_workflow.py
- [x] T025 [US2] On workflow startup, load existing parity_workflow_state.json and determine last_verified_step safely in scripts/iterative_parity_workflow.py
- [x] T026 [US2] Implement resume logic that starts from next unverified window based on last_verified_step and window_size in scripts/iterative_parity_workflow.py
- [x] T027 [US2] Handle missing or corrupted state files gracefully by starting from step 0 with an explanatory message in scripts/iterative_parity_workflow.py
- [x] T028 [US2] Ensure that after each successfully verified window the workflow updates last_verified_step and persists state in scripts/iterative_parity_workflow.py
- [ ] T029 [US2] Validate resume behavior by running multiple windows, interrupting, and restarting as described in specs/001-multi-impl-parity/quickstart.md using scripts/iterative_parity_workflow.py

**Checkpoint**: User Story 1 (parity per window) and User Story 2 (resume behavior) should both work independently.

---

## Phase 5: User Story 3 – Inspect parity status and divergences (Priority: P3)

**Goal**: Provide human-readable artifacts that summarize parity progress and highlight divergences for reviewers.

**Independent Test**: After several windows have been processed, a reviewer can inspect status artifacts to see which windows are verified, which diverge, and the basic shape of differences without reading raw traces.

### Implementation for User Story 3

- [x] T030 [US3] Define parity status artifact fields based on the Parity Status Artifact entity in specs/001-multi-impl-parity/data-model.md
- [x] T031 [US3] Implement parity status artifact generation (e.g., parity_workflow_dashboard.json) in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/iterative_parity_workflow.py
- [x] T032 [US3] Ensure parity status artifact is updated after each verified window with current_step, last_verified_step, window counts, and recent activity in scripts/iterative_parity_workflow.py
- [x] T033 [US3] Ensure divergence reports are written in a structured JSON format derived from execution_window_analysis outputs in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/iterative_parity_workflow.py
- [x] T034 [US3] Link divergence reports to specific parity windows (by start_step and end_step) in scripts/iterative_parity_workflow.py or post-processing scripts
- [x] T035 [US3] Update specs/001-multi-impl-parity/quickstart.md to describe how to read parity status and divergence reports
- [ ] T036 [US3] Validate that after several windows and at least one divergence, the status artifact and divergence reports match the behaviors described in specs/001-multi-impl-parity/spec.md

**Checkpoint**: All user stories should now be independently functional: parity per window, resume behavior, and status inspection.

---

## Phase N: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and long-term maintainability.

- [x] T037 [P] Add or refine inline documentation and docstrings in all new scripts under /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/
- [x] T038 [P] Ensure all new Python modules stay under file size guidelines and refactor into smaller modules where necessary in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/
- [ ] T039 Run quickstart.md validation by following the documented steps end-to-end for a short parity run using specs/001-multi-impl-parity/quickstart.md
- [x] T040 [P] Integrate parity workflow into existing comparison/reporting scripts where beneficial in /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/scripts/
- [x] T041 [Compliance] Implement lock mechanism for parity workflow per constitution V in scripts/iterative_parity_workflow.py (stale-lock definition, cleanup on exit/crash). See plan.md Complexity Tracking.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies – can start immediately.
- **Foundational (Phase 2)**: Depends on Setup completion – BLOCKS all user stories.
- **User Stories (Phase 3+)**: All depend on Foundational phase completion.
  - User Story 1 (P1) delivers the MVP parity workflow.
  - User Story 2 (P2) depends on User Story 1’s core execution path.
  - User Story 3 (P3) depends on data produced by User Story 1 and state from User Story 2.
- **Polish (Final Phase)**: Depends on all desired user stories being complete.

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2); no dependency on other stories.
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) but is most valuable once US1’s basic parity execution is in place.
- **User Story 3 (P3)**: Can start after Foundational (Phase 2); consumes outputs from US1 and US2 but should still be independently testable from a reviewer’s perspective.

### Within Each User Story

- Implement core behavior first (harness/analysis/workflow for US1; state/resume logic for US2; artifact generation for US3).
- Add refinement and documentation within the same story phase.
- Ensure each story is independently completable and testable as described in the spec and quickstart.

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel.
- All Foundational tasks marked [P] can run in parallel within Phase 2 (they touch different modules).
- Within User Story 1, runners for different implementations (C, Zig, Laiko, Taiko) can be wired in parallel as they live in separate code paths.
- User Story 2 and User Story 3 can partially overlap, provided state schema and basic workflow execution are stable.

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup.
2. Complete Phase 2: Foundational (CRITICAL – blocks all stories).
3. Complete Phase 3: User Story 1 (core parity per window).
4. **STOP and VALIDATE**: Run the single-window parity flow (steps 0–5) and confirm behavior matches the spec.

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready.
2. Add User Story 1 → Test independently → use as baseline parity tool.
3. Add User Story 2 → Test independently (resume behavior) → improve usability for long runs.
4. Add User Story 3 → Test independently (status/divergence inspection) → improve observability.

### Parallel Team Strategy

With multiple contributors:

1. Team completes Setup + Foundational together.
2. Once Foundational is done:
   - Developer A: User Story 1 (core parity).
   - Developer B: User Story 2 (state and resume).
   - Developer C: User Story 3 (status and reporting).
3. Stories complete and integrate independently and can be validated with the quickstart steps.
