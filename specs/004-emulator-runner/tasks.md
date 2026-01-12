# Tasks: Emulator Runner Scripts for Interlisp

**Input**: Design documents from `/specs/004-emulator-runner/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: No test tasks included - feature specification does not request TDD approach.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- Scripts in `medley/scripts/`
- Test scripts in `tests/scripts/`
- Existing run-medley script will be modified

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic script structure

- [x] T001 Create medley/scripts/emulator_utils.sh with basic utility functions
- [x] T002 Create tests/scripts/ directory structure for test scripts
- [x] T003 [P] Backup existing run-medley script for rollback capability

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Implement emulator type validation function in medley/scripts/emulator_utils.sh
- [x] T005 Implement platform detection logic in medley/scripts/emulator_utils.sh
- [x] T006 Implement emulator path resolution (unified build + fallback) in medley/scripts/emulator_utils.sh
- [x] T007 Implement basic file validation (exists, executable, size) in medley/scripts/emulator_utils.sh
- [x] T008 Implement lock file creation/management in medley/scripts/emulator_utils.sh

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Run Interlisp with Selected Emulator (Priority: P1) 🎯 MVP

**Goal**: Enable developers to run Interlisp with C, Zig, or Lisp emulators via command-line selection

**Independent Test**: Can be fully tested by running `./run-medley --emulator zig` and verifying that the Zig emulator is used to start Interlisp, delivering the ability to choose emulators at runtime.

### Implementation for User Story 1

- [x] T009 [US1] Modify run-medley script to accept --emulator flag with validation
- [x] T010 [US1] Integrate emulator path resolution into run-medley execution flow
- [x] T011 [US1] Add error handling for invalid/missing emulators in run-medley
- [x] T012 [US1] Update run-medley to use selected emulator for Interlisp startup
- [x] T013 [US1] Test US1 independently with all three emulators

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently

---

## Phase 4: User Story 2 - Set Default Emulator Preference (Priority: P2)

**Goal**: Allow developers to set a default emulator preference via environment variable

**Independent Test**: Can be fully tested by setting `MEDLEY_EMULATOR=zig` and running `./run-medley` without the `--emulator` flag, verifying the Zig emulator is used by default.

### Implementation for User Story 2

- [x] T014 [US2] Add MEDLEY_EMULATOR environment variable support to run-medley
- [x] T015 [US2] Implement precedence logic (command-line > environment > default) in emulator_utils.sh
- [x] T016 [US2] Update run-medley help text to document environment variable usage
- [x] T017 [US2] Test US2 independently with environment variable scenarios

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently

---

## Phase 5: User Story 3 - Automatic Emulator Building on First Run (Priority: P3)

**Goal**: Automatically build missing emulators when --auto-build flag is used

**Independent Test**: Can be fully tested by removing a built emulator and running `./run-medley --emulator zig --auto-build`, verifying the Zig emulator is built automatically before Interlisp starts.

### Implementation for User Story 3

- [x] T018 [US3] Add --auto-build flag parsing to run-medley
- [x] T019 [US3] Implement build command detection for each emulator type
- [x] T020 [US3] Add automatic build execution before emulator validation
- [x] T021 [US3] Handle build failures with clear error messages
- [x] T022 [US3] Test US3 independently with build scenarios

**Checkpoint**: All user stories should now be independently functional

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [x] T023 [P] Update run-medley help text with comprehensive usage examples
- [x] T024 [P] Add shell script linting and validation
- [x] T025 Improve error messages for better user experience
- [x] T026 [P] Create comprehensive test scripts in tests/scripts/
- [x] T027 Validate backward compatibility with existing run-medley usage
- [x] T028 Update documentation and quickstart validation

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - Builds on US1 but independently testable
- **User Story 3 (P3)**: Can start after Foundational (Phase 2) - Builds on US1 but independently testable

### Within Each User Story

- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes, all user stories can start in parallel (if team capacity allows)
- Different user stories can be worked on in parallel by different team members

---

## Parallel Example: User Story 1

```bash
# Launch foundational utilities together:
Task: "Implement emulator type validation function in medley/scripts/emulator_utils.sh"
Task: "Implement platform detection logic in medley/scripts/emulator_utils.sh"

# Launch US1 implementation tasks:
Task: "Modify run-medley script to accept --emulator flag with validation"
Task: "Integrate emulator path resolution into run-medley execution flow"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo
4. Add User Story 3 → Test independently → Deploy/Demo
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1
   - Developer B: User Story 2
   - Developer C: User Story 3
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
