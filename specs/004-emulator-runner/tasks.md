# Tasks: Emulator Runner Scripts for Interlisp

**Input**: Design documents from `specs/004-emulator-runner/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: No test tasks included (not requested in specification)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create shared utility functions and infrastructure for emulator runner scripts

- [X] T001 Create shared utility script for emulator runner functions in medley/scripts/medley/emulator_utils.sh
- [X] T002 [P] Create lock file directory structure helper function in medley/scripts/medley/emulator_utils.sh
- [X] T003 [P] Add error message formatting functions to medley/scripts/medley/emulator_utils.sh

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core functions that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T004 Implement select_emulator() function in medley/scripts/medley/emulator_utils.sh (handles command-line, environment, default precedence)
- [X] T005 Implement find_emulator_executable() function in medley/scripts/medley/emulator_utils.sh (unified build location → MAIKODIR → PATH)
- [X] T006 Implement validate_emulator_executable() function in medley/scripts/medley/emulator_utils.sh (checks existence, permissions, basic validation)
- [X] T007 Implement acquire_run_lock() function in medley/scripts/medley/emulator_utils.sh (PID-based lock with stale detection)
- [X] T008 Implement release_run_lock() function in medley/scripts/medley/emulator_utils.sh (cleanup on exit)
- [X] T009 Implement build_emulator_if_needed() function in medley/scripts/medley/emulator_utils.sh (calls build-emulator.sh when --auto-build specified)
- [X] T010 [P] Integrate platform detection utilities (osversion, machinetype) into emulator_utils.sh
- [X] T011 [P] Add error message constants and formatting to medley/scripts/medley/emulator_utils.sh

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Run Interlisp with Selected Emulator (Priority: P1) 🎯 MVP

**Goal**: Enable developers to run Interlisp using a specific emulator implementation (C, Zig, or Lisp) via `--emulator` command-line argument

**Independent Test**: Run `./run-medley --emulator zig` and verify that the Zig emulator is used to start Interlisp, delivering the ability to choose emulators at runtime.

### Implementation for User Story 1

- [X] T012 [US1] Add --emulator argument parsing to medley/run-medley (accept c|zig|lisp, validate input)
- [X] T013 [US1] Integrate select_emulator() function into medley/run-medley argument processing
- [X] T014 [US1] Integrate find_emulator_executable() function into medley/run-medley (replace existing emulator location logic)
- [X] T015 [US1] Integrate validate_emulator_executable() function into medley/run-medley (before emulator invocation)
- [X] T016 [US1] Integrate acquire_run_lock() and release_run_lock() into medley/run-medley (prevent concurrent runs)
- [X] T017 [US1] Update emulator executable selection logic in medley/run-medley to use selected emulator type
- [X] T018 [US1] Add --override-lock argument parsing to medley/run-medley
- [X] T019 [US1] Update medley/scripts/medley/medley.command to add --emulator argument parsing
- [X] T020 [US1] Integrate select_emulator() function into medley/scripts/medley/medley.command
- [X] T021 [US1] Enhance check_unified_build_location() in medley/scripts/medley/medley.command to use selected emulator
- [X] T022 [US1] Integrate validate_emulator_executable() into medley/scripts/medley/medley.command
- [X] T023 [US1] Integrate lock mechanism into medley/scripts/medley/medley.command
- [X] T024 [US1] Update medley/scripts/medley/medley_run.sh to support emulator selection from environment/arguments
- [X] T025 [US1] Integrate validate_emulator_executable() into medley/scripts/medley/medley_run.sh
- [X] T026 [US1] Ensure all existing Medley run script functionality is preserved (sysout selection, display options, memory settings)

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Developers can run `./run-medley --emulator <c|zig|lisp>` and Interlisp will start with the selected emulator.

---

## Phase 4: User Story 2 - Set Default Emulator Preference (Priority: P2)

**Goal**: Enable developers to set a default emulator preference via `MEDLEY_EMULATOR` environment variable so they don't have to specify it every time

**Independent Test**: Set `MEDLEY_EMULATOR=zig` and run `./run-medley` without the `--emulator` flag, verifying the Zig emulator is used by default.

### Implementation for User Story 2

- [X] T027 [US2] Enhance select_emulator() function in medley/scripts/medley/emulator_utils.sh to read MEDLEY_EMULATOR environment variable
- [X] T028 [US2] Ensure command-line --emulator argument takes precedence over MEDLEY_EMULATOR environment variable in select_emulator()
- [X] T029 [US2] Update select_emulator() to default to "c" when neither command-line nor environment variable is set
- [X] T030 [US2] Verify environment variable precedence works correctly in medley/run-medley
- [X] T031 [US2] Verify environment variable precedence works correctly in medley/scripts/medley/medley.command
- [X] T032 [US2] Verify environment variable precedence works correctly in medley/scripts/medley/medley_run.sh
- [X] T033 [US2] Add documentation for MEDLEY_EMULATOR environment variable usage

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Developers can set `MEDLEY_EMULATOR` for default preference, and command-line arguments override it.

---

## Phase 5: User Story 3 - Automatic Emulator Building on First Run (Priority: P3)

**Goal**: Enable automatic emulator building when emulator is missing and --auto-build flag is specified

**Independent Test**: Remove a built emulator and run `./run-medley --emulator zig --auto-build`, verifying the Zig emulator is built automatically before Interlisp starts.

### Implementation for User Story 3

- [X] T034 [US3] Add --auto-build argument parsing to medley/run-medley
- [X] T035 [US3] Integrate build_emulator_if_needed() function into medley/run-medley (call when --auto-build and emulator missing)
- [X] T036 [US3] Add error handling for build failures in medley/run-medley (display clear error, exit with code 3)
- [X] T037 [US3] Add --auto-build argument parsing to medley/scripts/medley/medley.command
- [X] T038 [US3] Integrate build_emulator_if_needed() into medley/scripts/medley/medley.command
- [X] T039 [US3] Add error handling for build failures in medley/scripts/medley/medley.command
- [X] T040 [US3] Add --auto-build argument parsing to medley/scripts/medley/medley_run.sh (if needed)
- [X] T041 [US3] Integrate build_emulator_if_needed() into medley/scripts/medley/medley_run.sh (if needed)
- [X] T042 [US3] Ensure build_emulator_if_needed() displays build progress to user
- [X] T043 [US3] Verify error message when emulator missing and --auto-build not specified (exit code 2, clear message)

**Checkpoint**: All user stories should now be independently functional. Developers can use --auto-build to automatically build missing emulators before running Interlisp.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and ensure production readiness

- [X] T044 [P] Update medley/BUILDING.md to document emulator selection features
- [X] T045 [P] Update medley/scripts/README.md (if exists) with emulator runner documentation
- [X] T046 [P] Add usage examples to quickstart.md validation
- [X] T047 Verify 100% backward compatibility with existing Medley run script functionality
- [X] T048 Verify all error messages are clear and actionable (per contracts/run-scripts-api.md)
- [X] T049 Verify exit codes match contract specifications (0=success, 1=general error, 2=not found, 3=build failed, 4=lock held, 5=validation failed)
- [ ] T050 Test emulator selection on all supported platforms (Linux, macOS, FreeBSD)
- [X] T051 Verify lock file cleanup on script exit (trap handlers work correctly)
- [X] T052 Verify stale lock detection works correctly (5 minute timeout, dead process detection)
- [X] T053 Verify platform-specific emulator availability handling (graceful error messages)
- [X] T054 Code cleanup and refactoring (ensure consistent error handling patterns)
- [ ] T055 Verify all three emulator types work correctly (C, Zig, Lisp)
- [ ] T056 Run quickstart.md validation scenarios

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed sequentially in priority order (P1 → P2 → P3)
  - US2 and US3 depend on US1 being complete (they enhance US1 functionality)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Depends on US1 completion - Enhances select_emulator() function from US1
- **User Story 3 (P3)**: Depends on US1 completion - Adds auto-build feature to US1 workflow

### Within Each User Story

- Core functions (select_emulator, find_emulator_executable) before script integration
- Argument parsing before function integration
- Validation before emulator invocation
- Lock mechanism before emulator execution
- Error handling throughout

### Parallel Opportunities

- Setup tasks T002 and T003 can run in parallel (different functions in same file, but independent)
- Foundational tasks T010 and T011 can run in parallel (different functions)
- Within US1: Tasks T019-T025 (medley.command and medley_run.sh modifications) can be worked on in parallel after T012-T018 (run-medley) are complete
- Polish tasks T044-T046 can run in parallel (different documentation files)

---

## Parallel Example: User Story 1

```bash
# After completing run-medley integration (T012-T018), these can run in parallel:
Task: "Update medley/scripts/medley/medley.command to add --emulator argument parsing" (T019)
Task: "Update medley/scripts/medley/medley_run.sh to support emulator selection" (T024)

# These validation tasks can run in parallel:
Task: "Integrate validate_emulator_executable() into medley.command" (T022)
Task: "Integrate validate_emulator_executable() into medley_run.sh" (T025)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (shared utilities)
2. Complete Phase 2: Foundational (core functions) - **CRITICAL - blocks all stories**
3. Complete Phase 3: User Story 1 (emulator selection via --emulator)
4. **STOP and VALIDATE**: Test User Story 1 independently with `./run-medley --emulator zig`
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo (default preference)
4. Add User Story 3 → Test independently → Deploy/Demo (auto-build)
5. Each story adds value without breaking previous stories

### Sequential Strategy (Recommended)

Since this is a script enhancement project with tight integration:

1. Complete Setup + Foundational together
2. Implement User Story 1 completely (all three scripts: run-medley, medley.command, medley_run.sh)
3. Test User Story 1 thoroughly
4. Implement User Story 2 (enhances US1)
5. Test User Story 2
6. Implement User Story 3 (enhances US1)
7. Test User Story 3
8. Polish and cross-cutting concerns

---

## Notes

- [P] tasks = different files or independent functions, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- All modifications must preserve 100% backward compatibility
- Error messages must be clear and actionable per contracts
- Lock mechanism must prevent concurrent runs (FR-016)
- All three run scripts (run-medley, medley.command, medley_run.sh) must support all features
