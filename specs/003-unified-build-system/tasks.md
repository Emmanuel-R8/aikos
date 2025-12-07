# Tasks: Unified Build System for Medley with Multiple Maiko Emulators

**Input**: Design documents from `specs/003-unified-build-system/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are OPTIONAL - not explicitly requested in feature specification, so no test tasks included.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [x] T001 Create build scripts directory structure in medley/scripts/build/
- [x] T002 Create unified build output directory structure maiko/build/ with subdirectories for each emulator
- [x] T003 [P] Create common.sh with shared constants and helper functions in medley/scripts/build/common.sh
- [x] T004 [P] Create detect-platform.sh script skeleton in medley/scripts/build/detect-platform.sh
- [x] T005 [P] Create check-prerequisites.sh script skeleton in medley/scripts/build/check-prerequisites.sh

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T006 Implement detect_platform() function in medley/scripts/build/common.sh using maiko/bin/osversion and maiko/bin/machinetype
- [x] T007 Implement fallback platform detection using uname in medley/scripts/build/common.sh if Maiko utilities unavailable
- [x] T008 Implement check_prerequisites() function in medley/scripts/build/common.sh for C emulator (clang/gcc, cmake/make, X11/SDL2)
- [x] T009 Implement check_prerequisites() function in medley/scripts/build/common.sh for Zig emulator (zig compiler 0.11+, SDL2)
- [x] T010 Implement check_prerequisites() function in medley/scripts/build/common.sh for Lisp emulator (SBCL, SDL3)
- [x] T011 Implement is_build_needed() function in medley/scripts/build/common.sh with timestamp comparison logic
- [x] T012 Implement find_emulator_executable() function in medley/scripts/build/common.sh with unified location search
- [x] T013 Implement error handling and exit code constants in medley/scripts/build/common.sh
- [x] T014 Implement build output directory creation logic in medley/scripts/build/common.sh

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Build All Maiko Emulators (Priority: P1) 🎯 MVP

**Goal**: Build all three Maiko emulator implementations (C, Zig, Lisp) from a single command, with each emulator built using its native build tool, and all executables placed in a unified location structure.

**Independent Test**: Run `./build-all-emulators.sh` and verify executables exist in `maiko/build/<emulator>/<os>.<arch>/` for all three emulators without requiring Medley loadup scripts.

### Implementation for User Story 1

- [x] T015 [US1] Create build-c-emulator.sh script with argument parsing in medley/scripts/build/build-c-emulator.sh
- [x] T016 [US1] Implement C emulator build logic using CMake in medley/scripts/build/build-c-emulator.sh
- [x] T017 [US1] Implement C emulator build logic using Make in medley/scripts/build/build-c-emulator.sh
- [x] T018 [US1] Implement executable copying from C build output to maiko/build/c/<os>.<arch>/ in medley/scripts/build/build-c-emulator.sh
- [x] T019 [US1] Create build-zig-emulator.sh script with argument parsing in medley/scripts/build/build-zig-emulator.sh
- [x] T020 [US1] Implement Zig emulator build logic using zig build in medley/scripts/build/build-zig-emulator.sh
- [x] T021 [US1] Implement executable copying from Zig build output to maiko/build/zig/<os>.<arch>/ in medley/scripts/build/build-zig-emulator.sh
- [x] T022 [US1] Create build-lisp-emulator.sh script with argument parsing in medley/scripts/build/build-lisp-emulator.sh
- [x] T023 [US1] Implement Lisp emulator build logic using ASDF/SBCL in medley/scripts/build/build-lisp-emulator.sh
- [x] T024 [US1] Implement executable wrapper creation for Lisp emulator in maiko/build/lisp/<os>.<arch>/ in medley/scripts/build/build-lisp-emulator.sh
- [x] T025 [US1] Create build-emulator.sh script with emulator type routing in medley/scripts/build/build-emulator.sh
- [x] T026 [US1] Create build-all-emulators.sh main orchestrator script in medley/scripts/build/build-all-emulators.sh
- [x] T027 [US1] Implement build orchestration logic to build all available emulators in medley/scripts/build/build-all-emulators.sh
- [x] T028 [US1] Implement error handling for missing emulator sources with informative messages in medley/scripts/build/build-all-emulators.sh
- [x] T029 [US1] Implement error handling for build failures with clear error messages in medley/scripts/build/build-all-emulators.sh
- [x] T030 [US1] Implement --help option for all build scripts in medley/scripts/build/
- [x] T031 [US1] Implement --verbose option for all build scripts in medley/scripts/build/

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Running `./build-all-emulators.sh` should build all three emulators and place executables in unified locations.

---

## Phase 4: User Story 2 - Integrated Medley Loadup with Emulator Selection (Priority: P1)

**Goal**: Build Medley sysout files using any of the three Maiko emulators, with the build system automatically building the required emulator if it's not already built, and integrating the emulator build into the loadup workflow.

**Independent Test**: Run `./loadup-all.sh --emulator zig` on a clean repository and verify the Zig emulator is built first, then the loadup proceeds using the Zig emulator.

### Implementation for User Story 2

- [x] T032 [US2] Add --emulator argument parsing to loadup-all.sh in medley/scripts/loadups/loadup-all.sh
- [x] T033 [US2] Add --auto-build and --no-auto-build argument parsing to loadup-all.sh in medley/scripts/loadups/loadup-all.sh
- [x] T034 [US2] Implement emulator selection logic with default to C emulator in medley/scripts/loadups/loadup-all.sh
- [x] T035 [US2] Implement automatic emulator building before loadup in medley/scripts/loadups/loadup-all.sh
- [x] T036 [US2] Implement emulator executable location resolution using find_emulator_executable() in medley/scripts/loadups/loadup-all.sh
- [x] T037 [US2] Implement incomplete build detection and rebuild logic in medley/scripts/loadups/loadup-all.sh
- [x] T038 [US2] Modify loadup-init.sh to support emulator selection in medley/scripts/loadups/loadup-init.sh
- [x] T039 [US2] Modify loadup-mid-from-init.sh to support emulator selection in medley/scripts/loadups/loadup-mid-from-init.sh
- [x] T040 [US2] Modify loadup-lisp-from-mid.sh to support emulator selection in medley/scripts/loadups/loadup-lisp-from-mid.sh
- [x] T041 [US2] Modify loadup-full-from-lisp.sh to support emulator selection in medley/scripts/loadups/loadup-full-from-lisp.sh
- [x] T042 [US2] Preserve backward compatibility with existing Maiko location resolution in medley/scripts/loadups/
- [x] T043 [US2] Update run-medley script to support emulator selection in medley/run-medley

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Loadup scripts should automatically build and use selected emulators.

---

## Phase 5: User Story 3 - Select Emulator for Loadup (Priority: P2)

**Goal**: Choose which Maiko emulator to use for building Medley sysout files, either through command-line arguments, environment variables, or configuration files, with sensible defaults.

**Independent Test**: Run loadup commands with different emulator selections (`--emulator zig`, `MEDLEY_EMULATOR=lisp`, no argument) and verify the correct emulator is used for each loadup.

### Implementation for User Story 3

- [x] T044 [US3] Implement MEDLEY_EMULATOR environment variable support in medley/scripts/loadups/loadup-all.sh
- [x] T045 [US3] Implement environment variable precedence (command-line > environment > default) in medley/scripts/loadups/loadup-all.sh
- [x] T046 [US3] Add --emulator argument to all individual loadup scripts in medley/scripts/loadups/ (via MEDLEY_EMULATOR env var)
- [x] T047 [US3] Implement emulator selection validation (must be c, zig, or lisp) in medley/scripts/loadups/loadup-all.sh
- [x] T048 [US3] Add emulator selection help text to loadup scripts in medley/scripts/loadups/ (via man page and error messages)
- [x] T049 [US3] Update medley_run.sh to support emulator selection in medley/scripts/medley/medley_run.sh
- [x] T050 [US3] Update medley.command to support emulator selection in medley/scripts/medley/medley.command

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. Developers can select emulators via command-line, environment variable, or default.

---

## Phase 6: User Story 4 - Build Individual Emulators (Priority: P2)

**Goal**: Build a specific emulator without building all three, either for faster iteration during development or because they only need one emulator.

**Independent Test**: Run `./build-emulator.sh --emulator c` and verify only the C emulator is built, not Zig or Lisp.

### Implementation for User Story 4

- [x] T051 [US4] Implement --emulator option in build-all-emulators.sh to build single emulator in medley/scripts/build/build-all-emulators.sh
- [x] T052 [US4] Implement build-emulator.sh script that routes to appropriate build wrapper in medley/scripts/build/build-emulator.sh
- [x] T053 [US4] Add validation for --emulator argument (must be c, zig, or lisp) in medley/scripts/build/build-emulator.sh
- [x] T054 [US4] Implement single emulator build logic that skips other emulators in medley/scripts/build/build-emulator.sh
- [x] T055 [US4] Add --help option to build-emulator.sh in medley/scripts/build/build-emulator.sh

**Checkpoint**: At this point, User Stories 1-4 should all work independently. Developers can build individual emulators or all emulators.

---

## Phase 7: User Story 5 - Cross-Platform Build Support (Priority: P3)

**Goal**: Build emulators on different operating systems and architectures, with the build system detecting the platform and building appropriately for each emulator's capabilities.

**Independent Test**: Run builds on different platforms (Linux x86_64, macOS ARM64) and verify platform-specific builds are created correctly in `maiko/build/<emulator>/<os>.<arch>/`.

### Implementation for User Story 5

- [x] T056 [US5] Implement --platform option in build-all-emulators.sh for manual platform specification in medley/scripts/build/build-all-emulators.sh
- [x] T057 [US5] Implement --platform option in build-emulator.sh in medley/scripts/build/build-emulator.sh
- [x] T058 [US5] Add platform validation (must match <os>.<arch> pattern) in medley/scripts/build/common.sh
- [x] T059 [US5] Implement platform-specific build option passing to native build tools in medley/scripts/build/build-c-emulator.sh
- [x] T060 [US5] Implement platform-specific build option passing to native build tools in medley/scripts/build/build-zig-emulator.sh
- [x] T061 [US5] Implement platform-specific build option passing to native build tools in medley/scripts/build/build-lisp-emulator.sh
- [x] T062 [US5] Add platform information to build output messages in medley/scripts/build/build-all-emulators.sh

**Checkpoint**: At this point, all user stories should be independently functional. Build system supports cross-platform builds with automatic or manual platform detection.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T063 [P] Implement --display-backend option support across all build scripts in medley/scripts/build/
- [ ] T064 [P] Implement --release option support for C emulator in medley/scripts/build/build-c-emulator.sh
- [ ] T065 [P] Implement --network-type option support for C emulator in medley/scripts/build/build-c-emulator.sh
- [ ] T066 [P] Implement --optimize option support across all build scripts in medley/scripts/build/
- [ ] T067 [P] Implement --force option to force rebuilds in medley/scripts/build/
- [ ] T068 [P] Implement --skip-prereq-check option in medley/scripts/build/build-all-emulators.sh
- [ ] T069 Implement incremental build detection improvements with metadata file support in medley/scripts/build/common.sh
- [ ] T070 Implement concurrent build prevention (lock file) in medley/scripts/build/common.sh
- [ ] T071 [P] Add comprehensive error messages with installation hints in medley/scripts/build/check-prerequisites.sh
- [ ] T072 [P] Update BUILDING.md documentation with unified build system instructions in medley/BUILDING.md
- [ ] T073 [P] Update README.md with unified build system usage in medley/scripts/build/README.md
- [ ] T074 Validate quickstart.md examples work correctly
- [ ] T075 Code cleanup and refactoring across all build scripts in medley/scripts/build/
- [ ] T076 Verify all scripts have proper shebang and executable permissions in medley/scripts/build/

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
- **User Story 2 (P1)**: Depends on User Story 1 completion (needs build scripts to exist)
- **User Story 3 (P2)**: Depends on User Story 2 completion (needs loadup integration)
- **User Story 4 (P2)**: Depends on User Story 1 completion (needs build scripts)
- **User Story 5 (P3)**: Depends on User Story 1 completion (needs build scripts with platform support)

### Within Each User Story

- Build wrapper scripts before orchestrator scripts
- Individual emulator build scripts before build-all-emulators.sh
- Core build functionality before integration with loadup scripts
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel (T003, T004, T005)
- All Foundational tasks can run in parallel after T006 (platform detection must be first)
- Once Foundational phase completes:
  - User Story 1 tasks can be parallelized by emulator type (C, Zig, Lisp build scripts)
  - User Story 2 and User Story 4 can start in parallel (both depend on US1)
  - User Story 3 can start after US2
  - User Story 5 can start after US1
- All Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all emulator-specific build scripts in parallel:
Task: "Create build-c-emulator.sh script with argument parsing in medley/scripts/build/build-c-emulator.sh"
Task: "Create build-zig-emulator.sh script with argument parsing in medley/scripts/build/build-zig-emulator.sh"
Task: "Create build-lisp-emulator.sh script with argument parsing in medley/scripts/build/build-lisp-emulator.sh"

# After build wrappers exist, implement build logic in parallel:
Task: "Implement C emulator build logic using CMake in medley/scripts/build/build-c-emulator.sh"
Task: "Implement Zig emulator build logic using zig build in medley/scripts/build/build-zig-emulator.sh"
Task: "Implement Lisp emulator build logic using ASDF/SBCL in medley/scripts/build/build-lisp-emulator.sh"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test User Story 1 independently by running `./build-all-emulators.sh` and verifying executables in `maiko/build/<emulator>/<os>.<arch>/`
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo (Integrated loadup)
4. Add User Story 3 → Test independently → Deploy/Demo (Emulator selection)
5. Add User Story 4 → Test independently → Deploy/Demo (Individual builds)
6. Add User Story 5 → Test independently → Deploy/Demo (Cross-platform)
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (C emulator build wrapper)
   - Developer B: User Story 1 (Zig emulator build wrapper)
   - Developer C: User Story 1 (Lisp emulator build wrapper)
3. After User Story 1 complete:
   - Developer A: User Story 2 (Loadup integration)
   - Developer B: User Story 4 (Individual builds)
   - Developer C: User Story 5 (Cross-platform support)
4. Developer A: User Story 3 (Emulator selection) after US2 complete
5. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- All scripts must have proper shebang (`#!/usr/bin/env bash`) and executable permissions
- Error messages should be clear and actionable
- All scripts should support `--help` option
