# Tasks: Zig Emulator Completion - Bring to Parity with C Implementation

**Input**: Design documents from `specs/005-zig-completion/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/, quickstart.md

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Zig Implementation**: `maiko/alternatives/zig/src/`
- **Tests**: `maiko/alternatives/zig/tests/`
- **Documentation**: `.ai_assistant_db/`

---

## Phase 1: User Story 1 - Load and Run Existing Sysout Files (Priority: P1) 🎯 MVP

**Goal**: Successfully load existing sysout files and initialize VM state, enabling the emulator to enter the dispatch loop.

**Independent Test**: Run `maiko-zig medley/internal/loadups/starter.sysout` and the emulator successfully loads the sysout, initializes VM state, and enters the dispatch loop (even if it doesn't execute code yet).

### Implementation for User Story 1

- [X] T001 [US1] Fix IFPAGE_KEYVAL constant from 0x12345678 to 0x15e3 in maiko/alternatives/zig/src/data/sysout.zig
- [X] T002 [US1] Implement complete IFPAGE structure matching C ifpage.h exactly (~100 fields) in maiko/alternatives/zig/src/utils/types.zig
- [X] T003 [US1] Update IFPAGE struct definition in maiko/alternatives/zig/src/data/sysout.zig to use complete structure from types.zig
- [X] T004 [US1] Implement FPtoVP table loading function loadFPtoVPTable in maiko/alternatives/zig/src/data/sysout.zig
- [X] T005 [US1] Add FPtoVPTable data structure with entries array and is_bigvm flag in maiko/alternatives/zig/src/data/sysout.zig
- [X] T006 [US1] Implement FPtoVP offset calculation (ifpage.fptovpstart - 1) * BYTESPER_PAGE + offset in maiko/alternatives/zig/src/data/sysout.zig
- [X] T007 [US1] Implement FPtoVP entry reading with BIGVM and non-BIGVM format support in maiko/alternatives/zig/src/data/sysout.zig
- [X] T008 [US1] Implement page loading algorithm loadMemoryPages in maiko/alternatives/zig/src/data/sysout.zig
- [X] T009 [US1] Add virtual memory allocation using Zig allocator in maiko/alternatives/zig/src/data/sysout.zig
- [X] T010 [US1] Implement page iteration loop checking FPtoVP entries for sparse pages (0xFFFF marker) in maiko/alternatives/zig/src/data/sysout.zig
- [X] T011 [US1] Implement page data reading from sysout file at calculated file page offset in maiko/alternatives/zig/src/data/sysout.zig
- [X] T012 [US1] Implement page data writing to virtual memory at virtual_page * BYTESPER_PAGE address in maiko/alternatives/zig/src/data/sysout.zig
- [X] T013 [US1] Add byte swapping support for cross-platform compatibility in maiko/alternatives/zig/src/data/sysout.zig
- [X] T014 [US1] Update validateSysout function to use correct IFPAGE_KEYVAL (0x15e3) in maiko/alternatives/zig/src/data/sysout.zig
- [X] T015 [US1] Add version compatibility checks (lversion >= LVERSION, minbversion <= MINBVERSION) in maiko/alternatives/zig/src/data/sysout.zig
- [X] T016 [US1] Update loadSysout function to call FPtoVP loading and page loading functions in maiko/alternatives/zig/src/data/sysout.zig
- [X] T017 [US1] Return SysoutLoadResult with ifpage, virtual_memory, and fptovp from loadSysout in maiko/alternatives/zig/src/data/sysout.zig
- [X] T018 [US1] Initialize VM state from IFPAGE fields (stackbase, endofstack, currentfxp) in maiko/alternatives/zig/src/vm/dispatch.zig
- [X] T019 [US1] Add VM state initialization function initializeVMState in maiko/alternatives/zig/src/vm/dispatch.zig
- [X] T020 [US1] Uncomment and activate dispatch loop in maiko/alternatives/zig/src/main.zig after sysout loading
- [X] T021 [US1] Set up initial program counter from sysout state in maiko/alternatives/zig/src/main.zig
- [X] T022 [US1] Initialize interrupt handling state before entering dispatch loop in maiko/alternatives/zig/src/main.zig

**Checkpoint**: At this point, User Story 1 should be fully functional - sysout files load successfully and VM enters dispatch loop.

---

## Phase 2: User Story 2 - Execute Basic Bytecode Instructions (Priority: P1)

**Goal**: Execute basic bytecode instructions and produce correct results matching the C emulator.

**Independent Test**: Load a sysout and execute a simple Lisp expression that produces correct results matching C emulator output.

### Implementation for User Story 2

- [X] T023 [US2] Verify existing arithmetic opcodes (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM) produce correct results in maiko/alternatives/zig/src/vm/opcodes/arithmetic.zig
- [X] T024 [US2] Verify existing stack operations (PUSH, POP, SWAP) handle stack state correctly in maiko/alternatives/zig/src/vm/opcodes/stack_ops.zig
- [X] T025 [US2] Implement function call opcode CALL handler in maiko/alternatives/zig/src/vm/opcodes/function_calls.zig (FN0-FN4 handlers implemented)
- [X] T026 [US2] Implement function return opcode RETURN handler in maiko/alternatives/zig/src/vm/opcodes/function_calls.zig
- [X] T027 [US2] Implement stack frame creation for function calls in maiko/alternatives/zig/src/vm/function.zig
- [X] T028 [US2] Implement stack frame cleanup for function returns in maiko/alternatives/zig/src/vm/function.zig
- [X] T029 [US2] Add error handling for stack overflow in stack operations in maiko/alternatives/zig/src/vm/stack.zig
- [X] T030 [US2] Add error handling for stack underflow in stack operations in maiko/alternatives/zig/src/vm/stack.zig
- [X] T031 [US2] Implement error handling matching C emulator behavior for invalid opcodes in maiko/alternatives/zig/src/vm/dispatch.zig and maiko/alternatives/zig/src/vm/dispatch/execution.zig
- [X] T032 [US2] Add test case for arithmetic opcode execution matching C emulator results in maiko/alternatives/zig/tests/opcodes.zig
- [X] T033 [US2] Add test case for stack operation execution matching C emulator results in maiko/alternatives/zig/tests/stack.zig
- [X] T034 [US2] Add test case for function call and return execution in maiko/alternatives/zig/tests/function_calls.zig

**Checkpoint**: At this point, User Stories 1 AND 2 should both work - basic bytecode execution produces correct results.

---

## Phase 3: User Story 3 - Complete Essential Opcodes for Medley Startup (Priority: P1)

**Goal**: Execute enough opcodes to successfully start Medley Interlisp and reach the Lisp prompt.

**Independent Test**: Run `maiko-zig medley/internal/loadups/lisp.sysout` and Medley starts successfully, displaying the Interlisp prompt.

### Implementation for User Story 3

- [X] T035 [US3] Complete cons cell CAR operation implementation in maiko/alternatives/zig/src/data/cons.zig
- [X] T036 [US3] Complete cons cell CDR operation implementation in maiko/alternatives/zig/src/data/cons.zig
- [X] T037 [US3] Complete cons cell CONS operation implementation in maiko/alternatives/zig/src/data/cons.zig
- [X] T038 [US3] Implement CAR opcode handler calling cons cell operation in maiko/alternatives/zig/src/vm/opcodes/data_ops.zig
- [X] T039 [US3] Implement CDR opcode handler calling cons cell operation in maiko/alternatives/zig/src/vm/opcodes/data_ops.zig
- [X] T040 [US3] Implement CONS opcode handler calling cons cell operation in maiko/alternatives/zig/src/vm/opcodes/data_ops.zig
- [X] T041 [US3] Complete variable access IVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig
- [X] T042 [US3] Complete variable access PVAR implementation for all variants (PVAR_0-PVAR_6, PVARX_) in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig
- [X] T043 [US3] Complete variable access FVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig
- [X] T044 [US3] Complete variable access GVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig
- [X] T045 [US3] Implement JUMP opcode variants (JUMP0-JUMP15) in maiko/alternatives/zig/src/vm/opcodes/control_flow.zig
- [X] T046 [US3] Implement FJUMP opcode variants (FJUMP0-FJUMP15) in maiko/alternatives/zig/src/vm/opcodes/control_flow.zig
- [X] T047 [US3] Implement TJUMP opcode variants (TJUMP0-TJUMP15) in maiko/alternatives/zig/src/vm/opcodes/control_flow.zig
- [C] T048 [US3] ~~Implement LIST opcode handler for list creation~~ **CANCELLED**: LIST opcode does not exist in C implementation (maiko/inc/opcodes.h). Lists are created using CONS opcode, which is already implemented.
- [C] T049 [US3] ~~Implement APPEND opcode handler for list concatenation~~ **CANCELLED**: APPEND opcode does not exist in C implementation (maiko/inc/opcodes.h). List concatenation is handled via other mechanisms (RESTLIST, RPLCONS, or Lisp-level functions).
- [X] T050 [US3] Implement RPLACA opcode handler for replacing CAR in maiko/alternatives/zig/src/vm/opcodes/data_ops.zig
- [X] T051 [US3] Implement RPLACD opcode handler for replacing CDR in maiko/alternatives/zig/src/vm/opcodes/data_ops.zig
- [X] T052 [US3] Implement UNWIND opcode handler for stack unwinding in maiko/alternatives/zig/src/vm/opcodes/type_checking.zig
- [X] T053 [US3] Add test case for cons cell operations (CAR, CDR, CONS) in maiko/alternatives/zig/tests/cons.zig
- [X] T054 [US3] Add test case for variable access operations in maiko/alternatives/zig/tests/variable_access.zig
- [X] T055 [US3] Add test case for jump opcode variants in maiko/alternatives/zig/tests/jump_variants.zig
- [X] T056 [US3] Add test case for list operations in maiko/alternatives/zig/tests/rplaca_rplacd.zig
- [X] T057 [US3] Test Medley startup with lisp.sysout and verify initialization completes in maiko/alternatives/zig/tests/integration.zig
- [X] T058 [US3] Verify basic Lisp expression evaluation produces correct results in maiko/alternatives/zig/tests/integration.zig
- [X] T059 [US3] Verify error handling works correctly during Medley execution in maiko/alternatives/zig/tests/integration.zig

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work - Medley starts successfully and reaches Lisp prompt.

---

## Phase 4: User Story 4 - Complete GC Operations for Memory Management (Priority: P2)

**Goal**: Properly manage memory using reference-counting GC matching the C implementation.

**Independent Test**: Run Medley for an extended period, create and destroy many objects, and memory is properly reclaimed without leaks.

### Implementation for User Story 4

- [X] T060 [US4] Implement ADDREF operation to increment reference count in maiko/alternatives/zig/src/memory/gc.zig
- [X] T061 [US4] Implement ADDREF hash table insertion into HTmain in maiko/alternatives/zig/src/memory/gc.zig
- [X] T062 [US4] Implement ADDREF overflow handling into HTcoll when HTmain bucket is full in maiko/alternatives/zig/src/memory/gc.zig
- [X] T063 [US4] Implement DELREF operation to decrement reference count in maiko/alternatives/zig/src/memory/gc.zig
- [X] T064 [US4] Implement DELREF hash table removal when count reaches zero in maiko/alternatives/zig/src/memory/gc.zig
- [X] T065 [US4] Implement reclamation logic to mark objects for reclamation when count is zero in maiko/alternatives/zig/src/memory/gc.zig
- [X] T066 [US4] Implement free list management for reclaimed memory in maiko/alternatives/zig/src/memory/gc.zig
- [X] T067 [US4] Add hash function for object address to compute hash bucket in maiko/alternatives/zig/src/memory/gc.zig
- [X] T068 [US4] Implement HTmain hash table using std.HashMap in maiko/alternatives/zig/src/memory/gc.zig
  - NOTE: Using array-based hash table (HTmain as []HashEntry) matching C implementation structure, not std.HashMap
- [X] T069 [US4] Implement HTcoll hash table using std.HashMap for overflow entries in maiko/alternatives/zig/src/memory/gc.zig
  - NOTE: Using array-based hash table (HTcoll as []LispPTR) matching C implementation structure, not std.HashMap
- [X] T070 [US4] Add test case for ADDREF operation tracking reference counts correctly in maiko/alternatives/zig/tests/gc.zig
- [X] T071 [US4] Add test case for DELREF operation removing references correctly in maiko/alternatives/zig/tests/gc.zig
- [X] T072 [US4] Add test case for reclamation when count reaches zero in maiko/alternatives/zig/tests/gc.zig
- [X] T073 [US4] Add test case for referenced objects not being reclaimed in maiko/alternatives/zig/tests/gc.zig
- [X] T074 [US4] Add integration test for extended Medley session without memory leaks in maiko/alternatives/zig/tests/gc.zig

**Checkpoint**: At this point, User Stories 1-4 should all work - memory management is complete and leak-free.

---

## Phase 5: User Story 5 - SDL2 Display Integration for Interactive Sessions (Priority: P2)

**Goal**: Display graphics and handle input via SDL2, enabling interactive Medley sessions.

**Independent Test**: Run Medley and see graphics rendered correctly in an SDL2 window, with keyboard and mouse input working.

### Implementation for User Story 5

- [ ] T075 [US5] Implement SDL2 initialization (SDL_Init, window creation) in maiko/alternatives/zig/src/display/sdl_backend.zig
- [ ] T076 [US5] Create SDL_Window with specified dimensions and title in maiko/alternatives/zig/src/display/sdl_backend.zig
- [ ] T077 [US5] Create SDL_Renderer for window in maiko/alternatives/zig/src/display/sdl_backend.zig
- [ ] T078 [US5] Create SDL_Texture for display buffer rendering in maiko/alternatives/zig/src/display/sdl_backend.zig
- [ ] T079 [US5] Implement BitBLT operation copying display buffer to texture in maiko/alternatives/zig/src/display/graphics.zig
- [ ] T080 [US5] Implement texture rendering to screen in maiko/alternatives/zig/src/display/sdl_backend.zig
- [ ] T081 [US5] Implement BitBLT COPY mode operation in maiko/alternatives/zig/src/display/graphics.zig
- [ ] T082 [US5] Implement BitBLT XOR mode operation in maiko/alternatives/zig/src/display/graphics.zig
- [ ] T083 [US5] Connect BitBLT operations to SDL2 rendering pipeline in maiko/alternatives/zig/src/display/sdl_backend.zig
- [ ] T084 [US5] Implement SDL2 event polling loop in maiko/alternatives/zig/src/display/events.zig
- [ ] T085 [US5] Implement keyboard event handling (SDL_KEYDOWN, SDL_KEYUP) in maiko/alternatives/zig/src/display/events.zig
- [ ] T086 [US5] Implement keycode translation from SDL keycodes to Lisp keycodes in maiko/alternatives/zig/src/display/events.zig
- [ ] T087 [US5] Deliver keyboard events to Lisp event queue in maiko/alternatives/zig/src/io/keyboard.zig
- [ ] T088 [US5] Implement mouse event handling (SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP) in maiko/alternatives/zig/src/display/events.zig
- [ ] T089 [US5] Implement mouse coordinate translation from SDL coordinates to Lisp coordinates in maiko/alternatives/zig/src/display/events.zig
- [ ] T090 [US5] Deliver mouse events to Lisp event queue in maiko/alternatives/zig/src/io/mouse.zig
- [ ] T091 [US5] Integrate SDL2 display initialization into main.zig startup sequence in maiko/alternatives/zig/src/main.zig
- [ ] T092 [US5] Add test case for SDL2 window creation and display in maiko/alternatives/zig/tests/display.zig
- [ ] T093 [US5] Add test case for BitBLT rendering operations in maiko/alternatives/zig/tests/display.zig
- [ ] T094 [US5] Add test case for keyboard event translation and delivery in maiko/alternatives/zig/tests/io.zig
- [ ] T095 [US5] Add test case for mouse event translation and delivery in maiko/alternatives/zig/tests/io.zig
- [ ] T096 [US5] Add integration test for interactive Medley session with graphics and input in maiko/alternatives/zig/tests/integration.zig

**Checkpoint**: At this point, all user stories should be complete - full interactive Medley sessions work with graphics and input.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final documentation.

- [X] T097 [P] Update .ai_assistant_db/rewrite-spec/data-structures/sysout-format.md with complete IFPAGE structure details
- [X] T098 [P] Update .ai_assistant_db/implementations/zig-implementation.md with completion status and final statistics
- [X] T099 [P] Document FPtoVP table loading algorithm with exact byte offset calculations in .ai_assistant_db/rewrite-spec/data-structures/sysout-format.md
- [X] T100 [P] Document page loading algorithm with byte swapping details in .ai_assistant_db/rewrite-spec/data-structures/sysout-format.md
- [X] T101 [P] Document essential opcodes list for Medley startup in .ai_assistant_db/implementations/zig-implementation.md
- [X] T102 [P] Code cleanup and refactoring across all modified files
  - ✅ Split opcodes.zig into 13 modular files (2025-01-27)
  - ✅ Split dispatch.zig into 3 modular files (2025-01-27)
  - ✅ Fixed bug in comparison.zig eqlDeep function (duplicate code removed)
  - ✅ All files now under 500 lines (user preference)
- [ ] T103 [P] Performance optimization for sysout loading (target < 5 seconds)
- [ ] T104 [P] Performance optimization for bytecode execution
- [ ] T105 [P] Run quickstart.md validation to ensure all steps work correctly
- [ ] T106 [P] Add comprehensive error messages for all failure cases
- [ ] T107 [P] Verify all tests pass with zig build test
- [ ] T108 [P] Compare execution results with C emulator for validation

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - can start immediately (MVP)
- **User Story 2 (Phase 2)**: Depends on User Story 1 completion (needs sysout loading and VM activation)
- **User Story 3 (Phase 3)**: Depends on User Story 2 completion (needs basic execution working)
- **User Story 4 (Phase 4)**: Can start after User Story 1 (GC independent of execution, but needs VM state)
- **User Story 5 (Phase 5)**: Depends on User Story 3 completion (needs Medley running for display)
- **Polish (Phase 6)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Foundation - must complete first
- **User Story 2 (P1)**: Requires User Story 1 (sysout loading and VM activation)
- **User Story 3 (P1)**: Requires User Story 2 (basic execution)
- **User Story 4 (P2)**: Can start after User Story 1 (GC independent)
- **User Story 5 (P2)**: Requires User Story 3 (Medley running)

### Within Each User Story

- Data structures before operations
- Core operations before integration
- Error handling after core implementation
- Tests after implementation (or alongside for TDD)
- Story complete before moving to next priority

### Parallel Opportunities

- **User Story 1**: Tasks T001-T022 can be done sequentially (strong dependencies)
- **User Story 2**: Tasks T023-T034 can be done sequentially (dependencies on US1)
- **User Story 3**: Tasks T035-T059 can be done in parallel groups:
  - T035-T040 (cons cells) can be parallel
  - T041-T044 (variable access) can be parallel
  - T045-T047 (jump variants) can be parallel
  - T048-T051 (list operations) can be parallel
- **User Story 4**: Tasks T060-T074 can be done in parallel groups:
  - T060-T062 (ADDREF) can be sequential
  - T063-T064 (DELREF) can be sequential
  - T065-T069 (reclamation) can be parallel
- **User Story 5**: Tasks T075-T096 can be done in parallel groups:
  - T075-T083 (display rendering) can be sequential
  - T084-T090 (event handling) can be parallel
- **Polish**: All tasks T097-T108 marked [P] can run in parallel

---

## Parallel Example: User Story 3

```bash
# Launch cons cell operations in parallel:
Task: "Complete cons cell CAR operation implementation in maiko/alternatives/zig/src/data/cons.zig"
Task: "Complete cons cell CDR operation implementation in maiko/alternatives/zig/src/data/cons.zig"
Task: "Complete cons cell CONS operation implementation in maiko/alternatives/zig/src/data/cons.zig"

# Launch variable access implementations in parallel:
Task: "Complete variable access IVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig"
Task: "Complete variable access PVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig"
Task: "Complete variable access FVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig"
Task: "Complete variable access GVAR implementation for all variants in maiko/alternatives/zig/src/vm/opcodes/variable_access.zig"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (Load and Run Existing Sysout Files)
2. **STOP and VALIDATE**: Test that sysout files load successfully and VM enters dispatch loop
3. Deploy/demo if ready

**MVP Deliverable**: Emulator can load sysout files and enter dispatch loop (even if execution fails)

### Incremental Delivery

1. Add User Story 1 → Test independently → Validate (MVP!)
2. Add User Story 2 → Test independently → Validate (Basic execution)
3. Add User Story 3 → Test independently → Validate (Medley startup)
4. Add User Story 4 → Test independently → Validate (Memory management)
5. Add User Story 5 → Test independently → Validate (Interactive sessions)
6. Polish → Final validation

Each story adds value without breaking previous stories.

### Parallel Team Strategy

With multiple developers:

1. **Developer A**: User Story 1 (foundation - must complete first)
2. Once User Story 1 is done:
   - **Developer A**: User Story 2 (basic execution)
   - **Developer B**: User Story 4 (GC operations - independent)
3. Once User Story 2 is done:
   - **Developer A**: User Story 3 (essential opcodes)
   - **Developer B**: Continue User Story 4
4. Once User Story 3 is done:
   - **Developer A**: User Story 5 (SDL2 display)
   - **Developer B**: Polish and documentation

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- All file paths are relative to repository root
- Reference C implementation in `maiko/src/` for correct behavior
- Document all new insights in `.ai_assistant_db/` as work progresses

---

## Task Summary

**Total Tasks**: 108

**Tasks by User Story**:

- User Story 1 (P1 - MVP): 22 tasks
- User Story 2 (P1): 12 tasks
- User Story 3 (P1): 25 tasks
- User Story 4 (P2): 15 tasks
- User Story 5 (P2): 22 tasks
- Polish & Documentation: 12 tasks

**Parallel Opportunities**:

- User Story 3: Cons cells, variable access, jump variants, list operations can be parallelized
- User Story 4: Reclamation operations can be parallelized
- User Story 5: Event handling can be parallelized
- Polish: All documentation tasks can be parallelized

**Independent Test Criteria**:

- **US1**: Sysout loads, VM enters dispatch loop
- **US2**: Basic bytecode execution produces correct results
- **US3**: Medley starts and reaches Lisp prompt
- **US4**: Extended session without memory leaks
- **US5**: Graphics display and input work correctly

**Suggested MVP Scope**: User Story 1 only (22 tasks) - enables sysout loading and VM activation
