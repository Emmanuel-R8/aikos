# Tasks: Maiko Emulator Implementation in Zig

**Input**: Design documents from `/specs/001-zig-implementation/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are OPTIONAL - Zig test framework tasks included for validation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Zig implementation**: `alternatives/zig/` at repository root
- Source code in `alternatives/zig/src/`
- Tests in `alternatives/zig/tests/`

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [x] T001 Create project directory structure `alternatives/zig/` with subdirectories per plan.md
- [x] T002 [P] Create `alternatives/zig/build.zig` with Zig build system configuration
- [x] T003 [P] Create `alternatives/zig/build.zig.zon` with SDL3 dependency configuration
- [x] T004 [P] Create `alternatives/zig/README.md` with project overview and build instructions
- [x] T005 [P] Create `alternatives/zig/.gitignore` with Zig build artifacts

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T006 Create core types module `alternatives/zig/src/utils/types.zig` with LispPTR, DLword, ByteCode definitions per data-model.md
- [x] T007 [P] Create error types module `alternatives/zig/src/utils/errors.zig` with VMError, MemoryError, DisplayError, IOError per contracts
- [x] T008 [P] Create address utilities module `alternatives/zig/src/utils/address.zig` with hiloc/loloc functions per data-model.md
- [x] T009 Configure build.zig to link SDL3 library and set up C interop per research.md
- [x] T010 [P] Create main entry point `alternatives/zig/src/main.zig` with basic VM structure initialization

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Developer Implements VM Core in Zig (Priority: P1) 🎯 MVP

**Goal**: Implement working bytecode interpreter with dispatch loop, stack management, and basic opcode execution

**Independent Test**: A developer can compile and run the Zig implementation, load a simple sysout file, and execute bytecode instructions that produce correct results matching Maiko's C implementation behavior.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T011 [P] [US1] Create opcode execution test `alternatives/zig/tests/opcodes.zig` with test cases for arithmetic opcodes (IPLUS2, IDIFFERENCE, etc.)
- [x] T012 [P] [US1] Create stack management test `alternatives/zig/tests/stack.zig` with test cases for frame allocation and stack operations
- [x] T013 [P] [US1] Create dispatch loop test `alternatives/zig/tests/dispatch.zig` with test cases for instruction fetch/decode/execute cycle

### Implementation for User Story 1

- [x] T014 [P] [US1] Create stack frame structure `alternatives/zig/src/vm/stack.zig` with FX struct per data-model.md
- [x] T015 [US1] Implement stack allocation functions in `alternatives/zig/src/vm/stack.zig` (allocateStackFrame, freeStackFrame, extendStack) per contracts/vm-core-interface.zig
- [x] T016 [US1] Create dispatch loop module `alternatives/zig/src/vm/dispatch.zig` with fetchInstruction, decodeOpcode, executeOpcode per contracts/vm-core-interface.zig
- [x] T017 [US1] Implement main dispatch function in `alternatives/zig/src/vm/dispatch.zig` per `.ai_assistant_db/rewrite-spec/vm-core/execution-model.md`
- [x] T018 [P] [US1] Create opcode handlers module `alternatives/zig/src/vm/opcodes.zig` with opcode handler function signatures
- [x] T019 [US1] Implement arithmetic opcodes (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM) in `alternatives/zig/src/vm/opcodes.zig` per `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md`
- [x] T020 [US1] Implement stack manipulation opcodes (PUSH, POP, STKSCAN) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T021 [US1] Create function call module `alternatives/zig/src/vm/function.zig` with callFunction, returnFromFunction per contracts/vm-core-interface.zig
- [x] T022 [US1] Implement function call opcodes (CALL, CALLN, RET) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T023 [US1] Create interrupt handling module `alternatives/zig/src/vm/interrupt.zig` with checkInterrupts, handleInterrupt per contracts/vm-core-interface.zig
- [x] T024 [US1] Integrate interrupt checking into dispatch loop in `alternatives/zig/src/vm/dispatch.zig` per `.ai_assistant_db/rewrite-spec/vm-core/interrupt-handling.md`
- [x] T025 [US1] Implement address translation functions (translateAddress2, translateAddress4) in `alternatives/zig/src/utils/address.zig` per contracts/vm-core-interface.zig

**Checkpoint**: At this point, User Story 1 should enable basic bytecode execution with arithmetic and stack operations

---

## Phase 4: User Story 2 - Developer Implements Memory Management in Zig (Priority: P1)

**Goal**: Implement garbage collection and memory management system with sysout file compatibility

**Independent Test**: A developer can load an existing sysout file created by Maiko C implementation, and the Zig implementation correctly maps memory regions, tracks object references, and reclaims memory when objects become unreferenced.

### Tests for User Story 2

- [x] T026 [P] [US2] Create memory allocation test `alternatives/zig/tests/memory.zig` with test cases for cons cell and array allocation
- [x] T027 [P] [US2] Create GC test `alternatives/zig/tests/gc.zig` with test cases for reference counting and memory reclamation
- [x] T028 [P] [US2] Create sysout loading test `alternatives/zig/tests/sysout.zig` with test cases for loading and validating sysout files

### Implementation for User Story 2

- [x] T029 [P] [US2] Create cons cell structure `alternatives/zig/src/data/cons.zig` with ConsCell packed struct per data-model.md
- [x] T030 [US2] Implement CDR coding functions in `alternatives/zig/src/data/cons.zig` per `.ai_assistant_db/rewrite-spec/data-structures/cons-cells.md`
- [x] T031 [P] [US2] Create array structures `alternatives/zig/src/data/array.zig` with array type definitions per data-model.md
- [x] T032 [P] [US2] Create function header structure `alternatives/zig/src/data/function_header.zig` with FunctionHeader packed struct per data-model.md
- [x] T033 [US2] Create virtual memory module `alternatives/zig/src/memory/virtual.zig` with page mapping and address translation per contracts/memory-interface.zig
- [x] T034 [US2] Implement FPtoVP mapping in `alternatives/zig/src/memory/virtual.zig` per `.ai_assistant_db/rewrite-spec/memory/address-translation.md`
- [x] T035 [US2] Create memory layout module `alternatives/zig/src/memory/layout.zig` with memory region offsets per data-model.md
- [x] T036 [US2] Create GC module `alternatives/zig/src/memory/gc.zig` with HashEntry structure and hash table per data-model.md
- [x] T037 [US2] Implement addReference and deleteReference functions in `alternatives/zig/src/memory/gc.zig` per contracts/memory-interface.zig
- [x] T038 [US2] Implement markStackReference function in `alternatives/zig/src/memory/gc.zig` per contracts/memory-interface.zig
- [x] T039 [US2] Implement findInHashTable function in `alternatives/zig/src/memory/gc.zig` per `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md`
- [x] T040 [US2] Implement runGC function in `alternatives/zig/src/memory/gc.zig` per rewrite documentation
- [x] T041 [US2] Create storage allocation module `alternatives/zig/src/memory/storage.zig` with allocateConsCell, allocateArray per contracts/memory-interface.zig
- [x] T042 [US2] Implement checkStorageFull function in `alternatives/zig/src/memory/storage.zig` per contracts/memory-interface.zig
- [x] T043 [US2] Create sysout loading module `alternatives/zig/src/data/sysout.zig` with IFPAGE structure per data-model.md
- [x] T044 [US2] Implement loadSysout function in `alternatives/zig/src/data/sysout.zig` per contracts/memory-interface.zig
- [x] T045 [US2] Implement validateSysout function in `alternatives/zig/src/data/sysout.zig` per contracts/memory-interface.zig
- [x] T046 [US2] Integrate sysout loading into main.zig to load sysout files on startup

**Checkpoint**: At this point, User Stories 1 AND 2 should enable VM core with memory management and sysout file loading

---

## Phase 5: User Story 3 - Developer Implements I/O and Display with SDL in Zig (Priority: P2)

**Goal**: Implement I/O and display subsystems using SDL3, enabling interactive Lisp sessions with graphics and input handling

**Independent Test**: A developer can run the Zig implementation, interact with Lisp through keyboard and mouse input, and see graphics rendered correctly in an SDL window.

### Tests for User Story 3

- [x] T047 [P] [US3] Create keyboard event test `alternatives/zig/tests/keyboard.zig` with test cases for keycode translation
- [x] T048 [P] [US3] Create mouse event test `alternatives/zig/tests/mouse.zig` with test cases for mouse event translation
- [x] T049 [P] [US3] Create display rendering test `alternatives/zig/tests/display.zig` with test cases for BitBLT operations
- [x] T050 [P] [US3] Create filesystem test `alternatives/zig/tests/filesystem.zig` with test cases for pathname translation

### Implementation for User Story 3

- [x] T051 [P] [US3] Create SDL backend module `alternatives/zig/src/display/sdl_backend.zig` with DisplayInterface struct per data-model.md
- [x] T052 [US3] Implement initDisplay function in `alternatives/zig/src/display/sdl_backend.zig` per contracts/display-interface.zig
- [x] T053 [US3] Implement destroyDisplay function in `alternatives/zig/src/display/sdl_backend.zig` per contracts/display-interface.zig
- [x] T054 [US3] Create graphics operations module `alternatives/zig/src/display/graphics.zig` with renderRegion function per contracts/display-interface.zig
- [x] T055 [US3] Implement BitBLT operation in `alternatives/zig/src/display/graphics.zig` per `.ai_assistant_db/rewrite-spec/display/graphics-operations.md`
- [x] T056 [US3] Implement flushDisplayRegion function in `alternatives/zig/src/display/graphics.zig` per contracts/display-interface.zig
- [x] T057 [US3] Create event handling module `alternatives/zig/src/display/events.zig` with pollEvents function per contracts/display-interface.zig
- [x] T058 [US3] Create keyboard module `alternatives/zig/src/io/keyboard.zig` with KeyboardEvent struct per data-model.md
- [x] T059 [US3] Implement translateKeycode function in `alternatives/zig/src/io/keyboard.zig` per contracts/io-interface.zig
- [x] T060 [US3] Implement enqueueKeyEvent and dequeueKeyEvent functions in `alternatives/zig/src/io/keyboard.zig` per contracts/io-interface.zig
- [x] T061 [US3] Create mouse module `alternatives/zig/src/io/mouse.zig` with MouseEvent struct per data-model.md
- [x] T062 [US3] Implement translateMouseEvent function in `alternatives/zig/src/io/mouse.zig` per contracts/io-interface.zig
- [x] T063 [US3] Implement updateMousePosition and getMousePosition functions in `alternatives/zig/src/io/mouse.zig` per contracts/io-interface.zig
- [x] T064 [US3] Create filesystem module `alternatives/zig/src/io/filesystem.zig` with pathname translation functions per contracts/io-interface.zig
- [x] T065 [US3] Implement lispToPlatformPathname and platformToLispPathname functions in `alternatives/zig/src/io/filesystem.zig` per `.ai_assistant_db/rewrite-spec/io/file-system.md`
- [x] T066 [US3] Implement file I/O functions (openFile, readFile, writeFile, closeFile) in `alternatives/zig/src/io/filesystem.zig` per contracts/io-interface.zig
- [x] T067 [US3] Integrate SDL event loop into main.zig to handle keyboard and mouse events
- [x] T068 [US3] Integrate display rendering into VM interrupt handling to update display during execution

**Checkpoint**: At this point, all user stories should enable fully functional emulator with graphics and I/O

---

## Phase 6: Complete Instruction Set

**Purpose**: Implement remaining opcodes to complete full instruction set support

- [x] T069 [P] Implement control flow opcodes (JUMP, JUMPIF, JUMPIFNIL, etc.) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T070 [P] Implement data access opcodes (CAR, CDR, RPLACA, RPLACD, etc.) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T071 [P] Implement array access opcodes (GETAEL1, GETAEL2, SETAEL1, SETAEL2, etc.) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T072 [P] Implement comparison opcodes (EQ, EQL, LESSP, GREATERP, etc.) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T073 [P] Implement type checking opcodes (TYPEP, FIXP, SMALLP, etc.) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T074 [P] Implement string/character opcodes (CHARCODE, CHARN, etc.) in `alternatives/zig/src/vm/opcodes.zig` per rewrite documentation
- [x] T075 [P] Implement remaining opcodes to complete all 256 opcodes in `alternatives/zig/src/vm/opcodes.zig` per `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md`

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [x] T076 [P] Create implementation documentation `alternatives/zig/docs/IMPLEMENTATION.md` with design decisions and notes
- [x] T077 [P] Update README.md in `alternatives/zig/README.md` with usage examples and troubleshooting
- [x] T078 Code cleanup and refactoring across all modules
- [x] T079 [P] Add comprehensive unit tests for all opcodes in `alternatives/zig/tests/opcodes.zig`
- [x] T080 [P] Add integration tests for complete Lisp program execution in `alternatives/zig/tests/integration.zig`
- [x] T081 [P] Create compatibility test suite `alternatives/zig/tests/compatibility.zig` comparing against C implementation
- [x] T082 Run quickstart.md validation to ensure implementation matches quickstart guide
- [x] T083 Performance optimization across all subsystems
- [x] T084 Add error handling improvements and better error messages
- [x] T085 Validate sysout file compatibility with multiple test sysout files
- [x] T086 [P] Verify all 256 bytecode opcodes are implemented: Create opcode coverage test `alternatives/zig/tests/opcode_coverage.zig` that enumerates all 256 opcodes and verifies each has a handler implementation per FR-001
- [x] T087 [P] Validate SC-001: Create integration test `alternatives/zig/tests/sysout_loading_validation.zig` that loads and executes at least 3 different existing sysout files without errors per SC-001
- [x] T088 [P] Validate SC-002: Create test coverage validation script that verifies at least 50 test cases covering all opcode categories produce identical results to Maiko C implementation per SC-002
- [x] T089 [P] Validate SC-004: Create reference behavior test suite `alternatives/zig/tests/reference_behaviors.zig` that runs test cases from `.ai_assistant_db/rewrite-spec/validation/reference-behaviors.md` and verifies at least 80% pass per SC-004
- [ ] T090 [P] Validate SC-003: Create integration test `alternatives/zig/tests/interactive_session_validation.zig` that verifies an interactive Lisp session runs with SDL display, keyboard input, and mouse input working correctly per SC-003
- [ ] T091 [P] Validate SC-005: Create build validation script `alternatives/zig/scripts/validate_build.sh` that verifies the implementation compiles successfully on Linux and macOS using Zig 0.15.1 compiler per SC-005
- [ ] T092 [P] Validate SC-006: Create memory safety analysis document `alternatives/zig/docs/memory_safety_analysis.md` that documents memory safety improvements (e.g., no use-after-free, bounds checking) compared to C implementation while maintaining compatibility per SC-006
- [ ] T093 [P] Validate SC-007: Create developer onboarding guide `alternatives/zig/docs/ONBOARDING.md` that enables a developer unfamiliar with Maiko C code to build and run the implementation using only the rewrite documentation within 1 week per SC-007
- [ ] T094 [P] Validate SC-008: Create sysout compatibility test `alternatives/zig/tests/sysout_compatibility.zig` that verifies sysout file format compatibility (can load files created by C implementation and vice versa) per SC-008

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User Story 1 (VM Core) can start after Foundational
  - User Story 2 (Memory) can start after Foundational (may use VM core types)
  - User Story 3 (I/O & Display) depends on VM core and memory being functional
- **Complete Instruction Set (Phase 6)**: Depends on User Story 1 completion
- **Polish (Phase 7)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Uses VM core types but should be independently testable
- **User Story 3 (P2)**: Depends on User Stories 1 and 2 - Needs VM core for execution and memory for display buffer

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Data structures before functions
- Core functions before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes, User Stories 1 and 2 can start in parallel
- All tests for a user story marked [P] can run in parallel
- Data structures within a story marked [P] can run in parallel
- Opcode implementations in Phase 6 marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Create opcode execution test in tests/opcodes.zig"
Task: "Create stack management test in tests/stack.zig"
Task: "Create dispatch loop test in tests/dispatch.zig"

# Launch all data structures for User Story 1 together:
Task: "Create stack frame structure in src/vm/stack.zig"
Task: "Create opcode handlers module in src/vm/opcodes.zig"
```

---

## Implementation Strategy

### MVP First (User Stories 1 & 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (VM Core with basic opcodes)
4. Complete Phase 4: User Story 2 (Memory Management with sysout loading)
5. **STOP and VALIDATE**: Test User Stories 1 and 2 independently
6. Load sysout file and execute basic Lisp programs
7. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Basic bytecode execution (MVP!)
3. Add User Story 2 → Test independently → Sysout file loading
4. Add User Story 3 → Test independently → Interactive graphics and I/O
5. Complete Instruction Set → Full opcode support
6. Polish → Production ready
7. Each phase adds value without breaking previous phases

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (VM Core)
   - Developer B: User Story 2 (Memory Management)
3. Once User Stories 1 & 2 complete:
   - Developer A: User Story 3 (I/O & Display)
   - Developer B: Complete Instruction Set (Phase 6)
4. All developers: Polish phase

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Follow rewrite documentation in `.ai_assistant_db/rewrite-spec/` for implementation details
- Maintain exact compatibility with C implementation for sysout files
- Use Zig's memory safety features while maintaining compatibility
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
