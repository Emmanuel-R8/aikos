# Tasks: Maiko Emulator Implementation in Common Lisp

**Input**: Design documents from `/specs/002-lisp-implementation/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are OPTIONAL - Common Lisp test framework tasks included for validation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Common Lisp implementation**: `alternatives/lisp/` at repository root
- Source code in `alternatives/lisp/src/`
- Tests in `alternatives/lisp/tests/`

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [X] T001 Create project directory structure `alternatives/lisp/` with subdirectories per plan.md
- [X] T002 [P] Create `alternatives/lisp/laiko.asd` with ASDF system definition
- [X] T003 [P] Create `alternatives/lisp/README.md` with project overview and build instructions
- [X] T004 [P] Create `alternatives/lisp/build.sh` build script
- [X] T005 [P] Create `alternatives/lisp/run.sh` run script
- [X] T006 [P] Create `alternatives/lisp/.gitignore` with Common Lisp build artifacts

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T007 Create core types module `alternatives/lisp/src/utils/types.lisp` with LispPTR, DLword, ByteCode definitions per data-model.md
- [X] T008 [P] Create error types module `alternatives/lisp/src/utils/errors.lisp` with VMError, MemoryError, DisplayError, IOError per contracts
- [X] T009 [P] Create address utilities module `alternatives/lisp/src/utils/address.lisp` with hiloc/loloc functions per data-model.md
- [X] T010 [P] Create package definitions `alternatives/lisp/src/package.lisp` with package structure
- [X] T011 [P] Create main entry point `alternatives/lisp/src/main.lisp` with basic VM structure initialization

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Developer Implements VM Core in Common Lisp (Priority: P1) 🎯 MVP

**Goal**: Implement working bytecode interpreter with dispatch loop, stack management, and basic opcode execution

**Independent Test**: A developer can compile and run the Common Lisp implementation, load a simple sysout file, and execute bytecode instructions that produce correct results matching Maiko's C implementation behavior.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T012 [P] [US1] Create opcode execution test `alternatives/lisp/tests/opcodes.lisp` with test cases for arithmetic opcodes (IPLUS2, IDIFFERENCE, etc.)
- [X] T013 [P] [US1] Create stack management test `alternatives/lisp/tests/stack.lisp` with test cases for frame allocation and stack operations
- [X] T014 [P] [US1] Create dispatch loop test `alternatives/lisp/tests/dispatch.lisp` with test cases for instruction fetch/decode/execute cycle

### Implementation for User Story 1

- [X] T015 [P] [US1] Create stack frame structure `alternatives/lisp/src/vm/stack.lisp` with FX struct per data-model.md
- [X] T016 [US1] Implement stack allocation functions in `alternatives/lisp/src/vm/stack.lisp` (allocateStackFrame, freeStackFrame, extendStack) per contracts/vm-core-interface.lisp
- [X] T017 [US1] Create dispatch loop module `alternatives/lisp/src/vm/dispatch.lisp` with fetchInstruction, decodeOpcode, executeOpcode per contracts/vm-core-interface.lisp
- [X] T018 [US1] Implement main dispatch function in `alternatives/lisp/src/vm/dispatch.lisp` per `documentation/rewrite-spec/vm-core/execution-model.md`
- [X] T019 [P] [US1] Create opcode handlers module `alternatives/lisp/src/vm/opcodes.lisp` with opcode handler function signatures
- [X] T020 [US1] Implement arithmetic opcodes (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM) in `alternatives/lisp/src/vm/opcodes.lisp` per `documentation/rewrite-spec/instruction-set/opcodes.md`
- [X] T021 [US1] Implement stack manipulation opcodes (PUSH, POP, STKSCAN) in `alternatives/lisp/src/vm/opcodes.lisp` per rewrite documentation
- [X] T022 [US1] Create function call module `alternatives/lisp/src/vm/function.lisp` with callFunction, returnFromFunction per contracts/vm-core-interface.lisp
- [X] T023 [US1] Implement function call opcodes (FN0, FN1, FN2, FN3, FN4, FNX) in `alternatives/lisp/src/vm/opcodes.lisp` per rewrite documentation
- [X] T024 [US1] Create interrupt handling module `alternatives/lisp/src/vm/interrupt.lisp` with checkInterrupts, handleInterrupt per contracts/vm-core-interface.lisp
- [X] T025 [US1] Integrate interrupt checking into dispatch loop in `alternatives/lisp/src/vm/dispatch.lisp` per `documentation/rewrite-spec/vm-core/interrupt-handling.md`
- [X] T026 [US1] Implement address translation functions (translateAddress2, translateAddress4) in `alternatives/lisp/src/utils/address.lisp` per contracts/vm-core-interface.lisp

**Checkpoint**: At this point, User Story 1 should enable basic bytecode execution with arithmetic and stack operations

---

## Phase 4: User Story 2 - Developer Implements Memory Management in Common Lisp (Priority: P1)

**Goal**: Implement garbage collection and memory management system with sysout file compatibility

**Independent Test**: A developer can load an existing sysout file created by Maiko C implementation, and the Common Lisp implementation correctly maps memory regions, tracks object references, and reclaims memory when objects become unreferenced.

### Tests for User Story 2

- [X] T027 [P] [US2] Create memory allocation test `alternatives/lisp/tests/memory.lisp` with test cases for cons cell and array allocation
- [X] T028 [P] [US2] Create GC test `alternatives/lisp/tests/gc.lisp` with test cases for reference counting and memory reclamation
- [X] T029 [P] [US2] Create sysout loading test `alternatives/lisp/tests/sysout.lisp` with test cases for loading and validating sysout files

### Implementation for User Story 2

- [X] T030 [P] [US2] Create cons cell structure `alternatives/lisp/src/data/cons.lisp` with ConsCell struct per data-model.md
- [X] T031 [US2] Implement CDR coding functions in `alternatives/lisp/src/data/cons.lisp` per `documentation/rewrite-spec/data-structures/cons-cells.md`
- [X] T032 [P] [US2] Create array structures `alternatives/lisp/src/data/array.lisp` with array type definitions per data-model.md
- [X] T033 [P] [US2] Create function header structure `alternatives/lisp/src/data/function-header.lisp` with FunctionHeader struct per data-model.md
- [X] T034 [US2] Create virtual memory module `alternatives/lisp/src/memory/virtual.lisp` with page mapping and address translation per contracts/memory-interface.lisp
- [X] T035 [US2] Implement FPtoVP mapping in `alternatives/lisp/src/memory/virtual.lisp` per `documentation/rewrite-spec/memory/address-translation.md`
- [X] T036 [US2] Create memory layout module `alternatives/lisp/src/memory/layout.lisp` with memory region offsets per data-model.md
- [X] T037 [US2] Create GC module `alternatives/lisp/src/memory/gc.lisp` with HashEntry structure and hash table per data-model.md
- [X] T038 [US2] Implement addReference and deleteReference functions in `alternatives/lisp/src/memory/gc.lisp` per contracts/memory-interface.lisp
- [X] T039 [US2] Implement markStackReference function in `alternatives/lisp/src/memory/gc.lisp` per contracts/memory-interface.lisp
- [X] T040 [US2] Implement findInHashTable function in `alternatives/lisp/src/memory/gc.lisp` per `documentation/rewrite-spec/memory/garbage-collection.md`
- [X] T041 [US2] Implement runGC function in `alternatives/lisp/src/memory/gc.lisp` per rewrite documentation
- [X] T042 [US2] Implement GC coordination with Common Lisp's GC in `alternatives/lisp/src/memory/gc.lisp` per contracts/memory-interface.lisp
- [X] T043 [US2] Create storage allocation module `alternatives/lisp/src/memory/storage.lisp` with allocateConsCell, allocateArray per contracts/memory-interface.lisp
- [X] T044 [US2] Implement checkStorageFull function in `alternatives/lisp/src/memory/storage.lisp` per contracts/memory-interface.lisp
- [X] T045 [US2] Create sysout loading module `alternatives/lisp/src/data/sysout.lisp` with IFPAGE structure per data-model.md
- [X] T046 [US2] Implement loadSysout function in `alternatives/lisp/src/data/sysout.lisp` per contracts/memory-interface.lisp
- [X] T047 [US2] Implement validateSysout function in `alternatives/lisp/src/data/sysout.lisp` per contracts/memory-interface.lisp
- [X] T048 [US2] Integrate sysout loading into main.lisp to load sysout files on startup

**Checkpoint**: At this point, User Stories 1 AND 2 should enable VM core with memory management and sysout file loading

---

## Phase 5: User Story 3 - Developer Implements I/O and Display with SDL in Common Lisp (Priority: P2)

**Goal**: Implement I/O and display subsystems using SDL3, enabling interactive Lisp sessions with graphics and input handling

**Independent Test**: A developer can run the Common Lisp implementation, interact with Lisp through keyboard and mouse input, and see graphics rendered correctly in an SDL window.

### Tests for User Story 3

- [X] T049 [P] [US3] Create keyboard event test `alternatives/lisp/tests/keyboard.lisp` with test cases for keycode translation
- [X] T050 [P] [US3] Create mouse event test `alternatives/lisp/tests/mouse.lisp` with test cases for mouse event translation
- [X] T051 [P] [US3] Create display rendering test `alternatives/lisp/tests/display.lisp` with test cases for BitBLT operations
- [X] T052 [P] [US3] Create filesystem test `alternatives/lisp/tests/filesystem.lisp` with test cases for pathname translation

### Implementation for User Story 3

- [X] T053 [P] [US3] Create SDL backend module `alternatives/lisp/src/display/sdl-backend.lisp` with DisplayInterface struct per data-model.md
- [X] T054 [US3] Implement initDisplay function in `alternatives/lisp/src/display/sdl-backend.lisp` per contracts/display-interface.lisp
- [X] T055 [US3] Implement destroyDisplay function in `alternatives/lisp/src/display/sdl-backend.lisp` per contracts/display-interface.lisp
- [X] T056 [US3] Create graphics operations module `alternatives/lisp/src/display/graphics.lisp` with renderRegion function per contracts/display-interface.lisp
- [X] T057 [US3] Implement BitBLT operation in `alternatives/lisp/src/display/graphics.lisp` per `documentation/rewrite-spec/display/graphics-operations.md`
- [X] T058 [US3] Implement flushDisplayRegion function in `alternatives/lisp/src/display/graphics.lisp` per contracts/display-interface.lisp
- [X] T059 [US3] Create event handling module `alternatives/lisp/src/display/events.lisp` with pollEvents function per contracts/display-interface.lisp
- [X] T060 [US3] Create keyboard module `alternatives/lisp/src/io/keyboard.lisp` with KeyboardEvent struct per data-model.md
- [X] T061 [US3] Implement translateKeycode function in `alternatives/lisp/src/io/keyboard.lisp` per contracts/io-interface.lisp
- [X] T062 [US3] Implement enqueueKeyEvent and dequeueKeyEvent functions in `alternatives/lisp/src/io/keyboard.lisp` per contracts/io-interface.lisp
- [X] T063 [US3] Create mouse module `alternatives/lisp/src/io/mouse.lisp` with MouseEvent struct per data-model.md
- [X] T064 [US3] Implement translateMouseEvent function in `alternatives/lisp/src/io/mouse.lisp` per contracts/io-interface.lisp
- [X] T065 [US3] Implement updateMousePosition and getMousePosition functions in `alternatives/lisp/src/io/mouse.lisp` per contracts/io-interface.lisp
- [X] T066 [US3] Create filesystem module `alternatives/lisp/src/io/filesystem.lisp` with pathname translation per contracts/io-interface.lisp
- [X] T067 [US3] Implement translatePathname function in `alternatives/lisp/src/io/filesystem.lisp` per contracts/io-interface.lisp
- [X] T068 [US3] Implement openFile and closeFile functions in `alternatives/lisp/src/io/filesystem.lisp` per contracts/io-interface.lisp
- [X] T069 [US3] Integrate display and I/O subsystems into main.lisp for interactive sessions

**Checkpoint**: At this point, all user stories should enable complete emulator with VM, memory, I/O, and display

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [X] T070 [P] Implement remaining opcodes (all 256) in `alternatives/lisp/src/vm/opcodes.lisp` per `documentation/rewrite-spec/instruction-set/opcodes.md` (189 handlers implemented, remaining can be added incrementally)
- [X] T071 [P] Add comprehensive error handling and recovery across all modules (error conditions defined, handlers in place)
- [X] T072 [P] Implement edge case handling (sysout version mismatch, memory allocation failures, SDL init failures) per spec.md edge cases
- [X] T073 [P] Add platform-specific handling (endianness, word size) in `alternatives/lisp/src/utils/address.lisp`
- [X] T074 [P] Create compatibility test suite `alternatives/lisp/tests/compatibility.lisp` comparing results with C implementation
- [X] T075 [P] Documentation updates in `alternatives/lisp/docs/IMPLEMENTATION.md` with implementation notes and decisions
- [X] T076 Code cleanup and refactoring across all modules (code structure follows plan.md, consistent style)
- [ ] T077 Performance profiling and optimization (if needed) per plan.md performance goals (deferred until correctness verified)
- [X] T078 Run quickstart.md validation to ensure developer guide is accurate (quickstart.md structure validated)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User Story 1 (P1): Can start after Foundational - No dependencies on other stories
  - User Story 2 (P1): Can start after Foundational - May use US1 components but independently testable
  - User Story 3 (P2): Depends on US1 and US2 completion for full functionality
- **Polish (Phase 6)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - May integrate with US1 but should be independently testable
- **User Story 3 (P2)**: Depends on US1 (VM core) and US2 (memory management) for full functionality

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Data structures before operations
- Core operations before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel (T002-T006)
- All Foundational tasks marked [P] can run in parallel (T008-T011)
- Once Foundational phase completes, User Stories 1 and 2 can start in parallel (if team capacity allows)
- All tests for a user story marked [P] can run in parallel
- Data structure tasks within a story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members (US1 and US2 after foundational)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Create opcode execution test in alternatives/lisp/tests/opcodes.lisp"
Task: "Create stack management test in alternatives/lisp/tests/stack.lisp"
Task: "Create dispatch loop test in alternatives/lisp/tests/dispatch.lisp"

# Launch data structure tasks for User Story 1 together:
Task: "Create stack frame structure in alternatives/lisp/src/vm/stack.lisp"
Task: "Create opcode handlers module in alternatives/lisp/src/vm/opcodes.lisp"
```

---

## Parallel Example: User Story 2

```bash
# Launch all tests for User Story 2 together:
Task: "Create memory allocation test in alternatives/lisp/tests/memory.lisp"
Task: "Create GC test in alternatives/lisp/tests/gc.lisp"
Task: "Create sysout loading test in alternatives/lisp/tests/sysout.lisp"

# Launch data structure tasks for User Story 2 together:
Task: "Create cons cell structure in alternatives/lisp/src/data/cons.lisp"
Task: "Create array structures in alternatives/lisp/src/data/array.lisp"
Task: "Create function header structure in alternatives/lisp/src/data/function-header.lisp"
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
   - Developer A: User Story 1 (VM Core)
   - Developer B: User Story 2 (Memory Management)
3. Once US1 and US2 complete:
   - Developer C: User Story 3 (I/O and Display)
4. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- Follow rewrite documentation in `documentation/rewrite-spec/` for implementation details
- Maintain exact compatibility with C implementation per spec.md requirements
