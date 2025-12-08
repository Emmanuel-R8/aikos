# Feature Specification: Zig Emulator Completion - Bring to Parity with C Implementation

**Feature Branch**: `005-zig-completion`
**Created**: 2025-12-07
**Status**: Draft
**Input**: User description: "Make sure that the Zig implementation is the next key priority. We need to bring it on par with the C one in terms of functionalities. CRITICAL: any new insights or knowledge must be documented and improve the knowledge in @.ai_assistant_db"

## Context

The Zig emulator implementation (`maiko/alternatives/zig/`) has a complete framework in place but is missing critical functionality to run Medley Interlisp. The C emulator is fully functional and successfully runs Medley. This feature focuses on completing the Zig implementation to achieve functional parity with the C emulator.

## Current Status

### What Works
- ✅ Build system (Zig 0.15.2, SDL2 linked)
- ✅ Project structure and module organization
- ✅ Core types and utilities
- ✅ VM core framework (dispatch loop structure, stack management framework)
- ✅ Basic opcode handlers (arithmetic, comparison, type checking - ~50 opcodes)
- ✅ Memory management structure (GC framework, storage allocation framework)
- ✅ Data structure frameworks (cons cells, arrays, function headers)
- ✅ I/O subsystem structure (keyboard, mouse, filesystem frameworks)
- ✅ Display subsystem structure (SDL backend framework)
- ✅ Comprehensive test suite structure

### Critical Blockers
- ❌ **Sysout loading fails**: Wrong IFPAGE_KEYVAL (uses 0x12345678, should be 0x15e3)
- ❌ **Incomplete IFPAGE structure**: Missing many fields compared to C implementation
- ❌ **No FPtoVP table loading**: Required for memory page mapping
- ❌ **No page loading algorithm**: Cannot load sysout memory pages
- ❌ **VM dispatch loop not activated**: Commented out in main.zig
- ❌ **Many opcodes are placeholders**: ~200 opcodes need implementation
- ❌ **GC hash table operations incomplete**: Structure exists but operations pending
- ❌ **SDL2 display not integrated**: Framework ready but rendering not implemented

## User Scenarios & Testing

### User Story 1 - Load and Run Existing Sysout Files (Priority: P1) 🎯 MVP

A developer wants to load an existing sysout file (created by C emulator) and have the Zig emulator successfully initialize and enter the dispatch loop.

**Why this priority**: Without sysout loading, the emulator cannot run any Lisp code. This is the absolute minimum for functionality.

**Independent Test**: A developer can run `maiko-zig medley/loadups/starter.sysout` and the emulator successfully loads the sysout, initializes VM state, and enters the dispatch loop (even if it doesn't execute code yet).

**Acceptance Scenarios**:

1. **Given** a sysout file from C emulator, **When** Zig emulator loads it, **Then** IFPAGE validation passes (correct keyval 0x15e3)
2. **Given** a sysout file, **When** Zig emulator loads it, **Then** FPtoVP table is read correctly
3. **Given** a sysout file, **When** Zig emulator loads it, **Then** memory pages are mapped to virtual addresses correctly
4. **Given** a sysout file is loaded, **When** VM initializes, **Then** stack pointers, frame pointers, and VM state are set from IFPAGE correctly

---

### User Story 2 - Execute Basic Bytecode Instructions (Priority: P1)

A developer wants the Zig emulator to execute bytecode instructions and produce correct results matching the C emulator.

**Why this priority**: After sysout loading works, basic execution is the next critical step. Without execution, the emulator is just a loader.

**Independent Test**: A developer can load a sysout and execute a simple Lisp expression that produces correct results matching C emulator output.

**Acceptance Scenarios**:

1. **Given** a sysout is loaded, **When** VM executes arithmetic opcodes (IPLUS2, IDIFFERENCE), **Then** results match C emulator exactly
2. **Given** a sysout is loaded, **When** VM executes stack operations (PUSH, POP), **Then** stack state matches C emulator
3. **Given** a sysout is loaded, **When** VM executes function calls, **Then** stack frames are managed correctly
4. **Given** bytecode execution, **When** an error occurs, **Then** error handling matches C emulator behavior

---

### User Story 3 - Complete Essential Opcodes for Medley Startup (Priority: P1)

A developer wants the Zig emulator to execute enough opcodes to successfully start Medley Interlisp and reach the Lisp prompt.

**Why this priority**: Medley requires a specific set of opcodes to initialize. Completing these enables actual Medley execution.

**Independent Test**: A developer can run `maiko-zig medley/loadups/lisp.sysout` and Medley starts successfully, displaying the Interlisp prompt.

**Acceptance Scenarios**:

1. **Given** a full sysout (lisp.sysout), **When** Zig emulator runs it, **Then** Medley initialization completes without errors
2. **Given** Medley is running, **When** basic Lisp expressions are evaluated, **Then** results are correct
3. **Given** Medley is running, **When** errors occur, **Then** error handling works correctly

---

### User Story 4 - Complete GC Operations for Memory Management (Priority: P2)

A developer wants the Zig emulator to properly manage memory using reference-counting GC matching the C implementation.

**Why this priority**: Without proper GC, the emulator will have memory leaks or incorrect behavior during long-running sessions.

**Independent Test**: A developer can run Medley for an extended period, create and destroy many objects, and memory is properly reclaimed without leaks.

**Acceptance Scenarios**:

1. **Given** objects are allocated, **When** references are added/removed, **Then** GC hash table tracks counts correctly
2. **Given** objects have zero references, **When** GC runs, **Then** memory is reclaimed correctly
3. **Given** GC runs, **When** referenced objects exist, **Then** they are not reclaimed

---

### User Story 5 - SDL2 Display Integration for Interactive Sessions (Priority: P2)

A developer wants the Zig emulator to display graphics and handle input via SDL2, enabling interactive Medley sessions.

**Why this priority**: Display and input are essential for a usable emulator, but depend on VM core being functional first.

**Independent Test**: A developer can run Medley and see graphics rendered correctly in an SDL2 window, with keyboard and mouse input working.

**Acceptance Scenarios**:

1. **Given** Medley is running, **When** BitBLT operations occur, **Then** graphics are rendered correctly in SDL2 window
2. **Given** SDL2 window is open, **When** keyboard events occur, **Then** keycodes are translated and delivered to Lisp correctly
3. **Given** SDL2 window is open, **When** mouse events occur, **Then** mouse coordinates and buttons are translated correctly

---

## Edge Cases

- What happens when sysout file is corrupted or has wrong version? → Must validate and provide clear error messages
- How does the implementation handle byte swapping for different endianness? → Must detect and swap bytes correctly
- What if FPtoVP table indicates pages not present in file? → Must handle sparse pages correctly (0177777 marker)
- How are memory allocation failures handled? → Must trigger storage full interrupt and allow GC to reclaim
- What if opcode handler encounters invalid data? → Must handle gracefully with error reporting
- How does the implementation handle stack overflow? → Must detect and trigger interrupt correctly
- What if GC hash table overflows? → Must handle reference count overflow using overflow table
- How are platform-specific differences handled (endianness, word size)? → Must handle byte swapping and alignment correctly

## Requirements

### Functional Requirements

- **FR-001**: Implementation MUST load existing sysout files created by C emulator successfully
- **FR-002**: Implementation MUST use correct IFPAGE_KEYVAL (0x15e3) matching C implementation
- **FR-003**: Implementation MUST implement complete IFPAGE structure matching C implementation exactly
- **FR-004**: Implementation MUST load FPtoVP table from sysout file correctly
- **FR-005**: Implementation MUST implement page loading algorithm mapping file pages to virtual addresses
- **FR-006**: Implementation MUST initialize VM state from IFPAGE (stack pointers, frame pointers, registers)
- **FR-007**: Implementation MUST activate VM dispatch loop to execute bytecode
- **FR-008**: Implementation MUST implement all opcodes required for Medley startup (minimum viable set)
- **FR-009**: Implementation MUST execute opcodes with semantics matching C implementation exactly
- **FR-010**: Implementation MUST implement complete GC hash table operations (ADDREF, DELREF, reclamation)
- **FR-011**: Implementation MUST integrate SDL2 display rendering for BitBLT operations
- **FR-012**: Implementation MUST integrate SDL2 event handling for keyboard and mouse input
- **FR-013**: Implementation MUST handle byte swapping for cross-platform compatibility
- **FR-014**: Implementation MUST validate sysout version compatibility (lversion, minbversion checks)
- **FR-015**: Implementation MUST document all new insights and knowledge in `.ai_assistant_db/`
- **FR-016**: Implementation MUST maintain exact memory layout compatibility with C implementation

### Key Entities

- **IFPAGE**: Interface page structure containing VM state, must match C implementation exactly
- **FPtoVP Table**: File page to virtual page mapping table, required for sysout loading
- **Memory Pages**: 256-byte pages containing Lisp data, mapped via FPtoVP
- **VM State**: Stack pointers, frame pointers, registers initialized from IFPAGE
- **Opcode Handlers**: Functions implementing bytecode instruction semantics
- **GC Hash Tables**: Reference counting tables (HTmain, HTcoll) for memory management

## Success Criteria

### Measurable Outcomes

- **SC-001**: Zig emulator can successfully load at least 3 different existing sysout files (starter.sysout, lisp.sysout, full.sysout) without errors
- **SC-002**: Zig emulator executes bytecode producing identical results to C emulator for at least 100 test cases covering essential opcodes
- **SC-003**: Zig emulator can start Medley Interlisp and reach the Lisp prompt successfully
- **SC-004**: Zig emulator can run Medley for at least 10 minutes without memory leaks or crashes
- **SC-005**: Zig emulator displays graphics correctly in SDL2 window matching C emulator output
- **SC-006**: All new knowledge and insights are documented in `.ai_assistant_db/` with cross-references

## Assumptions

- Zig 0.15.2+ is available (already verified)
- SDL2 2.32.58+ is available (already verified and linked)
- Existing sysout files from C emulator are available for testing
- C emulator implementation serves as reference for correct behavior
- Rewrite documentation in `.ai_assistant_db/rewrite-spec/` is accurate and complete

## Dependencies

- **Spec 001 (Zig Implementation)**: Existing framework and structure
- **C Emulator**: Reference implementation for correct behavior
- **Rewrite Documentation**: `.ai_assistant_db/rewrite-spec/` for specifications
- **Unified Build System (Spec 003)**: For building and testing

## Out of Scope

- Implementing all 256 opcodes (focus on essential set for Medley startup first)
- Performance optimization (correctness first, optimization later)
- Advanced features beyond C emulator capabilities
- Platform support beyond Linux (macOS/Windows can be added incrementally)
