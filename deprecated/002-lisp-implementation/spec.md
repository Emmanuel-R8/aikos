# Feature Specification: Maiko Emulator Implementation in Common Lisp

**Feature Branch**: `002-lisp-implementation`
**Created**: 2025-12-04
**Status**: Draft
**Input**: User description: "Implement a complete Maiko emulator in Common Lisp (SBCL), following the rewrite documentation specifications. The implementation should be compatible with existing sysout files and support SDL display backend (no X11). Start with VM core and memory management as MVP, then add I/O and display subsystems incrementally."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Developer Implements VM Core in Common Lisp (Priority: P1)

A developer wants to implement the Maiko VM core (bytecode interpreter, dispatch loop, stack management) in Common Lisp, following the rewrite documentation specifications. They need a working bytecode interpreter that can execute Lisp programs.

**Why this priority**: The VM core is the foundation of the emulator. Without a working bytecode interpreter, no other functionality can be tested or used. This is the minimum viable product.

**Independent Test**: A developer can compile and run the Common Lisp implementation, load a simple sysout file, and execute bytecode instructions that produce correct results matching Maiko's C implementation behavior.

**Acceptance Scenarios**:

1. **Given** the Common Lisp implementation is built, **When** a developer loads a sysout file, **Then** the VM initializes correctly and enters the dispatch loop
2. **Given** bytecode instructions are loaded, **When** the VM executes arithmetic opcodes (e.g., IPLUS2), **Then** results match Maiko C implementation exactly
3. **Given** a Lisp function is called, **When** the VM executes function call opcodes, **Then** stack frames are managed correctly and function returns work properly
4. **Given** the VM is running, **When** an interrupt occurs (I/O, timer), **Then** interrupts are handled correctly and execution resumes

---

### User Story 2 - Developer Implements Memory Management in Common Lisp (Priority: P1)

A developer wants to implement the garbage collection and memory management system in Common Lisp, ensuring compatibility with existing sysout files and correct object lifetime management.

**Why this priority**: Memory management is critical for correctness. Without proper GC, the emulator will have memory leaks or incorrect behavior. Sysout file compatibility requires exact memory layout matching. Common Lisp's built-in GC can be leveraged, but must be coordinated with Maiko's reference-counting GC algorithm.

**Independent Test**: A developer can load an existing sysout file created by Maiko C implementation, and the Common Lisp implementation correctly maps memory regions, tracks object references, and reclaims memory when objects become unreferenced.

**Acceptance Scenarios**:

1. **Given** a sysout file from Maiko C implementation, **When** the Common Lisp implementation loads it, **Then** all memory regions are mapped correctly and data structures are accessible
2. **Given** objects are allocated in the Common Lisp VM, **When** references are added/removed, **Then** the GC hash table tracks reference counts correctly
3. **Given** objects have zero references, **When** GC runs, **Then** memory is reclaimed correctly without affecting referenced objects
4. **Given** cons cells are allocated, **When** CDR coding is used, **Then** memory layout matches Maiko C implementation exactly

---

### User Story 3 - Developer Implements I/O and Display with SDL in Common Lisp (Priority: P2)

A developer wants to implement the I/O and display subsystems in Common Lisp using SDL, enabling interactive Lisp sessions with graphics and input handling.

**Why this priority**: I/O and display are essential for a usable emulator, but they depend on VM core and memory management being complete. SDL provides cross-platform graphics without X11 dependencies. Common Lisp bindings for SDL (e.g., cl-sdl3) can be used.

**Independent Test**: A developer can run the Common Lisp implementation, interact with Lisp through keyboard and mouse input, and see graphics rendered correctly in an SDL window.

**Acceptance Scenarios**:

1. **Given** the Common Lisp implementation is running, **When** a developer presses keys, **Then** keycodes are translated correctly to Lisp keycodes and events are delivered
2. **Given** mouse events occur, **When** the Common Lisp implementation processes them, **Then** mouse coordinates and button events are translated correctly
3. **Given** Lisp code performs BitBLT operations, **When** graphics are rendered, **Then** the SDL display shows correct output matching Maiko C implementation
4. **Given** file operations are requested, **When** the Common Lisp implementation handles them, **Then** Lisp pathnames are translated correctly to platform paths and file I/O works

---

### Edge Cases

- What happens when sysout file format differs from expected version? → Implementation must validate sysout version and handle version compatibility
- How does the implementation handle memory allocation failures? → Must trigger storage full interrupt and allow GC to reclaim memory
- What if SDL initialization fails? → Must provide clear error message and graceful exit
- How are platform-specific differences handled (endianness, word size)? → Must handle byte swapping and alignment correctly
- What happens when bytecode contains invalid opcodes? → Must handle gracefully with error reporting
- How does the implementation handle stack overflow? → Must detect overflow and trigger interrupt correctly
- What if GC hash table overflows? → Must handle reference count overflow using overflow table
- How does Common Lisp's GC interact with Maiko's reference-counting GC? → Must coordinate both GC systems correctly

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Implementation MUST execute all 256 bytecode opcodes with semantics matching Maiko C implementation exactly
- **FR-002**: Implementation MUST use ASDF (Another System Definition Facility) for build system and dependency management
- **FR-003**: Implementation MUST support SDL display backend (SDL3) and MUST NOT require X11
- **FR-004**: Implementation MUST load and execute existing sysout files created by Maiko C implementation
- **FR-005**: Implementation MUST implement reference-counting garbage collection algorithm matching Maiko C implementation
- **FR-006**: Implementation MUST maintain exact memory layout compatibility (regions, offsets, data structures) with Maiko C implementation
- **FR-007**: Implementation MUST translate LispPTR virtual addresses to native memory addresses using FPtoVP mapping
- **FR-008**: Implementation MUST handle keyboard and mouse events, translating OS events to Lisp event format
- **FR-009**: Implementation MUST support file I/O operations with Lisp pathname translation to platform paths
- **FR-010**: Implementation MUST implement BitBLT graphics operations rendering correctly to SDL display
- **FR-011**: Implementation MUST handle interrupts (I/O, timer, system) correctly between bytecode instructions
- **FR-012**: Implementation MUST support incremental development (VM core → memory → I/O → display) with clear dependencies
- **FR-013**: Implementation MUST follow rewrite documentation specifications in `documentation/rewrite-spec/`
- **FR-014**: Implementation MUST leverage Common Lisp's type system and memory safety features while maintaining compatibility
- **FR-015**: Implementation MUST support at least Linux and macOS platforms (Windows optional)
- **FR-016**: Implementation MUST use SBCL (Steel Bank Common Lisp) as the primary implementation

### Key Entities

- **Common Lisp VM**: The main emulator implementation in Common Lisp, organized in `alternatives/lisp/` directory
- **Bytecode Interpreter**: Core execution engine implementing dispatch loop and opcode handlers
- **Memory Manager**: GC system and virtual memory management (coordinating Common Lisp GC with Maiko's reference-counting)
- **Display Backend**: SDL-based graphics and event handling subsystem
- **I/O Subsystem**: Keyboard, mouse, file system, and network interfaces
- **Sysout File**: Saved Lisp image format that must be loadable by Common Lisp implementation

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Common Lisp implementation can load and execute at least 3 different existing sysout files without errors
- **SC-002**: Common Lisp implementation executes bytecode producing identical results to Maiko C implementation for at least 50 test cases covering all opcode categories
- **SC-003**: Common Lisp implementation successfully runs an interactive Lisp session with SDL display, keyboard input, and mouse input working correctly
- **SC-004**: Common Lisp implementation passes at least 80% of reference behavior test cases from `documentation/rewrite-spec/validation/reference-behaviors.md`
- **SC-005**: Common Lisp implementation compiles successfully on Linux and macOS using SBCL
- **SC-006**: Common Lisp implementation demonstrates type safety and memory safety improvements compared to C implementation while maintaining compatibility
- **SC-007**: Common Lisp implementation can be built and run by a developer unfamiliar with Maiko C code using only the rewrite documentation within 1 week
- **SC-008**: Common Lisp implementation maintains sysout file format compatibility (can load files created by C implementation and vice versa)

## Assumptions

- Target audience has Common Lisp programming language knowledge (SBCL)
- Developers have access to rewrite documentation in `documentation/rewrite-spec/`
- SDL3 libraries are available on target platforms
- Common Lisp libraries (ASDF, cl-sdl3 or similar) can be used for dependencies
- Implementation will be organized in `alternatives/lisp/` directory separate from C codebase
- Common Lisp implementation can use CFFI (Common Foreign Function Interface) for platform APIs (SDL, file system) where needed
- Incremental development approach is acceptable (MVP first, then full feature set)
- SBCL's native code compiler provides sufficient performance for VM execution

## Dependencies

- Rewrite documentation in `documentation/rewrite-spec/` (already complete)
- SBCL (Steel Bank Common Lisp)
- SDL3 development libraries
- ASDF (included with SBCL)
- Common Lisp SDL bindings (e.g., cl-sdl3, cl-sdl2, or CFFI bindings)
- Access to test sysout files for validation
- Reference Maiko C implementation for comparison testing

## Out of Scope

- X11 display backend support (SDL only)
- Network subsystem implementation (can be added later)
- Performance optimization beyond correctness (focus on compatibility first)
- Debugging tools or development utilities
- LispP parser implementation (focus on VM execution)
- Foreign function interface (FFI) support beyond SDL and file system
- Platform-specific optimizations beyond basic portability
- Support for Common Lisp implementations other than SBCL (may be added later)
