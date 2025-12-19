# Research: Zig Emulator Completion

**Date**: 2025-12-07
**Feature**: Zig Emulator Completion - Bring to Parity with C Implementation
**Phase**: Phase 0 - Research

## Research Objectives

Determine the exact implementation details needed to complete the Zig emulator, focusing on critical blockers preventing Medley from running. Research C emulator implementation to understand correct behavior and identify gaps in Zig implementation.

## Research Tasks

### Task 1: IFPAGE Structure and Validation Key

**Research Question**: What is the correct IFPAGE structure and validation key used by C emulator?

**Findings**:

- **IFPAGE_KEYVAL**: C implementation uses `0x15e3` (defined in `maiko/inc/ifpage.h:15`)
- **Zig Implementation Error**: Currently uses `0x12345678` (incorrect placeholder value)
- **IFPAGE Location**: Located at fixed address `IFPAGE_ADDRESS = 512` (offset 512 bytes from file start)
- **IFPAGE Structure**: Complex structure with ~100 fields, varies by BIGVM and BYTESWAP flags
- **Key Field Position**: Field `key` is at offset within IFPAGE structure (position varies by byte order)

**Decision**: Use `IFPAGE_KEYVAL = 0x15e3` matching C implementation. Implement complete IFPAGE structure matching C `ifpage.h` exactly using `packed struct` in Zig.

**Rationale**: Exact keyval match is required for sysout validation. Complete IFPAGE structure is needed to access all VM state fields during initialization.

**Alternatives Considered**:
- Using simplified IFPAGE: Breaks compatibility, many fields are needed for VM initialization
- Different keyval: Would break sysout file compatibility

**Source**: `maiko/inc/ifpage.h`, `maiko/src/ldsout.c:212-216`

---

### Task 2: FPtoVP Table Loading Algorithm

**Research Question**: How does C emulator load and use the FPtoVP (File Page to Virtual Page) table?

**Findings**:

- **Table Location**: `fptovp_offset = (ifpage.fptovpstart - 1) * BYTESPER_PAGE + offset`
  - BIGVM: `+ 4` bytes offset
  - Non-BIGVM: `+ 2` bytes offset
- **Table Format**: Array of entries mapping file page numbers to virtual page numbers
- **Entry Size**:
  - BIGVM: 32-bit entries (unsigned int)
  - Non-BIGVM: 16-bit entries (DLword)
- **Special Value**: `0177777` (0xFFFF) indicates page not present in file
- **Table Size**: `sysout_size * 2` entries (sysout_size in half-pages)
- **Usage**: Iterate through file pages, check FPtoVP entry, if not 0177777, load page data

**Decision**: Implement FPtoVP table loading matching C algorithm exactly. **REQUIRED: Use BIGVM format only** (32-bit entries). Non-BIGVM format is NOT supported. Handle sparse pages (0xFFFF in GETPAGEOK) correctly.

**Rationale**: FPtoVP table is essential for mapping sysout file pages to virtual memory addresses. Incorrect loading breaks memory mapping.

**Alternatives Considered**:
- Simplified page loading: Would break sparse page handling
- Different table format: Would break sysout compatibility

**Source**: `maiko/src/ldsout.c:197-250`, `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md`

---

### Task 3: Page Loading Algorithm

**Research Question**: How does C emulator load memory pages from sysout file into virtual memory?

**Findings**:

- **Page Size**: `BYTESPER_PAGE = 512` bytes (256 words)
- **File Organization**: Sysout file organized as 256-byte pages (half-pages in C terminology)
- **Loading Process**:
  1. Allocate virtual memory space (`mmap` or equivalent)
  2. Read FPtoVP table
  3. For each file page, check FPtoVP entry
  4. If entry != 0177777, seek to file page offset and read page data
  5. Map page data to virtual address: `virtual_address = virtual_page * BYTESPER_PAGE`
- **Byte Swapping**: If `BYTESWAP` defined, swap 16-bit words in each page
- **Memory Allocation**: Allocate `process_size * MBYTE` bytes for virtual memory

**Decision**: Implement page loading algorithm matching C implementation. Use Zig's memory allocation (allocator) instead of `mmap` for portability. Handle byte swapping for cross-platform compatibility.

**Rationale**: Page loading is core to sysout file loading. Must match C behavior exactly for compatibility.

**Alternatives Considered**:
- Memory-mapped files: Zig doesn't have direct mmap equivalent, allocator approach is portable
- Different page size: Would break compatibility

**Source**: `maiko/src/ldsout.c:250-350`, `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md:119-148`

---

### Task 4: Essential Opcodes for Medley Startup

**Research Question**: Which opcodes are essential for Medley to start up and reach the Lisp prompt?

**Findings**:

- **Current Status**: Zig has ~50 opcodes implemented (arithmetic, comparison, type checking, basic stack ops)
- **Critical Opcodes Needed**:
  - **Function Calls**: CALL, RETURN, UNWIND (framework exists, needs completion)
  - **Variable Access**: IVAR, PVAR, FVAR, GVAR (partially implemented)
  - **Cons Cell Operations**: CAR, CDR, CONS (framework ready, needs implementation)
  - **List Operations**: LIST, APPEND, RPLACA, RPLACD (placeholders exist)
  - **Control Flow**: JUMP variants (some implemented, need completion)
  - **Memory Access**: Array access (AREF, ASET - placeholders exist)
  - **Type Operations**: TYPEP, LISTP, ATOMP (partially implemented)
- **Medley Startup Sequence**:
  1. Load sysout → Initialize VM state
  2. Execute initialization code (requires function calls, variable access)
  3. Load greet files (requires file I/O, evaluation)
  4. Start main loop (requires function calls, control flow)

**Decision**: Prioritize implementing opcodes in this order:
1. Function calls (CALL, RETURN) - critical for any execution
2. Cons cell operations (CAR, CDR, CONS) - fundamental Lisp operations
3. Variable access completion (IVAR, PVAR, FVAR, GVAR) - needed for function execution
4. Control flow (JUMP variants) - needed for conditionals and loops
5. List operations (LIST, APPEND) - needed for data manipulation

**Rationale**: Function calls are absolutely essential - without them, no Lisp code can execute. Cons cells are fundamental Lisp data structures. Variable access is needed for function parameters and locals.

**Alternatives Considered**:
- Implement all 256 opcodes first: Too large scope, essential set enables Medley startup
- Different opcode order: Function calls must come first, others can follow

**Source**: `zaiko/src/vm/opcodes.zig`, `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md`

---

### Task 5: GC Hash Table Operations

**Research Question**: How does C emulator implement GC hash table operations (ADDREF, DELREF, reclamation)?

**Findings**:

- **Hash Tables**: Two tables - HTmain (primary) and HTcoll (collision/overflow)
- **ADDREF**: Increment reference count, add to HTmain or HTcoll if overflow
- **DELREF**: Decrement reference count, remove from tables when count reaches zero
- **Reclamation**: When count reaches zero, mark object for reclamation, add to free list
- **Stack References**: Special handling for stack references (STKREF marking)
- **Hash Function**: Uses object address to compute hash bucket
- **Collision Handling**: Overflow entries go to HTcoll table

**Decision**: Implement GC hash table operations matching C implementation. Use Zig hash maps (std.HashMap) for HTmain and HTcoll. Implement ADDREF, DELREF, and reclamation logic.

**Rationale**: GC is essential for memory management. Reference counting must match C behavior for correctness.

**Alternatives Considered**:
- Simplified GC: Would break memory management correctness
- Different hash implementation: Must match C behavior for compatibility

**Source**: `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md`, `maiko/src/gc*.c`

---

### Task 6: SDL2 Display Integration

**Research Question**: How should SDL2 display be integrated for BitBLT operations and event handling?

**Findings**:

- **SDL2 Status**: Already linked in build.zig (enabled in recent commit)
- **Framework Ready**: `src/display/sdl_backend.zig` and `src/display/graphics.zig` have structure
- **C Implementation Pattern**:
  - Initialize SDL2 window and renderer
  - Create texture for display region
  - BitBLT operations copy to texture, render to screen
  - Event loop polls for keyboard/mouse events
- **Display Region**: Memory-mapped region representing screen contents
- **BitBLT Modes**: COPY, XOR, and other modes need implementation

**Decision**: Implement SDL2 initialization, texture-based rendering for BitBLT, and event polling loop. Follow C emulator's SDL2 implementation patterns.

**Rationale**: SDL2 is already linked. Framework is ready. Need to complete rendering and event handling.

**Alternatives Considered**:
- SDL3 instead: C emulator uses SDL2, matching is better for compatibility
- Different rendering approach: Texture-based matches C implementation

**Source**: `maiko/src/sdl.c`, `zaiko/src/display/`

---

### Task 7: VM Dispatch Loop Activation

**Research Question**: How should the VM dispatch loop be activated and integrated with sysout loading?

**Findings**:

- **Current Status**: Dispatch loop is commented out in `main.zig:338-339`
- **C Implementation Pattern**:
  1. Load sysout → Initialize memory
  2. Initialize VM state from IFPAGE (stack pointers, frame pointers)
  3. Call `start_lisp()` → Initialize stack, set up interrupts
  4. Call `dispatch()` → Enter dispatch loop
- **Dispatch Loop**: Fetches bytecode, executes opcodes, handles interrupts
- **Initial PC**: Program counter initialized from sysout state (via IFPAGE or function entry point)

**Decision**: After sysout loading completes, initialize VM state from IFPAGE, set up stack and frame pointers, then activate dispatch loop. Initialize program counter from sysout state.

**Rationale**: Dispatch loop is the execution engine. Must be activated after sysout loading and VM state initialization.

**Alternatives Considered**:
- Activate before sysout loading: Would fail - no code loaded yet
- Different initialization order: Must match C emulator sequence

**Source**: `maiko/src/main.c:776-817`, `maiko/src/xc.c` (dispatch implementation)

---

### Task 8: Knowledge Base Documentation Updates

**Research Question**: What new insights should be documented in `.ai_assistant_db/`?

**Findings**:

- **IFPAGE_KEYVAL Correction**: Documentation should specify correct value (0x15e3, not 0x12345678)
- **IFPAGE Structure**: Complete structure details from C header should be documented
- **FPtoVP Algorithm**: Detailed algorithm with byte offset calculations
- **Page Loading**: Complete algorithm with byte swapping details
- **Essential Opcodes**: List of opcodes needed for Medley startup
- **Zig Implementation Status**: Update with completion progress and known issues

**Decision**: Update `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` with correct IFPAGE details. Create/update `.ai_assistant_db/implementations/zig-implementation.md` with completion status. Document FPtoVP and page loading algorithms with exact details.

**Rationale**: Knowledge base must be accurate and complete. Future implementations benefit from correct documentation. Critical for maintaining project knowledge.

**Alternatives Considered**:
- No documentation updates: Violates FR-015 requirement
- Minimal documentation: Insufficient for future reference

**Source**: `.ai_assistant_db/` directory structure, existing documentation patterns

---

## Key Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| IFPAGE_KEYVAL | 0x15e3 | Matches C implementation, required for sysout validation |
| IFPAGE Structure | Complete C structure | Needed for VM state initialization |
| FPtoVP Loading | Match C algorithm exactly | Required for correct memory mapping |
| Page Loading | Match C algorithm with byte swapping | Required for sysout compatibility |
| Essential Opcodes | Function calls, cons cells, variables first | Enables Medley startup |
| GC Operations | Match C hash table implementation | Required for memory management |
| SDL2 Integration | Texture-based rendering, event polling | Matches C implementation pattern |
| Documentation | Update .ai_assistant_db/ with all findings | Required by FR-015, improves project knowledge |

## Unresolved Questions

None - all research questions resolved with concrete findings from C implementation and documentation.

## Next Steps

Proceed to Phase 1: Design to create data model, contracts, and quickstart guide with these research findings.
