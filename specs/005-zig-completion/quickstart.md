# Quickstart: Zig Emulator Completion

**Date**: 2025-12-07
**Feature**: Zig Emulator Completion - Bring to Parity with C Implementation

## Overview

This guide provides a quick start for completing the Zig emulator implementation to achieve functional parity with the C emulator. It covers the critical path from sysout loading to Medley execution.

## Prerequisites

- Zig 0.15.2+ installed
- SDL2 2.32.58+ development libraries
- Existing Zig emulator build (from spec 001)
- C emulator as reference implementation
- Medley sysout files for testing

## Critical Path

### Step 1: Fix Sysout Loading (P1 - MVP)

**Goal**: Successfully load existing sysout files

**Tasks**:
1. Fix IFPAGE_KEYVAL: Change from `0x12345678` to `0x15e3` in `src/data/sysout.zig`
2. Implement complete IFPAGE structure: Match C `ifpage.h` exactly with all ~100 fields
3. Implement FPtoVP table loading: Read table at correct offset (BIGVM format - 32-bit entries, REQUIRED)
4. Implement page loading algorithm: Iterate through file pages, map via FPtoVP, load into virtual memory
5. Add byte swapping support: Handle BYTESWAP for cross-platform compatibility

**Files to Modify**:
- `maiko/alternatives/zig/src/data/sysout.zig` - Complete sysout loading implementation
- `maiko/alternatives/zig/src/utils/types.zig` - Add IFPAGE structure definition

**Testing**:
```bash
cd maiko/alternatives/zig
zig build
./zig-out/bin/maiko-zig ../../medley/loadups/starter.sysout
# Should successfully load sysout (may not execute yet)
```

**Success Criteria**: Sysout file loads without `SysoutLoadFailed` error

---

### Step 2: Activate VM Dispatch Loop (P1)

**Goal**: Enter dispatch loop after sysout loading

**Tasks**:
1. Initialize VM state from IFPAGE: Set stack pointers, frame pointer, program counter
2. Uncomment dispatch loop in `main.zig`: Activate execution
3. Set up initial program counter: From sysout state or function entry point
4. Initialize interrupt handling: Set up interrupt state

**Files to Modify**:
- `maiko/alternatives/zig/src/main.zig` - Activate dispatch loop
- `maiko/alternatives/zig/src/vm/dispatch.zig` - Ensure dispatch loop is ready
- `maiko/alternatives/zig/src/vm/` - VM state initialization

**Testing**:
```bash
zig build
./zig-out/bin/maiko-zig ../../medley/loadups/starter.sysout
# Should enter dispatch loop (may fail on opcode execution)
```

**Success Criteria**: VM enters dispatch loop without immediate errors

---

### Step 3: Implement Essential Opcodes (P1)

**Goal**: Execute enough opcodes for Medley startup

**Tasks**:
1. Complete function call opcodes: CALL, RETURN, UNWIND
2. Complete cons cell operations: CAR, CDR, CONS
3. Complete variable access: IVAR, PVAR, FVAR, GVAR (all variants)
4. Complete control flow: JUMP variants (JUMP0-JUMP15, etc.)
5. Complete list operations: LIST, APPEND, RPLACA, RPLACD

**Files to Modify**:
- `maiko/alternatives/zig/src/vm/opcodes.zig` - Implement opcode handlers
- `maiko/alternatives/zig/src/data/cons.zig` - Complete cons cell operations

**Testing**:
```bash
zig build test  # Run opcode tests
zig build
./zig-out/bin/maiko-zig ../../medley/loadups/lisp.sysout
# Should progress further in startup
```

**Success Criteria**: Medley initialization progresses further, fewer opcode errors

---

### Step 4: Complete GC Operations (P2)

**Goal**: Proper memory management with reference counting

**Tasks**:
1. Implement ADDREF: Increment reference count, add to HTmain or HTcoll
2. Implement DELREF: Decrement reference count, remove when zero
3. Implement reclamation: Mark objects for reclamation when count reaches zero
4. Implement hash table operations: HTmain and HTcoll management

**Files to Modify**:
- `maiko/alternatives/zig/src/memory/gc.zig` - Complete GC operations

**Testing**:
```bash
zig build test  # Run GC tests
zig build
./zig-out/bin/maiko-zig ../../medley/loadups/lisp.sysout
# Should run without memory leaks
```

**Success Criteria**: No memory leaks during extended execution

---

### Step 5: Integrate SDL2 Display (P2)

**Goal**: Display graphics and handle input

**Tasks**:
1. Initialize SDL2: Create window, renderer, texture
2. Implement BitBLT rendering: Copy display buffer to texture, render to screen
3. Implement event polling: Keyboard and mouse event handling
4. Integrate with VM: Connect display operations to VM execution

**Files to Modify**:
- `maiko/alternatives/zig/src/display/sdl_backend.zig` - Complete SDL2 integration
- `maiko/alternatives/zig/src/display/graphics.zig` - Implement BitBLT operations

**Testing**:
```bash
zig build
./zig-out/bin/maiko-zig ../../medley/loadups/starter.sysout -sc 1024x768
# Should open SDL2 window and display graphics
```

**Success Criteria**: SDL2 window opens, graphics display, input works

---

## Testing Strategy

### Unit Tests

Test individual components:
```bash
cd maiko/alternatives/zig
zig build test
```

**Test Coverage**:
- Sysout loading and validation
- FPtoVP table loading
- Page loading algorithm
- Opcode handlers
- GC operations

### Integration Tests

Test with actual sysout files:
```bash
./zig-out/bin/maiko-zig ../../medley/loadups/starter.sysout
./zig-out/bin/maiko-zig ../../medley/loadups/lisp.sysout
./zig-out/bin/maiko-zig ../../medley/loadups/full.sysout
```

### Comparison Tests

Compare behavior with C emulator:
```bash
# C emulator
../../linux.x86_64/ldesdl ../../medley/loadups/starter.sysout -sc 1024x768

# Zig emulator
./zig-out/bin/maiko-zig ../../medley/loadups/starter.sysout -sc 1024x768
```

Compare execution results, memory usage, and behavior.

---

## Debugging Tips

### Sysout Loading Issues

- **Wrong keyval**: Check IFPAGE_KEYVAL is 0x15e3
- **Version mismatch**: Check lversion and minbversion compatibility
- **Page loading fails**: Check FPtoVP table loading and page mapping
- **Memory errors**: Check virtual memory allocation size

### VM Execution Issues

- **Invalid opcode**: Check opcode handler implementation
- **Stack errors**: Check stack pointer initialization and bounds
- **Memory access errors**: Check address translation and bounds

### Display Issues

- **SDL2 not found**: Check SDL2 linking in build.zig
- **Window doesn't open**: Check SDL2 initialization
- **No graphics**: Check BitBLT implementation and texture rendering

---

## Documentation Updates

After completing each step, update knowledge base:

1. **IFPAGE Structure**: Update `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` with correct IFPAGE details
2. **FPtoVP Algorithm**: Document exact algorithm with byte offset calculations
3. **Page Loading**: Document complete algorithm with byte swapping
4. **Zig Implementation Status**: Update `.ai_assistant_db/implementations/zig-implementation.md` with completion status

---

## Success Criteria

### MVP (Minimum Viable Product)

- ✅ Sysout file loads successfully
- ✅ VM enters dispatch loop
- ✅ Basic opcodes execute correctly
- ✅ Medley starts (may not reach prompt yet)

### Full Completion

- ✅ All essential opcodes implemented
- ✅ GC operations complete
- ✅ SDL2 display integrated
- ✅ Medley reaches Lisp prompt
- ✅ Can run interactive sessions
- ✅ All knowledge documented in `.ai_assistant_db/`

---

## Next Steps

After completing this quickstart:

1. Implement remaining opcodes (beyond essential set)
2. Performance optimization
3. Additional platform support (macOS, Windows)
4. Comprehensive test coverage
5. Documentation completion

---

## Resources

- **C Implementation Reference**: `maiko/src/ldsout.c`, `maiko/src/main.c`
- **IFPAGE Structure**: `maiko/inc/ifpage.h`
- **Rewrite Documentation**: `.ai_assistant_db/rewrite-spec/`
- **Zig Implementation**: `maiko/alternatives/zig/`
- **Medley Documentation**: `.ai_assistant_db/medley/`

---

## Getting Help

- Check C emulator implementation for reference behavior
- Review rewrite documentation for specifications
- Compare with existing Zig implementation patterns
- Test incrementally after each change
