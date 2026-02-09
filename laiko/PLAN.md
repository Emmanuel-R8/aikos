# Laiko (Common Lisp Maiko) Implementation Plan

**Date**: 2026-02-06
**Status**: Phase 1 In Progress
**Target**: SBCL + cl-sdl3

## Progress Summary

### Phase 1: Sysout Loading Foundation ✅ COMPLETE

| Task | Status | Notes |
|------|--------|-------|
| 1.1 Fix IFPAGE constants | ✅ Done | `#x15e3`, 512 offset |
| 1.2 Add IFPAGE_ADDRESS constant | ✅ Done | |
| 1.3 Read IFPAGE at offset 512 | ✅ Done | |
| 1.4 Extract FPtoVP table | ✅ Done | |
| 1.5 Build virtual memory pages | ✅ Done | |
| 1.6 Initialize VM memory | ✅ Done | |
| 1.7 Bytecode extraction | ✅ Done | `extract-bytecode-from-vm` |

### Phase 2: Unified Trace Format ✅ IMPLEMENTED

| Task | Status | Notes |
|------|--------|-------|
| 2.1 Create trace module | ✅ Done | `src/vm/trace.lisp` |
| 2.2 Implement trace logging | ✅ Done | Unified format columns |
| 2.3 EMULATOR_MAX_STEPS support | ✅ Done | Environment variable |
| 2.4 Add Laiko to comparison script | ⏳ Pending | Script update |

### File Organization Improvements

| File | Lines | Status |
|------|-------|--------|
| `src/data/sysout.lisp` | 157 | ✅ Under 500 |
| `src/data/sysout-utils.lisp` | 55 | ✅ New |
| `src/data/bytecode.lisp` | 92 | ✅ New |
| `src/vm/trace.lisp` | 48 | ✅ New |
| `src/vm/opcodes.lisp` | 1354 | ⚠️ Needs splitting |

### Phase 2: Unified Trace Format - NOT STARTED

### Phase 3: Complete Opcode Implementation - NOT STARTED

### Phase 4: SDL3 Display Backend - NOT STARTED

### Phase 5: Parity Testing - NOT STARTED

## Overview

Laiko is the Common Lisp implementation of the Maiko emulator (Interlisp virtual machine). This plan outlines resuming development to achieve functional sysout execution with unified trace format for parity testing against C and Zig implementations.

## Project Structure

```
laiko/
├── maiko-lisp.asd          # ASDF system definition
├── README.md                # User-facing documentation
├── build.sh                # Build script
├── run.sh                  # Run script
├── PLAN.md                 # This plan
├── src/
│   ├── main.lisp           # Entry point, command-line parsing
│   ├── package.lisp        # Package definitions
│   ├── vm/
│   │   ├── dispatch.lisp   # Dispatch loop, opcode execution
│   │   ├── opcodes.lisp    # Opcode handlers (189/256 implemented)
│   │   ├── stack.lisp      # Stack frame management
│   │   └── function.lisp   # Function call/return
│   ├── memory/
│   │   ├── virtual.lisp    # Virtual memory, FPtoVP mapping
│   │   ├── storage.lisp   # Heap allocation
│   │   ├── layout.lisp     # Memory layout constants
│   │   └── gc.lisp         # Garbage collection
│   ├── data/
│   │   ├── sysout.lisp     # Sysout file loading
│   │   ├── cons.lisp       # Cons cell operations
│   │   ├── array.lisp      # Array handling
│   │   └── function-header.lisp # Function header parsing
│   ├── display/
│   │   ├── sdl-backend.lisp # SDL3 display backend
│   │   ├── graphics.lisp   # BitBLT, rendering
│   │   └── events.lisp     # Event handling
│   ├── io/
│   │   ├── keyboard.lisp   # Keyboard input
│   │   ├── mouse.lisp      # Mouse input
│   │   └── filesystem.lisp # File I/O
│   └── utils/
│       ├── types.lisp      # Type definitions
│       ├── errors.lisp      # Error conditions
│       └── address.lisp     # Address calculations
└── tests/
    ├── opcodes.lisp        # Opcode tests
    ├── sysout.lisp         # Sysout loading tests
    ├── stack.lisp          # Stack tests
    └── ...
```

## Current State

| Component | Status | Notes |
|-----------|--------|-------|
| Opcode handlers | 189/256 (74%) | Missing FP, BitBLT, I/O ops |
| Dispatch loop | Complete | Needs trace logging |
| Stack management | Complete | — |
| Memory management | Complete | — |
| Sysout loading | Partial | IFPAGE constants wrong |
| SDL3 backend | Structure in place | Needs cl-sdl3 integration |
| Unified trace | Not implemented | — |

## Critical Issues

| Issue | Location | Severity |
|-------|----------|----------|
| `IFPAGE_KEYVAL` wrong | `sysout.lisp:7` | CRITICAL |
| IFPAGE offset wrong | `sysout.lisp` | CRITICAL |
| No bytecode extraction | `main.lisp:226` | CRITICAL |
| 67 opcodes missing | `opcodes.lisp` | HIGH |
| Unified trace format | Not implemented | HIGH |

---

# Implementation Plan

## Phase 1: Sysout Loading Foundation

**Goal**: Load actual sysout bytecode instead of hardcoded test array

### Task 1.1: Fix IFPAGE Constants

**File**: `laiko/src/data/sysout.lisp`

**Changes**:
```lisp
;; Current (WRONG)
(defconstant +sysout-keyval+ #x12345678)

;; Correct (per maiko/inc/ifpage.h:15)
(defconstant +sysout-keyval+ #x15e3)
```

**Source Reference**: `maiko/inc/ifpage.h:15`

### Task 1.2: Add IFPAGE Address Constant

**File**: `laiko/src/data/sysout.lisp`

**Changes**:
```lisp
;; IFPAGE is at byte offset 512 in sysout file
(defconstant +ifpage-address+ 512)
```

**Source Reference**: `maiko/inc/ifpage.h` (general structure)

### Task 1.3: Read IFPAGE at Correct Offset

**File**: `laiko/src/data/sysout.lisp:load-sysout`

**Changes**: Seek to byte 512 before reading IFPAGE structure

**Source Reference**: `maiko/src/xc.c` (sysout loading, `read_ifpage`)

### Task 1.4: Extract FPtoVP Table

**File**: `laiko/src/data/sysout.lisp`

**Changes**:
- Read `fptovpstart` from IFPAGE
- Read FPtoVP table at computed offset
- Build mapping array: file page → virtual page

**Source Reference**:
- `maiko/src/xc.c` (`fptovp` table loading)
- `maiko/inc/fptovp.h`

### Task 1.5: Build Virtual Memory Pages

**File**: `laiko/src/data/sysout.lisp`

**Changes**:
- Read `nactivepages` from IFPAGE
- For each active page, read into virtual memory
- Use FPtoVP mapping to place pages correctly

**Source Reference**: `maiko/src/xc.c` (page loading, `readpage`)

### Task 1.6: Initialize VM Memory

**File**: `laiko/src/memory/virtual.lisp`

**Changes**:
- Initialize virtual memory from FPtoVP mapping
- Set up memory regions per sysout layout
- Configure `currentfxp` (current frame pointer) from IFPAGE

**Source Reference**: `maiko/src/xc.c` (memory initialization)

### Task 1.7: Bytecode Extraction

**File**: `laiko/src/main.lisp:226-228`

**Current** (WRONG):
```lisp
(let ((test-code (make-array 10 ... :initial-contents '(#xD8 #xD9 #xBF ...))))
  ...)
```

**Replace with**: Extract actual bytecode from virtual memory at PC location from IFPAGE

**Source Reference**: `maiko/src/xc.c` (bytecode extraction from sysout)

---

## Phase 2: Unified Trace Format

**Goal**: Create Laiko trace output compatible with C/Zig for parity testing

### Task 2.1: Create/Extend Unified Trace Specification

**File**: `documentation/implementations/unified-trace-format.typ`

**Changes**: Document Laiko trace format requirements

**Source Reference**: `zaiko/src/vm/execution_trace.zig`

### Task 2.2: Implement Trace Logging in Dispatch Loop

**File**: `laiko/src/vm/dispatch.lisp`

**Changes**: Add trace logging before each instruction execution

**Trace Format**:
```
LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
```

**Trace Columns**:
| Column | Description | Example |
|--------|-------------|---------|
| LINE# | Line number | 1 |
| PC | Program counter (byte offset) | 0x60f130 |
| INSTRUCTION | Opcode name | POP |
| OPCODE | Opcode byte (hex) | 0xBF |
| OPERANDS | Operand bytes | — |
| REGISTERS | Register state | — |
| FLAGS | Condition flags | Z:0,N:0 |
| SP_FP | Stack/Frame pointers | SP:0x012e8a FP:0x012e72 |
| STACK_SUMMARY | Top of stack | TOS:0x00000000 |
| MEMORY_CONTEXT | Memory state | @mem:? [vpage:N off:0xNNN] |
| FP_VP_FO_VA | Address mapping | [vpage:...] |
| BS_MEM | Byte swap state | — |
| NOTES | Additional info | — |

**Source Reference**:
- `zaiko/src/vm/execution_trace.zig` (format implementation)
- `documentation/implementations/unified-trace-format.typ`

### Task 2.3: Environment Variable Support

**File**: `laiko/src/main.lisp`

**Changes**: Support `EMULATOR_MAX_STEPS=N` environment variable

**Source Reference**: `zaiko/src/main.zig` (environment variable handling)

### Task 2.4: Add Laiko to Comparison Script

**File**: `scripts/compare_emulator_execution.sh`

**Changes**: Add Laiko trace generation and comparison

---

## Phase 3: Complete Opcode Implementation

**Goal**: Implement remaining 67 opcodes

### Task 3.1: Floating-Point Operations

**Opcodes**: FPLUS2 (0xE8), FDIFFERENCE (0xE9), FTIMES2 (0xEA), FQUOTIENT (0xEB)

**File**: `laiko/src/vm/opcodes.lisp`

**Current**: Fall back to integer operations

**Implement**: Proper floating-point arithmetic

**Source Reference**:
- `maiko/src/float.c`
- `maiko/src/fnmath.c`

### Task 3.2: BitBLT Operations

**Opcodes**: BitBLT, COPY, variants

**File**: `laiko/src/vm/opcodes.lisp`

**Source Reference**: `maiko/src/bitblt.c`, `maiko/src/bbt.c`

### Task 3.3: I/O Operations

**Opcodes**: DSKO$, etc.

**File**: `laiko/src/vm/opcodes.lisp`

**Source Reference**: `maiko/src/dsk.c`, `maiko/src/usrdsk.c`

### Task 3.4: Remaining Arithmetic/Comparison

**Source Reference**: `maiko/src/llisparith.c`

---

## Phase 4: SDL3 Display Backend

**Goal**: Implement display using cl-sdl3

### Task 4.1: Import cl-sdl3

**File**: `laiko/src/display/sdl-backend.lisp`

**Note**: User will install cl-sdl3 separately

### Task 4.2: BitBLT Rendering

**File**: `laiko/src/display/graphics.lisp`

**Source Reference**: `maiko/src/bitblt.c`

### Task 4.3: Event Translation

**File**: `laiko/src/display/events.lisp`

**Source Reference**: `maiko/src/xevent.c`

### Task 4.4: Window Lifecycle

**File**: `laiko/src/display/sdl-backend.lisp`

**Source Reference**: `maiko/src/xwindow.c`

---

## Phase 5: Parity Testing

**Goal**: Validate Laiko matches C/Zig

### Task 5.1: Trace Comparison with C

```bash
EMULATOR_MAX_STEPS=50 ./scripts/compare_emulator_execution.sh --emulator c
```

### Task 5.2: Trace Comparison with Zig

```bash
EMULATOR_MAX_STEPS=50 ./scripts/compare_emulator_execution.sh --emulator zig
```

### Task 5.3: Fix Divergences

Per trace analysis, fix systematic differences

---

# Documentation Updates (Incremental)

Per AGENTS.md §2 and `documentation/core/critical-memory.typ`:

| Finding Type | Location |
|--------------|----------|
| General (emulator-independent) | `documentation/specifications/` |
| Laiko-specific | `documentation/implementations/` |

---

# Build & Run Commands

```bash
# Build
sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)"

# Run tests
sbcl --load maiko-lisp.asd --eval "(asdf:test-system :maiko-lisp)"

# Run with sysout
sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)" -- \
  -sysout medley/internal/loadups/starter.sysout

# Trace comparison
EMULATOR_MAX_STEPS=50 ./scripts/compare_emulator_execution.sh
```

---

# Source References

| Resource | Path |
|----------|------|
| C implementation | `maiko/src/` |
| C headers | `maiko/inc/` |
| Zig trace format | `zaiko/src/vm/execution_trace.zig` |
| Unified trace spec | `documentation/implementations/unified-trace-format.typ` |
| Memory layout | `documentation/specifications/memory-layout.typ` |
| Opcode specs | `documentation/specifications/instruction-set/opcodes.typ` |
| IFPAGE def | `maiko/inc/ifpage.h` |
| FPtoVP | `maiko/inc/fptovp.h` |

---

# Timeline

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| 1. Sysout Loading | 2-3 days | None |
| 2. Unified Trace | 2 days | None |
| 3. Missing Opcodes | 3-5 days | maiko/src/ |
| 4. SDL3 Backend | 3-4 days | cl-sdl3 |
| 5. Parity Testing | Ongoing | Phases 1-4 |

**Total**: 8-12 days to functional execution + ongoing parity

---

**Status**: In Progress
**Last Updated**: 2026-02-06
**Started**: 2026-02-06
