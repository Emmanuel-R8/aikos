# Data Model: Zig Emulator Completion

**Date**: 2025-12-07
**Feature**: Zig Emulator Completion - Bring to Parity with C Implementation

## Overview

This document defines the data structures and entities needed to complete the Zig emulator implementation, focusing on sysout loading, VM state management, and execution.

## Core Entities

### IFPAGE (Interface Page)

**Purpose**: Contains VM state and metadata for sysout file validation and initialization.

**Structure**: Matches C `IFPAGE` structure exactly (from `maiko/inc/ifpage.h`)

**Key Fields**:
- `key: DLword` - Validation key (must be `IFPAGE_KEYVAL = 0x15e3`)
- `lversion: DLword` - Lisp version
- `minbversion: DLword` - Minimum bytecode version
- `process_size: DLword` / `unsigned` - Process size in MB
- `nactivepages: int` - Number of active pages
- `fptovpstart: DLword` - FPtoVP table start offset
- `currentfxp: DLword` - Current frame pointer
- `endofstack: DLword` - End of stack
- `stackbase: DLword` - Stack base address
- `storagefullstate: DLword` - Storage state
- ~90 additional VM state fields

**Validation Rules**:
- `key == IFPAGE_KEYVAL` (0x15e3)
- `lversion >= LVERSION` (version compatibility check)
- `minbversion <= MINBVERSION` (bytecode version compatibility)

**Location**: Fixed offset `IFPAGE_ADDRESS = 512` bytes from sysout file start

**Zig Implementation**: `packed struct` matching C structure exactly

---

### FPtoVP (File Page to Virtual Page Table)

**Purpose**: Maps file page numbers to virtual page numbers for sparse page loading.

**Structure**:
- **BIGVM**: Array of `u32` entries
- **Non-BIGVM**: Array of `u16` (DLword) entries

**Entry Values**:
- `0xFFFF` (0177777): Page not present in file (sparse page marker)
- Other values: Virtual page number where file page should be mapped

**Location**:
- Offset: `(ifpage.fptovpstart - 1) * BYTESPER_PAGE + offset`
  - BIGVM: `+ 4` bytes
  - Non-BIGVM: `+ 2` bytes

**Size**: `sysout_size * 2` entries (sysout_size in half-pages)

**Usage**: Iterate through file pages, check FPtoVP entry, if not 0xFFFF, load page data to virtual address.

**Zig Implementation**: Dynamic array allocated based on sysout size

---

### Memory Page

**Purpose**: Represents a 256-byte page of memory data from sysout file.

**Structure**:
- **Size**: `BYTESPER_PAGE = 512` bytes (256 words)
- **Content**: Raw memory data (may need byte swapping)

**Loading Process**:
1. Check FPtoVP entry for file page
2. If entry != 0xFFFF, seek to file page offset
3. Read 512 bytes
4. Apply byte swapping if needed (BYTESWAP)
5. Write to virtual address: `virtual_page * BYTESPER_PAGE`

**Zig Implementation**: `[512]u8` array, copied to virtual memory

---

### Virtual Memory

**Purpose**: Represents the complete Lisp virtual memory space.

**Structure**:
- **Size**: `process_size * MBYTE` bytes (from IFPAGE)
- **Organization**: Page-based (512-byte pages)
- **Regions**: Stack space, atom space, heap space (MDS), interface page

**Allocation**: Allocated using Zig allocator (equivalent to C `mmap`)

**Mapping**: Pages mapped from sysout file via FPtoVP table

**Zig Implementation**: Allocated buffer, accessed via pointer arithmetic

---

### VM State

**Purpose**: Represents the current execution state of the VM.

**Key Components**:
- **Stack Pointers**: `stackbase`, `endofstack`, `currentfxp` (from IFPAGE)
- **Program Counter**: Current bytecode execution position
- **Frame Pointer**: Current stack frame
- **Registers**: VM registers (if any)
- **Interrupt State**: Pending interrupts

**Initialization**: Set from IFPAGE fields after sysout loading

**Zig Implementation**: Fields in VM struct, initialized from IFPAGE

---

### Opcode Handler

**Purpose**: Function implementing a bytecode instruction's semantics.

**Structure**:
- **Input**: VM state, opcode arguments
- **Output**: Modified VM state, return value (if applicable)
- **Side Effects**: Stack manipulation, memory access, I/O operations

**Categories**:
- **Essential**: Function calls, cons cells, variable access (priority)
- **Basic**: Arithmetic, comparison, type checking (already implemented)
- **Advanced**: Complex operations, I/O, display (lower priority)

**Zig Implementation**: Function pointer or switch case in dispatch loop

---

### GC Hash Table Entry

**Purpose**: Tracks reference count for a memory object.

**Structure**:
- **Key**: Object address (LispPTR)
- **Value**: Reference count (u32)
- **Overflow**: Linked to HTcoll if hash collision

**Operations**:
- **ADDREF**: Increment count, add to HTmain or HTcoll
- **DELREF**: Decrement count, remove if zero
- **Reclamation**: Mark for reclamation when count reaches zero

**Zig Implementation**: `std.HashMap` for HTmain and HTcoll

---

### SDL2 Display Context

**Purpose**: Manages SDL2 window, renderer, and texture for display output.

**Structure**:
- **Window**: SDL_Window pointer
- **Renderer**: SDL_Renderer pointer
- **Texture**: SDL_Texture pointer (for display region)
- **Display Buffer**: Memory region representing screen contents

**Operations**:
- **Initialize**: Create window, renderer, texture
- **BitBLT**: Copy display buffer to texture, render to screen
- **Event Polling**: Poll for keyboard/mouse events

**Zig Implementation**: C interop with SDL2, managed via Zig struct

---

## Relationships

### IFPAGE → FPtoVP
- IFPAGE contains `fptovpstart` field pointing to FPtoVP table location
- One-to-one relationship

### FPtoVP → Memory Pages
- FPtoVP maps file pages to virtual pages
- One-to-many relationship (one FPtoVP entry per file page)

### Memory Pages → Virtual Memory
- Memory pages are loaded into virtual memory at mapped addresses
- Many-to-one relationship (many pages in one virtual memory space)

### IFPAGE → VM State
- IFPAGE contains VM state fields used to initialize VM
- One-to-one relationship

### VM State → Opcode Handlers
- VM state is modified by opcode handlers during execution
- Many-to-many relationship

### Virtual Memory → GC Hash Tables
- GC hash tables track references to objects in virtual memory
- One-to-many relationship (one memory space, many tracked objects)

### SDL2 Display Context → Virtual Memory
- Display buffer is a region in virtual memory
- One-to-one relationship

## State Transitions

### Sysout Loading State Machine

```
[Start] → [Open File] → [Read IFPAGE] → [Validate IFPAGE] → [Allocate Memory]
    ↓
[Read FPtoVP] → [Load Pages] → [Initialize VM State] → [Ready]
```

**Validation Points**:
- IFPAGE key validation (must be 0x15e3)
- Version compatibility checks
- File size validation
- Page count validation

### VM Execution State Machine

```
[Ready] → [Initialize Stack] → [Set Program Counter] → [Enter Dispatch Loop]
    ↓
[Dispatch Loop] → [Fetch Opcode] → [Execute Handler] → [Handle Interrupts]
    ↓                                                          ↑
    └──────────────────────────────────────────────────────────┘
```

**State Transitions**:
- Ready → Executing: After sysout loading completes
- Executing → Paused: On interrupt
- Paused → Executing: After interrupt handling

## Validation Rules

### IFPAGE Validation
- `key == IFPAGE_KEYVAL` (0x15e3)
- `lversion >= LVERSION`
- `minbversion <= MINBVERSION`
- `process_size > 0` and `process_size <= MAX_EXPLICIT_SYSOUTSIZE`

### FPtoVP Validation
- Table size matches `nactivepages`
- File offset calculation is valid
- Entry values are valid virtual page numbers or 0xFFFF

### Memory Page Validation
- Page size is exactly 512 bytes
- Virtual address is within allocated memory range
- Byte swapping applied correctly if needed

### VM State Validation
- Stack pointers are within valid range
- Frame pointer is valid
- Program counter points to valid code

## Constraints

### Memory Constraints
- Virtual memory size must match `process_size` from IFPAGE
- Page addresses must be aligned to 512-byte boundaries
- Stack must not overflow allocated space

### Compatibility Constraints
- IFPAGE structure must match C implementation exactly
- FPtoVP format must match C implementation
- Opcode semantics must match C implementation exactly
- Memory layout must match C implementation

### Performance Constraints
- Sysout loading should complete in < 5 seconds for typical files
- Page loading should be efficient (sequential file reads)
- GC operations should not block execution for long periods

## Data Flow

### Sysout Loading Flow

```
Sysout File → IFPAGE (read at offset 512)
    ↓
IFPAGE → Validation (key, version checks)
    ↓
IFPAGE → Virtual Memory Allocation (process_size)
    ↓
IFPAGE → FPtoVP Table (read at fptovpstart)
    ↓
FPtoVP → Memory Pages (iterate, load if not 0xFFFF)
    ↓
Memory Pages → Virtual Memory (write at virtual addresses)
    ↓
IFPAGE → VM State (initialize from IFPAGE fields)
```

### VM Execution Flow

```
VM State → Dispatch Loop (fetch bytecode)
    ↓
Bytecode → Opcode Handler (execute instruction)
    ↓
Opcode Handler → VM State (modify state)
    ↓
VM State → Interrupt Check (handle if pending)
    ↓
Interrupt Check → Dispatch Loop (continue or handle)
```

### GC Flow

```
Memory Access → ADDREF (increment count)
    ↓
Reference Removed → DELREF (decrement count)
    ↓
Count Reaches Zero → Reclamation (mark for free)
    ↓
Reclamation → Free List (add to available memory)
```
