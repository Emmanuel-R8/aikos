# Data Model: Zig Implementation

**Date**: 2025-12-04
**Feature**: Maiko Emulator Implementation in Zig
**Phase**: Phase 1 - Design

## Overview

This document defines the data structures and types for the Zig implementation of Maiko emulator. All structures must maintain exact compatibility with C implementation for sysout file compatibility.

## Core Types

### LispPTR

```zig
// 32-bit virtual address (matches C LispPTR)
pub const LispPTR = u32;

// Address component extraction (matches C macros)
pub fn hiloc(ptr: LispPTR) u16 {
    return @truncate(u16, ptr >> 16);
}

pub fn loloc(ptr: LispPTR) u16 {
    return @truncate(u16, ptr);
}
```

### DLword

```zig
// 16-bit word (matches C DLword)
pub const DLword = u16;
```

### ByteCode

```zig
// Bytecode instruction byte
pub const ByteCode = u8;
```

## VM Core Structures

### Stack Frame (FX)

```zig
// Stack frame structure (matches C FX)
pub const FX = packed struct {
    nextblock: LispPTR,      // Next stack block pointer
    link: LispPTR,           // Activation link (previous frame)
    fnheader: LispPTR,       // Function header pointer
    pcoffset: DLword,        // PC offset
    // ... local variables follow
};
```

### Function Header

```zig
// Function header (matches C fnhead)
pub const FunctionHeader = packed struct {
    stkmin: DLword,          // Minimum stack size
    na: DLword,              // Number of arguments
    pv: DLword,              // Number of PVars
    startpc: DLword,         // Starting PC
    framename: LispPTR,      // Frame name atom
    ntsize: DLword,          // Name table size
    nlocals: DLword,         // Number of local variables
    fvaroffset: DLword,      // FVar offset
    // ... name table and code follow
};
```

## Memory Management Structures

### Cons Cell

```zig
// Cons cell (matches C ConsCell)
pub const ConsCell = packed struct {
    car_field: LispPTR,      // CAR field
    cdr_code: u8,            // CDR coding (low 8 bits)
    // CDR coding values: CDR_NIL, CDR_ONPAGE, CDR_INDIRECT
};
```

### GC Hash Table Entry

```zig
// GC hash table entry (matches C hash table structure)
pub const HashEntry = packed struct {
    count: u15,              // Reference count (15 bits)
    stackref: u1,            // Stack reference flag (1 bit)
    segnum: u15,             // Segment number (15 bits)
    collision: u1,            // Collision flag (1 bit)
};
```

### Memory Regions

```zig
// Memory region offsets (matches C defines)
pub const MemoryOffsets = struct {
    pub const IFPAGE_OFFSET: u32 = 0x00000000;
    pub const STK_OFFSET: u32 = 0x00010000;
    pub const ATMHT_OFFSET: u32 = 0x00020000;
    pub const ATOMS_OFFSET: u32 = 0x00030000;
    pub const MDS_OFFSET: u32 = 0x00100000;
    // ... other offsets
};
```

## Display Structures

### Display Interface

```zig
// Display interface (abstracts SDL backend)
pub const DisplayInterface = struct {
    window: *sdl.SDL_Window,
    renderer: *sdl.SDL_Renderer,
    texture: *sdl.SDL_Texture,
    width: u32,
    height: u32,
    display_region: []DLword, // Display region buffer
};
```

## I/O Structures

### Keyboard Event

```zig
// Keyboard event (translated to Lisp format)
pub const KeyboardEvent = struct {
    event_type: EventType,   // KEY_PRESS or KEY_RELEASE
    keycode: u16,            // Lisp keycode
    modifiers: u16,          // Modifier flags
    timestamp: u32,          // Event timestamp
};
```

### Mouse Event

```zig
// Mouse event (translated to Lisp format)
pub const MouseEvent = struct {
    event_type: EventType,   // BUTTON_PRESS, BUTTON_RELEASE, MOTION
    button: u8,              // Button number (1-3) or 0 for motion
    x: i32,                  // X coordinate
    y: i32,                  // Y coordinate
    modifiers: u16,          // Modifier flags
    timestamp: u32,          // Event timestamp
};
```

## Sysout File Structures

### IFPAGE

```zig
// Interface page (matches C IFPAGE)
pub const IFPAGE = packed struct {
    keyval: u32,             // Validation key
    version: u32,            // Version number
    // ... VM state fields
    currentfxp: LispPTR,     // Current frame pointer
    endofstack: LispPTR,     // End of stack
    // ... other state
};
```

## Relationships

- **VM State**: Contains stack frames, current PC, interrupt state
- **Memory Manager**: Manages heap, GC hash tables, page mapping
- **Display Backend**: Uses display region buffer, renders via SDL
- **I/O Subsystem**: Translates OS events to Lisp events, manages file I/O

## Validation Rules

- All structures must use `packed struct` for exact byte layout
- Alignment must match C implementation (use `align()` attribute)
- Endianness must be handled for cross-platform compatibility
- Memory offsets must match C implementation exactly

## Platform Considerations

- **Endianness**: Use `@byteSwap()` for byte swapping when needed
- **Alignment**: Explicit alignment attributes ensure compatibility
- **Word Size**: Maintain 16-bit words internally regardless of platform
- **Pointer Size**: LispPTR is always 32-bit, native pointers may differ
