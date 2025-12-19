---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# API Reference Overview

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [Architecture](../architecture.md) | [Components](../components/) | [Glossary](../glossary.md) | [Build System](../build-system.md)

This directory contains API documentation for Maiko functions, data structures, and interfaces.

**Related Documentation**:

- [VM Core Component](../components/vm-core.md) - Detailed VM core functions
- [Memory Management Component](../components/memory-management.md) - Memory and GC functions
- [Display Component](../components/display.md) - Display functions
- [I/O Systems Component](../components/io.md) - I/O functions

## Organization

### Core APIs

- **[VM Core](../components/vm-core.md)**: Dispatch loop, stack management, instruction execution
  - See [Key Functions](../components/vm-core.md#key-files) for function listings
- **[Memory Management](../components/memory-management.md)**: Allocation, GC, virtual memory
  - See [Key Functions](../components/memory-management.md#key-functions) for function listings
- **[Display](../components/display.md)**: Graphics output, window management
  - See [Key Files](../components/display.md#key-files) for function listings
- **[I/O](../components/io.md)**: Keyboard, mouse, file system, network
  - See [Key Files](../components/io.md#key-files) for function listings

### Function Categories

#### Initialization Functions

- `main()` - Entry point
- `start_lisp()` - VM startup
- `init_storage()` - Storage initialization
- `init_keyboard()` - Keyboard initialization
- `init_dsp()` - Display initialization

#### Execution Functions

- `dispatch()` - Main dispatch loop
- `OP_*()` - Opcode handlers
- `lcfuncall()` - Function call
- `make_FXcopy()` - Hard return

#### Memory Functions

- `cons()` - Allocate cons cell
- `newpage()` - Allocate page
- `OP_gcref()` - GC reference
- `gcmapscan()` - GC scan

#### Display Functions

- `Open_Display()` - Open display
- `Create_LispWindow()` - Create window
- `clipping_Xbitblt()` - BitBLT operation
- `init_SDL()` - Initialize SDL

#### I/O Functions

- `kb_trans()` - Key translation
- File I/O functions
- Network functions

## Data Structures

### Core Types

- `LispPTR` - Virtual address
- `DLword` - 16-bit word
- `ByteCode` - Bytecode instruction
- `struct state` - Execution state
- `struct fnhead` - Function header
- `FX` - Stack frame

### Memory Types

- `ConsCell` - Cons cell structure
- `GCENTRY` - GC hash table entry
- `INTSTAT` - Interrupt state

### Display Types

- `DspInterface` - Display interface
- `MRegion` - Memory region

## Header Files

Key header files defining APIs:

- `maiko/inc/lispemul.h` - Core types and macros
- `maiko/inc/lsptypes.h` - Lisp types
- `maiko/inc/stack.h` - Stack definitions
- `maiko/inc/gcdefs.h` - GC functions
- `maiko/inc/dspifdefs.h` - Display interface
- `maiko/inc/kbdif.h` - Keyboard interface

## Function Naming Conventions

- `OP_*` - Opcode handlers
- `init_*` - Initialization functions
- `*_68k` - Functions operating on 68k addresses
- `Native*` - Address translation functions
- `Get_*` - Getter macros/functions
- `Set_*` - Setter macros/functions

## Macros

### Address Translation

- `NativeAligned2FromLAddr()` - Convert to 16-bit aligned native pointer
- `NativeAligned4FromLAddr()` - Convert to 32-bit aligned native pointer
- `LAddrFromNative()` - Convert native address to Lisp address

### Memory Access

- `Get_DLword()` - Get 16-bit word
- `Get_Pointer()` - Get pointer value
- `GETWORD()` - Get word macro

### Stack Operations

- `PushStack()` - Push value on stack
- `PopStack()` - Pop value from stack
- `FastRetCALL` - Fast return macro

## See Also

- [Component Documentation](../components/) - Detailed component documentation
  - [VM Core](../components/vm-core.md) - Execution functions
  - [Memory Management](../components/memory-management.md) - Memory functions
  - [Display](../components/display.md) - Display functions
  - [I/O Systems](../components/io.md) - I/O functions
- [Glossary](../glossary.md) - Terminology reference
  - [Core Concepts](../glossary.md#core-concepts) - Data types and basic concepts
  - [Execution Terms](../glossary.md#execution-terms) - Execution-related terms
- [Architecture Overview](../architecture.md) - System architecture context
