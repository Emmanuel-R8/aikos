= API Reference Overview

This directory contains API documentation for Maiko functions, data structures, and interfaces.

== Organization

=== Core APIs

- *VM Core*: Dispatch loop, stack management, instruction execution
- *Memory Management*: Allocation, GC, virtual memory
- *Display*: Graphics output, window management
- *I/O*: Keyboard, mouse, file system, network

=== Function Categories

==== Initialization Functions

- `main()` - Entry point
- `start_lisp()` - VM startup
- `init_storage()` - Storage initialization
- `init_keyboard()` - Keyboard initialization
- `init_dsp()` - Display initialization

==== Execution Functions

- `dispatch()` - Main dispatch loop
- `OP_ pointer()` - Opcode handlers
- `lcfuncall()` - Function call
- `make_FXcopy()` - Hard return

==== Memory Functions

- `cons()` - Allocate cons cell
- `newpage()` - Allocate page
- `OP_gcref()` - GC reference
- `gcmapscan()` - GC scan

==== Display Functions

- `Open_Display()` - Open display
- `Create_LispWindow()` - Create window
- `clipping_Xbitblt()` - BitBLT operation
- `init_SDL()` - Initialize SDL

==== I/O Functions

- `kb_trans()` - Key translation
- File I/O functions
- Network functions

== Data Structures

=== Core Types

- `LispPTR` - Virtual address
- `DLword` - 16-bit word
- `ByteCode` - Bytecode instruction
- `struct state` - Execution state
- `struct fnhead` - Function header
- `FX` - Stack frame

=== Memory Types

- `ConsCell` - Cons cell structure
- `GCENTRY` - GC hash table entry
- `INTSTAT` - Interrupt state

=== Display Types

- `DspInterface` - Display interface
- `MRegion` - Memory region

== Header Files

Key header files defining APIs:

- `maiko/inc/lispemul.h` - Core types and macros
- `maiko/inc/lsptypes.h` - Lisp types
- `maiko/inc/stack.h` - Stack definitions
- `maiko/inc/gcdefs.h` - GC functions
- `maiko/inc/dspifdefs.h` - Display interface
- `maiko/inc/kbdif.h` - Keyboard interface

== Function Naming Conventions

- `OP_ pointer` - Opcode handlers
- `init_ pointer` - Initialization functions - `*_ref` - Reference operations - `*_scan` - Scanning operations

== API Documentation Standards

Functions are documented with:

- Function signature
- Parameters
- Return value
- Side effects - Related functions
