= API Reference Overview


This directory contains API documentation for Maiko functions, data structures, and interfaces.


- VM Core Component - Detailed VM core functions
- Memory Management Component - Memory and GC functions
- Display Component - Display functions
- I/O Systems Component - I/O functions

== Organization

=== Core APIs
- *VM Core*: Dispatch loop, stack management, instruction execution
- See Key Functions for function listings*
- *Memory Management*: Allocation, GC, virtual memory
  - See Key Functions for function listings - *Display*: Graphics output, window management
- See Key Files for function listings
- *I/O*: Keyboard, mouse, file system, network
  - See Key Files for function listings

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
- `init_ pointer` - Initialization functions - `*_68k` - Functions operating on 68k addresses
- `Native pointer` - Address translation functions
- `Get_ pointer` - Getter macros/functions
- `Set_ pointer` - Setter macros/functions

== Macros

=== Address Translation

- `NativeAligned2FromLAddr()` - Convert to 16-bit aligned native  - `NativeAligned4FromLAddr()` - Convert to 32-bit aligned native  - `LAddrFromNative()` - Convert native address to Lisp address

=== Memory Access

- `Get_DLword()` - Get 16-bit word
- `Get_Pointer()` - Get value
- `GETWORD()` - Get word macro

=== Stack Operations

- `PushStack()` - Push value on stack
- `PopStack()` - Pop value from stack
- `FastRetCALL` - Fast return macro

== See Also

- Component Documentation - Detailed component documentation
  - VM Core - Execution functions
  - Memory Management - Memory functions
  - Display - Display functions
  - I/O Systems - I/O functions
- Glossary - Terminology reference
  - Core Concepts - Data types and basic concepts
  - Execution Terms - Execution-related terms
- Architecture Overview - System architecture context
