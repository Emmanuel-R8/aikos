# Maiko Glossary

**Navigation**: [README](README.md) | [Index](INDEX.md) | [Architecture](architecture.md) | [Components](components/) | [API](api/) | [Build System](build-system.md)

Terminology and concepts used throughout the Maiko codebase.

**Related Documentation**:

- [VM Core Component](components/vm-core.md) - Execution-related terms
- [Memory Management Component](components/memory-management.md) - Memory-related terms
- [Display Component](components/display.md) - Display-related terms
- [I/O Systems Component](components/io.md) - I/O-related terms

## Core Concepts

### LispPTR

Virtual address pointer in Lisp address space. A 32-bit (or larger with [BIGVM](#bigvm)) value that represents a location in the Lisp heap. Must be translated to native addresses using address translation functions (see [Address Translation](components/vm-core.md#address-translation)).

**See also**: [Memory Layout](components/memory-management.md#memory-layout), [Address Translation](components/memory-management.md#address-translation)

### DLword

Double-Length word. A 16-bit unsigned integer type used throughout the VM for addresses, offsets, and data.

**See also**: [Core Data Types](components/vm-core.md#key-data-structures)

### ByteCode

Single byte representing a Lisp bytecode instruction. The VM executes sequences of ByteCode values (see [Dispatch Loop](components/vm-core.md#dispatch-loop-structure)).

**See also**: [Opcode](#opcode), [Instruction Format](components/vm-core.md#instruction-format)

### Sysout

System output file. A saved Lisp image (`.virtualmem` file) containing the complete Lisp state including code, data, and execution state.

**See also**: [Startup Sequence](architecture.md#startup-sequence), [Storage Management](components/memory-management.md#storage-management)

### Dispatch Loop

The main execution loop that fetches bytecode instructions and calls the appropriate handler function. Implemented in `dispatch()` function (see [Dispatch Loop Structure](components/vm-core.md#dispatch-loop-structure)).

**See also**: [VM Core Component](components/vm-core.md), [Execution Model](components/vm-core.md#execution-model)

### Stack Frame (FX)

Frame eXtended. A stack frame containing function activation information including local variables, program counter, and activation link.

### Activation Link

Pointer to the previous stack frame, forming a chain of function activations.

## Memory Terms

### Virtual Memory

The Lisp heap uses virtual addressing where Lisp addresses are mapped to physical memory through translation tables.

### FPtoVP

Frame Pointer to Virtual Pointer mapping table. Translates virtual addresses to native addresses.

### Page

A unit of virtual memory allocation. Pages are mapped to physical memory pages.

### Cons Cell

A pair cell containing CAR (first element) and CDR (rest of list). Fundamental Lisp data structure.

### CDR Coding

Compact representation of cons cells where CDR values are encoded in a 4-bit field to reduce memory usage.

### MDS

Memory Data Structure. The heap space where arrays and other data structures are allocated.

### Atom Space

Memory region containing the atom table and symbol storage.

### Property List Space

Memory region containing property lists associated with atoms.

## Execution Terms

### Program Counter (PC)

Pointer to the current bytecode instruction being executed.

### Top of Stack (TOS)

The value currently on top of the evaluation stack.

### IVar

IVar pointer. Points to the current function's local variables.

### PVar

PVar pointer. Points to the current function's parameter variables.

### Function Object (FuncObj)

Pointer to the function header of the currently executing function.

### Opcode

A bytecode instruction opcode. Values 0-255 map to specific operations.

### UFN

Undefined Function Name. A function call to a function that hasn't been defined yet. Handled specially by the dispatch loop.

### Hard Return

A return that requires copying the stack frame, used when the frame may be referenced after return.

## Garbage Collection Terms

### GC

Garbage Collection. The process of reclaiming unused memory.

### Reference Counting

GC algorithm that tracks the number of references to each object.

### Hash Table (HTmain, HTcoll)

Hash tables used to track object references for garbage collection.

### ADDREF

Operation to increment an object's reference count.

### DELREF

Operation to decrement an object's reference count.

### STKREF

Stack reference. Special marking for objects referenced from the stack.

### Reclamation

Process of freeing unreferenced objects back to the heap.

## Display Terms

### BitBLT

Bit-Block Transfer. Graphics operation that copies a rectangular region of pixels from source to destination.

### Display Region

Memory-mapped area representing the screen contents.

### DspInterface

Display Interface structure containing display subsystem state and window handles.

### X11

X Window System. Unix/Linux windowing system.

### SDL

Simple DirectMedia Layer. Cross-platform multimedia library.

## I/O Terms

### Keycode

Numeric code representing a key press or release.

### Keymap

Mapping table converting OS keycodes to Lisp keycodes.

### Mouse Event

Event representing mouse button press/release or movement.

### File Descriptor

OS-level handle for open files.

### Serial Port

RS-232 serial communication port.

### Ethernet

Network interface for packet-based communication.

## Build Terms

### RELEASE

Build configuration specifying the Medley release version (115, 200, 201, 210, 300, 350, 351).

### BIGVM

Build flag enabling larger virtual memory address space.

### BIGATOMS

Build flag enabling larger atom indices.

### OPDISP

Build flag enabling computed goto dispatch (faster but GCC-specific).

### Platform Detection

Automatic detection of OS and CPU architecture during build.

## Version Terms

### LVERSION

Lisp Version. Minimum Lisp version required to run with this emulator.

### MINBVERSION

Minimum Bytecode Version. Current emulator version, must be >= sysout's minimum version.

### Version Compatibility

Ensuring emulator and sysout versions are compatible.

## System Terms

### Interface Page (IFPAGE)

Communication area between VM and Lisp code containing system variables and state.

### I/O Page (IOPAGE)

I/O control page containing I/O state and buffers.

### Interrupt State

State structure tracking pending interrupts (keyboard, mouse, timer, etc.).

### URAID

Unix RAID. File system operations subsystem.

## Architecture Terms

### Native Address

Host system memory address (C pointer).

### Lisp Address

Virtual address in Lisp address space (LispPTR).

### Address Translation

Converting between Lisp addresses and native addresses.

### Alignment

Memory alignment requirements (2-byte, 4-byte alignment).

## Error Terms

### Error Exit

Flag indicating the VM should exit due to an error.

### Stack Overflow

Condition when stack space is exhausted.

### Storage Full

Condition when heap space is exhausted.

### VMEM Full

Condition when virtual memory is exhausted.
