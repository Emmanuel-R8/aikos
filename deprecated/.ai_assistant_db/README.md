---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Maiko Source Code Documentation

**Navigation**: [Index](INDEX.md) | [Architecture](architecture.md) | [Components](components/) | [API](api/) | [Glossary](glossary.md) | [Build System](build-system.md) | [Implementations](implementations/)

This directory contains comprehensive documentation of the Maiko virtual machine source code. Maiko is the implementation of the Medley Interlisp virtual machine for a byte-coded Lisp instruction set.

## Quick Start

1. **New to Maiko?** → Start with [Architecture Overview](architecture.md)
2. **Understanding a Component?** → See [Component Documentation](components/)
3. **Looking for a Term?** → Check [Glossary](glossary.md)
4. **Building the Project?** → See [Build System](build-system.md)
5. **Need Quick Reference?** → Use [Index](INDEX.md)

## Documentation Structure

### [Architecture Overview](architecture.md)

High-level system architecture, component relationships, and design principles. Includes Mermaid diagrams showing system structure, data flow, and memory layout.

**Key Topics**:

- [System Architecture](architecture.md#high-level-architecture) - Overall system design
- [Core Components](architecture.md#core-components) - Major subsystems
- [Data Flow](architecture.md#data-flow) - Execution and memory flow
- [Platform Abstraction](architecture.md#platform-abstraction) - Cross-platform support

### [Component Documentation](components/)

Detailed documentation organized by functional area:

- **[VM Core](components/vm-core.md)** - Bytecode interpreter, dispatch loop, stack management
  - [Dispatch Loop](components/vm-core.md#dispatch-loop-structure) - Instruction execution
  - [Stack Management](components/vm-core.md#stack-management) - Frame handling
  - [Function Calls](components/vm-core.md#function-call-mechanism) - Call/return mechanism

- **[Memory Management](components/memory-management.md)** - Garbage collection, storage, virtual memory
  - [GC Algorithm](components/memory-management.md#garbage-collection-algorithm) - Reference counting GC
  - [Memory Layout](components/memory-management.md#memory-layout) - Address spaces
  - [Storage Allocation](components/memory-management.md#storage-allocation) - Heap management

- **[Display Subsystems](components/display.md)** - X11 and SDL integration
  - [X11 Implementation](components/display.md#x11-implementation) - X Window System
  - [SDL Implementation](components/display.md#sdl-implementation) - SDL backend
  - [BitBLT Operations](components/display.md#bitblt-operations) - Graphics rendering

- **[I/O Systems](components/io.md)** - Keyboard, mouse, file system, networking
  - [Keyboard System](components/io.md#keyboard-system) - Key event processing
  - [Mouse System](components/io.md#mouse-system) - Mouse events
  - [File System](components/io.md#file-system) - File operations
  - [Network Communication](components/io.md#network-communication) - Ethernet and Internet

### [API Reference](api/)

Function signatures, data structures, and interface documentation.

- [API Overview](api/overview.md) - Function categories and data structures
- [Core APIs](api/overview.md#core-apis) - VM, memory, display, I/O functions

### [Glossary](glossary.md)

Terminology, concepts, and abbreviations used throughout the codebase.

**Categories**:

- [Core Concepts](glossary.md#core-concepts) - LispPTR, DLword, ByteCode, etc.
- [Memory Terms](glossary.md#memory-terms) - Virtual memory, GC, cons cells
- [Execution Terms](glossary.md#execution-terms) - Dispatch loop, stack frames
- [Display Terms](glossary.md#display-terms) - BitBLT, display regions
- [I/O Terms](glossary.md#io-terms) - Keycodes, file descriptors

### [Build System](build-system.md)

Build configuration, platform support, and feature flags.

- [CMake Build](build-system.md#cmake-build-system) - Modern build system
- [Make Build](build-system.md#make-build-system) - Traditional build system
- [Platform Support](build-system.md#platform-specific-considerations) - OS and architecture support

### [Alternative Implementations](implementations/)

Documentation for alternative implementations of the Maiko emulator:

- **[Common Lisp Implementation](implementations/lisp-implementation.md)** - Complete SBCL implementation
  - Status: 77/78 tasks complete (98.7%)
  - 189 of 256 opcodes implemented
  - ASDF build system, SDL3 display backend
  - Located in `alternatives/lisp/`

## Quick Navigation

### By Component Type

- **[Core VM](components/vm-core.md)** - Execution engine and bytecode interpreter
- **[Memory Management](components/memory-management.md)** - GC and virtual memory
- **[Display](components/display.md)** - Graphics output subsystems
- **[I/O](components/io.md)** - Input/output systems

### By Source File

- **Main Entry**: `maiko/src/main.c` → See [VM Core - Main Entry Point](components/vm-core.md#main-entry-point)
- **Garbage Collection**: `maiko/src/gc*.c` → See [Memory Management - GC Core](components/memory-management.md#garbage-collection-core)
- **Display Init**: `maiko/src/xinit.c`, `maiko/src/sdl.c` → See [Display - Initialization](components/display.md#initialization-sequence)
- **Keyboard**: `maiko/src/kbdif.c` → See [I/O - Keyboard System](components/io.md#keyboard-system)

## Project Context

Maiko is part of the Medley Interlisp system, which provides:

- A complete Lisp development environment
- Bytecode-based virtual machine execution
- Cross-platform support (macOS, Linux, FreeBSD, Solaris, Windows)
- Multiple architecture support (x86_64, ARM64, SPARC, etc.)

See [Architecture Overview](architecture.md) for more details on the system design.

## Documentation Conventions

- **File References**: `maiko/src/filename.c` refers to source files in the repository
- **Function Names**: `function_name()` refers to C functions (see [API Overview](api/overview.md))
- **Data Types**: `LispPTR`, `DLword` refer to VM-specific types (see [Glossary](glossary.md))
- **Constants**: `CONSTANT_NAME` refers to preprocessor definitions
- **Links**: All documentation files are cross-linked for easy navigation
- **Diagrams**: Mermaid diagrams illustrate system architecture and data flow

## Related Resources

- **Source Code**: `maiko/src/` directory in repository root
- **Header Files**: `maiko/inc/` directory - type and function definitions
- **Build Files**: `maiko/CMakeLists.txt`, `maiko/bin/makeright` - build configuration
- **Project README**: `/README.md` - project overview

## Last Updated

Documentation generated: 2025-12-04
Last updated with Mermaid diagrams and cross-linking: 2025-12-04
