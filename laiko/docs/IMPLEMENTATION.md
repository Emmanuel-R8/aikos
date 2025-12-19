# Implementation Notes: Maiko Emulator in Common Lisp

**Feature**: 002-lisp-implementation
**Date**: 2025-12-04
**Status**: In Progress

## Overview

This document contains implementation notes, decisions, and technical details for the Common Lisp implementation of the Maiko emulator.

## Architecture Decisions

### Build System

- **ASDF**: Used for system definition and dependency management
- **SBCL**: Primary Common Lisp implementation target
- **Optional Dependencies**: `uiop`, `alexandria`, `cl-sdl3` are optional to improve build flexibility

### Memory Management

- **Storage Allocation**: Simple heap-based allocation with free list (to be improved)
- **GC Coordination**: Uses `sb-sys:with-pinned-objects` to prevent Common Lisp GC from moving Maiko-managed objects
- **Reference Counting**: Maiko's reference-counting GC implemented as separate system on top of Common Lisp memory

### Display Backend

- **SDL3**: Target display backend (no X11 requirement)
- **Fallback**: When `cl-sdl3` is unavailable, display structure is created but SDL operations are stubbed
- **Graphics**: BitBLT operations implemented with COPY and XOR modes

### I/O Subsystem

- **Keyboard**: Event queue with OS keycode to Lisp keycode translation
- **Mouse**: Position tracking and event translation
- **Filesystem**: Pathname translation for platform compatibility (Unix/Windows)

## Implementation Status

### Completed Modules

- ✅ VM Core: Dispatch loop, stack management, opcode handlers (189 handlers implemented)
- ✅ Memory Management: Storage allocation, GC hash table, virtual memory, FPtoVP mapping
- ✅ Data Structures: Cons cells, arrays, function headers, sysout loading
- ✅ Display: SDL backend structure, graphics operations, event handling
- ✅ I/O: Keyboard, mouse, filesystem operations

### Partially Complete

- ⚠️ Opcodes: 189 of 256 opcodes implemented (remaining opcodes need handlers)
- ⚠️ SDL Integration: Structure in place, but actual SDL3 calls need cl-sdl3 library
- ⚠️ Edge Cases: Basic error handling in place, but some edge cases need refinement

### Known Limitations

1. **SDL3**: Requires `cl-sdl3` library for full functionality
2. **Bytecode Extraction**: Sysout bytecode extraction not yet implemented
3. **Some Opcodes**: Not all 256 opcodes have handlers (stub implementations for some)
4. **GC Coordination**: Basic coordination implemented, may need refinement

## Testing

Test files created for:
- Opcode execution (arithmetic, stack operations)
- Stack management
- Dispatch loop
- Memory allocation
- GC operations
- Sysout loading
- Keyboard/mouse events
- Display operations
- Filesystem operations

## Performance Considerations

- **Correctness First**: Implementation prioritizes correctness over performance
- **Type Declarations**: Used throughout for SBCL optimization hints
- **Profiling**: Performance profiling can be done after correctness is verified

## Platform Support

- **Linux**: Primary target platform
- **macOS**: Supported (same as Linux for most operations)
- **Windows**: Pathname translation implemented, but not fully tested

## Future Work

1. Complete all 256 opcode handlers
2. Integrate cl-sdl3 for full SDL3 support
3. Implement bytecode extraction from sysout files
4. Add comprehensive compatibility tests
5. Performance optimization if needed
