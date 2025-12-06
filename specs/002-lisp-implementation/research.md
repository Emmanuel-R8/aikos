# Research: Maiko Emulator Implementation in Common Lisp

**Feature**: 002-lisp-implementation
**Date**: 2025-12-04
**Status**: Complete

## Research Questions & Decisions

### SDL3 Bindings for Common Lisp

**Question**: What SDL3 bindings are available for Common Lisp, and what is their API?

**Decision**: Use `cl-sdl3` library if available, otherwise use CFFI bindings to SDL3.

**Rationale**:
- `cl-sdl3` provides native Common Lisp bindings for SDL3
- CFFI (Common Foreign Function Interface) provides fallback for direct SDL3 C API access
- SDL3 is the target display backend (no X11 requirement)
- Both approaches maintain platform portability

**Alternatives Considered**:
- `cl-sdl2`: SDL2 bindings (outdated, SDL3 preferred)
- Direct CFFI: More work but full control
- Native X11: Out of scope per requirements

**Implementation Notes**:
- Check for `cl-sdl3` availability via Quicklisp/ASDF
- If unavailable, create CFFI bindings for required SDL3 functions
- Focus on window creation, event handling, and BitBLT operations

### Memory-Mapped File Handling in SBCL

**Question**: How to handle memory-mapped sysout files in SBCL?

**Decision**: Use SBCL's `sb-posix:mmap` or `mmap` via CFFI for sysout file memory mapping.

**Rationale**:
- SBCL provides POSIX system call access via `sb-posix` package
- Memory mapping is essential for efficient sysout file loading
- Allows direct access to sysout file contents without full file read
- Maintains compatibility with C implementation's memory-mapped approach

**Alternatives Considered**:
- Full file read: Simpler but less efficient for large sysout files
- Stream-based access: More complex, less direct memory access
- Native file I/O: Doesn't provide memory mapping benefits

**Implementation Notes**:
- Use `sb-posix:mmap` for POSIX-compliant systems (Linux, macOS)
- Handle byte order (endianness) correctly when reading sysout structures
- Map sysout file regions to virtual memory addresses per FPtoVP mapping

### Coordination Between Common Lisp GC and Maiko's Reference-Counting GC

**Question**: How to coordinate Common Lisp's automatic GC with Maiko's reference-counting GC?

**Decision**: Use explicit memory pinning for Maiko-managed objects and coordinate GC triggers.

**Rationale**:
- Common Lisp GC may move objects, breaking Maiko's pointer-based memory model
- Maiko's reference-counting GC requires stable memory addresses
- Need to prevent Common Lisp GC from collecting Maiko-managed objects prematurely
- Must coordinate GC triggers to avoid conflicts

**Alternatives Considered**:
- Disable Common Lisp GC: Not possible, GC is always active
- Full manual memory management: Defeats purpose of using Common Lisp
- Separate memory pool: Complex but provides isolation

**Implementation Notes**:
- Use `sb-sys:with-pinned-objects` or similar to pin Maiko-managed memory regions
- Implement Maiko GC as separate reference-counting system on top of Common Lisp memory
- Trigger Common Lisp GC only when Maiko GC indicates safe points
- Use weak pointers for cross-references between systems if needed

### Performance Characteristics of Common Lisp for VM Execution

**Question**: Will SBCL's native code compiler provide sufficient performance for VM execution?

**Decision**: SBCL's native code compiler provides sufficient performance; correctness prioritized over optimization.

**Rationale**:
- SBCL compiles to native machine code, providing good performance
- VM execution is primarily dispatch loop and opcode handlers (straightforward code)
- Requirements prioritize correctness over performance
- Can optimize hot paths later if needed

**Alternatives Considered**:
- Interpreted execution: Too slow for VM core
- JIT compilation: Unnecessary complexity for initial implementation
- C interop for hot paths: Premature optimization

**Implementation Notes**:
- Use SBCL's type declarations for performance hints
- Profile dispatch loop and opcode handlers after initial implementation
- Optimize only if performance becomes an issue
- Target: Comparable to C implementation, not necessarily faster

### CFFI vs Native Bindings for SDL

**Question**: Should we use CFFI bindings or native Common Lisp SDL library?

**Decision**: Prefer native `cl-sdl3` library; fallback to CFFI if unavailable.

**Rationale**:
- Native libraries provide better Common Lisp integration
- Less boilerplate code for common operations
- Better error handling and type safety
- CFFI provides fallback for missing functionality

**Alternatives Considered**:
- Pure CFFI: More control but more code
- Pure native: May lack some SDL3 features
- Hybrid: Best of both worlds

**Implementation Notes**:
- Check `cl-sdl3` availability in Quicklisp/ASDF
- Create CFFI bindings for any missing SDL3 functionality
- Abstract SDL interface behind display backend abstraction
- Test on both Linux and macOS platforms

## Additional Technical Decisions

### Data Structure Representation

**Decision**: Use Common Lisp structs (`defstruct`) for exact C compatibility.

**Rationale**:
- Structs provide explicit memory layout control
- Can match C structure layouts exactly
- Better performance than CLOS classes for VM core
- Supports `:conc-name` for field access patterns

**Implementation Notes**:
- Use `(defstruct (name (:conc-name prefix-)) ...)` for field naming
- Ensure struct alignment matches C implementation
- Use `(unsigned-byte 32)` for LispPTR, `(unsigned-byte 16)` for DLword

### Error Handling

**Decision**: Use Common Lisp condition system for error handling.

**Rationale**:
- Common Lisp conditions provide flexible error handling
- Can define custom error types matching Maiko error categories
- Supports error recovery and restarts
- Better than C's error codes

**Implementation Notes**:
- Define error conditions: `vm-error`, `memory-error`, `display-error`, `io-error`
- Use `error` and `signal` for error reporting
- Provide error recovery where possible (e.g., GC retry on allocation failure)

### Build System

**Decision**: Use ASDF (Another System Definition Facility) for build system.

**Rationale**:
- ASDF is standard for Common Lisp projects
- Included with SBCL
- Supports dependency management
- Provides system loading and compilation

**Implementation Notes**:
- Create `maiko-lisp.asd` system definition
- Define dependencies: `cl-sdl3` (optional), `cffi` (if needed)
- Organize source files by subsystem (vm/, memory/, display/, io/)
- Provide build scripts for convenience

## Summary

All research questions resolved. Technical approach determined:

- **SDL3**: Use `cl-sdl3` with CFFI fallback
- **Memory Mapping**: Use `sb-posix:mmap` for sysout files
- **GC Coordination**: Pin Maiko objects, coordinate GC triggers
- **Performance**: SBCL native code sufficient; correctness first
- **Data Structures**: Use `defstruct` for C compatibility
- **Error Handling**: Use Common Lisp condition system
- **Build System**: ASDF for project management

Ready for Phase 1: Design.
