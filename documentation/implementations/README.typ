= Alternative Implementations

*Navigation*: Main README | Index

This directory contains documentation for alternative implementations of the Maiko emulator.

== Implementations

=== [Common Lisp Implementation](lisp-implementation.md)

Complete implementation of the Maiko emulator in Common Lisp (SBCL).

- *Status*: ✅ Complete (77/78 tasks, 98.7%)
- *Location*: `alternatives/lisp/`
- *Build System*: ASDF
- *Display Backend*: SDL3
- *Opcodes*: 189 of 256 implemented (73.8%)
- *Source Files*: 24 Lisp files
- *Test Files*: 11 test files

*Key Features*:

- Complete VM core with dispatch loop
- Memory management (storage, GC, virtual memory)
- Display subsystem (SDL3 backend)
- I/O subsystem (keyboard, mouse, filesystem)
- Sysout file loading with endianness handling
- Comprehensive error handling
- Platform-specific support (endianness, pathnames)

=== [Zig Implementation](zig-implementation.md)

Implementation of the Maiko emulator in Zig programming language.

- *Status*: 🔄 In Progress - Completion Phase
- *Location*: `alternatives/zig/`
- *Build System*: Zig build system (`build.zig`)
- *Display Backend*: SDL2 (linked, integration pending)
- *Opcodes*: ~50 of 256 implemented (essential set pending)
- *Source Files*: Multiple Zig modules
- *Test Files*: Test suite structure

*Key Features*:

- Complete framework structure
- VM core framework (dispatch loop structure)
- Memory management framework (GC, storage, virtual memory)
- Display subsystem framework (SDL2 backend)
- I/O subsystem framework (keyboard, mouse, filesystem)
- Comprehensive opcode enumeration

*Critical Blockers*:

- Sysout loading fails (wrong IFPAGE_KEYVAL: 0x12345678 vs 0x15e3)
- Incomplete IFPAGE structure
- FPtoVP table loading not implemented
- Page loading algorithm not implemented
- VM dispatch loop not activated
- Essential opcodes need implementation

== Implementation Status

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Implementation | Language    | Status             | Opcodes | Tasks         | Location                   |
// | -------------- | ----------- | ------------------ | ------- | ------------- | -------------------------- |
// | Common Lisp    | SBCL        | ✅ Complete         | 189/256 | 77/78         | `laiko/` |
// | Zig            | Zig 0.15.2+ | 🔄 Phase 1 Complete | ~50/256 | 22/22 Phase 1 | `zaiko/`  |
// 

== Related Documentation

- Rewrite Specifications - Language-agnostic specifications
- Component Documentation - System architecture
- API Reference - Function signatures
