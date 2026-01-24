= Zig VM Core Implementation

*Navigation*: Spec | C Implementation | Lisp Implementation

This document describes the Zig implementation of the VM Core subsystem, consolidated from `implementations/zig-implementation.typ`.

== Overview

The Zig VM Core implementation provides a complete bytecode interpreter with dispatch loop, stack management, and opcode execution.

== Key Components

=== Dispatch Loop

- Located in `zaiko/src/vm/dispatch/execution.zig`
- Implements the main instruction execution loop
- Handles opcode dispatch and PC advancement

=== Stack Management

- Stack operations in `zaiko/src/vm/stack/`
- Frame management for function calls
- Stack pointer synchronization with C emulator

=== Opcode Implementation

- Opcodes in `zaiko/src/vm/opcodes/`
- Essential opcodes implemented for Medley startup
- Parity debugging against C reference

== Critical Findings

=== PC Cache Initialization

Fixed critical bug where `pccache` was uninitialized, causing undefined behavior in instruction fetching.

=== IFPAGE Struct Layout

Discovered that IFPAGE struct uses BYTESWAP field ordering for little-endian machines.

=== FPtoVP Byte-Swapping

Found incomplete byte-swapping in FPtoVP table loading.

== Status

- ✅ Sysout loading complete
- ✅ Basic execution working
- ✅ Essential opcodes implemented
- ✅ GC operations complete
- ✅ SDL2 display integration complete

See `implementations/zig-implementation.typ` for detailed implementation notes.