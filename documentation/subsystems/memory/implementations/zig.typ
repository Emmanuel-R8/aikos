= Zig Memory Management Implementation

*Navigation*: Spec | C Implementation | Lisp Implementation

This document describes the Zig implementation of the Memory Management subsystem.

== Overview

The Zig memory implementation provides virtual memory management, garbage collection, and address translation.

== Key Components

=== Memory Manager

- Centralized memory management in `zaiko/src/memory/manager.zig`
- Address translation between LispPTR and byte offsets
- Endianness handling and byte swapping

=== Virtual Memory

- FPtoVP table loading and management
- Page-based memory allocation
- Sysout loading with proper byte order handling

=== Garbage Collection

- Reference counting GC implementation
- Hash table operations for GC
- Memory reclamation logic

== Critical Findings

=== FPtoVP Incomplete Byte-Swapping

Discovered that C emulator only swaps first half of FPtoVP entries.

=== Memory Layout

Proper handling of BIGVM and BIGATOMS configurations.

== Status

- ✅ Sysout loading complete
- ✅ Virtual memory working
- ✅ GC operations implemented

See `implementations/zig-implementation.typ` for details.