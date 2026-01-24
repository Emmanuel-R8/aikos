= Common Lisp Memory Management Implementation

*Navigation*: Spec | C Implementation | Zig Implementation

This document describes the Common Lisp implementation of the Memory Management subsystem.

== Overview

The Common Lisp memory implementation uses Lisp's garbage collection with Maiko's reference counting on top.

== Key Components

=== Storage Allocation

- Heap-based allocation with free list management
- Uses `sb-sys:with-pinned-objects` to prevent CL GC from moving objects

=== GC Coordination

- Maiko's reference-counting GC as separate system
- Coordination with Common Lisp's garbage collector

=== Virtual Memory

- FPtoVP mapping for address translation
- Endianness-aware sysout loading

== Key Implementation Decisions

- *Storage Allocation*: Heap-based with free list
- *GC Coordination*: Pinned objects to prevent movement
- *Reference Counting*: Separate from CL GC
- *Virtual Memory*: FPtoVP mapping implemented
- *Sysout Loading*: Endianness-aware with version validation

== Status

- ⏳ Implementation in progress
- Basic memory layout established

See `implementations/lisp-implementation.typ` for details.