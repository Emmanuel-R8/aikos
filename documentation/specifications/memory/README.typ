= Memory Management Specification

*Navigation*: README | Index | VM Core | Data Structures

Complete specification of memory management, including virtual memory, garbage collection, and memory layout.

== Overview

The Maiko VM uses a virtual memory model with reference-counting garbage collection. Memory is organized into pages that are mapped between Lisp virtual addresses and native memory addresses.

== Documentation Structure

- *Virtual Memory* - Address spaces and page mapping
- *Address Translation* - LispPTR to native address conversion
- *Garbage Collection* - Reference counting GC algorithm
- *Memory Layout* - Memory regions and organization

== Key Concepts

=== Virtual Address Space

Lisp uses a virtual address space (LispPTR) that is independent of the underlying hardware:

- *LispPTR*: 32-bit virtual address
- *Page-based*: Memory organized into pages
- *Translation*: Converted to native addresses via FPtoVP mapping

=== Garbage Collection

Reference-counting based GC:

- *Reference Counting*: Tracks references to objects
- *Hash Tables*: HTmain and HTcoll for reference tracking
- *Reclamation*: Objects with zero references are reclaimed
- *Phases*: Scan, mark, reclaim phases

=== Memory Regions

- *Stack Space*: Function activation frames
- *Heap Space*: Cons cells, arrays, code
- *Atom Space*: Symbol table
- *Interface Page*: VM state and control structures

== Related Documentation

- VM Core - Uses memory for execution
- Data Structures - Memory layouts for data types
- Instruction Set - Memory operations
