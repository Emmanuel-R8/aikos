---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Memory Management Specification

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [VM Core](../vm-core/) | [Data Structures](../data-structures/)

Complete specification of memory management, including virtual memory, garbage collection, and memory layout.

## Overview

The Maiko VM uses a virtual memory model with reference-counting garbage collection. Memory is organized into pages that are mapped between Lisp virtual addresses and native memory addresses.

## Documentation Structure

- **[Virtual Memory](virtual-memory.md)** - Address spaces and page mapping
- **[Address Translation](address-translation.md)** - LispPTR to native address conversion
- **[Garbage Collection](garbage-collection.md)** - Reference counting GC algorithm
- **[Memory Layout](memory-layout.md)** - Memory regions and organization

## Key Concepts

### Virtual Address Space

Lisp uses a virtual address space (LispPTR) that is independent of the underlying hardware:

- **LispPTR**: 32-bit virtual address
- **Page-based**: Memory organized into pages
- **Translation**: Converted to native addresses via FPtoVP mapping

### Garbage Collection

Reference-counting based GC:

- **Reference Counting**: Tracks references to objects
- **Hash Tables**: HTmain and HTcoll for reference tracking
- **Reclamation**: Objects with zero references are reclaimed
- **Phases**: Scan, mark, reclaim phases

### Memory Regions

- **Stack Space**: Function activation frames
- **Heap Space**: Cons cells, arrays, code
- **Atom Space**: Symbol table
- **Interface Page**: VM state and control structures

## Related Documentation

- [VM Core](../vm-core/) - Uses memory for execution
- [Data Structures](../data-structures/) - Memory layouts for data types
- [Instruction Set](../instruction-set/) - Memory operations
