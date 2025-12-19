---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Data Structures Specification

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [Memory Management](../memory/) | [VM Core](../vm-core/)

Complete specification of all VM data structure formats, including cons cells, arrays, function headers, and sysout file format.

## Overview

This section documents the binary formats of all data structures used by the VM. These specifications are essential for:

- Reading/writing sysout files
- Implementing memory management
- Understanding data layout
- Maintaining compatibility

## Documentation Structure

- **[Atom Table](atom-table.md)** - Atom table structure, DefCell access, and atom cell layout
- **[Cons Cells](cons-cells.md)** - Cons cell format and CDR coding
- **[Arrays](arrays.md)** - Array formats and layouts
- **[Function Headers](function-headers.md)** - Function metadata format
- **[Sysout Format](sysout-format.md)** - Sysout file structure

## Data Structure Categories

### Heap Objects

Objects allocated in the heap (MDS space):

- Cons cells
- Arrays
- Code blocks
- User-defined structures

### System Structures

VM-internal structures:

- Function headers
- Stack frames
- GC hash table entries
- Interface page structures

### File Formats

Persistent formats:

- Sysout files
- Atom tables
- Property lists

## Key Concepts

### Type Tags

All objects have type tags:

- **Type Number**: Identifies object type
- **Type Table**: Maps types to DTDs
- **Type Checking**: Runtime type validation

### Alignment

Data structures have alignment requirements:

- **2-byte alignment**: DLword, most structures
- **4-byte alignment**: LispPTR, some structures
- **Page alignment**: Page-based structures

### Endianness

- **Byte order**: Little-endian
- **Word order**: 16-bit words in little-endian
- **Consistency**: Must match sysout file format

## Related Documentation

- [Memory Management](../memory/) - How structures are allocated
- [VM Core](../vm-core/) - How structures are used
- [Instruction Set](../instruction-set/) - Operations on structures
