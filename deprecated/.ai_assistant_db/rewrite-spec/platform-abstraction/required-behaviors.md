---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Required Behaviors Specification

**Navigation**: [README](README.md) | [Implementation Choices](implementation-choices.md)

Complete specification of behaviors that MUST match exactly for compatibility. These are non-negotiable requirements.

## Overview

Required behaviors are aspects of the VM that must match exactly to maintain compatibility with existing sysout files and Lisp programs. Deviations from these behaviors will break compatibility.

## VM Core Required Behaviors

### Bytecode Execution

**MUST MATCH**:

- Opcode execution semantics (all 256 opcodes)
- Stack effects of each opcode
- Error conditions and error handling
- Program counter advancement rules

**Rationale**: Bytecode execution must be identical for programs to run correctly.

### Stack Frame Layout

**MUST MATCH**:

- Frame structure (FX format)
- Field offsets and sizes
- Activation link structure
- Name table layout

**Rationale**: Stack frames must be compatible for function calls to work.

### Address Translation

**MUST MATCH**:

- LispPTR format (32-bit virtual address)
- Address component extraction (segment, page, offset)
- Translation algorithm (FPtoVP mapping)
- Alignment requirements

**Rationale**: Memory addresses must translate correctly for memory access.

## Memory Management Required Behaviors

### Garbage Collection Algorithm

**MUST MATCH**:

- Reference counting algorithm
- Hash table structure (HTmain, HTcoll)
- Reference count overflow handling
- Reclamation phases

**Rationale**: GC must behave identically for memory management correctness.

### Memory Layout

**MUST MATCH**:

- Memory region offsets (STK_OFFSET, MDS_OFFSET, etc.)
- Page size (256 bytes)
- Data structure formats (cons cells, arrays, etc.)
- Sysout file format

**Rationale**: Memory layout must match for sysout compatibility.

### Data Structure Formats

**MUST MATCH**:

- Cons cell format (8 bytes, CDR coding)
- Array header format
- Function header format
- All structure field offsets

**Rationale**: Data structures must match for correct data access.

## Display Required Behaviors

### Keycode Translation

**MUST MATCH**:

- OS keycode → Lisp keycode mapping
- Modifier key encoding
- Special key codes (function keys, arrows, etc.)

**Rationale**: Keyboard input must translate correctly for user interaction.

### Graphics Operations

**MUST MATCH**:

- BitBLT operation semantics (COPY, XOR, AND, OR, etc.)
- Coordinate system (top-left origin, Y increases downward)
- Display region memory layout
- Pixel format encoding

**Rationale**: Graphics must render identically for visual compatibility.

### Event Coordinate System

**MUST MATCH**:

- Origin at top-left (0, 0)
- X increases rightward
- Y increases downward
- Coordinate units (pixels)

**Rationale**: Event coordinates must match for input handling.

## I/O Required Behaviors

### Pathname Translation

**MUST MATCH**:

- Lisp pathname format parsing
- Platform pathname conversion rules
- Special character quoting rules
- Version number handling

**Rationale**: File operations must work with existing pathnames.

### File I/O Semantics

**MUST MATCH**:

- File open modes and recognition types
- Read/write byte order (if applicable)
- Error code mapping
- File attribute semantics

**Rationale**: File I/O must behave identically for data compatibility.

### Network Packet Format

**MUST MATCH**:

- Ethernet packet structure
- Checksum calculation algorithm
- TCP/IP protocol semantics
- Packet header formats

**Rationale**: Network communication must use compatible protocols.

## Compatibility Requirements

### Sysout File Compatibility

**MUST MATCH**:

- Sysout file format
- IFPAGE structure
- FPtoVP table format
- Page loading algorithm

**Rationale**: Must load and run existing sysout files.

### Bytecode Compatibility

**MUST MATCH**:

- All 256 opcode values
- Instruction encoding
- Operand formats
- Execution semantics

**Rationale**: Must execute existing bytecode correctly.

### Data Structure Compatibility

**MUST MATCH**:

- All data structure formats
- Field layouts
- Encoding schemes (CDR coding, etc.)
- Alignment requirements

**Rationale**: Must access data structures correctly.

## Validation

### Compatibility Testing

Required behaviors can be validated by:

- Loading existing sysout files
- Running test programs
- Comparing execution results
- Verifying data structure access

### Test Cases

Reference test cases should verify:

- Opcode execution correctness
- Memory access correctness
- File I/O correctness
- Display rendering correctness

## Related Documentation

- [Implementation Choices](implementation-choices.md) - What may differ
- [Validation](../validation/compatibility-criteria.md) - Compatibility testing
- [Contracts](../contracts/) - Interface contracts
