---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Zig Implementation Critical Findings

**Navigation**: [Zig Implementation Status](zig-implementation.md) | [Implementations README](README.md) | [Main README](../README.md)

**Date**: 2025-12-12 15:59  
**Status**: Active - Critical implementation findings and solutions

## Overview

This document provides an index to detailed critical findings, implementation challenges, and solutions discovered during the Zig implementation of the Maiko emulator.

For the main implementation status, see [Zig Implementation Status](zig-implementation.md).

## Findings Documents

- [Sysout Loading Findings](zig-implementation-findings-sysout.md) - IFPAGE, FPtoVP table, page loading, version constants, opcode conflicts
- [VM Execution Findings](zig-implementation-findings-vm.md) - TopOfStack, stack byte-order, PC initialization, stack operations, frame structure
- [Opcode Implementation Findings](zig-implementation-findings-opcodes.md) - All opcode implementation details

## Quick Reference

### Sysout Loading
- IFPAGE_KEYVAL correction (0x15e3)
- IFPAGE structure (~100 fields)
- FPtoVP table loading (BIGVM format)
- Page loading algorithm with byte-swapping
- Version constants (LVERSION, MINBVERSION)
- Opcode conflicts discovered

For detailed sysout loading findings, see [Sysout Loading Findings](zig-implementation-findings-sysout.md).

### VM Execution
- TopOfStack cached value implementation
- Stack byte-order handling
- PC initialization using FastRetCALL
- Stack operations: LispPTR storage format
- Frame structure field layout fix
- Frame reading with byte-swapping

For detailed VM execution findings, see [VM Execution Findings](zig-implementation-findings-vm.md).

### Opcode Implementation
- Arithmetic opcodes: SMALLP/FIXP handling
- Array operations implementation
- Variable access with DLword offsets
- Frame information opcodes
- Atom table access implementation
- Type checking implementation
- Base operations implementation
- Function lookup implementation
- Binding operations implementation
- Comparison operations implementation
- GC operations integration
- FIXP handling in base operations
- GC integration in GVAR_
- List operations implementation
- RPLPTR_N implementation
- FIXP box operations implementation
- Type predicates implementation
- Compilation issues fixed

For detailed opcode implementation findings, see [Opcode Implementation Findings](zig-implementation-findings-opcodes.md).

## Related Documentation

- [Zig Implementation Status](zig-implementation.md) - Main implementation status
- [Rewrite Specifications](../rewrite-spec/) - Complete specifications
- [C Implementation Reference](../../maiko/src/) - Reference implementation
