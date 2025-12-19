---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Zig Implementation Findings - Sysout Loading

**Navigation**: [Zig Implementation Findings](zig-implementation-findings.md) | [Zig Implementation Status](zig-implementation.md) | [Implementations README](README.md)

Sysout loading related findings and implementations.

## Zig-Specific Implementation Notes

### Zig Implementation Details

**IFPAGE Structure**:
- **Zig Location**: `maiko/alternatives/zig/src/utils/types.zig:24-95`
- **C Reference**: `maiko/inc/ifpage.h` (non-BIGVM, non-BYTESWAP version used as base)
- **Status**: ✅ Complete with ~100 fields matching C implementation exactly

**FPtoVP Table Loading**:
- **Zig Implementation**: `maiko/alternatives/zig/src/data/sysout.zig:loadFPtoVPTable`
- **C Reference**: `maiko/src/ldsout.c:197-250`
- **Status**: ✅ Implemented with BIGVM format support (32-bit entries)

**Page Loading**:
- **Zig Implementation**: `maiko/alternatives/zig/src/data/sysout.zig:loadMemoryPages`
- **C Reference**: `maiko/src/ldsout.c:250-350`
- **Status**: ✅ Implemented with sparse page handling and byte-swapping

**Version Constants**:
- **Zig Implementation**: `maiko/alternatives/zig/src/data/sysout.zig:18-19`
- **Constants**: `LVERSION = 21000`, `MINBVERSION = 21001`

### Zig-Specific Issues Fixed

**IFPAGE_KEYVAL Correction**:
- **Issue**: Initially used `0x12345678` instead of correct `0x15e3`
- **Fix**: Updated in `maiko/alternatives/zig/src/data/sysout.zig:14` and `maiko/alternatives/zig/src/utils/types.zig:95`
- **Impact**: This was a critical blocker preventing sysout validation from working

**Opcode Conflicts Discovered**:
- Several opcodes in Zig implementation don't exist in C implementation
- **Resolution**: Commented out in dispatch switch statements
- **Details**: See `zig-opcode-findings.md` for Zig-specific conflicts

## Related Documentation

- [Zig Implementation Findings](zig-implementation-findings.md) - Complete findings index
- [VM Execution Findings](zig-implementation-findings-vm.md) - VM execution findings
- [Opcode Implementation Findings](zig-implementation-findings-opcodes.md) - Opcode implementation findings
