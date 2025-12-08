# Zig Implementation Opcode Findings

**Navigation**: [Zig Implementation](zig-implementation.md) | [Implementations README](README.md)

**Date**: 2025-12-07  
**Status**: Critical Findings from Phase 1 Implementation

## Overview

During Phase 1 implementation of the Zig emulator, several opcode conflicts and discrepancies were discovered between the Zig implementation and the C reference implementation (`maiko/inc/opcodes.h`).

## Opcode Conflicts Discovered

### Generic Jump Opcodes (Removed)

**Issue**: Generic `JUMP`, `FJUMP`, `TJUMP` opcodes don't exist in C implementation.

**C Implementation**: Only specific variants exist:
- `JUMPX` (176), `JUMPXX` (177) - with 2-byte offset
- `JUMP0`-`JUMP15` (128-143) - optimized variants with offset encoded in opcode
- `FJUMPX` (178), `FJUMP0`-`FJUMP15` (144-159) - false jump variants
- `TJUMPX` (179), `TJUMP0`-`TJUMP15` (160-175) - true jump variants

**Zig Implementation**: Had generic `JUMP = 0x20`, `FJUMP = 0x30`, `TJUMP = 0x31` which conflicted with:
- `JUMP = 0x20` conflicted with `BIN = 0x20` (32)
- `FJUMP = 0x30` conflicted with `BUSBLT = 0x30` (48)
- `TJUMP = 0x30` conflicted with `BUSBLT = 0x30` (48)

**Resolution**: Removed generic jump opcodes. Use `JUMPX`, `JUMPXX`, or optimized variants (`JUMP0`-`JUMP15`, etc.).

**Location**: `maiko/alternatives/zig/src/vm/dispatch.zig:73-75` (removed)

### Character Opcodes (Removed)

**Issue**: `CHARCODE` and `CHARN` opcodes conflict with jump opcodes.

**Conflict**:
- `CHARCODE = 0xB4` conflicted with `NFJUMPX = 0xB4` (180)
- `CHARN = 0xB5` conflicted with `NTJUMPX = 0xB5` (181)

**C Implementation**: No `CHARCODE` or `CHARN` opcodes found in `maiko/inc/opcodes.h`.

**Resolution**: Commented out. Character operations may be handled via different mechanisms or may not be needed as separate opcodes.

**Location**: `maiko/alternatives/zig/src/vm/dispatch.zig:274-275` (commented out)

### Array Element Opcodes (Removed)

**Issue**: `GETAEL1`, `GETAEL2`, `SETAEL1`, `SETAEL2` opcodes conflict with optimized jump opcodes.

**Conflict**:
- `GETAEL1 = 0x80` conflicted with `JUMP0 = 0x80` (128)
- `GETAEL2 = 0x81` conflicted with `JUMP1 = 0x81` (129)
- `SETAEL1 = 0x82` conflicted with `JUMP2 = 0x82` (130)
- `SETAEL2 = 0x83` conflicted with `JUMP3 = 0x83` (131)

**C Implementation**: No `GETAEL1/2` or `SETAEL1/2` opcodes found. Array operations use:
- `AREF1` (182), `AREF2` (238)
- `ASET1` (183), `ASET2` (239)

**Resolution**: Commented out. Use `AREF1/2` and `ASET1/2` instead.

**Location**: `maiko/alternatives/zig/src/vm/dispatch.zig:246-253` (commented out)

### Type Checking Opcodes (Removed)

**Issue**: `FIXP`, `SMALLP`, `LISTP` opcodes conflict with optimized jump opcodes.

**Conflict**:
- `FIXP = 0xA0` conflicted with `TJUMP0 = 0xA0` (160)
- `SMALLP = 0xA1` conflicted with `TJUMP1 = 0xA1` (161)
- `LISTP = 0xA2` conflicted with `TJUMP2 = 0xA2` (162)

**C Implementation**: No separate `FIXP`, `SMALLP`, `LISTP` opcodes found. Type checking uses:
- `TYPEP` (5) - with type code operand

**Resolution**: Commented out. Use `TYPEP` opcode with appropriate type codes.

**Location**: `maiko/alternatives/zig/src/vm/dispatch.zig:264-270` (commented out)

### Stack Operation Opcodes (Removed)

**Issue**: `PUSH` opcode conflicts with `ADDBASE`.

**Conflict**:
- `PUSH = 0xD0` conflicted with `ADDBASE = 0xD0` (208)

**C Implementation**: No `PUSH` opcode found in `maiko/inc/opcodes.h`. Stack operations use:
- `POP` (191), `POP_N` (192)
- Various opcodes that implicitly push/pop

**Resolution**: Commented out. Stack operations are handled implicitly by other opcodes.

**Location**: `maiko/alternatives/zig/src/vm/dispatch.zig:317` (commented out)

### Comparison Opcodes (Fixed)

**Issue**: Comparison opcodes had incorrect values.

**Fixed**:
- `EQ = 0xF0` (240) - was incorrectly 0x90
- `EQL = 0x3A` (58) - correct
- `IGREATERP = 0xF1` (241) - was incorrectly 0x94
- `FGREATERP = 0xF2` (242) - was incorrectly 0x95
- `GREATERP = 0xF3` (243) - correct
- `EQUAL = 0xF4` (244) - correct

**C Reference**: `maiko/inc/opcodes.h:264-268`

**Resolution**: Corrected all comparison opcode values to match C implementation.

**Location**: `maiko/alternatives/zig/src/vm/dispatch.zig:255-261`

### Duplicate Opcodes (Fixed)

**Issue**: Some opcodes were defined multiple times.

**Fixed**:
- `UBFLOAT3` - duplicate removed (kept at 0x32)
- `GVAR_` - duplicate removed (kept at 0x17)
- `FGREATERP` - duplicate removed (kept at 0xF2)
- `TYPEP` - duplicate removed (kept at 0x05)

**Resolution**: Removed duplicates, kept correct values matching C implementation.

## Impact

These findings reveal that the initial Zig opcode enumeration included several opcodes that don't exist in the C implementation. This was likely due to:
1. Incomplete reference to C opcodes.h
2. Assumptions about opcode naming conventions
3. Missing verification against C implementation

## Recommendations

1. **Verify All Opcodes**: Cross-reference all opcodes in Zig implementation against `maiko/inc/opcodes.h`
2. **Use C Enum Values**: Always use exact values from C enum, not assumed values
3. **Test Opcode Decoding**: Verify opcode decoding matches C implementation behavior
4. **Document Opcode Gaps**: Document which opcodes are missing and why

## Related Documentation

- [C Opcodes Reference](../../maiko/inc/opcodes.h) - Source of truth for opcode values
- [Zig Implementation Status](zig-implementation.md) - Overall implementation status
- [Rewrite Specifications](../rewrite-spec/) - Language-agnostic specifications

