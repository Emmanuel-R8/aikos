---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Number Type Encoding Specification

**Navigation**: [Data Structures README](../README.md) | [Instruction Set](../instruction-set/opcodes.md)

Complete specification of how numbers are encoded and represented in the VM.

## Overview

The VM supports multiple number representations:

- **SMALLP**: Small integers encoded directly in the address
- **FIXP**: Large integers stored as heap objects
- **FLOATP**: Floating-point numbers stored as heap objects

## SMALLP Encoding

Small integers are encoded directly in the LispPTR address using segment markers.

### Segment Constants

- **S_POSITIVE**: `0xE0000` - Segment marker for positive small integers
- **S_NEGATIVE**: `0xF0000` - Segment marker for negative small integers
- **SEGMASK**: `0xfff0000` (non-BIGVM) or `0xff0000` (BIGVM) - Mask to extract segment

### Value Ranges

- **MAX_SMALL**: `65535` (0xFFFF) - Maximum positive small integer
- **MIN_SMALL**: `-65536` (0xFFFF0000) - Minimum negative small integer

### Encoding Algorithm

**Positive Small Integers**:

```pseudocode
function EncodeSmallPositive(value):
    if value < 0 or value > MAX_SMALL:
        return error  // Value out of range
    return S_POSITIVE | value  // Segment marker OR value
```

**Negative Small Integers**:

```pseudocode
function EncodeSmallNegative(value):
    if value >= 0 or value < MIN_SMALL:
        return error  // Value out of range
    return S_NEGATIVE | (value & 0xFFFF)  // Segment marker OR low 16 bits
```

### Decoding Algorithm

**Extract Integer from SMALLP**:

```pseudocode
function ExtractSmallInteger(lisp_ptr):
    segment = lisp_ptr & SEGMASK

    if segment == S_POSITIVE:
        return lisp_ptr & 0xFFFF  // Extract low 16 bits
    else if segment == S_NEGATIVE:
        return sign_extend(lisp_ptr & 0xFFFF)  // Sign extend to 32-bit
    else:
        return error  // Not a SMALLP
```

## FIXP Encoding

Large integers that don't fit in SMALLP range are stored as heap objects.

### FIXP Object Structure

```pseudocode
struct FixpObject:
    type_tag: u8      // TYPE_FIXP = 2
    value: i32        // 32-bit signed integer value
```

### Value Ranges

- **MAX_FIXP**: `2147483647` (0x7FFFFFFF) - Maximum fixnum value
- **MIN_FIXP**: `-2147483648` (0x80000000) - Minimum fixnum value

### Encoding Algorithm

**Encode Integer Result** (N_ARITH_SWITCH equivalent):

```pseudocode
function EncodeIntegerResult(value):
    // Try SMALLP first
    if value >= 0 and value <= MAX_SMALL:
        return S_POSITIVE | value
    else if value < 0 and value >= MIN_SMALL:
        return S_NEGATIVE | (value & 0xFFFF)

    // Value doesn't fit in SMALLP, create FIXP object
    fixp_obj = AllocateFixpObject()
    fixp_obj.type_tag = TYPE_FIXP
    fixp_obj.value = value
    return LAddrFromNative(fixp_obj)
```

**Extract Integer from FIXP**:

```pseudocode
function ExtractFixpInteger(lisp_ptr):
    // Check type tag
    if GetTypeTag(lisp_ptr) != TYPE_FIXP:
        return error  // Not a FIXP

    fixp_obj = NativeAligned4FromLAddr(lisp_ptr)
    return fixp_obj.value
```

## Number Extraction (N_IGETNUMBER)

The `N_IGETNUMBER` macro extracts integers from either SMALLP or FIXP:

```pseudocode
function ExtractInteger(lisp_ptr):
    segment = lisp_ptr & SEGMASK

    // Check for SMALLP first
    if segment == S_POSITIVE:
        return lisp_ptr & 0xFFFF
    else if segment == S_NEGATIVE:
        return sign_extend(lisp_ptr & 0xFFFF)

    // Not SMALLP, check for FIXP
    if GetTypeTag(lisp_ptr) == TYPE_FIXP:
        fixp_obj = NativeAligned4FromLAddr(lisp_ptr)
        return fixp_obj.value

    // Could also be FLOATP (converted to int)
    if GetTypeTag(lisp_ptr) == TYPE_FLOATP:
        float_obj = NativeAligned4FromLAddr(lisp_ptr)
        float_value = float_obj.value
        if float_value > MAX_FIXP or float_value < MIN_FIXP:
            return error  // Float out of integer range
        return int(float_value)

    return error  // Not a valid integer type
```

## Arithmetic Overflow Handling

Arithmetic operations must check for overflow when encoding results:

```pseudocode
function CheckOverflowAdd(a, b, result):
    // Overflow occurs when signs match but result sign differs
    if ((a >= 0) == (b >= 0)) and ((result >= 0) != (a >= 0)):
        return true  // Overflow detected
    return false

function CheckOverflowSub(a, b, result):
    // Overflow occurs when signs differ but result sign matches first operand
    if ((a >= 0) != (b < 0)) and ((result >= 0) != (a >= 0)):
        return true  // Overflow detected
    return false

function CheckOverflowMul(a, b, result):
    // Overflow occurs when signs match but result sign differs
    if ((a >= 0) == (b >= 0)) and ((result >= 0) != (a >= 0)):
        return true  // Overflow detected
    return false
```

## Related Documentation

- [Arithmetic Opcodes](../instruction-set/opcodes.md#arithmetic-operations) - How numbers are used in opcodes
- [Memory Layout](../memory/layout.md) - How FIXP objects are stored in memory
- [GC Operations](../memory/gc.md) - How number objects are managed by GC
