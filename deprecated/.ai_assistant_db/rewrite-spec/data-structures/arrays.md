---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Array Specification

**Navigation**: [README](README.md) | [Cons Cells](cons-cells.md) | [Function Headers](function-headers.md)

Complete specification of array formats, including array headers, element access, and array types.

## Overview

Arrays in Maiko are stored with headers describing their structure, type, and dimensions. Arrays support various element types and can be one-dimensional or multi-dimensional.

## Array Header Structure

### One-Dimensional Array (OneDArray)

```pseudocode
struct OneDArray:
    base: 24-28 bits      // Base address of array data
    nil1: 4-8 bits        // Reserved
    offset: DLword        // Offset into base
    typenumber: 8 bits    // Element type code
    extendablep: 1 bit    // Extendable flag
    fillpointerp: 1 bit   // Has fill pointer
    displacedp: 1 bit     // Displaced array flag
    ajustablep: 1 bit     // Adjustable flag
    stringp: 1 bit        // String array flag
    bitp: 1 bit           // Bit array flag
    indirectp: 1 bit      // Indirect array flag
    readonlyp: 1 bit      // Read-only flag
    totalsize: DLword/int32_t  // Total size
    fillpointer: DLword/int32_t // Fill pointer (if fillpointerp)
```

**Size**: Variable (depends on BIGVM)
**Alignment**: 4-byte aligned

### Multi-Dimensional Array (LispArray)

```pseudocode
struct LispArray:
    base: 24-28 bits      // Base address
    nil1: 4-8 bits        // Reserved
    Dim0: int32_t/DLword  // Dimension 0
    typenumber: 8 bits    // Element type
    extendablep: 1 bit
    fillpointerp: 1 bit
    displacedp: 1 bit
    ajustablep: 1 bit
    stringp: 1 bit
    bitp: 1 bit
    indirectp: 1 bit
    readonlyp: 1 bit
    Dim1: int32_t/DLword  // Dimension 1
    Dim2: int32_t/DLword  // Dimension 2
    totalsize: int32_t/DLword  // Total size
```

## Array Types

### Type Numbers

- **0**: Bit array (1 bit per element)
- **3**: Unsigned 8-bit
- **4**: Unsigned 16-bit
- **20**: Signed 16-bit
- **22**: Signed 32-bit
- **38**: Pointer (LispPTR)
- **54**: Float (32-bit)
- **67**: Character (8-bit)
- **68**: Character (16-bit)
- **86**: Xpointer

### Type-Specific Access

```pseudocode
function GetElementSize(typenumber):
    switch typenumber:
        case 0: return 1 bit
        case 3: return 1 byte
        case 4: return 2 bytes
        case 20: return 2 bytes
        case 22: return 4 bytes
        case 38: return 4 bytes
        case 54: return 4 bytes
        case 67: return 1 byte
        case 68: return 2 bytes
        case 86: return 4 bytes
```

## Array Access

### One-Dimensional Access (AREF1)

```pseudocode
function AREF1(array_address, index):
    array = GetArray(array_address)

    // Validate index
    if index >= array.totalsize:
        Error("Index out of range")

    // Calculate element offset
    element_offset = array.offset + index
    element_size = GetElementSize(array.typenumber)

    // Get base address
    base_address = array.base

    // Access element
    element_address = base_address + (element_offset * element_size)
    return ReadElement(element_address, array.typenumber)
```

### Two-Dimensional Access (AREF2)

```pseudocode
function AREF2(array_address, index0, index1):
    array = GetArray(array_address)

    // Validate indices
    if index0 >= array.Dim0 or index1 >= array.Dim1:
        Error("Index out of range")

    // Calculate linear index
    linear_index = (index0 * array.Dim1) + index1

    // Calculate element offset
    element_offset = array.offset + linear_index
    element_size = GetElementSize(array.typenumber)

    // Access element
    base_address = array.base
    element_address = base_address + (element_offset * element_size)
    return ReadElement(element_address, array.typenumber)
```

### Array Set (ASET1, ASET2)

```pseudocode
function ASET1(array_address, index, value):
    array = GetArray(array_address)

    // Check readonly
    if array.readonlyp:
        Error("Array is read-only")

    // Validate index
    if index >= array.totalsize:
        Error("Index out of range")

    // Calculate element address
    element_offset = array.offset + index
    element_size = GetElementSize(array.typenumber)
    base_address = array.base
    element_address = base_address + (element_offset * element_size)

    // Write element
    WriteElement(element_address, value, array.typenumber)
```

## Array Block Structure

### Array Block Header

```pseudocode
struct ArrayBlock:
    password: 13 bits     // Block password
    gctype: 2 bits        // GC type
    inuse: 1 bit          // In-use flag
    arlen: DLword         // Array length
    fwd: LispPTR          // Forward pointer (for GC)
    bkwd: LispPTR         // Backward pointer (for GC)
```

## Array Allocation

### Allocate Array

```pseudocode
function AllocateArray(dimensions, typenumber, flags):
    // Calculate total size
    totalsize = CalculateTotalSize(dimensions, typenumber)

    // Allocate array block
    array_block = AllocateArrayBlock(totalsize)

    // Initialize array header
    array_header = GetArrayHeader(array_block)
    array_header.base = GetDataBase(array_block)
    array_header.typenumber = typenumber
    array_header.totalsize = totalsize
    array_header.offset = 0

    // Set flags
    array_header.readonlyp = flags.readonly
    array_header.adjustablep = flags.adjustable
    array_header.fillpointerp = flags.fillpointer

    return LispAddressOf(array_block)
```

## String Arrays

Strings are special arrays:

```pseudocode
struct StringArray:
    // Same as OneDArray but with stringp flag set
    base: 24-28 bits
    type: 4 bits          // String type
    readonly: 1 bit
    substringed: 1 bit
    origin: 1 bit
    length: DLword/int32_t
    offset: LispPTR/DLword
```

## Displaced Arrays

Displaced arrays share data with another array:

```pseudocode
function CreateDisplacedArray(base_array, offset, length):
    array = AllocateArrayHeader()
    array.base = base_array.base
    array.offset = base_array.offset + offset
    array.totalsize = length
    array.displacedp = true
    return array
```

## Related Documentation

- [Memory Management](../memory/) - Array allocation
- [Garbage Collection](../memory/garbage-collection.md) - Array reclamation
- [Instruction Set](../instruction-set/) - Array opcodes
