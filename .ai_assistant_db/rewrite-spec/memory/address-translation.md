# Address Translation Specification

**Navigation**: [README](README.md) | [Virtual Memory](virtual-memory.md) | [Memory Layout](memory-layout.md)

Complete specification of how LispPTR virtual addresses are translated to native memory addresses.

## Overview

Address translation converts Lisp virtual addresses (LispPTR) to native memory addresses that can be used by the host system. This translation is performed for all memory accesses.

## Address Format

### LispPTR Structure

```pseudocode
struct LispPTR:
    // 32-bit virtual address
    bits: uint32

    // Address components (without BIGVM)
    segment: bits[24:31]    // High 8 bits
    page: bits[16:23]        // Middle 8 bits
    offset: bits[0:7]        // Low 8 bits

    // Address components (with BIGVM)
    segment: bits[20:31]    // High 12 bits
    page: bits[12:19]        // Middle 8 bits
    offset: bits[0:7]        // Low 8 bits
```

### Address Extraction Macros

```pseudocode
function HILOC(lisp_address):
    // Extract high bits (segment)
    return (lisp_address & SEGMASK) >> 16

function LOLOC(lisp_address):
    // Extract low bits (offset)
    return lisp_address & 0x0FFFF

function POINTER_PAGE(lisp_address):
    // Extract page number
    if BIGVM:
        return (lisp_address & 0x0FFFFF00) >> 8
    else:
        return (lisp_address & 0x0FFFF00) >> 8

function POINTER_PAGEBASE(lisp_address):
    // Extract page base address
    if BIGVM:
        return lisp_address & 0x0FFFFF00
    else:
        return lisp_address & 0x0FFFF00
```

## Translation Algorithm

### Basic Translation

```pseudocode
function TranslateAddress(lisp_address, alignment):
    // Get page base
    page_base = POINTER_PAGEBASE(lisp_address)

    // Translate page to native address
    native_page = TranslatePage(page_base)

    // Get offset within page
    offset = lisp_address & 0xFF

    // Combine native page and offset
    native_address = native_page + offset

    // Apply alignment
    if alignment == 2:
        native_address = AlignTo2Bytes(native_address)
    else if alignment == 4:
        native_address = AlignTo4Bytes(native_address)

    return native_address
```

### Page Translation

```pseudocode
function TranslatePage(virtual_page_base):
    // Calculate virtual page number
    virtual_page_num = virtual_page_base >> 8

    // Get native base address
    native_base = Lisp_world

    // Calculate native address
    native_page = native_base + virtual_page_base

    return native_page
```

## Translation Functions

### NativeAligned2FromLAddr

Translates to 2-byte aligned native address:

```pseudocode
function NativeAligned2FromLAddr(lisp_address):
    // Direct translation (Lisp_world is base)
    native_ptr = Lisp_world + lisp_address
    return native_ptr  // Already 2-byte aligned
```

### NativeAligned4FromLAddr

Translates to 4-byte aligned native address:

```pseudocode
function NativeAligned4FromLAddr(lisp_address):
    // Check alignment
    if lisp_address & 1:
        Error("Misaligned pointer")

    // Direct translation
    native_ptr = Lisp_world + lisp_address
    return native_ptr  // Already 4-byte aligned if input aligned
```

### NativeAligned4FromLPage

Translates page base to 4-byte aligned address:

```pseudocode
function NativeAligned4FromLPage(lisp_page):
    // Page base is always aligned
    native_ptr = Lisp_world + (lisp_page << 8)
    return native_ptr
```

## Reverse Translation

### LAddrFromNative

Converts native address back to LispPTR:

```pseudocode
function LAddrFromNative(native_address):
    // Check alignment
    if native_address & 1:
        Error("Misaligned pointer")

    // Calculate offset from Lisp_world base
    lisp_address = native_address - Lisp_world

    return lisp_address
```

### StackOffsetFromNative

Converts native stack address to stack offset:

```pseudocode
function StackOffsetFromNative(stack_address):
    // Calculate offset from stack base
    offset = stack_address - Stackspace

    // Validate range
    if offset > 0xFFFF or offset < 0:
        Error("Stack offset out of range")

    return offset
```

## Address Construction

### VAG2

Constructs LispPTR from segment and offset:

```pseudocode
function VAG2(high_bits, low_bits):
    // Combine high and low bits
    lisp_address = (high_bits << 16) | low_bits
    return lisp_address
```

### ADDBASE

Adds word offset to address:

```pseudocode
function ADDBASE(pointer, word_offset):
    // Add offset (in words, converted to bytes)
    return pointer + (word_offset * 2)
```

## Alignment Requirements

### 2-Byte Alignment

- Required for: DLword access
- Check: Address must be even
- Alignment: `address & 1 == 0`

### 4-Byte Alignment

- Required for: LispPTR, structures
- Check: Address must be multiple of 4
- Alignment: `address & 3 == 0`

## Address Validation

### Valid Address Check

```pseudocode
function IsValidAddress(lisp_address):
    // Check range
    if lisp_address < MIN_ADDRESS:
        return false
    if lisp_address > MAX_ADDRESS:
        return false

    // Check alignment (if required)
    if requires_alignment and (lisp_address & alignment_mask):
        return false

    return true
```

### Page Validation

```pseudocode
function IsValidPage(virtual_page):
    // Check if page is allocated
    if not IsPageAllocated(virtual_page):
        return false

    // Check if page is accessible
    if IsPageLocked(virtual_page):
        return true  // Locked pages are valid

    return true
```

## Performance Considerations

### Translation Caching

Some implementations may cache translations:

- Cache recent page translations
- Reduce translation overhead
- Invalidate on page operations

### Direct Translation

For contiguous memory:

- Direct offset calculation
- No lookup required
- Fastest translation method

## Related Documentation

- [Virtual Memory](virtual-memory.md) - Virtual memory system
- [Memory Layout](memory-layout.md) - Memory organization
- [VM Core](../vm-core/) - Address usage in execution
