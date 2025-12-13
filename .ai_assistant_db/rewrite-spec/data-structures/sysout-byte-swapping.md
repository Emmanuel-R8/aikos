# Sysout Byte Swapping and Endianness

**Navigation**: [Sysout Format](sysout-format.md) | [Data Structures README](README.md) | [Main README](../../README.md)

Complete specification of byte-endianness handling for sysout files, including byte-swapping procedures and best practices.

## Overview

Sysout files are stored in **big-endian byte order** (network byte order). When loading on little-endian machines, byte swapping is required for all multi-byte values. This document provides detailed procedures and best practices for handling byte-endianness in sysout file operations.

## Byte-Endianness Best Practices

### General Rules

1. **Sysout Format**: Always store data in **big-endian format**
2. **Host Adaptation**: Byte-swap when loading on little-endian machines
3. **Memory Access**: Use specialized macros that handle byte order differences
4. **Address Handling**: Never byte-swap address values (LispPTR) - they are opaque 32-bit offsets

### Data vs Address Endianness

**CRITICAL DISTINCTION**: The Maiko emulator handles **data values** and **address values** differently:

#### Data Values (Subject to Byte-Swapping)

- **Stored in**: Big-endian format in sysout files
- **Byte-swapped**: When loaded on little-endian hosts
- **Accessed via**: Specialized macros that handle byte order differences
- **Examples**: DLword values, LispPTR values used as data, frame fields, function header fields

#### Address Values (Never Byte-Swapped)

- **Treated as**: DLword offsets from Lisp_world base (NOT byte offsets!) - **BUT SEE EXCEPTION BELOW**
- **No byte-swapping**: The numeric value is used directly in pointer arithmetic
- **Converted via**: Pointer arithmetic (`Lisp_world + LispPTR`) where `Lisp_world` is `DLword*`
  - Since `Lisp_world` is `DLword*`, adding `LispPTR` adds `LispPTR` DLwords = `LispPTR * 2` bytes
  - Example: `FX_FNHEADER = 0x307864` → `FuncObj = Lisp_world + 0x307864` = `Lisp_world + (0x307864 * 2) bytes`
- **CRITICAL**: LispPTR values are DLword offsets, not byte offsets. Per `maiko/inc/lspglob.h:89`: "Pointers in Cell or any object means DLword offset from Lisp_world"
- **EXCEPTION - FX_FNHEADER in FastRetCALL**: Based on actual execution logs, FX_FNHEADER appears to be treated as a **byte offset** in FastRetCALL context:
  - Observed: `PC = 0x307898 = FX_FNHEADER (0x307864) + 0x34 (52 bytes = 104/2)`
  - This suggests: `FuncObj = FX_FNHEADER` (byte offset), `PC = FuncObj + (CURRENTFX->pc / 2)`
  - **This contradicts the general rule** and needs verification with C emulator debug output
- **Examples**: LispPTR values used for address translation (FX_FNHEADER, frame pointers, function header pointers)

### Implementation Checklist

When implementing sysout loading:

- [ ] Read data from sysout file (big-endian format)
- [ ] Check host byte order (little-endian vs big-endian)
- [ ] Byte-swap data if host is little-endian (using `word_swap_page()` or equivalent)
- [ ] Use native byte order for runtime operations
- [ ] Never byte-swap address calculations (LispPTR values used as offsets)

### Common Pitfalls

1. **Mixed Byte Order**: Don't mix big-endian and little-endian data in the same structure
2. **Address Confusion**: Don't byte-swap LispPTR values used for address calculations - they are offsets, not data
3. **Inconsistent Swapping**: Apply byte-swapping consistently to all multi-byte fields in a page
4. **Performance Impact**: Be aware that byte-swapping has performance overhead - do it once during loading, not during runtime
5. **Structure Field Order**: Don't assume structure field order matches memory layout - verify actual byte offsets

### Example: Value `0x01234567`

#### As Data (Subject to Byte-Swapping)

- **Sysout File**: `0x01 0x23 0x45 0x67` (big-endian)
- **Little-Endian Host (After Byte-Swap)**: `0x67 0x45 0x23 0x01` (native little-endian)
- **Big-Endian Host (No Swap)**: `0x01 0x23 0x45 0x67` (native big-endian)

#### As Address (No Byte-Swapping)

- **All Architectures**: `0x01234567` (treated as DLword offset, NOT byte offset)
- **Translation**: `Lisp_world + 0x01234567` where `Lisp_world` is `DLword*`
  - This adds `0x01234567` DLwords = `0x01234567 * 2` bytes = `0x02468ace` bytes
- **No byte order dependency**: The numeric value is used directly in pointer arithmetic
- **CRITICAL**: LispPTR is a DLword offset, so byte offset = LispPTR * 2

### Memory Access Macros

The C implementation provides different macros for data access based on `BYTESWAP`:

- **Non-BYTESWAP (Big-Endian Host)**: Direct memory access
- **BYTESWAP (Little-Endian Host)**: Pointer adjustments via XOR operations to handle byte-swapped layout

**C Reference**: `maiko/inc/lsptypes.h:370-376` (non-BYTESWAP), `maiko/inc/lsptypes.h:565-572` (BYTESWAP)

## Byte Swapping Procedures

### Byte Order in Sysout Files

- **File Format**: Big-endian
- **DLword fields**: Stored as `[high_byte, low_byte]`
- **LispPTR fields**: Stored as two big-endian DLwords `[h1, l1, h2, l2]`
- **IFPAGE structure**: All fields stored in big-endian format

### Byte Swap Detection

```pseudocode
function NeedsByteSwap():
    // Sysout files are always big-endian
    // Swap needed if host is little-endian
    return host_byte_order == LITTLE_ENDIAN
```

**Note**: The C implementation uses `#ifdef BYTESWAP` to conditionally compile byte swapping code. On little-endian machines (e.g., x86_64), `BYTESWAP` is defined and byte swapping is performed.

### Byte Swap Procedure for IFPAGE

The IFPAGE structure must be byte-swapped immediately after reading from the file, before validation:

```pseudocode
function LoadSysoutFile(filename):
    // Read IFPAGE from file
    ifpage = ReadIFPAGE(file, IFPAGE_ADDRESS)

    // CRITICAL: Byte-swap IFPAGE if needed
    if NeedsByteSwap():
        SwapIFPAGE(ifpage)

    // Now validate (key check will work correctly)
    ValidateSysout(ifpage)
    // ... rest of loading ...
```

### IFPAGE Byte Swapping

The C implementation uses `word_swap_page()` which swaps 32-bit words using `ntohl()`:

```pseudocode
function SwapIFPAGE(ifpage):
    // C: word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4)
    // This treats IFPAGE as array of u32 words and swaps each using ntohl()
    // ntohl() converts: [b0, b1, b2, b3] -> [b3, b2, b1, b0]

    num_u32_words = (3 + sizeof(IFPAGE)) / 4
    for i = 0 to num_u32_words:
        word = ReadU32(ifpage, i * 4)
        swapped_word = ntohl(word)  // Network to host long (32-bit)
        WriteU32(ifpage, i * 4, swapped_word)
```

**Alternative Approach**: Since IFPAGE contains only DLword (u16) and LispPTR (u32) fields, swapping u16 words also works correctly:

- DLword fields: Swap bytes `[b0, b1] -> [b1, b0]`
- LispPTR fields: Swapped twice (once per u16), resulting in correct little-endian u32

### FPtoVP Table Byte Swapping

The FPtoVP table entries also need byte swapping:

```pseudocode
function LoadFPtoVPTable(file, ifpage):
    // Read table entries
    entries = ReadFPtoVPEntries(file, ...)

    // Byte-swap entries if needed
    if NeedsByteSwap():
        for i = 0 to num_entries:
            if is_bigvm:
                entries[i] = ntohl(entries[i])  // 32-bit entry
            else:
                entries[i] = ntohs(entries[i])  // 16-bit entry
```

**C Reference**: `maiko/src/ldsout.c:117-119, 254-270` - Byte swapping for IFPAGE and FPtoVP table

### Memory Pages Byte Swapping

**CRITICAL**: All memory pages loaded from sysout files MUST be byte-swapped after loading when running on little-endian hosts. The C implementation uses `word_swap_page()` which swaps 32-bit longwords in the page, converting from big-endian (sysout format) to little-endian (native format on x86_64).

**CRITICAL Implementation Detail**: `word_swap_page()` swaps **32-bit longwords**, NOT 16-bit DLwords!
- Function signature: `void word_swap_page(void *page, unsigned longwordcount)`
- Uses `ntohl()` to swap each 32-bit value: `[b0, b1, b2, b3] -> [b3, b2, b1, b0]`
- Parameter `128` = number of 32-bit longwords (128 * 4 = 512 bytes = 1 page)
- **Common mistake**: Swapping 16-bit DLwords instead of 32-bit longwords will produce incorrect results

**C Reference**: 
- `maiko/src/ldsout.c:707-708` - `word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128);`
- `maiko/src/byteswap.c:31-34` - Implementation using `ntohl()` for 32-bit longwords

**Note**: This byte-swapping applies to ALL pages (both code and data pages). There is no distinction between code and data pages regarding byte-swapping - all pages are byte-swapped uniformly.

## Frame Structure Reading

When reading frame structures (FX) from sysout files, multi-byte fields must be byte-swapped:

```pseudocode
function ReadFrame(virtual_memory, frame_offset):
    // FX structure fields (all stored big-endian in sysout):
    // - nextblock: LispPTR (4 bytes, offset 0)
    // - link: LispPTR (4 bytes, offset 4)
    // - fnheader: LispPTR (4 bytes, offset 8)
    // - pcoffset: DLword (2 bytes, offset 12)

    // Read fnheader field (offset 8 bytes from frame start)
    // CRITICAL: Byte-swap LispPTR from big-endian to little-endian
    fnheader_be = ReadU32BigEndian(virtual_memory, frame_offset + 8)
    fnheader_addr = ByteSwapU32(fnheader_be)

    // Read pcoffset field (offset 12 bytes from frame start)
    // CRITICAL: Byte-swap DLword from big-endian to little-endian
    pcoffset_be = ReadU16BigEndian(virtual_memory, frame_offset + 12)
    pcoffset = ByteSwapU16(pcoffset_be)

    return Frame(fnheader_addr, pcoffset, ...)
```

**CRITICAL**: All multi-byte fields in frame structures are stored in big-endian format in sysout files. When reading on little-endian machines, byte swapping is required.

**C Reference**: `maiko/src/main.c:797-807` - Frame reading and PC initialization

## Related Documentation

- [Sysout Format](sysout-format.md) - Complete sysout file format specification
- [Stack Management](../../vm-core/stack-management.md) - Frame structure details
- [Memory Layout](../../memory/memory-layout.md) - Memory organization
