# C Emulator Memory Loading Analysis

**Date**: 2025-01-27  
**Purpose**: Comprehensive analysis of C emulator memory loading logic

## Overview

This document provides a complete analysis of how the C emulator (Maiko) loads sysout files into virtual memory, including FPtoVP table parsing, byte-swapping logic, and address conversion.

## FPtoVP Table Structure (BIGVM)

### Table Format

- **Type**: `unsigned int *` (32-bit entries)
- **Entry Format**: 
  - Low 16 bits: Virtual page number (`GETFPTOVP`)
  - High 16 bits: Page OK flag (`GETPAGEOK`)
- **Location**: Calculated from `IFPAGE.fptovpstart`
  - Offset: `(fptovpstart - 1) * BYTESPER_PAGE + 4` bytes (BIGVM)

### Byte-Swapping

The FPtoVP table is byte-swapped using `ntohl()`:

```c
word_swap_page((unsigned short *)fptovp, (sysout_size / 2) + 1);
```

**How `ntohl()` works**:
- Converts from network byte order (big-endian) to host byte order (little-endian)
- Example: Raw bytes `00 00 18 3c` → After read: `0x3c180000` → After `ntohl`: `0x0000183c`
- `GETFPTOVP` extracts low 16 bits: `0x183c` = 6204 ✓

**Swap Boundary**: First `(sysout_size / 2) + 1` entries are swapped

## Page Loading Algorithm

```c
for (unsigned i = 0; i < (sysout_size / 2); i++) {
    if (GETPAGEOK(fptovp, i) != 0177777) {  // Skip sparse pages
        unsigned short vpage = GETFPTOVP(fptovp, i);  // Get virtual page
        lispworld_offset = vpage * BYTESPER_PAGE;      // Calculate byte offset
        // Read file page i from sysout
        // Byte-swap page content (32-bit longword swap)
        // Write to lispworld_scratch + lispworld_offset
    }
}
```

### Verified Mapping

- **File page 5178** → **Virtual page 6204** (confirmed, only mapping)
- PC location: Virtual page 6204, offset 0x98 (152 bytes)

## Page Content Byte-Swapping

Each page (512 bytes) is byte-swapped using 32-bit longword swap:

```c
word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128);
```

**Swap Logic**: `[b0,b1,b2,b3] → [b3,b2,b1,b0]` (per 32-bit longword)

### Verified Bytes

**File page 5178, PC offset 0x98**:
- Raw bytes: `00 0e 00 05 00 36 00 3f`
- After 32-bit swap: `05 00 0e 00 3f 00 36 00`
- Matches C emulator loading trace: ✓

## Address Conversion Logic

### Lisp_world Type

- **Declaration**: `extern DLword *Lisp_world;`
- **Assignment**: `Lisp_world = (DLword *)lispworld_scratch;`

### NativeAligned4FromLAddr

```c
static inline LispPTR *NativeAligned4FromLAddr(LispPTR LAddr)
{
    return (void *)(Lisp_world + LAddr);
}
```

**Critical Understanding**:
- `Lisp_world` is `DLword *`, so pointer arithmetic multiplies by `sizeof(DLword) = 2`
- `Lisp_world + LAddr` adds `(LAddr * 2)` bytes
- **LAddr is a DLword offset, not byte offset!**

**Example**:
- `FX_FNHEADER = 0x183c18` (DLword offset)
- `FuncObj = Lisp_world + 0x183c18`
- Byte offset = `0x183c18 * 2 = 0x307830` bytes

## Verification Results

All logic has been verified against:
- Execution log (`c_emulator_execution_log_1000.txt`)
- Sysout file (`medley/internal/loadups/starter.sysout`)
- C emulator loading trace output

### Verified Components

1. ✅ **FPtoVP Mapping**: File page 5178 → Virtual page 6204
2. ✅ **Byte-Swapping**: Both FPtoVP table and page content verified
3. ✅ **Address Conversion**: DLword offset → byte offset (multiply by 2)
4. ✅ **Page Loading**: Algorithm matches C emulator behavior

## Key Findings

1. **Single Mapping**: Only one file page (5178) maps to virtual page 6204 (no overwrites)
2. **Byte-Swapping**: `ntohl()` correctly converts big-endian to little-endian
3. **Address Arithmetic**: DLword offsets must be multiplied by 2 for byte offsets
4. **Consistency**: All calculations match execution log and loading trace

## References

- `maiko/src/ldsout.c` - Sysout loading implementation
- `maiko/inc/adr68k.h` - Address conversion macros
- `maiko/inc/lispemul.h` - FPtoVP macro definitions
