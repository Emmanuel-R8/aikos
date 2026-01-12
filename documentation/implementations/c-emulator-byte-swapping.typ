# C Emulator Byte-Swapping Logic

**Date**: 2025-01-27  
**Purpose**: Complete analysis of byte-swapping logic in the C emulator

## Overview

The C emulator performs byte-swapping at two levels:
1. **FPtoVP Table**: Converts big-endian entries to little-endian
2. **Page Content**: Swaps 32-bit longwords within each page

## FPtoVP Table Byte-Swapping

### Implementation

```c
#ifdef BYTESWAP
word_swap_page((unsigned short *)fptovp, (sysout_size / 2) + 1);
#endif
```

### word_swap_page Function

```c
void word_swap_page(void *page, unsigned longwordcount) {
    unsigned int *longpage = (unsigned int *)page;
    for (unsigned int i = 0; i < longwordcount; i++) { 
        *(longpage + i) = ntohl(*(longpage + i)); 
    }
}
```

### How ntohl() Works

`ntohl()` (network-to-host long) converts from network byte order (big-endian) to host byte order (little-endian):

- **On big-endian hosts**: No-op (returns value as-is)
- **On little-endian hosts** (x86_64): Swaps byte order

**Example**:
```
Raw bytes from file: 00 00 18 3c
After read() into memory: 0x3c180000 (little-endian interpretation)
After ntohl(): 0x0000183c (converts big-endian to little-endian)
GETFPTOVP (low 16 bits): 0x183c = 6204 ✓
```

### Swap Boundary

- **Boundary**: First `(sysout_size / 2) + 1` entries are swapped
- **File page 5178**: `5178 < 16636` → **SWAPPED** ✓

## Page Content Byte-Swapping

### Implementation

```c
#ifdef BYTESWAP
word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128);
#endif
```

### Swap Logic

Each page is 512 bytes = 128 DLwords = 64 longwords (32-bit)

**32-bit longword swap**: `[b0, b1, b2, b3] → [b3, b2, b1, b0]`

### Verified Example

**File page 5178, PC offset 0x98**:
- Raw bytes: `00 0e 00 05 00 36 00 3f`
- After 32-bit swap:
  - `[00, 0e, 00, 05] → [05, 00, 0e, 00]`
  - `[00, 36, 00, 3f] → [3f, 00, 36, 00]`
- Result: `05 00 0e 00 3f 00 36 00`
- Matches C emulator loading trace: ✓

## Byte-Swapping Summary

### FPtoVP Table

| Component | Value |
|-----------|-------|
| Function | `ntohl()` |
| Input | Big-endian 32-bit values |
| Output | Little-endian 32-bit values |
| Boundary | First `(sysout_size / 2) + 1` entries |
| Purpose | Convert table entries to host byte order |

### Page Content

| Component | Value |
|-----------|-------|
| Function | `word_swap_page()` with `ntohl()` |
| Unit | 32-bit longwords |
| Per Page | 64 longwords (512 bytes) |
| Swap | `[b0,b1,b2,b3] → [b3,b2,b1,b0]` |
| Purpose | Convert page content to host byte order |

## Verification Results

All byte-swapping logic has been verified:

1. ✅ **FPtoVP ntohl()**: Correctly converts big-endian to little-endian
2. ✅ **Page content swap**: 32-bit longword swap verified
3. ✅ **Swap boundaries**: Correctly applied to first half of FPtoVP table
4. ✅ **Consistency**: Matches C emulator loading trace output

## Key Findings

1. **Two-Level Swapping**: Both FPtoVP table and page content are swapped
2. **Different Functions**: FPtoVP uses `ntohl()`, page content uses `word_swap_page()` with `ntohl()`
3. **Partial Swapping**: Only first half of FPtoVP table is swapped
4. **Consistency**: All swapping matches C emulator behavior

## References

- `maiko/src/byteswap.c` - Byte-swapping implementation
- `maiko/src/ldsout.c` - Sysout loading with byte-swapping
- `maiko/inc/byteswapdefs.h` - Byte-swap function declarations
