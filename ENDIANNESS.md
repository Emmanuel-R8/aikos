# Byte Endianness Handling in Maiko C Implementation

**Date**: 2025-12-12
**Status**: COMPLETE - Comprehensive analysis of byte-endianness handling

## Executive Summary

The Maiko C implementation uses a **big-endian byte order** for sysout files (network byte order) and performs byte-swapping when running on little-endian machines. The key findings are:

1. **Sysout File Format**: Always stored in **big-endian byte order**
2. **Host Detection**: Automatically detects host byte order via `__BYTE_ORDER__` or `__BIG_ENDIAN__`/`__LITTLE_ENDIAN__`
3. **Byte Swapping**: Conditionally compiled using `BYTESWAP` macro
4. **Value Encoding**: For `0x01234567`, stored as `0x01 0x23 0x45 0x67` in sysout files

## Detailed Analysis

### 1. Byte Order Detection

The byte order is detected in [`maiko/inc/maiko/platform.h`](maiko/inc/maiko/platform.h:185-202):

```c
/* Modern GNU C, Clang, Sun Studio  provide __BYTE_ORDER__ */
#if defined(__BYTE_ORDER__)
#  if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#    define BYTESWAP 1
#  elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#    undef BYTESWAP
#  else
#    error "Unknown byte order"
#  endif
#elif __BIG_ENDIAN__ == 1
#    undef BYTESWAP
#elif __LITTLE_ENDIAN__ == 1
#    define BYTESWAP 1
#else
#  error "Could not detect byte order"
#endif
```

**Key Insight**: `BYTESWAP` is defined as `1` on little-endian machines, undefined on big-endian machines.

### 2. Data Type Definitions

The core data types are defined in [`maiko/inc/lispemul.h`](maiko/inc/lispemul.h:18-24):

```c
typedef unsigned short DLword;  /* 16-bit unsigned integer */
typedef unsigned int LispPTR;   /* 32-bit unsigned integer */
```

### 3. Byte Swapping Implementation

The byte swapping is implemented in [`maiko/src/byteswap.c`](maiko/src/byteswap.c:31-34):

```c
void word_swap_page(void *page, unsigned longwordcount) {
  unsigned int *longpage = (unsigned int *)page;
  for (unsigned int i = 0; i < longwordcount; i++) {
    *(longpage + i) = ntohl(*(longpage + i));
  }
}
```

**Key Insight**: Uses `ntohl()` (network-to-host long) which converts from big-endian to host byte order.

### 4. Memory Access Macros

The C code provides two sets of memory access macros based on `BYTESWAP`:

#### Non-BYTESWAP (Big-Endian Host)

[`maiko/inc/lispemul.h`](maiko/inc/lispemul.h:101-105):

```c
#define Get_DLword(ptr) ((Get_BYTE(ptr) << 8) | Get_BYTE((ptr) + 1))
#define Get_Pointer(ptr) ((Get_BYTE(ptr) << 16) | (Get_BYTE((ptr) + 1) << 8) | Get_BYTE((ptr) + 2))
```

#### BYTESWAP (Little-Endian Host)

[`maiko/inc/lispemul.h`](maiko/inc/lispemul.h:240-247):

```c
#define Get_DLword(ptr) ((Get_BYTE(ptr) << 8) | Get_BYTE((ptr) + 1))
#define Get_Pointer(ptr) ((Get_BYTE(ptr) << 24) | (Get_BYTE((ptr) + 1) << 16) | (Get_BYTE((ptr) + 2) << 8) | Get_BYTE((ptr) + 3))
```

### 5. Value Encoding in Sysout Files

For a 32-bit value `0x01234567`, the encoding in sysout files is:

**Big-Endian Format (Sysout File)**:

```
Byte 0: 0x01 (most significant byte)
Byte 1: 0x23
Byte 2: 0x45
Byte 3: 0x67 (least significant byte)
```

**Memory Layout**:

```
Address: [0x01][0x23][0x45][0x67]
```

### 6. Byte Swapping in Practice

The byte swapping occurs in several key locations:

#### IFPAGE Loading

[`maiko/src/ldsout.c`](maiko/src/ldsout.c:117-119):

```c
#ifdef BYTESWAP
word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4);
#endif
```

#### FPtoVP Table Loading

[`maiko/src/ldsout.c`](maiko/src/ldsout.c:336-337):

```c
#ifdef BYTESWAP
word_swap_page((unsigned short *)fptovp, (sysout_size / 4) + 1);
#endif
```

#### Page Data Loading

[`maiko/src/ldsout.c`](maiko/src/ldsout.c:677-678):

```c
#ifdef BYTESWAP
word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128);
#endif
```

### 7. Structure Definitions

The code maintains two sets of structure definitions:

#### Normal Byte Order (Big-Endian)

[`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h:189-287):

```c
struct dtd {
  DLword dtd_namelo;
  DLword dtd_size;
  LispPTR dtd_free;
  // ... other fields
};
```

#### Byte-Swapped (Little-Endian)

[`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h:465-484):

```c
struct dtd {
  DLword dtd_size;
  DLword dtd_namelo;
  LispPTR dtd_free;
  // ... fields in different order
};
```

### 8. Memory Access Patterns

The code uses specialized macros for memory access that handle byte order:

#### GETWORDBASEWORD Macro

[`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h:374):

```c
#define GETWORDBASEWORD(base, offset) (*(((DLword *)(base)) + (offset)))
```

#### BYTESWAP Version

[`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h:568):

```c
#define GETWORDBASEWORD(base, offset) (*(DLword *)(2 ^ (UNSIGNED)((base) + (offset))))
```

### 9. Practical Example

For a value `0x01234567`:

**Sysout File (Big-Endian)**:

```
Bytes: [0x01, 0x23, 0x45, 0x67]
```

**On Little-Endian Host (After Byte Swap)**:

```
Memory: [0x67, 0x45, 0x23, 0x01]  (native little-endian)
Value:  0x67452301
```

**On Big-Endian Host (No Swap)**:

```
Memory: [0x01, 0x23, 0x45, 0x67]  (native big-endian)
Value:  0x01234567
```

### 10. Key Files Involved

1. **Byte Order Detection**: [`maiko/inc/maiko/platform.h`](maiko/inc/maiko/platform.h:185-202)
2. **Byte Swapping Implementation**: [`maiko/src/byteswap.c`](maiko/src/byteswap.c)
3. **Type Definitions**: [`maiko/inc/lispemul.h`](maiko/inc/lispemul.h)
4. **Structure Definitions**: [`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h)
5. **Sysout Loading**: [`maiko/src/ldsout.c`](maiko/src/ldsout.c)

## Data vs Address Endianness Handling

### Fundamental Differences

The Maiko emulator handles **data** and **addresses** differently with respect to endianness:

1. **Data Values**: Subject to byte-swapping based on host architecture
2. **Addresses (LispPTR)**: Always treated as opaque 32-bit values, not byte-swapped

### Data Access Patterns

#### Non-BYTESWAP (Big-Endian Host)

[`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h:370-376):

```c
#define GETBYTE(base) (*(base))
#define GETBASEWORD(base, offset) (*((base) + (offset)))
#define GETWORDBASEWORD(base, offset) (*(((DLword *)(base)) + (offset)))
#define GETWORD(base) (*(base))
#define WORDPTR(base) (base)
```

#### BYTESWAP (Little-Endian Host)

[`maiko/inc/lsptypes.h`](maiko/inc/lsptypes.h:565-572):

```c
#define GETBYTE(base) (*(unsigned char *)(3 ^ (UNSIGNED)(base)))
#define GETBASEWORD(base, offset) GETWORDBASEWORD((base), (offset))
#define GETWORDBASEWORD(base, offset) (*(DLword *)(2 ^ (UNSIGNED)((base) + (offset))))
#define GETWORD(base) (*(DLword *)(2 ^ (UNSIGNED)(base)))
#define WORDPTR(base) ((DLword *)(2 ^ (UNSIGNED)(base)))
```

**Key Insight**: The `^` (XOR) operations with 2 and 3 perform pointer arithmetic adjustments to handle byte-swapped memory layouts.

### Address Translation Functions

Address translation functions treat LispPTR values as opaque 32-bit quantities:

#### LAddrFromNative

[`maiko/inc/adr68k.h`](maiko/inc/adr68k.h:36-42):

```c
static inline LispPTR LAddrFromNative(void *NAddr) {
  if ((uintptr_t)NAddr & 1) {
    printf("Misaligned pointer in LAddrFromNative %p\n", NAddr);
  }
  return (LispPTR)(((DLword *)NAddr) - Lisp_world);
}
```

#### NativeAligned4FromLAddr

[`maiko/inc/adr68k.h`](maiko/inc/adr68k.h:49-55):

```c
static inline LispPTR *NativeAligned4FromLAddr(LispPTR LAddr) {
  if (LAddr & 1) {
    printf("Misaligned pointer in NativeAligned4FromLAddr 0x%x\n", LAddr);
  }
  return (void *)(Lisp_world + LAddr);
}
```

**Key Insight**: Address translation functions perform **pointer arithmetic**, not byte manipulation. They treat LispPTR values as offsets into the virtual memory space.

### Practical Examples

#### Data Access Example

```c
// Reading a 16-bit data value from memory
DLword data_value = GETWORD(some_pointer);

// On little-endian host with BYTESWAP:
// GETWORD macro expands to: *(DLword *)(2 ^ (UNSIGNED)(some_pointer))
// This performs pointer adjustment for byte-swapped layout
```

#### Address Translation Example

```c
// Converting between native and Lisp addresses
void *native_ptr = NativeAligned4FromLAddr(lisp_address);
LispPTR lisp_addr = LAddrFromNative(native_ptr);

// No byte-swapping occurs - pure pointer arithmetic
```

### Memory Access Macros Comparison

| Macro             | Purpose                          | Non-BYTESWAP                       | BYTESWAP                                         |
| ----------------- | -------------------------------- | ---------------------------------- | ------------------------------------------------ |
| `GETBYTE`         | Read 8-bit value                 | `*(base)`                          | `*(unsigned char *)(3 ^ (UNSIGNED)(base))`       |
| `GETWORD`         | Read 16-bit value                | `*(base)`                          | `*(DLword *)(2 ^ (UNSIGNED)(base))`              |
| `GETBASEWORD`     | Read 16-bit from 32-bit boundary | `*((base) + (offset))`             | `GETWORDBASEWORD((base), (offset))`              |
| `GETWORDBASEWORD` | Read 16-bit from 16-bit boundary | `*(((DLword *)(base)) + (offset))` | `*(DLword *)(2 ^ (UNSIGNED)((base) + (offset)))` |

### Key Differences Summary

1. **Data Values**:

   - Stored in big-endian format in sysout files
   - Byte-swapped when loaded on little-endian hosts
   - Accessed via macros that handle byte order differences
   - Subject to `word_swap_page()` during loading

2. **Addresses (LispPTR)**:

   - Treated as opaque 32-bit offsets
   - No byte-swapping applied
   - Converted via pointer arithmetic
   - Used directly in address translation functions

3. **Memory Layout**:
   - Data: Byte order depends on host architecture
   - Addresses: Always consistent regardless of host

### Example: Value `0x01234567`

#### As Data (subject to byte-swapping):

- **Sysout File**: `0x01 0x23 0x45 0x67` (big-endian)
- **Little-Endian Host**: `0x67 0x45 0x23 0x01` (after byte-swap)
- **Big-Endian Host**: `0x01 0x23 0x45 0x67` (no swap)

#### As Address (no byte-swapping):

- **All Architectures**: `0x01234567` (treated as 32-bit offset)
- **Translation**: `Lisp_world + 0x01234567`
- **No byte order dependency**

### FPtoVP Table Handling

The FPtoVP (File Page to Virtual Page) table demonstrates the distinction:

```c
// FPtoVP entries are 16-bit values (DLword)
unsigned short vpage = GETFPTOVP(fptovp, file_page_index);

// On little-endian host:
#ifdef BYTESWAP
word_swap_page((unsigned short *)fptovp, (sysout_size / 4) + 1);
#endif
```

The table contains **data values** (virtual page numbers) that are byte-swapped, but the **address calculation** to access the table is not affected by endianness.

## Conclusion

The Maiko C implementation follows these principles:

1. **Sysout files are always stored in big-endian format** (network byte order)
2. **Byte swapping is performed automatically** when running on little-endian hosts
3. **The `BYTESWAP` macro controls** all byte-swapping behavior
4. **Memory access macros abstract** the byte order differences
5. **Structure definitions are duplicated** for both byte orders

For the value `0x01234567`, it is encoded as `0x01 0x23 0x45 0x67` in sysout files (big-endian), and byte-swapped to `0x67 0x45 0x23 0x01` when loaded on little-endian machines.

## Documentation Accuracy

The existing documentation in `.ai_assistant_db` is **mostly accurate** but contains some inconsistencies:

1. **Correct**: Sysout files are stored in big-endian format
2. **Correct**: Byte swapping is required on little-endian machines
3. **Inconsistent**: Some files mention "little-endian for multi-byte operands" which is incorrect - sysout files are always big-endian

**Recommendation**: Update documentation to clarify that sysout files are **always big-endian** regardless of host architecture, and byte swapping is performed as needed during loading.

## Documentation Changes Required

Based on the analysis of recent changes to the `.ai_assistant_db` documentation, the following updates are needed to ensure consistency with the C implementation's byte-endianness handling:

### 1. Stack Management Documentation Updates

**File**: `.ai_assistant_db/rewrite-spec/vm-core/stack-management.md`

**Required Changes**:

- **Lines 171-172**: Add explicit clarification that stack memory stores DLwords in BIG-ENDIAN format
- **Lines 208-209**: Add explicit clarification that stack memory stores DLwords in BIG-ENDIAN format
- **Lines 184-185**: Add detailed explanation of big-endian byte-swapping for stack operations
- **Lines 223-224**: Add detailed explanation of big-endian byte-swapping for stack operations

**Specific Additions Needed**:

```markdown
**CRITICAL**: Stack memory from sysout stores DLwords in BIG-ENDIAN format. When writing to stack memory, values must be stored in big-endian format to maintain compatibility with sysout format.

**CRITICAL**: Stack memory from sysout stores DLwords in BIG-ENDIAN format. When reading from stack memory, values must be byte-swapped from big-endian to native format.
```

### 2. Sysout Format Documentation Updates

**File**: `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md`

**Required Changes**:

- **Lines 141-142**: Add explicit statement that all fields are stored in big-endian format
- **Lines 371-372**: Add explicit statement that all DLwords in each page must be byte-swapped
- **Lines 401-402**: Add explicit statement that all multi-byte fields in frame structures are stored in big-endian format

**Specific Additions Needed**:

```markdown
**CRITICAL**: All fields are packed (no padding between fields) and stored in big-endian format in sysout files.

**CRITICAL**: Page data is stored in big-endian format in sysout files. When loading on little-endian machines, pages must be byte-swapped after loading to convert DLwords from big-endian to little-endian format.

**CRITICAL**: All multi-byte fields in frame structures are stored in big-endian format in sysout files. When reading on little-endian machines, byte swapping is required.
```

### 3. Zig Implementation Documentation Updates

**File**: `.ai_assistant_db/implementations/zig-implementation.md`

**Required Changes**:

- **Lines 45-46**: Add explicit statement that byte-swapping is stubbed and needs cross-platform testing
- **Lines 67-68**: Add explicit statement that pages are byte-swapped when loading from sysout file
- **Lines 149-156**: Add detailed explanation of stack byte-order handling

**Specific Additions Needed**:

```markdown
⚠️ **Byte swapping support** (stubbed, needs cross-platform testing)

✅ **Page byte-swapping** (2025-12-12): Pages are now byte-swapped when loading from sysout file (matching C `word_swap_page`)

**CRITICAL**: Stack memory from sysout stores DLwords in BIG-ENDIAN format. Stack operations must byte-swap when reading/writing on little-endian machines.
```

### 4. General Documentation Consistency

**Files**: All documentation files in `.ai_assistant_db/`

**Required Changes**:

- **Consistency Check**: Ensure all files consistently state that sysout files are stored in BIG-ENDIAN format
- **Clarification**: Remove any mentions of "little-endian for multi-byte operands" as this is incorrect
- **Standardization**: Use consistent terminology: "big-endian" (not "network byte order") throughout

### 5. New Section: Byte-Endianness Best Practices

**File**: `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md`

**Required Addition** (after line 520):

```markdown
## Byte-Endianness Best Practices

### General Rules

1. **Sysout Format**: Always store data in BIG-ENDIAN format
2. **Host Adaptation**: Byte-swap when loading on little-endian machines
3. **Memory Access**: Use specialized macros that handle byte order differences
4. **Address Handling**: Never byte-swap address values (LispPTR)

### Implementation Checklist

- [ ] Read data from sysout file (big-endian format)
- [ ] Check host byte order (little-endian vs big-endian)
- [ ] Byte-swap data if host is little-endian
- [ ] Use native byte order for runtime operations
- [ ] Never byte-swap address calculations

### Common Pitfalls

1. **Mixed Byte Order**: Don't mix big-endian and little-endian data in the same structure
2. **Address Confusion**: Don't byte-swap LispPTR values used for address calculations
3. **Inconsistent Swapping**: Apply byte-swapping consistently to all multi-byte fields
4. **Performance Impact**: Be aware that byte-swapping has performance overhead
```

## Related Documentation

- [Zig Implementation Notes](.ai_assistant_db/implementations/zig-implementation.md) - Contains Zig-specific byte-swapping details
- [Sysout Format Specification](.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md) - General sysout format documentation
- [Memory Layout Specification](.ai_assistant_db/rewrite-spec/memory/memory-layout.md) - Memory organization details

## Key Constants

- `IFPAGE_KEYVAL`: `0x15e3` (must match C implementation)
- `BYTESPER_PAGE`: `512` bytes (256 DLwords)
- `DLWORDSPER_CELL`: `2` (16-bit words per 32-bit cell)

## Build Configuration

The byte order handling is controlled by the build system:

- Automatically detects host byte order
- Defines `BYTESWAP` macro appropriately
- Compiles correct versions of memory access functions

This ensures that the emulator works correctly on both big-endian and little-endian architectures while maintaining compatibility with the big-endian sysout file format.

## Conclusion: Data vs Address Handling

The Maiko emulator maintains a clear separation between **data values** and **address values**:

1. **Data Values**: Are subject to byte-order conversion based on the host architecture
2. **Address Values**: Are treated as opaque 32-bit quantities and are never byte-swapped
3. **Memory Access**: Uses specialized macros that abstract byte-order differences for data
4. **Address Translation**: Uses pointer arithmetic that works consistently across architectures

This dual approach allows the emulator to:

- Maintain compatibility with big-endian sysout files
- Run efficiently on both big-endian and little-endian hosts
- Preserve the integrity of address calculations
- Handle data values according to host conventions

The distinction is crucial for understanding how the emulator maintains consistency while adapting to different host architectures.
