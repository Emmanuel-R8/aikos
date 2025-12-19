= Address Translation Specification

*Navigation*: README | Virtual Memory | Memory Layout

Complete specification of how LispPTR virtual addresses are translated to native memory addresses.

== Overview

Address translation converts Lisp virtual addresses (LispPTR) to native memory addresses that can be used by the host system. This translation is performed for all memory accesses.

== Address Format

=== LispPTR Structure

#codeblock(lang: "pseudocode", [
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
])

=== Address Extraction Macros

#codeblock(lang: "pseudocode", [
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
])

== Translation Algorithm

=== Basic Translation

#codeblock(lang: "pseudocode", [
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
])

=== Page Translation

#codeblock(lang: "pseudocode", [
function TranslatePage(virtual_page_base):
    // Calculate virtual page number
    virtual_page_num = virtual_page_base >> 8

    // Get native base address
    native_base = Lisp_world

    // Calculate native address
    native_page = native_base + virtual_page_base

    return native_page
])

== Translation Functions

=== NativeAligned2FromLAddr

Translates to 2-byte aligned native address:

#codeblock(lang: "pseudocode", [
function NativeAligned2FromLAddr(lisp_address):
    // CRITICAL: lisp_address is a DLword offset from Lisp_world, not byte offset!
    // Per maiko/inc/lspglob.h: "Pointers in Cell or any object means DLword offset from Lisp_world"
    // Lisp_world is DLword*, so pointer arithmetic: Lisp_world + lisp_address = lisp_address DLwords = lisp_address * 2 bytes
    native_ptr = Lisp_world + lisp_address
    return native_ptr  // Already 2-byte aligned
])

*C Reference*: `maiko/inc/adr68k.h:44-47` - `return (Lisp_world + LAddr);`

*INVESTIGATION NOTE (2025-12-12 16:45)*: There is an ongoing investigation into whether LispPTR values are actually stored as byte offsets despite the documentation stating they are DLword offsets. See "Address Translation Investigation" section below.

=== NativeAligned4FromLAddr

Translates to 4-byte aligned native address:

#codeblock(lang: "pseudocode", [
function NativeAligned4FromLAddr(lisp_address):
    // Check alignment
    if lisp_address & 1:
        Error("Misaligned pointer")

    // CRITICAL: lisp_address is a DLword offset from Lisp_world, not byte offset!
    // Per maiko/inc/lspglob.h: "Pointers in Cell or any object means DLword offset from Lisp_world"
    // Lisp_world is DLword*, so pointer arithmetic: Lisp_world + lisp_address = lisp_address DLwords = lisp_address * 2 bytes
    native_ptr = (void *)(Lisp_world + lisp_address)
    return native_ptr  // Already 4-byte aligned if input aligned
])

*C Reference*: `maiko/inc/adr68k.h:49-55` - `return (void *)(Lisp_world + LAddr);`

*INVESTIGATION NOTE (2025-12-12 16:45)*: There is an ongoing investigation into whether this function should actually use byte addressing: `(char *)Lisp_world + lisp_address`. See "Address Translation Investigation" section below.

=== NativeAligned4FromLPage

Translates page base to 4-byte aligned address:

#codeblock(lang: "pseudocode", [
function NativeAligned4FromLPage(lisp_page):
    // Page base is always aligned
    native_ptr = Lisp_world + (lisp_page << 8)
    return native_ptr
])

== Reverse Translation

=== LAddrFromNative

Converts native address back to LispPTR:

#codeblock(lang: "pseudocode", [
function LAddrFromNative(native_address):
    // Check alignment
    if native_address & 1:
        Error("Misaligned pointer")

    // Calculate offset from Lisp_world base
    lisp_address = native_address - Lisp_world

    return lisp_address
])

=== StackOffsetFromNative

Converts native stack address to stack offset:

#codeblock(lang: "pseudocode", [
function StackOffsetFromNative(stack_address):
    // Calculate offset from stack base
    offset = stack_address - Stackspace

    // Validate range
    if offset > 0xFFFF or offset < 0:
        Error("Stack offset out of range")

    return offset
])

== Address Construction

=== VAG2

Constructs LispPTR from segment and offset:

#codeblock(lang: "pseudocode", [
function VAG2(high_bits, low_bits):
    // Combine high and low bits
    lisp_address = (high_bits << 16) | low_bits
    return lisp_address
])

=== ADDBASE

Adds word offset to address:

#codeblock(lang: "pseudocode", [
function ADDBASE(pointer, word_offset):
    // Add offset (in words, converted to bytes)
    return pointer + (word_offset * 2)
])

== Alignment Requirements

=== 2-Byte Alignment

- Required for: DLword access
- Check: Address must be even
- Alignment: `address & 1 == 0`

=== 4-Byte Alignment

- Required for: LispPTR, structures
- Check: Address must be multiple of 4
- Alignment: `address & 3 == 0`

== Address Validation

=== Valid Address Check

#codeblock(lang: "pseudocode", [
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
])

=== Page Validation

#codeblock(lang: "pseudocode", [
function IsValidPage(virtual_page):
    // Check if page is allocated
    if not IsPageAllocated(virtual_page):
        return false

    // Check if page is accessible
    if IsPageLocked(virtual_page):
        return true  // Locked pages are valid

    return true
])

== Performance Considerations

=== Translation Caching

Some implementations may cache translations:

- Cache recent page translations
- Reduce translation overhead
- Invalidate on page operations

=== Direct Translation

For contiguous memory:

- Direct offset calculation
- No lookup required
- Fastest translation method

== Address Translation Investigation

*Date*: 2025-12-12 16:45
*Status*: Investigation in progress

=== Hypothesis

*All addresses should be bytes everywhere* (makes sense since instructions are single bytes). This contradicts the documentation which states LispPTR is a DLword offset.

=== Key Observation

From C emulator execution log:
- PC: `0x307898`
- FX_FNHEADER: `0x307864`
- Difference: `0x34` = 52 bytes = `104/2`

This suggests: *PC = FX_FNHEADER + (CURRENTFX->pc / 2)*

=== Pattern in C Code

*When converting FROM native pointer TO LispPTR* (dividing by 2):
- `maiko/src/xc.c:546`: `pc_dlword_offset = (LispPTR)(pc_byte_offset / 2);`
- `maiko/src/xc.c:704`: `stack_ptr_offset = (LispPTR)((char *)CurrentStackPTR - (char *)Stackspace) / 2;`
- `maiko/src/xc.c:743`: `currentfx_offset = (LispPTR)((char *)CURRENTFX - (char *)Stackspace) / 2;`

*Pattern*: When converting native pointers to LispPTR, they:
1. Cast to `char *` to get byte offset
2. Divide by 2 to convert to DLword offset

*When converting FROM LispPTR TO native pointer*:
- `maiko/inc/adr68k.h:46`: `return (Lisp_world + LAddr);` - Uses DLword arithmetic
- `maiko/inc/adr68k.h:58`: `return (void *)(Lisp_world + LAddr);` - Uses DLword arithmetic

*Pattern*: When converting LispPTR to native pointers, they use DLword pointer arithmetic (no cast to `char *`).

=== Hypothesis

*Maybe LispPTR values are actually stored as byte offsets, not DLword offsets*, despite the documentation. The `NativeAligned4FromLAddr` function might need to cast to `char *` first:

#codeblock(lang: "c", [
// Current (documented):
return (void *)(Lisp_world + LAddr);  // DLword arithmetic

// Should be (if byte addressing):
return (void *)((char *)Lisp_world + LAddr);  // Byte arithmetic
])

=== Testing Status

- C code modified to test byte addressing (debug statements added)
- Zig code modified to test byte addressing
- Awaiting C emulator execution with debug output to verify actual behavior

=== Next Steps

1. Run C emulator with modified byte addressing to see actual behavior
2. Verify if `NativeAligned4FromLAddr` should cast to `char *` first
3. Check if CURRENTFX->pc is actually stored as 52 (bytes) or 104 (DLwords)
4. Update this documentation based on findings

== Related Documentation

- Virtual Memory - Virtual memory system
- Memory Layout - Memory organization
- VM Core - Address usage in execution
