== FPtoVP Table

=== Table Structure

[`struct FPtoVP:`]
[`    // Array mapping file page number to virtual page number`]
[`    entries: array[file_page_count] of virtual_page_number`]
[`    // Special values:`]
[`    // 0177777 (0xFFFF`]: Page not present in file
    // Other values: Virtual page number)

=== Table Location pointerCRITICAL: Exact byte offset calculation for FPtoVP table:

- *Base Offset*: `(ifpage.fptovpstart - 1) * BYTESPER_PAGE`
- `fptovpstart` is a page number (1-based), so subtract 1 to get 0-based page number
- Multiply by `BYTESPER_PAGE` (512) to get byte offset - *Offset Adjustment*:
- Non-BIGVM: `offset_adjust = 2` bytes (skip first DLword)
- BIGVM: `offset_adjust = 4` bytes (skip first LispPTR)
- *Final Offset*: `(ifpage.fptovpstart - 1) * BYTESPER_PAGE + offset_adjust`
- *Size Calculation*:
  - `num_file_pages = (file_size / BYTESPER_PAGE)`
  - Table size: `num_file_pages entry_size` bytes
- Non-BIGVM: `entry_size = 2` (u16), total = `num_file_pages × 2` bytes
- BIGVM: `entry_size = 4` (u32), total = `num_file_pages × 4` bytes - *Format*: Depends on BIGVM
- Non-BIGVM: 16-bit entries (u16), stored as big-endian in sysout, must byte-swap when reading on little-endian machines - BIGVM: 32-bit entries (u32), stored as big-endian in sysout, must byte-swap when reading on little-endian machines

=== BIGVM Configuration (REQUIRED)

*CRITICAL*: All implementations (Zig and Lisp) *MUST* support BIGVM mode only. The non-BIGVM code path is *NOT* supported and can be ignored.

*BIGVM is a build-time configuration that enables support for larger address spaces* (up to 256MB). It affects FPtoVP table structure and memory addressing:

==== FPtoVP Table Storage (BIGVM Format - REQUIRED)

*All implementations MUST use BIGVM format*:
- *Type*: `unsigned int pointerfptovp` (32-bit cells) *←* REQUIRED
- *Entry Size*: 32 bits per entry
- *Structure*:
- Low 16 bits: Virtual page number (accessed via `GETFPTOVP`)
- High 16 bits: Page OK flag (accessed via `GETPAGEOK`) - *Table Size*: `sysout_size × 2` bytes (each entry is 4 bytes) - *Reading*: Read `sysout_size × 2` bytes from sysout file

~~*non-BIGVM format*~~ *←* NOT SUPPORTED, IGNORE

==== Macro Definitions (BIGVM Format - REQUIRED)

*All implementations MUST use these macros* (`maiko/inc/lispemul.h:587-589`):
[`#define GETFPTOVP(b, o`] ((b)[o)           // Returns low 16 bits (virtual page number)
[`#define GETPAGEOK(b, o) ((b)[o] >> 16)     // Returns high 16 bits (page OK flag)`]
])

*Implementation Notes*:
- `b` is `unsigned int pointer` (32-bit array)
- `o` is the file page index
- `GETFPTOVP` returns the full 32-bit value, implicitly cast to `unsigned short` (low 16 bits) - `GETPAGEOK` returns high 16 bits via right shift

~~*non-BIGVM macros*~~ *←* NOT SUPPORTED, IGNORE

==== Current Investigation Status (2025-12-11)

*Maiko C Emulator Build*:
- *Configuration*: *BIGVM IS DEFINED* (confirmed by compile-time diagnostics)
- *BYTESWAP*: Also defined
- *fptovp Type*: `unsigned int pointer` (32-bit cells, not 16-bit words)
- *Runtime Behavior*: Confirmed BIGVM behavior
  - `GETPAGEOK(fptovp, 9427) = 0x0000` (high 16 bits)
- `GETFPTOVP(fptovp, 9427) = 0x012e (302)` (low 16 bits) - Full 32-bit value: `0x0000012e` (302)

*Mystery Solved*:
- In BIGVM mode, each FPtoVP entry is a 32-bit `unsigned int`
- `GETFPTOVP(b, o) = b[o]` returns the full 32-bit value, cast to `unsigned short` (low 16 bits)
- `GETPAGEOK(b, o) = ((b)[o] >> 16)` returns high 16 bits
- For entry 9427: `fptovp[9427] = 0x0000012e`, so:
- `GETFPTOVP` = `(unsigned short)(0x0000012e)` = `0x012e` (302) ✓ - `GETPAGEOK` = `(0x0000012e >> 16)` = `0x0000` ✓

*Sysout File*:
- *File*: `medley/internal/loadups/starter.sysout`
- *Format*: BIGVM format (32-bit FPtoVP entries)
- *FPtoVP Table*: Contains entries that map to virtual page 302 (frame location)

*Key Code Locations*:
- Macro definitions: `maiko/inc/lispemul.h:587-593`
- GETWORDBASEWORD: `maiko/inc/lsptypes.h:377-378` (non-BYTESWAP) or `580` (BYTESWAP) - FPtoVP reading: `maiko/src/ldsout.c:287-306` (BIGVM path) *←* USE THIS PATH ONLY  - Diagnostic code: `maiko/src/ldsout.c:199-220` (early), `304-340` (runtime), `499-540` (entry 9427)

=== Table Usage

[`function LoadPage(file, file_page_number`]:
    // Check if page exists
    virtual_page = FPtoVP[file_page_number]
    if virtual_page == 0177777:
        return  // Page not in file

    // Seek to file page
    file_offset = file_page_number BYTESPER_PAGE
    Seek(file, file_offset)

    // Read page data
    page_data = Read(file, BYTESPER_PAGE)

    // Map to virtual address
    virtual_address = virtual_page BYTESPER_PAGE
    WriteMemory(virtual_address, page_data))

