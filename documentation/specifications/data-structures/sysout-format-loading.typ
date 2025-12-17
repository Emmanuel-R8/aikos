== Page Loading Algorithm

=== Algorithm Overview

The page loading algorithm maps file pages to virtual memory pages using the FPtoVP table:

1. Iterate through file pages (0 to num_file_pages)
2. Check FPtoVP entry (skip if 0xFFFF = sparse page marker)
3. Seek to file page offset: `file_page BYTESPER_PAGE`
4. Read 512 bytes (BYTESPER_PAGE)
5. Write to virtual address: `virtual_page BYTESPER_PAGE`
   - C: `word_swap_page((DLword)(lispworld_scratch + lispworld_offset), 128);`
   - After byte-swapping, frame fields are in native little-endian format pointerVirtual Memory Initialization: - Virtual memory is zeroed after allocation to ensure sparse pages are initialized correctly
- Sparse pages (not loaded from sysout) remain zeros, matching C emulator behavior

=== Load Sysout File

[`function LoadSysoutFile(filename, process_size`]:
    // Open file
    file = OpenFile(filename)

    // Read IFPAGE
    ifpage = ReadIFPAGE(file, IFPAGE_ADDRESS)

    // CRITICAL: Byte-swap IFPAGE if host is little-endian
    // Sysout files are stored in big-endian format
    if NeedsByteSwap():
        SwapIFPAGE(ifpage)

    // Now validate (key check requires correct byte order)
    ValidateSysout(ifpage)

    // Allocate virtual memory
    virtual_memory = AllocateMemory(process_size)
    
    // CRITICAL: Zero virtual memory to ensure sparse pages are initialized correctly
    // C emulator initializes memory to zeros, so sparse/unmapped pages should be zero
    // This ensures that pages not loaded from sysout file contain zeros, not garbage
    ZeroMemory(virtual_memory, process_size)

    // Read FPtoVP table
    fptovp = ReadFPtoVP(file, ifpage.fptovpstart, ifpage.nactivepages)

    // Load pages
    for file_page = 0 to sysout_size:
        virtual_page = FPtoVP[file_page]
        if virtual_page != 0177777:
            LoadPage(file, file_page, virtual_page)

    // Initialize VM state from IFPAGE)

=== Page Loading with Byte Swapping pointerCRITICAL: Page data is stored in big-endian format in sysout files. When loading on little-endian machines, pages must be byte-swapped after loading to convert DLwords from big-endian to little-endian format.

[`function LoadPage(file, file_page_number, virtual_page_number`]:
    // Calculate file offset: file_page_number BYTESPER_PAGE
    file_offset = file_page_number × 512  // BYTESPER_PAGE = 512

    // Seek to file page
    Seek(file, file_offset)

    // Read page data (512 bytes)
    page_buffer = Read(file, 512)

    // Calculate virtual address: virtual_page_number BYTESPER_PAGE
    virtual_address = virtual_page_number × 512

    // Copy page data to virtual memory
    WriteMemory(virtual_address, page_buffer, 512)
    
    // C:
    // CRITICAL: word_swap_page() swaps 32-bit longwords using ntohl(), NOT 16-bit DLwords!
    // The parameter 128 is the number of 32-bit longwords (128 × 4 = 512 bytes = 1 page)
    page_longwords = GetU32Array(virtual_memory, virtual_address)
    num_longwords = 512 / 4  // 128 longwords per page
    for i = 0 to num_longwords:
        // Swap bytes in each 32-bit longword using ntohl equivalent
page_longwords[i] = ntohl(page_longwords[i)  // or equivalent byte-swap function pointerByte Swapping Details:
- *IFPAGE*: All DLword fields must be byte-swapped after reading
- *FPtoVP Table*: Each entry (u16 or u32) must be byte-swapped after reading
- *Memory Pages*: All 32-bit longwords in each page must be byte-swapped after loading (C: `word_swap_page`)
- *CRITICAL*: `word_swap_page()` swaps 32-bit longwords using `ntohl()`, NOT 16-bit DLwords
- Parameter `128` = number of 32-bit longwords (128 × 4 = 512 bytes = 1 page) - *Frame Structures*: After page byte-swapping, frame fields are in native little-endian format - *Function Headers*: After page byte-swapping, function header fields are in native little-endian format pointerC Reference: - `maiko/src/ldsout.c:707-708` - `word_swap_page((DLword)(lispworld_scratch + lispworld_offset), 128);` - `maiko/src/byteswap.c:31-34` - `word_swap_page()` implementation using `ntohl()` for 32-bit longwords

== Byte Swapping and Endianness

For detailed byte-swapping procedures, endianness best practices, and frame structure reading, see Sysout Byte Swapping and Endianness.

This document covers:
- Byte-endianness best practices
- Data vs address endianness handling
- Implementation checklist
- Common pitfalls
- Frame structure reading procedures - Detailed byte-swapping procedures for IFPAGE, FPtoVP table, and memory pages

== Memory Regions in Sysout

For details on memory regions, see Memory Layout.

*Summary*:
- *Stack Space*: Offset STK_OFFSET, contains stack frames and data
- *Atom Space*: Offset ATOMS_OFFSET, contains symbol table
- *Heap Space* (MDS): Offset MDS_OFFSET, contains cons cells, arrays, code
- *Interface Page*: Offset IFPAGE_ADDRESS (512 bytes), contains VM state (~100 fields)

== Byte Swapping

*CRITICAL*: Sysout files are stored in big-endian byte order. When loading on a little-endian machine, byte swapping is required for all multi-byte values.

For detailed byte-swapping procedures, see Sysout Byte Swapping and Endianness.

*Summary*:
- *File Format*: Big-endian
- *DLword fields*: Stored as `[high_byte, low_byte]`
- *LispPTR fields*: Stored as two big-endian DLwords `[h1, l1, h2, l2]`
- *IFPAGE structure*: All fields stored in big-endian format
- *Byte-swapping*: Required when loading on little-endian hosts (C: `#ifdef BYTESWAP`)
- *Memory Pages*: All pages MUST be byte-swapped after loading (C: `word_swap_page()`)

== Version Compatibility

=== Version Checking

[`function CheckVersionCompatibility(ifpage`]:
    // Check Lisp version
    // LVERSION = 21000 (from maiko/inc/version.h:54)
    if ifpage.lversion < LVERSION:
        Error("Sysout version %d < required %d", ifpage.lversion, LVERSION)

    // Check bytecode version
    // MINBVERSION = 21001* (from maiko/inc/version.h:55)
    if ifpage.minbversion > MINBVERSION:
        Error("Sysout bytecode version %d > supported %d", ifpage.minbversion, MINBVERSION)

    return true)

*CRITICAL*: Version constants are defined in `maiko/inc/version.h`:

- `LVERSION = 21000` - Minimum Lisp version required - `MINBVERSION = 21001` - Maximum bytecode version supported

Any implementation must use these exact values for version compatibility checking.

== File Size Validation

=== Size Checking

[`function ValidateFileSize(file, ifpage`]:
    file_size = GetFileSize(file)

    // BYTESPER_PAGE = 512 (from maiko/inc/lispemul.h:488)
    // Check page alignment
    if file_size mod BYTESPER_PAGE != 0:
        Warning("File size not page-aligned")

    // Check page count
    // sysout_size is calculated in half-pages:* (file_size / BYTESPER_PAGE) * 2
    sysout_size_halfpages = (file_size / BYTESPER_PAGE) × 2
    num_file_pages = sysout_size_halfpages / 2

    if num_file_pages != ifpage.nactivepages:
        Error("File size mismatch: %d vs %d pages", num_file_pages, ifpage.nactivepages))

== Version Constants pointerCRITICAL: Version constants from `maiko/inc/version.h`:

- `LVERSION = 21000` (minimum Lisp version required) - `MINBVERSION = 21001` (maximum bytecode version supported)

*Validation*: Sysout's `lversion` must be >= LVERSION, and `minbversion` must be <= MINBVERSION

== Saving Sysout

For details on saving sysout files, see Sysout Saving Procedures.

*Summary*: The save procedure writes IFPAGE, builds and writes the FPtoVP table, and writes all active memory pages to the file.

== Related Documentation

- Memory Layout - Memory regions
- Virtual Memory - Page mapping - Function Headers - Code in sysout
