# Sysout File Format Specification

**Navigation**: [README](README.md) | [Cons Cells](cons-cells.md) | [Arrays](arrays.md) | [Function Headers](function-headers.md)

Complete specification of the sysout file format, including file structure, page organization, and loading procedures.

## Overview

Sysout files are persistent snapshots of the Lisp VM state. They contain all memory pages, VM state, and metadata needed to restore a complete Lisp environment.

## File Structure

### File Layout

```mermaid
graph TD
    File[Sysout File] --> IFPage[Interface Page<br/>@ IFPAGE_ADDRESS]
    File --> FPtoVP[FPtoVP Table<br/>@ fptovpstart]
    File --> Pages[Memory Pages<br/>Mapped by FPtoVP]

    IFPage -->|Contains| State[VM State]
    IFPage -->|Contains| Offsets[Region Offsets]
    IFPage -->|Contains| Metadata[Version Info]

    FPtoVP -->|Maps| FilePage[File Page N]
    FPtoVP -->|To| VPage[Virtual Page M]
```

### File Organization

- **Page-based**: File organized into 256-byte pages
- **Sparse**: Not all pages present (FPtoVP indicates which)
- **Mapped**: FPtoVP table maps file pages to virtual pages

## Interface Page (IFPAGE)

### IFPAGE Structure

Located at fixed address: `IFPAGE_ADDRESS`

```pseudocode
struct IFPAGE:
    key: uint             // Validation key (IFPAGE_KEYVAL)
    lversion: uint        // Lisp version
    minbversion: uint     // Minimum bytecode version
    process_size: uint    // Process size in MB
    nactivepages: uint    // Number of active pages
    fptovpstart: uint     // FPtoVP table start offset
    storagefullstate: uint // Storage state
    // ... VM state fields ...
    stackbase: LispPTR    // Stack base address
    endofstack: LispPTR   // End of stack
    currentfxp: LispPTR   // Current frame pointer
    // ... other state ...
```

### IFPAGE Validation

```pseudocode
function ValidateSysout(file):
    // Read IFPAGE
    ifpage = ReadIFPAGE(file)

    // Check key
    if ifpage.key != IFPAGE_KEYVAL:
        Error("Invalid sysout file")

    // Check version compatibility
    if ifpage.lversion < LVERSION:
        Error("Sysout version too old")

    if ifpage.minbversion > MINBVERSION:
        Error("Sysout version too new")

    return true
```

## FPtoVP Table

### Table Structure

```pseudocode
struct FPtoVP:
    // Array mapping file page number to virtual page number
    entries: array[file_page_count] of virtual_page_number

    // Special values:
    // 0177777 (0xFFFF): Page not present in file
    // Other values: Virtual page number
```

### Table Location

- **Offset**: `ifpage.fptovpstart`
- **Size**: `nactivepages` entries
- **Format**: Depends on BIGVM (32-bit vs 16-bit entries)

### Table Usage

```pseudocode
function LoadPage(file, file_page_number):
    // Check if page exists
    virtual_page = FPtoVP[file_page_number]
    if virtual_page == 0177777:
        return  // Page not in file

    // Seek to file page
    file_offset = file_page_number * BYTESPER_PAGE
    Seek(file, file_offset)

    // Read page data
    page_data = Read(file, BYTESPER_PAGE)

    // Map to virtual address
    virtual_address = virtual_page * BYTESPER_PAGE
    WriteMemory(virtual_address, page_data)
```

## Page Loading Algorithm

### Load Sysout File

```pseudocode
function LoadSysoutFile(filename, process_size):
    // Open file
    file = OpenFile(filename)

    // Read IFPAGE
    ifpage = ReadIFPAGE(file)
    ValidateSysout(ifpage)

    // Allocate virtual memory
    virtual_memory = AllocateMemory(process_size)

    // Read FPtoVP table
    fptovp = ReadFPtoVP(file, ifpage.fptovpstart, ifpage.nactivepages)

    // Load pages
    for file_page = 0 to sysout_size:
        virtual_page = FPtoVP[file_page]
        if virtual_page != 0177777:
            LoadPage(file, file_page, virtual_page)

    // Initialize VM state from IFPAGE
    InitializeVMState(ifpage)

    return virtual_memory
```

## Memory Regions in Sysout

### Stack Space

- **Offset**: STK_OFFSET
- **Contents**: Stack frames and data
- **Size**: Variable

### Atom Space

- **Offset**: ATOMS_OFFSET
- **Contents**: Symbol table
- **Size**: Variable

### Heap Space (MDS)

- **Offset**: MDS_OFFSET
- **Contents**: Cons cells, arrays, code
- **Size**: Variable

### Interface Page

- **Offset**: IFPAGE_OFFSET
- **Contents**: VM state
- **Size**: 1 page (256 bytes)

## Byte Swapping

### Byte Swap Detection

```pseudocode
function NeedsByteSwap(ifpage):
    // Check if bytes need swapping
    // Based on host vs file byte order
    return host_byte_order != file_byte_order
```

### Byte Swap Procedure

```pseudocode
function SwapPage(page_data):
    // Swap 16-bit words
    for i = 0 to page_size / 2:
        word = page_data[i * 2]
        page_data[i * 2] = SwapBytes(word)
```

## Version Compatibility

### Version Checking

```pseudocode
function CheckVersionCompatibility(ifpage):
    // Check Lisp version
    if ifpage.lversion < LVERSION:
        Error("Sysout version %d < required %d", ifpage.lversion, LVERSION)

    // Check bytecode version
    if ifpage.minbversion > MINBVERSION:
        Error("Sysout bytecode version %d > supported %d", ifpage.minbversion, MINBVERSION)

    return true
```

## File Size Validation

### Size Checking

```pseudocode
function ValidateFileSize(file, ifpage):
    file_size = GetFileSize(file)
    expected_pages = ifpage.nactivepages

    // Check page alignment
    if file_size mod BYTESPER_PAGE != 0:
        Warning("File size not page-aligned")

    // Check page count
    file_pages = file_size / BYTESPER_PAGE
    if file_pages != expected_pages:
        Error("File size mismatch: %d vs %d pages", file_pages, expected_pages)
```

## Saving Sysout

### Save Procedure

```pseudocode
function SaveSysoutFile(filename):
    // Create file
    file = CreateFile(filename)

    // Write IFPAGE
    WriteIFPAGE(file, InterfacePage)

    // Build FPtoVP table
    fptovp = BuildFPtoVPTable()

    // Write FPtoVP table
    WriteFPtoVP(file, fptovp)

    // Write memory pages
    for virtual_page in active_pages:
        file_page = GetFilePageForVirtualPage(virtual_page)
        page_data = ReadMemoryPage(virtual_page)
        WritePage(file, file_page, page_data)

    CloseFile(file)
```

## Related Documentation

- [Memory Layout](../memory/memory-layout.md) - Memory regions
- [Virtual Memory](../memory/virtual-memory.md) - Page mapping
- [Function Headers](function-headers.md) - Code in sysout
