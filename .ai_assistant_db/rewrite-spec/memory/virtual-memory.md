# Virtual Memory Specification

**Navigation**: [README](README.md) | [Address Translation](address-translation.md) | [Garbage Collection](garbage-collection.md) | [Memory Layout](memory-layout.md)

Complete specification of virtual memory system, including address spaces, page mapping, and virtual memory management.

## Overview

Maiko uses a virtual memory model where Lisp addresses (LispPTR) are mapped to native memory addresses through a page-based translation system. This allows the VM to manage memory independently of the underlying hardware.

## Address Spaces

### Lisp Virtual Address Space

Lisp uses a 32-bit virtual address space:

- **LispPTR**: 32-bit virtual address
- **Page-based**: Memory organized into 256-byte pages
- **Segments**: Address space divided into segments
- **Independence**: Virtual addresses independent of native addresses

### Address Format

```pseudocode
struct LispPTR:
    // 32-bit address
    segment: 12-16 bits    // High bits (segment number)
    page: 8-12 bits        // Page number within segment
    offset: 8 bits         // Byte offset within page
```

**Without BIGVM**:

- Segment: 8 bits (high byte)
- Page: 8 bits (middle byte)
- Offset: 8 bits (low byte)

**With BIGVM**:

- Segment: 12 bits (high 12 bits)
- Page: 8 bits (middle 8 bits)
- Offset: 8 bits (low 8 bits)

## Page Mapping

### FPtoVP Table

The FPtoVP (File Page to Virtual Page) table maps file pages to virtual pages:

```pseudocode
struct FPtoVP:
    // Array mapping file page number to virtual page number
    entries: array[file_page_count] of virtual_page_number
```

**Purpose**:

- Maps sysout file pages to virtual memory pages
- Enables loading sysout files
- Supports virtual memory save/restore

### Page Allocation

```pseudocode
function AllocatePage(base_address):
    // Calculate virtual page number
    virtual_page = base_address >> 8

    // Increment active page count
    active_pages = active_pages + 1

    // Check if FPtoVP table needs expansion
    if (active_pages mod FPTOVP_ENTRIES_PER_PAGE) == 0:
        AllocateFPtoVPPage()

    // Map file page to virtual page
    FPtoVP[active_pages] = virtual_page

    return virtual_page
```

## Memory Regions

### Stack Space

- **Base**: STK_OFFSET
- **Purpose**: Function activation frames
- **Growth**: Grows downward
- **Management**: Stack overflow detection and extension

### Heap Space

- **Base**: MDS_OFFSET
- **Purpose**: Cons cells, arrays, code blocks
- **Growth**: Grows upward
- **Management**: GC and allocation

### Atom Space

- **Base**: ATOMS_OFFSET
- **Purpose**: Symbol table
- **Growth**: Fixed size or grows
- **Management**: Atom hash table

### Interface Page

- **Base**: IFPAGE_OFFSET
- **Purpose**: VM state and control structures
- **Size**: Fixed (one page)
- **Management**: System structures

## Page Management

### Page Structure

```pseudocode
struct MemoryPage:
    base_address: LispPTR      // Virtual base address
    native_address: void*      // Native memory address
    page_number: uint          // Virtual page number
    file_page: uint            // File page number (if loaded)
    flags: PageFlags           // Page flags (locked, etc.)
```

### Page Allocation Algorithm

```pseudocode
function AllocateMemoryPage(virtual_page):
    // Check if page already allocated
    if IsPageAllocated(virtual_page):
        return GetPageNativeAddress(virtual_page)

    // Allocate native memory
    native_page = AllocateNativePage()

    // Map virtual to native
    MapVirtualToNative(virtual_page, native_page)

    // Update FPtoVP if loading from file
    if loading_from_file:
        file_page = GetFilePageForVirtualPage(virtual_page)
        FPtoVP[file_page] = virtual_page

    return native_page
```

### Page Deallocation

```pseudocode
function DeallocateMemoryPage(virtual_page):
    // Get native address
    native_page = GetNativeAddress(virtual_page)

    // Free native memory
    FreeNativePage(native_page)

    // Clear mapping
    ClearVirtualMapping(virtual_page)
```

## Virtual Memory Operations

### Address Translation

See [Address Translation](address-translation.md) for complete specification.

### Memory Access

```pseudocode
function ReadMemory(lisp_address, size):
    native_address = TranslateAddress(lisp_address)
    return ReadFromNative(native_address, size)

function WriteMemory(lisp_address, value, size):
    native_address = TranslateAddress(lisp_address)
    WriteToNative(native_address, value, size)
```

### Page Locking

Some pages may be locked to prevent swapping:

```pseudocode
function LockPage(virtual_page):
    page = GetPage(virtual_page)
    page.flags = page.flags | LOCKED
    PreventPageSwap(page)
```

## Storage Management

### Storage States

```pseudocode
enum StorageState:
    SFS_NOTSWITCHABLE    // Cannot switch to secondary space
    SFS_SWITCHABLE      // Can switch to secondary space
    SFS_ARRAYSWITCHED    // Array space switched
    SFS_FULLYSWITCHED   // Fully switched to secondary space
```

### Storage Full Detection

```pseudocode
function CheckStorageFull(pages_needed):
    pages_available = CalculateAvailablePages()

    if pages_available < GUARD_STORAGE_FULL:
        SetStorageFullState()
        TriggerStorageFullInterrupt()
        return true

    if pages_needed > pages_available:
        if storage_state == SFS_SWITCHABLE:
            SwitchToSecondarySpace()
        else:
            TriggerStorageFullError()
        return true

    return false
```

## Secondary Space

When primary space is exhausted, VM can switch to secondary space:

```pseudocode
function SwitchToSecondarySpace():
    // Save current array space state
    SaveArraySpaceState()

    // Switch to secondary space
    ArraySpace = SecondaryArraySpace
    NextArrayPage = SecondaryArrayPage

    // Update storage state
    storage_state = SFS_ARRAYSWITCHED or SFS_FULLYSWITCHED
```

## Related Documentation

- [Address Translation](address-translation.md) - How addresses are translated
- [Garbage Collection](garbage-collection.md) - Memory reclamation
- [Memory Layout](memory-layout.md) - Memory organization
