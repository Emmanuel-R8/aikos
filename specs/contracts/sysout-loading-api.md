# Sysout Loading API Contract

**Date**: 2025-12-07
**Feature**: Zig Emulator Completion

## Overview

API contract for sysout file loading functionality, defining the interface for loading and validating sysout files.

## Functions

### `loadSysout`

**Signature**:
```zig
pub fn loadSysout(
    allocator: std.mem.Allocator,
    filename: []const u8,
) !SysoutLoadResult
```

**Purpose**: Load a sysout file into virtual memory and return VM state.

**Parameters**:
- `allocator`: Memory allocator for virtual memory and temporary buffers
- `filename`: Path to sysout file

**Returns**: `SysoutLoadResult` containing:
- `ifpage: IFPAGE` - Interface page with VM state
- `virtual_memory: []u8` - Allocated virtual memory space
- `fptovp: FPtoVPTable` - File page to virtual page mapping table

**Errors**:
- `error.SysoutFileNotFound` - File cannot be opened
- `error.InvalidSysoutKey` - IFPAGE key validation failed (not 0x15e3)
- `error.VersionMismatch` - Version compatibility check failed
- `error.AllocationFailed` - Memory allocation failed
- `error.SysoutLoadFailed` - General loading error

**Preconditions**:
- `filename` must be a valid file path
- `allocator` must be a valid allocator

**Postconditions**:
- Virtual memory allocated and pages loaded
- IFPAGE validated and returned
- FPtoVP table loaded and returned

---

### `validateSysout`

**Signature**:
```zig
pub fn validateSysout(ifpage: *const IFPAGE) !void
```

**Purpose**: Validate IFPAGE structure for sysout compatibility.

**Parameters**:
- `ifpage`: Pointer to IFPAGE structure to validate

**Returns**: `void` on success, error on validation failure

**Errors**:
- `error.InvalidSysoutKey` - Key is not IFPAGE_KEYVAL (0x15e3)
- `error.VersionTooOld` - Lisp version too old (lversion < LVERSION)
- `error.VersionTooNew` - Bytecode version too new (minbversion > MINBVERSION)

**Preconditions**:
- `ifpage` must point to valid IFPAGE structure

**Postconditions**:
- IFPAGE validated or error returned

---

### `loadFPtoVPTable`

**Signature**:
```zig
pub fn loadFPtoVPTable(
    allocator: std.mem.Allocator,
    file: std.fs.File,
    ifpage: *const IFPAGE,
) !FPtoVPTable
```

**Purpose**: Load FPtoVP (File Page to Virtual Page) mapping table from sysout file.

**Parameters**:
- `allocator`: Memory allocator for table storage
- `file`: Open sysout file handle
- `ifpage`: IFPAGE structure containing `fptovpstart` offset

**Returns**: `FPtoVPTable` containing mapping entries

**Errors**:
- `error.FileSeekFailed` - Cannot seek to FPtoVP table location
- `error.FileReadFailed` - Cannot read FPtoVP table data
- `error.AllocationFailed` - Memory allocation failed

**Preconditions**:
- `file` must be open and positioned correctly
- `ifpage` must contain valid `fptovpstart` value

**Postconditions**:
- FPtoVP table loaded and returned

---

### `loadMemoryPages`

**Signature**:
```zig
pub fn loadMemoryPages(
    allocator: std.mem.Allocator,
    file: std.fs.File,
    fptovp: *const FPtoVPTable,
    virtual_memory: []u8,
) !void
```

**Purpose**: Load memory pages from sysout file into virtual memory using FPtoVP mapping.

**Parameters**:
- `allocator`: Memory allocator for temporary buffers
- `file`: Open sysout file handle
- `fptovp`: FPtoVP table for page mapping
- `virtual_memory`: Pre-allocated virtual memory space

**Returns**: `void` on success, error on failure

**Errors**:
- `error.FileSeekFailed` - Cannot seek to page location
- `error.FileReadFailed` - Cannot read page data
- `error.InvalidPageAddress` - Virtual page address out of range

**Preconditions**:
- `file` must be open
- `fptovp` must be valid and loaded
- `virtual_memory` must be allocated with sufficient size

**Postconditions**:
- All pages mapped by FPtoVP are loaded into virtual memory
- Sparse pages (0xFFFF) are skipped

---

## Data Structures

### `IFPAGE`

**Definition**: `packed struct` matching C `IFPAGE` structure exactly

**Key Fields**:
- `key: DLword` - Validation key (must be 0x15e3)
- `lversion: DLword` - Lisp version
- `minbversion: DLword` - Minimum bytecode version
- `process_size: DLword` / `unsigned` - Process size in MB
- `nactivepages: int` - Number of active pages
- `fptovpstart: DLword` - FPtoVP table start offset
- `currentfxp: DLword` - Current frame pointer
- `endofstack: DLword` - End of stack
- `stackbase: DLword` - Stack base address
- ~90 additional VM state fields

**Constraints**:
- Must match C structure exactly (byte-for-byte)
- Must use `packed struct` for exact layout

---

### `FPtoVPTable`

**Definition**:
```zig
pub const FPtoVPTable = struct {
    entries: []u32, // BIGVM format - 32-bit entries (REQUIRED)
    allocator: std.mem.Allocator,
    
    pub fn getFPtoVP(self: *const FPtoVPTable, file_page: usize) u16;
    pub fn getPageOK(self: *const FPtoVPTable, file_page: usize) u16;
    pub fn isSparse(self: *const FPtoVPTable, file_page: usize) bool;
};
```

**Purpose**: Maps file page numbers to virtual page numbers

**CRITICAL**: All implementations MUST use BIGVM format (32-bit entries). Non-BIGVM format is NOT supported.

**Fields**:
- `entries`: Array of 32-bit mapping entries (BIGVM format)
- `allocator`: Memory allocator for table management

**Entry Structure** (BIGVM format):
- Each entry is a 32-bit `u32` value
- Low 16 bits: Virtual page number (accessed via `getFPtoVP()`)
- High 16 bits: Page OK flag (accessed via `getPageOK()`)
- `getPageOK() == 0xFFFF`: Page not present (sparse page marker)

**Constraints**:
- Entry size: 32 bits (4 bytes) per entry
- Table size: `nactivepages * 4` bytes (BIGVM format)
- Table reading: `sysout_size * 2` bytes from sysout file

---

### `SysoutLoadResult`

**Definition**:
```zig
pub const SysoutLoadResult = struct {
    ifpage: IFPAGE,
    virtual_memory: []u8,
    fptovp: FPtoVPTable,
};
```

**Purpose**: Result of successful sysout loading

**Fields**:
- `ifpage`: Validated IFPAGE structure
- `virtual_memory`: Allocated and loaded virtual memory
- `fptovp`: Loaded FPtoVP mapping table

**Lifetime**: Caller responsible for freeing `virtual_memory` and `fptovp.entries`

---

## Constants

### `IFPAGE_KEYVAL`

**Value**: `0x15e3`

**Purpose**: Validation key for IFPAGE structure

**Usage**: Must match `ifpage.key` for valid sysout file

---

### `IFPAGE_ADDRESS`

**Value**: `512`

**Purpose**: Byte offset of IFPAGE in sysout file

**Usage**: Seek to this offset to read IFPAGE

---

### `BYTESPER_PAGE`

**Value**: `512`

**Purpose**: Size of a memory page in bytes

**Usage**: Page size for file organization and virtual memory mapping

---

## Error Handling

All functions use Zig error unions (`!Type`) for error handling.

**Error Types**:
- `SysoutFileNotFound` - File cannot be opened
- `InvalidSysoutKey` - IFPAGE key validation failed
- `VersionMismatch` - Version compatibility check failed
- `AllocationFailed` - Memory allocation failed
- `SysoutLoadFailed` - General loading error
- `FileSeekFailed` - File seek operation failed
- `FileReadFailed` - File read operation failed
- `InvalidPageAddress` - Virtual page address out of range

**Error Propagation**: Errors are propagated using `try` keyword

---

## Usage Example

```zig
const allocator = std.heap.page_allocator;
const result = try sysout.loadSysout(allocator, "medley/loadups/starter.sysout");

// Use result.ifpage for VM state initialization
// Use result.virtual_memory for memory access
// Use result.fptovp for page mapping

// Cleanup
allocator.free(result.virtual_memory);
allocator.free(result.fptovp.entries);
```
