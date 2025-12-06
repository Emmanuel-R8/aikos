# Memory Management Interface Contract

**Version**: 1.0
**Date**: 2025-12-04
**Type**: Interface Specification
**Language**: Zig

## Overview

This contract specifies the interfaces for memory management components in the Zig implementation.

## Garbage Collection Interface

```zig
// Add reference
pub fn addReference(gc: *GC, ptr: LispPTR) !void;

// Delete reference
pub fn deleteReference(gc: *GC, ptr: LispPTR) !void;

// Mark stack reference
pub fn markStackReference(gc: *GC, ptr: LispPTR) !void;

// Run GC
pub fn runGC(gc: *GC) !void;

// Find in hash table
pub fn findInHashTable(gc: *GC, ptr: LispPTR, operation: GCOperation) !void;
```

**Preconditions**: Valid pointer, GC initialized

**Postconditions**: Reference count updated, GC run if needed

**Semantics**: Track object references for garbage collection.

## Memory Allocation Interface

```zig
// Allocate cons cell
pub fn allocateConsCell(mem: *Memory) !LispPTR;

// Allocate array
pub fn allocateArray(mem: *Memory, size: usize, type: ArrayType) !LispPTR;

// Allocate page
pub fn allocatePage(mem: *Memory) !LispPTR;

// Check storage full
pub fn checkStorageFull(mem: *Memory, pages_needed: usize) bool;
```

**Preconditions**: Memory manager initialized

**Postconditions**: Memory allocated, storage checked

**Semantics**: Allocate memory objects in Lisp heap.

## Virtual Memory Interface

```zig
// Translate address
pub fn translateAddress(mem: *Memory, lisp_addr: LispPTR, alignment: u2) ![*]u8;

// Map page
pub fn mapPage(mem: *Memory, page_num: u32) !void;

// Get page
pub fn getPage(mem: *Memory, page_num: u32) ![*]u8;
```

**Preconditions**: Valid Lisp address, page number valid

**Postconditions**: Address translated, page mapped

**Semantics**: Manage virtual memory and page mapping.

## Sysout File Interface

```zig
// Load sysout file
pub fn loadSysout(mem: *Memory, filename: []const u8) !void;

// Save sysout file
pub fn saveSysout(mem: *Memory, filename: []const u8) !void;

// Validate sysout
pub fn validateSysout(mem: *Memory, ifpage: *IFPAGE) bool;
```

**Preconditions**: Valid filename, file exists (for load)

**Postconditions**: Sysout loaded/saved, validated

**Semantics**: Load and save Lisp image files.

## Error Types

```zig
pub const MemoryError = error{
    AllocationFailed,
    InvalidAddress,
    PageMappingFailed,
    SysoutLoadFailed,
    SysoutSaveFailed,
    StorageFull,
};
```

## Related Documentation

- `.ai_assistant_db/rewrite-spec/memory/` - Memory management specifications
- `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` - Sysout format
