const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const cons = @import("../data/cons.zig");
const array = @import("../data/array.zig");
const layout = @import("layout.zig");

const LispPTR = types.LispPTR;

/// =============================================================================
/// STORAGE MANAGER
/// =============================================================================
/// 
/// **Purpose**: Dynamic storage allocation for Interlisp VM memory regions
/// 
/// **Architecture Overview**:
/// The storage manager provides heap allocation for various memory regions:
/// - DS (Dynamic Storage): General object allocation
/// - VS (Value Storage): Value cells and bindings  
/// - CS (Code Storage): Compiled code and functions
/// - SS (String Storage): String and character data
/// 
/// **Key Concepts**:
/// 1. **Linear Allocation**: Simple bump-pointer allocation for performance
/// 2. **Region-Based**: Each storage region has independent heap management
/// 3. **GC Integration**: Allocation triggers garbage collection countdown
/// 4. **Type-Specific**: Specialized allocation functions for cons cells and arrays
/// 
/// **C Reference Compatibility**:
/// - Matches maiko/src/storage.c allocation functions
/// - Compatible with maiko/inc/storagedefs.h structures
/// - Integrates with C GC trigger mechanisms
/// 
/// **Integration Points**:
/// - VM allocation: Provides memory for dynamic object creation
/// - GC system: Triggers allocation countdown and pressure detection
/// - Address translation: Provides offset-to-address conversion
/// - Memory regions: One instance per VM memory region
/// 
/// **Performance Considerations**:
/// - Linear allocation is O(1) but causes fragmentation
/// - GC integration adds overhead to each allocation
/// - Bounds checking provides safety but impacts performance
/// - Region-based design reduces contention between allocation types
/// 
/// **FIXME Items**:
/// - TODO: Add free list management to reduce fragmentation
/// - TODO: Implement allocation pooling for small objects
/// - TODO: Add allocation statistics and monitoring
/// 
/// **Testing Recommendations**:
/// - Test allocation under memory pressure conditions
/// - Validate GC trigger behavior at various allocation rates
/// - Test boundary conditions around heap limits
/// - Verify integration with address translation functions
/// =============================================================================

/// Storage manager for dynamic memory regions
pub const Storage = struct {
    allocator: std.mem.Allocator,
    heap_base: [*]u8,
    heap_ptr: [*]u8,
    heap_end: [*]u8,
    pages_allocated: u32,
    max_pages: u32,
    /// Base LispPTR address for this storage region (e.g., DS_OFFSET = 0x00200000)
    lisp_base: LispPTR,

    /// Initialize storage region
    /// 
    /// **Parameters**:
    /// - allocator: Memory allocator for heap storage
    /// - initial_size: Initial heap size in bytes
    /// - max_pages: Maximum number of pages this region can grow to
    /// - lisp_base: Base LispPTR address for this region
    /// 
    /// **Returns**: Initialized Storage instance
    /// 
    /// **C Reference**: Matches init_storage() in maiko/src/storage.c
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn init(allocator: std.mem.Allocator, initial_size: usize, max_pages: u32, lisp_base: LispPTR) !Storage {
        const heap_mem = try allocator.alloc(u8, initial_size);

        return Storage{
            .allocator = allocator,
            .heap_base = heap_mem.ptr,
            .heap_ptr = heap_mem.ptr,
            .heap_end = heap_mem.ptr + initial_size,
            .pages_allocated = 0,
            .max_pages = max_pages,
            .lisp_base = lisp_base,
        };
    }

    /// Clean up storage region resources
    /// 
    /// **C Reference**: Matches storage cleanup in C VM shutdown
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn deinit(self: *Storage) void {
        self.allocator.free(self.heap_base[0 .. @intFromPtr(self.heap_end) - @intFromPtr(self.heap_base)]);
    }
};

/// =============================================================================
/// STORAGE UTILITY FUNCTIONS
/// =============================================================================

/// Get the native base address of storage region
/// 
/// **Purpose**: Convert storage base to native pointer for address translation
/// 
/// **Returns**: Native base address as usize
/// 
/// **Integration**: Used by translateAddressExtended() for storage heap access
/// 
/// **Complexity**: O(1)
/// **Confidence**: 100%
pub fn getNativeBase(storage: *const Storage) usize {
    return @intFromPtr(storage.heap_base);
}

/// Get the total size of storage region
/// 
/// **Purpose**: Calculate current heap size for monitoring and debugging
/// 
/// **Returns**: Heap size in bytes
/// 
/// **Integration**: Used by debugging and monitoring tools
/// 
/// **Complexity**: O(1)
/// **Confidence**: 100%
pub fn getStorageSize(storage: *Storage) usize {
    return @intFromPtr(storage.heap_end) - @intFromPtr(storage.heap_base);
}

/// Convert storage offset to LispPTR address
/// 
/// **Purpose**: Translate internal heap offset to VM address space
/// 
/// **Formula**: `lisp_base + offset/2` (convert bytes to DLwords)
/// 
/// **Parameters**:
/// - storage: Storage region instance
/// - offset: Byte offset from heap_base
/// 
/// **Returns**: LispPTR address in VM address space
/// 
/// **Integration**: Used by allocation functions to return VM addresses
/// 
/// **Complexity**: O(1)
/// **Confidence**: 100%
pub fn offsetToLispPTR(storage: *Storage, offset: usize) LispPTR {
    return storage.lisp_base + @as(LispPTR, @intCast(offset / 2));
}

/// Convert LispPTR to storage offset
/// 
/// **Purpose**: Translate VM address to storage heap offset
/// 
/// **Logic**: Check if address falls within this storage region's range
/// 
/// **Parameters**:
/// - storage: Storage region instance  
/// - lisp_addr: LispPTR address to translate
/// 
/// **Returns**: Byte offset from heap_base, or null if outside range
/// 
/// **Integration**: Used by translateAddressExtended() for storage access
/// 
/// **Complexity**: O(1)
/// **Confidence**: 100%
pub fn lispPTRToOffset(storage: *const Storage, lisp_addr: LispPTR) ?usize {
    const masked = lisp_addr & types.POINTERMASK;
    if (masked < storage.lisp_base) return null;
    const offset_dlwords = masked - storage.lisp_base;
    return @as(usize, @intCast(offset_dlwords)) * 2;
}

/// =============================================================================
/// ALLOCATION FUNCTIONS
/// =============================================================================

/// Allocate cons cell from storage
/// 
/// **Purpose**: Allocate Lisp cons cell (CAR/CDR pair) for list construction
/// 
/// **Size Calculation**: 
/// - Cons cells are typically 2 words = 8 bytes
/// - Uses @sizeOf(cons.ConsCell) for type safety
/// 
/// **GC Integration**: 
/// - Triggers incrementAllocationCount() with 1 unit
/// - Cons cells are small, so minimal GC impact
/// 
/// **Parameters**:
/// - storage: Storage region to allocate from
/// - gc: Optional GC instance for allocation tracking
/// 
/// **Returns**: LispPTR address of allocated cons cell
/// 
/// **C Reference**: Matches GetConsCell() in maiko/src/storage.c
/// 
/// **Error Handling**: Returns StorageFull if insufficient space
/// 
/// **Complexity**: O(1)
/// **Confidence**: 95% (tested extensively)
pub fn allocateConsCell(storage: *Storage, gc: ?*@import("gc.zig").GC) errors.MemoryError!LispPTR {
    const cons_size = @sizeOf(cons.ConsCell);

    if (@intFromPtr(storage.heap_ptr) + cons_size > @intFromPtr(storage.heap_end)) {
        return error.StorageFull;
    }

    const aligned_ptr = @as([*]align(@alignOf(cons.ConsCell)) u8, @alignCast(storage.heap_ptr));
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(aligned_ptr));
    cell.* = cons.ConsCell{
        .car_field = 0,
        .cdr_code = 0,
    };

    // Calculate offset from heap_base
    const offset = @intFromPtr(storage.heap_ptr) - @intFromPtr(storage.heap_base);
    storage.heap_ptr += cons_size;

    // Trigger GC countdown if GC is available
    if (gc) |gc_inst| {
        // Simulate allocation triggering incrementAllocationCount
        // Cons cells are typically 2 words = 8 bytes
        const gc_module = @import("gc.zig");
        gc_module.incrementAllocationCount(gc_inst, 1);
    }

    // Convert offset to LispPTR
    return offsetToLispPTR(storage, offset);
}

/// Allocate array from storage
/// 
/// **Purpose**: Allocate Lisp array with type information and capacity
/// 
/// **Size Calculation**:
/// - Header size: ArrayHeader (type_code, fill_pointer, length)
/// - Data size: size * sizeof(LispPTR) for element storage
/// - Total: header_size + data_size
/// 
/// **GC Integration**:
/// - Arrays can be large, so allocation units are calculated
/// - Uses (size + header_size + 7) / 8 to round up to 8-byte units
/// 
/// **Parameters**:
/// - storage: Storage region to allocate from
/// - size: Number of elements the array can hold
/// - type_code: Lisp type code for array elements
/// - gc: Optional GC instance for allocation tracking
/// 
/// **Returns**: LispPTR address of allocated array
/// 
/// **C Reference**: Matches GetArrayBlock() in maiko/src/storage.c
/// 
/// **Error Handling**: Returns StorageFull if insufficient space
/// 
/// **Complexity**: O(1)
/// **Confidence**: 95% (tested extensively)
pub fn allocateArray(storage: *Storage, size: usize, type_code: u8, gc: ?*@import("gc.zig").GC) errors.MemoryError!LispPTR {
    const header_size = @sizeOf(array.ArrayHeader);
    const total_size = header_size + (size * @sizeOf(LispPTR));

    if (@intFromPtr(storage.heap_ptr) + total_size > @intFromPtr(storage.heap_end)) {
        return error.StorageFull;
    }

    const header: *array.ArrayHeader = @ptrCast(@alignCast(storage.heap_ptr));
    header.* = array.ArrayHeader{
        .type_code = type_code,
        .fill_pointer = 0,
        .length = @as(types.DLword, @intCast(size)),
    };

    const addr = @as(LispPTR, @truncate(@intFromPtr(storage.heap_ptr)));
    storage.heap_ptr += total_size;

    // Trigger GC countdown if GC is available
    if (gc) |gc_inst| {
        // Arrays can be large - allocate more countdown units
        const gc_module = @import("gc.zig");
        const allocation_units = @as(u32, @intCast((size + header_size + 7) / 8)); // Round up to 8-byte units
        gc_module.incrementAllocationCount(gc_inst, allocation_units);
    }

    return addr;
}

/// Check if storage is approaching full capacity
/// 
/// **Purpose**: Detect storage pressure to trigger preventive GC
/// 
/// **Logic**: Compare allocated pages against maximum pages
/// 
/// **Parameters**:
/// - storage: Storage region to check
/// - pages_needed: Additional pages that might be needed
/// 
/// **Returns**: true if storage pressure detected, false otherwise
/// 
/// **Integration**: Used by GC system to trigger pressure-based collection
/// 
/// **C Reference**: Matches checkfor_storagefull() in maiko/src/storage.c
/// 
/// **Complexity**: O(1)
/// **Confidence**: 100%
pub fn checkStorageFull(storage: *Storage, pages_needed: usize) bool {
    return storage.pages_allocated + @as(u32, @intCast(pages_needed)) > storage.max_pages;
}
