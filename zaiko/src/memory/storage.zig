const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const cons = @import("../data/cons.zig");
const array = @import("../data/array.zig");
const layout = @import("layout.zig");

const LispPTR = types.LispPTR;

/// Storage manager
pub const Storage = struct {
    allocator: std.mem.Allocator,
    heap_base: [*]u8,
    heap_ptr: [*]u8,
    heap_end: [*]u8,
    pages_allocated: u32,
    max_pages: u32,
    /// Base LispPTR address for this storage region (e.g., DS_OFFSET = 0x00200000)
    lisp_base: LispPTR,

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

    pub fn deinit(self: *Storage) void {
        self.allocator.free(self.heap_base[0 .. @intFromPtr(self.heap_end) - @intFromPtr(self.heap_base)]);
    }
};

/// Get the native base address of storage
pub fn getNativeBase(storage: *const Storage) usize {
    return @intFromPtr(storage.heap_base);
}

/// Get the size of storage
pub fn getStorageSize(storage: *Storage) usize {
    return @intFromPtr(storage.heap_end) - @intFromPtr(storage.heap_base);
}

/// Convert storage offset to LispPTR
pub fn offsetToLispPTR(storage: *Storage, offset: usize) LispPTR {
    return storage.lisp_base + @as(LispPTR, @intCast(offset / 2));
}

/// Convert LispPTR to storage offset (returns null if not in storage range)
pub fn lispPTRToOffset(storage: *const Storage, lisp_addr: LispPTR) ?usize {
    const masked = lisp_addr & types.POINTERMASK;
    if (masked < storage.lisp_base) return null;
    const offset_dlwords = masked - storage.lisp_base;
    return @as(usize, @intCast(offset_dlwords)) * 2;
}

/// Allocate cons cell
/// Per contracts/memory-interface.zig
pub fn allocateConsCell(storage: *Storage) errors.MemoryError!LispPTR {
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

    // Convert offset to LispPTR
    return offsetToLispPTR(storage, offset);
}

/// Allocate array
/// Per contracts/memory-interface.zig
pub fn allocateArray(storage: *Storage, size: usize, array_type: array.ArrayType) errors.MemoryError!LispPTR {
    const header_size = @sizeOf(array.ArrayHeader);
    const total_size = header_size + (size * @sizeOf(LispPTR));

    if (@intFromPtr(storage.heap_ptr) + total_size > @intFromPtr(storage.heap_end)) {
        return error.StorageFull;
    }

    const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(storage.heap_ptr));
    header.* = array.ArrayHeader{
        .type_code = @intFromEnum(array_type),
        .fill_pointer = 0,
        .length = @as(types.DLword, @intCast(size)),
    };

    const addr = @as(LispPTR, @intCast(@intFromPtr(storage.heap_ptr)));
    storage.heap_ptr += total_size;

    return addr;
}

/// Check storage full
/// Per contracts/memory-interface.zig
pub fn checkStorageFull(storage: *Storage, pages_needed: usize) bool {
    return storage.pages_allocated + @as(u32, @intCast(pages_needed)) > storage.max_pages;
}
