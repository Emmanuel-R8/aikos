const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const cons = @import("../data/cons.zig");
const array = @import("../data/array.zig");

const LispPTR = types.LispPTR;

/// Storage manager
pub const Storage = struct {
    allocator: std.mem.Allocator,
    heap_base: [*]u8,
    heap_ptr: [*]u8,
    heap_end: [*]u8,
    pages_allocated: u32,
    max_pages: u32,

    pub fn init(allocator: std.mem.Allocator, initial_size: usize, max_pages: u32) !Storage {
        const heap_mem = try allocator.alloc(u8, initial_size);

        return Storage{
            .allocator = allocator,
            .heap_base = heap_mem.ptr,
            .heap_ptr = heap_mem.ptr,
            .heap_end = heap_mem.ptr + initial_size,
            .pages_allocated = 0,
            .max_pages = max_pages,
        };
    }

    pub fn deinit(self: *Storage) void {
        self.allocator.free(self.heap_base[0..@intFromPtr(self.heap_end) - @intFromPtr(self.heap_base)]);
    }
};

/// Allocate cons cell
/// Per contracts/memory-interface.zig
pub fn allocateConsCell(storage: *Storage) errors.MemoryError!LispPTR {
    const cons_size = @sizeOf(cons.ConsCell);

    if (@intFromPtr(storage.heap_ptr) + cons_size > @intFromPtr(storage.heap_end)) {
        return error.StorageFull;
    }

    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(storage.heap_ptr));
    cell.* = cons.ConsCell{
        .car_field = 0,
        .cdr_code = 0,
    };

    const addr = @as(LispPTR, @intCast(@intFromPtr(storage.heap_ptr)));
    storage.heap_ptr += cons_size;

    return addr;
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