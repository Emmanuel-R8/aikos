const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const layout = @import("layout.zig");
const storage_module = @import("storage.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Virtual memory manager
pub const VirtualMemory = struct {
    allocator: std.mem.Allocator,
    pages: std.ArrayList([*]u8),
    fptovp: []LispPTR, // FPtoVP mapping table

    pub fn init(allocator: std.mem.Allocator, num_pages: usize) !VirtualMemory {
        const fptovp_table = try allocator.alloc(LispPTR, num_pages);
        @memset(fptovp_table, 0);

        const pages_list = std.ArrayList([*]u8).init(allocator);

        return VirtualMemory{
            .allocator = allocator,
            .pages = pages_list,
            .fptovp = fptovp_table,
        };
    }

    pub fn deinit(self: *VirtualMemory) void {
        self.allocator.free(self.fptovp);
        self.pages.deinit();
    }

    /// Map page
    pub fn mapPage(self: *VirtualMemory, page_num: u32) errors.MemoryError!void {
        // Allocate a new page (512 bytes)
        const page_size = 512;
        const page_data = try self.allocator.alloc(u8, page_size);
        @memset(page_data, 0);

        // Add to pages list
        try self.pages.append(@ptrCast(page_data));

        // Update FPtoVP table
        const vp_num = self.pages.items.len - 1; // Virtual page number
        self.fptovp[page_num] = @as(LispPTR, @intCast(vp_num));
    }

    /// Get page
    pub fn getPage(self: *VirtualMemory, page_num: u32) errors.MemoryError![*]u8 {
        // Look up virtual page number in FPtoVP table
        if (page_num >= self.fptovp.len) {
            return error.PageMappingFailed;
        }

        const vp_num = self.fptovp[page_num];
        if (vp_num >= self.pages.items.len) {
            return error.PageMappingFailed;
        }

        return self.pages.items[vp_num];
    }
};

/// Extended address translation that handles both virtual memory and storage heap
/// For addresses in the sysout (virtual_memory), returns pointer into virtual_memory
/// For addresses in the storage heap, returns pointer into storage heap
pub fn translateAddressExtended(
    virtual_memory: []const u8,
    storage: ?*const storage_module.Storage,
    lisp_addr: LispPTR,
    fptovp_table: *const @import("../data/sysout.zig").FPtoVPTable,
    alignment: u8,
) errors.MemoryError![*]u8 {
    _ = fptovp_table;
    _ = alignment;

    const masked: LispPTR = lisp_addr & types.POINTERMASK;
    const byte_offset: usize = @as(usize, @intCast(masked)) * 2;

    // Check if address is in virtual_memory range (sysout data)
    if (byte_offset < virtual_memory.len) {
        return @constCast(virtual_memory.ptr + byte_offset);
    }

    // Check if address is in storage heap range
    if (storage) |s| {
        if (storage_module.lispPTRToOffset(s, lisp_addr)) |offset| {
            const native_ptr = @as([*]u8, @ptrFromInt(storage_module.getNativeBase(s))) + offset;
            return @constCast(native_ptr);
        }
    }

    return error.InvalidAddress;
}

/// Translate LispPTR (LAddr) to a native pointer.
///
/// C reference: `NativeAligned2FromLAddr` / `NativeAligned4FromLAddr` in `maiko/inc/adr68k.h`.
/// Those functions treat a LispPTR as a **DLword offset** from `Lisp_world` (NOT a byte address),
/// and they do **not** consult FPtoVP at runtime.
///
/// In this Zig port, `virtual_memory` is byte-addressed, so:
///   byte_offset = (POINTERMASK & lisp_addr) * 2
///
/// NOTE: `fptovp_table` is currently unused; it is retained for call-site compatibility.
pub fn translateAddress(virtual_memory: []const u8, lisp_addr: LispPTR, fptovp_table: *const @import("../data/sysout.zig").FPtoVPTable, alignment: u8) errors.MemoryError![*]u8 {
    _ = fptovp_table;
    _ = alignment;

    const masked: LispPTR = lisp_addr & types.POINTERMASK;
    const byte_offset: usize = @as(usize, @intCast(masked)) * 2;

    // DEBUG: Trace translateAddress calls with marker value
    if (lisp_addr == 0xfffe0002) {
        std.debug.print("DEBUG translateAddress: FOUND! lisp_addr=0x{x}, masked=0x{x}, byte_offset=0x{x}\n", .{ lisp_addr, masked, byte_offset });
    }

    if (byte_offset >= virtual_memory.len) {
        return error.InvalidAddress;
    }

    return @constCast(virtual_memory.ptr + byte_offset);
}
