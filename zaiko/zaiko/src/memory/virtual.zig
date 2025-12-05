const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const layout = @import("layout.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Virtual memory manager
pub const VirtualMemory = struct {
    allocator: std.mem.Allocator,
    pages: std.ArrayList([*]u8),
    fptovp: []LispPTR,  // FPtoVP mapping table

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
        _ = self;
        _ = page_num;
        // TODO: Implement page mapping
    }

    /// Get page
    pub fn getPage(self: *VirtualMemory, page_num: u32) errors.MemoryError![*]u8 {
        _ = self;
        _ = page_num;
        return error.PageMappingFailed; // Placeholder
    }
};

/// Translate LispPTR to native pointer using FPtoVP mapping
/// Per rewrite documentation memory/address-translation.md
pub fn translateAddress(lisp_addr: LispPTR, fptovp: []LispPTR, alignment: u2) errors.MemoryError![*]u8 {
    const page_num = layout.getPageNumber(lisp_addr);
    const page_offset = layout.getPageOffset(lisp_addr);

    if (page_num >= fptovp.len) {
        return error.InvalidAddress;
    }

    const virtual_page = fptovp[page_num];
    if (virtual_page == 0) {
        return error.PageMappingFailed;
    }

    const native_page: [*]u8 = @as([*]u8, @ptrFromInt(@as(usize, virtual_page)));
    const native_addr = native_page + page_offset;

    // Align if needed
    if (alignment == 2) {
        // 2-byte alignment
        const aligned_addr = @as(usize, @intFromPtr(native_addr)) & ~@as(usize, 1);
        return @as([*]u8, @ptrFromInt(aligned_addr));
    } else if (alignment == 4) {
        // 4-byte alignment
        const aligned_addr = @as(usize, @intFromPtr(native_addr)) & ~@as(usize, 3);
        return @as([*]u8, @ptrFromInt(aligned_addr));
    }

    return native_addr;
}