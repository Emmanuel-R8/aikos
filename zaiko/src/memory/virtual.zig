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

    if (byte_offset >= virtual_memory.len) {
        std.debug.print("ERROR: Address translation exceeds virtual memory bounds\n", .{});
        std.debug.print("  LispPTR: 0x{x}\n", .{lisp_addr});
        std.debug.print("  Byte offset: {} (0x{x})\n", .{byte_offset, byte_offset});
        std.debug.print("  Virtual memory size: {} bytes\n", .{virtual_memory.len});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Invalid LispPTR (masked offset too large)\n", .{});
        std.debug.print("    - Virtual memory allocation too small\n", .{});
        return error.InvalidAddress;
    }

    return @constCast(virtual_memory.ptr + byte_offset);
}
