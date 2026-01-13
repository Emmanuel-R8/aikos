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

/// Translate LispPTR to native pointer using FPtoVP mapping
/// Per rewrite documentation memory/address-translation.md
/// virtual_memory: the virtual memory array
/// lisp_addr: LispPTR address to translate
/// fptovp_table: FPtoVP table (BIGVM format - 32-bit entries)
/// alignment: alignment requirement in bytes (1, 2, 4, 8, etc.)
pub fn translateAddress(virtual_memory: []const u8, lisp_addr: LispPTR, fptovp_table: *const @import("../data/sysout.zig").FPtoVPTable, alignment: u8) errors.MemoryError![*]u8 {
    const page_num = layout.getPageNumber(lisp_addr);
    const page_offset = layout.getPageOffset(lisp_addr);

    if (page_num >= fptovp_table.entries.len) {
        std.debug.print("ERROR: Invalid page number in address translation\n", .{});
        std.debug.print("  LispPTR: 0x{x}\n", .{lisp_addr});
        std.debug.print("  Page number: {} (extracted from LispPTR)\n", .{page_num});
        std.debug.print("  FPtoVP table size: {} entries\n", .{fptovp_table.entries.len});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Invalid LispPTR value (page number out of range)\n", .{});
        std.debug.print("    - FPtoVP table not properly loaded\n", .{});
        return error.InvalidAddress;
    }

    // Use GETFPTOVP to get virtual page number (low 16 bits of 32-bit entry)
    const virtual_page = fptovp_table.getFPtoVP(page_num);
    if (virtual_page == 0) {
        std.debug.print("ERROR: Page mapping failed (virtual page is 0 or sparse)\n", .{});
        std.debug.print("  LispPTR: 0x{x}\n", .{lisp_addr});
        std.debug.print("  File page: {}\n", .{page_num});
        std.debug.print("  Virtual page: {} (from GETFPTOVP)\n", .{virtual_page});
        std.debug.print("  Page OK flag: {} (from GETPAGEOK)\n", .{fptovp_table.getPageOK(page_num)});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Page is sparse (not loaded from sysout)\n", .{});
        std.debug.print("    - Invalid FPtoVP table entry\n", .{});
        std.debug.print("    - Address points to unmapped memory region\n", .{});
        return error.PageMappingFailed;
    }

    // Calculate byte offset in virtual memory
    const BYTESPER_PAGE = @import("../utils/types.zig").BYTESPER_PAGE;
    const byte_offset = @as(usize, virtual_page) * BYTESPER_PAGE + @as(usize, page_offset);

    if (byte_offset >= virtual_memory.len) {
        std.debug.print("ERROR: Address translation exceeds virtual memory bounds\n", .{});
        std.debug.print("  LispPTR: 0x{x}\n", .{lisp_addr});
        std.debug.print("  Virtual page: {}\n", .{virtual_page});
        std.debug.print("  Page offset: {} bytes\n", .{page_offset});
        std.debug.print("  Byte offset: {} (0x{x})\n", .{byte_offset, byte_offset});
        std.debug.print("  Virtual memory size: {} bytes\n", .{virtual_memory.len});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Invalid virtual page number\n", .{});
        std.debug.print("    - Page offset too large\n", .{});
        std.debug.print("    - Virtual memory allocation too small\n", .{});
        return error.InvalidAddress;
    }

    var native_addr = virtual_memory.ptr + byte_offset;

    // Align if needed
    if (alignment == 2) {
        // 2-byte alignment
        const addr_int = @intFromPtr(native_addr);
        const aligned_addr = addr_int & ~@as(usize, 1);
        native_addr = @as([*]u8, @ptrFromInt(aligned_addr));
    } else if (alignment == 4) {
        // 4-byte alignment
        const addr_int = @intFromPtr(native_addr);
        const aligned_addr = addr_int & ~@as(usize, 3);
        native_addr = @as([*]u8, @ptrFromInt(aligned_addr));
    }

    return @constCast(native_addr);
}
