const std = @import("std");
const types = @import("types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const BYTESPER_PAGE = types.BYTESPER_PAGE;

/// Dump memory region as hex (matching C emulator format)
/// Applies critical debugging technique: shows address, hex bytes, and analysis
pub fn dumpMemory(
    address: LispPTR,
    length: usize,
    vmem: []const u8,
    writer: anytype,
) !void {
    if (address >= vmem.len) {
        try writer.print("ERROR: Address 0x{x} is beyond virtual memory (len=0x{x})\n", .{ address, vmem.len });
        return;
    }

    const end_addr = @min(address + length, vmem.len);
    const actual_length = end_addr - address;

    try writer.print("Memory dump at address 0x{x} (length={} bytes):\n", .{ address, actual_length });
    try writer.print("  Address (dec): {}\n", .{address});
    try writer.print("  Address / 2: {}\n", .{address / 2});
    try writer.print("  Address * 2: {}\n", .{address * 2});

    var i: usize = 0;
    while (i < actual_length) : (i += 16) {
        const line_start = address + i;
        const line_end = @min(line_start + 16, end_addr);
        const line_length = line_end - line_start;

        // Address
        try writer.print("  0x{x:0>6}: ", .{line_start});

        // Hex bytes
        for (0..16) |j| {
            if (j < line_length) {
                const byte = vmem[line_start + j];
                try writer.print("{x:0>2} ", .{byte});
            } else {
                try writer.print("   ", .{});
            }
            if (j == 7) {
                try writer.print(" ", .{}); // Space between groups of 8
            }
        }

        // ASCII representation
        try writer.print(" |", .{});
        for (0..line_length) |j| {
            const byte = vmem[line_start + j];
            const char = if (byte >= 32 and byte < 127) byte else '.';
            try writer.print("{c}", .{char});
        }
        try writer.print("|\n", .{});
    }
}

/// Verify address calculation step-by-step
/// Applies critical debugging technique at each step
pub fn verifyAddressCalculation(
    pc: LispPTR,
    vmem: []const u8,
    writer: anytype,
) !void {
    try writer.print("\n=== Address Calculation Verification (Critical Debugging Technique) ===\n", .{});
    try writer.print("PC: 0x{x} ({})\n", .{ pc, pc });

    // Step 1: PC value analysis
    try writer.print("\nStep 1: PC Value Analysis\n", .{});
    try writer.print("  PC: 0x{x} ({})\n", .{ pc, pc });
    try writer.print("  PC / 2: 0x{x} ({})\n", .{ pc / 2, pc / 2 });
    try writer.print("  PC * 2: 0x{x} ({})\n", .{ pc * 2, pc * 2 });

    // Step 2: Virtual page calculation
    try writer.print("\nStep 2: Virtual Page Calculation\n", .{});
    const virtual_page = pc / BYTESPER_PAGE;
    const offset_in_page = pc % BYTESPER_PAGE;
    try writer.print("  Virtual page: {} (0x{x})\n", .{ virtual_page, virtual_page });
    try writer.print("  Virtual page / 2: {} (0x{x})\n", .{ virtual_page / 2, virtual_page / 2 });
    try writer.print("  Virtual page * 2: {} (0x{x})\n", .{ virtual_page * 2, virtual_page * 2 });
    try writer.print("  Offset in page: {} (0x{x}) bytes\n", .{ offset_in_page, offset_in_page });
    try writer.print("  Offset / 2: {} (0x{x})\n", .{ offset_in_page / 2, offset_in_page / 2 });
    try writer.print("  Offset * 2: {} (0x{x})\n", .{ offset_in_page * 2, offset_in_page * 2 });

    // Step 3: Virtual address calculation
    try writer.print("\nStep 3: Virtual Address Calculation\n", .{});
    const virtual_address = virtual_page * BYTESPER_PAGE;
    const pc_offset_from_page = pc - virtual_address;
    try writer.print("  Virtual address: 0x{x} ({})\n", .{ virtual_address, virtual_address });
    try writer.print("  Virtual address / 2: 0x{x} ({})\n", .{ virtual_address / 2, virtual_address / 2 });
    try writer.print("  Virtual address * 2: 0x{x} ({})\n", .{ virtual_address * 2, virtual_address * 2 });
    try writer.print("  PC offset from page start: 0x{x} ({}) bytes\n", .{ pc_offset_from_page, pc_offset_from_page });
    try writer.print("  Verification: PC - virtual_address = 0x{x} - 0x{x} = 0x{x}\n", .{ pc, virtual_address, pc_offset_from_page });

    // Step 4: Bounds checking
    try writer.print("\nStep 4: Bounds Checking\n", .{});
    try writer.print("  Virtual memory length: 0x{x} ({}) bytes\n", .{ vmem.len, vmem.len });
    try writer.print("  PC within bounds: {}\n", .{pc < vmem.len});
    try writer.print("  Virtual address within bounds: {}\n", .{virtual_address < vmem.len});
    try writer.print("  Page end within bounds: {}\n", .{virtual_address + BYTESPER_PAGE <= vmem.len});

    // Step 5: Memory content at PC
    if (pc < vmem.len and pc + 8 <= vmem.len) {
        try writer.print("\nStep 5: Memory Content at PC\n", .{});
        try writer.print("  Bytes at PC 0x{x}: ", .{pc});
        for (0..8) |i| {
            try writer.print("0x{x:0>2} ", .{vmem[pc + i]});
        }
        try writer.print("\n", .{});
    }

    try writer.print("\n=== End Address Calculation Verification ===\n\n", .{});
}

/// Compare memory regions byte-by-byte
/// Applies critical debugging technique to differences
pub fn compareMemory(
    address: LispPTR,
    length: usize,
    c_memory: []const u8,
    zig_memory: []const u8,
    writer: anytype,
) !void {
    try writer.print("\n=== Memory Comparison (Critical Debugging Technique) ===\n", .{});
    try writer.print("Address: 0x{x}, Length: {} bytes\n", .{ address, length });

    if (address >= c_memory.len or address >= zig_memory.len) {
        try writer.print("ERROR: Address out of bounds\n", .{});
        return;
    }

    const end_addr = @min(@min(address + length, c_memory.len), zig_memory.len);
    var mismatch_count: usize = 0;
    var first_mismatch: ?usize = null;

    var i: usize = 0;
    while (address + i < end_addr) : (i += 1) {
        const addr = address + i;
        const c_byte = c_memory[addr];
        const zig_byte = zig_memory[addr];

        if (c_byte != zig_byte) {
            if (first_mismatch == null) {
                first_mismatch = addr;
            }
            mismatch_count += 1;

            // Apply critical debugging technique
            try writer.print("  Mismatch at 0x{x}:\n", .{addr});
            try writer.print("    C:   0x{x:0>2} ({})\n", .{ c_byte, c_byte });
            try writer.print("    Zig: 0x{x:0>2} ({})\n", .{ zig_byte, zig_byte });
            try writer.print("    Difference: {}\n", .{if (c_byte > zig_byte) c_byte - zig_byte else zig_byte - c_byte});
            try writer.print("    C / 2: {}\n", .{c_byte / 2});
            try writer.print("    Zig / 2: {}\n", .{zig_byte / 2});
            try writer.print("    C * 2: {}\n", .{c_byte * 2});
            try writer.print("    Zig * 2: {}\n", .{zig_byte * 2});
        }
    }

    try writer.print("\nSummary:\n", .{});
    try writer.print("  Total bytes compared: {}\n", .{end_addr - address});
    try writer.print("  Mismatches: {}\n", .{mismatch_count});
    if (first_mismatch) |first| {
        try writer.print("  First mismatch at: 0x{x}\n", .{first});
    } else {
        try writer.print("  ✓ Memory matches!\n", .{});
    }
    try writer.print("\n=== End Memory Comparison ===\n\n", .{});
}

/// Verify file page to virtual page mapping
pub fn verifyFilePageMapping(
    file_page: usize,
    virtual_page: u16,
    fptovp_getter: fn (usize) u16,
    writer: anytype,
) !void {
    try writer.print("\n=== File Page Mapping Verification ===\n", .{});
    try writer.print("File page: {} (0x{x})\n", .{ file_page, file_page });
    try writer.print("  File page / 2: {} (0x{x})\n", .{ file_page / 2, file_page / 2 });
    try writer.print("  File page * 2: {} (0x{x})\n", .{ file_page * 2, file_page * 2 });

    const mapped_vpage = fptovp_getter(file_page);
    try writer.print("Mapped virtual page: {} (0x{x})\n", .{ mapped_vpage, mapped_vpage });
    try writer.print("Expected virtual page: {} (0x{x})\n", .{ virtual_page, virtual_page });

    if (mapped_vpage == virtual_page) {
        try writer.print("✓ Mapping correct!\n", .{});
    } else {
        try writer.print("✗ Mapping incorrect!\n", .{});
        try writer.print("  Difference: {}\n", .{if (mapped_vpage > virtual_page) mapped_vpage - virtual_page else virtual_page - mapped_vpage});
    }

    const file_offset = file_page * BYTESPER_PAGE;
    try writer.print("File offset: 0x{x} ({}) bytes\n", .{ file_offset, file_offset });
    try writer.print("  File offset / 2: 0x{x} ({})\n", .{ file_offset / 2, file_offset / 2 });
    try writer.print("  File offset * 2: 0x{x} ({})\n", .{ file_offset * 2, file_offset * 2 });

    try writer.print("\n=== End File Page Mapping Verification ===\n\n", .{});
}
