const std = @import("std");
const testing = std.testing;
const sysout = @import("../src/data/sysout.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

// T108: Compare execution results with C emulator for validation
// This test suite validates that Zig emulator produces identical results to C emulator

test "Sysout loading produces same IFPAGE values as C emulator" {
    // Test that IFPAGE structure matches C emulator after loading
    // This is a basic validation - full comparison requires running both emulators
    const allocator = testing.allocator;
    
    // Load sysout file
    const sysout_result = sysout.loadSysout(allocator, "../../../medley/internal/loadups/starter.sysout") catch |err| {
        std.debug.print("Failed to load sysout: {}\n", .{err});
        return err;
    };
    defer sysout_result.deinit();

    // Verify IFPAGE key matches expected value
    try testing.expect(sysout_result.ifpage.key == types.IFPAGE_KEYVAL);
    try testing.expect(sysout_result.ifpage.key == 0x15e3);

    // Verify FPtoVP table is loaded (BIGVM format)
    try testing.expect(sysout_result.fptovp.entries.len > 0);
    
    // Verify virtual memory is allocated
    try testing.expect(sysout_result.virtual_memory.len > 0);
}

test "FPtoVP table entries match C emulator format (BIGVM)" {
    // Verify that FPtoVP table uses BIGVM format (32-bit entries)
    const allocator = testing.allocator;
    
    const sysout_result = sysout.loadSysout(allocator, "../../../medley/internal/loadups/starter.sysout") catch |err| {
        std.debug.print("Failed to load sysout: {}\n", .{err});
        return err;
    };
    defer sysout_result.deinit();

    // Check that entry 9427 maps to virtual page 302 (frame page)
    // This matches C emulator behavior
    const file_page_9427: usize = 9427;
    if (file_page_9427 < sysout_result.fptovp.entries.len) {
        const vpage = sysout_result.fptovp.getFPtoVP(file_page_9427);
        const pageok = sysout_result.fptovp.getPageOK(file_page_9427);
        
        // C emulator shows: GETFPTOVP[9427] = 302, GETPAGEOK[9427] = 0
        try testing.expect(vpage == 302);
        try testing.expect(pageok == 0);
    }

    // Check that entry 16629 also maps to virtual page 302
    const file_page_16629: usize = 16629;
    if (file_page_16629 < sysout_result.fptovp.entries.len) {
        const vpage = sysout_result.fptovp.getFPtoVP(file_page_16629);
        try testing.expect(vpage == 302);
    }
}

test "Virtual memory size matches IFPAGE process_size" {
    // Verify that virtual memory allocation matches IFPAGE specification
    const allocator = testing.allocator;
    
    const sysout_result = sysout.loadSysout(allocator, "../../../medley/internal/loadups/starter.sysout") catch |err| {
        std.debug.print("Failed to load sysout: {}\n", .{err});
        return err;
    };
    defer sysout_result.deinit();

    // Calculate expected size from IFPAGE
    const DEFAULT_MAX_SYSOUTSIZE: u16 = 64; // Default 64MB
    const process_size_mb = if (sysout_result.ifpage.process_size == 0) DEFAULT_MAX_SYSOUTSIZE else sysout_result.ifpage.process_size;
    const expected_size = @as(u64, process_size_mb) * 1024 * 1024;

    // Verify virtual memory size matches
    try testing.expect(sysout_result.virtual_memory.len == expected_size);
}

test "FPtoVP table uses BIGVM format (32-bit entries)" {
    // Verify that FPtoVP table is using BIGVM format exclusively
    const allocator = testing.allocator;
    
    const sysout_result = sysout.loadSysout(allocator, "../../../medley/internal/loadups/starter.sysout") catch |err| {
        std.debug.print("Failed to load sysout: {}\n", .{err});
        return err;
    };
    defer sysout_result.deinit();

    // In BIGVM format, entries are u32 (4 bytes each)
    // Table should have been read as sysout_size * 2 bytes
    // Each entry is 32 bits, so we can verify by checking entry structure
    
    // Test that GETFPTOVP and GETPAGEOK return different values (BIGVM behavior)
    // In non-BIGVM, they would return the same value
    if (sysout_result.fptovp.entries.len > 9427) {
        const entry_9427 = sysout_result.fptovp.entries[9427];
        const vpage = sysout_result.fptovp.getFPtoVP(9427);
        const pageok = sysout_result.fptovp.getPageOK(9427);
        
        // In BIGVM: GETFPTOVP = low 16 bits, GETPAGEOK = high 16 bits
        // They should be different for entry 9427 (vpage=302, pageok=0)
        try testing.expect(vpage != pageok); // BIGVM behavior
        try testing.expect(vpage == 302);
        try testing.expect(pageok == 0);
        
        // Verify the full 32-bit value
        try testing.expect(entry_9427 == 0x0000012e); // 302 in low 16 bits
    }
}

// TODO: Add more comprehensive comparison tests:
// - Compare VM state initialization (PC, stack pointers, frame pointers)
// - Compare bytecode execution results (instruction by instruction)
// - Compare memory contents after execution
// - Compare error handling behavior
// - Compare GC operations results