const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const performance = @import("../utils/performance.zig");

// T103: Performance optimization - make debug output conditional
const DEBUG_SYSOUT_LOADING = @import("builtin").mode == .Debug;

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;
const IFPAGE_KEYVAL = types.IFPAGE_KEYVAL;
const IFPAGE_ADDRESS = types.IFPAGE_ADDRESS;
const BYTESPER_PAGE = types.BYTESPER_PAGE;

/// Sysout file loading and validation
/// See specifications/data-structures/sysout-format.typ
/// Sysout validation key (matches C IFPAGE_KEYVAL)
/// CRITICAL: Must be 0x15e3 to match C implementation (maiko/inc/ifpage.h:15)
pub const SYSOUT_KEYVAL: u16 = IFPAGE_KEYVAL;

/// FPtoVP table structure (BIGVM format - REQUIRED)
/// All implementations MUST use BIGVM format (32-bit entries)
pub const FPtoVPTable = struct {
    // Store as u32 array (BIGVM format)
    // Each entry: low 16 bits = virtual page, high 16 bits = page OK flag
    entries: []u32, // Array of 32-bit entries (BIGVM format)
    allocator: std.mem.Allocator,

    pub fn deinit(self: *FPtoVPTable) void {
        self.allocator.free(self.entries);
    }

    /// GETFPTOVP: Returns low 16 bits (virtual page number)
    /// C: #define GETFPTOVP(b, o) ((b)[o])
    pub fn getFPtoVP(self: *const FPtoVPTable, file_page: usize) u16 {
        return @as(u16, @truncate(self.entries[file_page]));
    }

    /// GETPAGEOK: Returns high 16 bits (page OK flag)
    /// C: #define GETPAGEOK(b, o) ((b)[o] >> 16)
    pub fn getPageOK(self: *const FPtoVPTable, file_page: usize) u16 {
        return @as(u16, @truncate(self.entries[file_page] >> 16));
    }

    /// Check if page is sparse (0xFFFF in page OK flag means sparse)
    pub fn isSparse(self: *const FPtoVPTable, file_page: usize) bool {
        return self.getPageOK(file_page) == 0xFFFF;
    }
};

/// Sysout load result
pub const SysoutLoadResult = struct {
    ifpage: IFPAGE,
    virtual_memory: []u8,
    fptovp: FPtoVPTable,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *SysoutLoadResult) void {
        self.allocator.free(self.virtual_memory);
        self.fptovp.deinit();
    }
};

/// Load sysout file
/// Per contracts/memory-interface.zig
/// Reads IFPAGE from offset IFPAGE_ADDRESS (512 bytes)
/// T103: Performance optimized - measures loading time
pub fn loadSysout(allocator: std.mem.Allocator, filename: []const u8) errors.MemoryError!SysoutLoadResult {
    var timer = performance.PerformanceTimer.start();
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("Opening sysout file: {s}\n", .{filename});
    }
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        std.debug.print("ERROR: Failed to open sysout file '{s}': {}\n", .{ filename, err });
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File does not exist at path: {s}\n", .{filename});
        std.debug.print("    - Insufficient permissions to read file\n", .{});
        std.debug.print("    - Path is incorrect (check relative/absolute path)\n", .{});
        return error.SysoutLoadFailed;
    };
    defer file.close();

    const file_size = file.getEndPos() catch |err| {
        std.debug.print("ERROR: Failed to get sysout file size for '{s}': {}\n", .{ filename, err });
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File is not a regular file (may be a directory or special file)\n", .{});
        std.debug.print("    - File system error occurred\n", .{});
        return error.SysoutLoadFailed;
    };
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("Sysout file size: {} bytes\n", .{file_size});
    }

    // Seek to IFPAGE address (512 bytes from start)
    file.seekTo(IFPAGE_ADDRESS) catch |err| {
        std.debug.print("ERROR: Failed to seek to IFPAGE address ({} bytes) in sysout file '{s}': {}\n", .{ IFPAGE_ADDRESS, filename, err });
        std.debug.print("  File size: {} bytes\n", .{file_size});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File is too small (must be at least {} bytes for IFPAGE)\n", .{IFPAGE_ADDRESS});
        std.debug.print("    - File is not a valid sysout file\n", .{});
        return error.SysoutLoadFailed;
    };

    // Read IFPAGE structure
    // Sysout files store DLwords in big-endian format, but we're on a little-endian machine
    // Need to byte-swap all DLword fields after reading
    // CRITICAL: Read exactly 512 bytes (IFPAGE size) into a buffer, then cast to struct
    var ifpage_buffer: [512]u8 = undefined;
    const bytes_read = file.read(&ifpage_buffer) catch |err| {
        std.debug.print("ERROR: Failed to read IFPAGE structure from sysout file '{s}': {}\n", .{ filename, err });
        std.debug.print("  Expected size: {} bytes\n", .{@sizeOf(IFPAGE)});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File read error (disk I/O failure)\n", .{});
        std.debug.print("    - File was truncated or corrupted\n", .{});
        return error.SysoutLoadFailed;
    };
    if (bytes_read != 512) {
        std.debug.print("ERROR: Incomplete IFPAGE read from sysout file '{s}'\n", .{filename});
        std.debug.print("  Read: {} bytes\n", .{bytes_read});
        std.debug.print("  Expected: 512 bytes (IFPAGE size)\n", .{});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File is truncated or corrupted\n", .{});
        std.debug.print("    - File is not a valid sysout file\n", .{});
        return error.SysoutLoadFailed;
    }

    // DEBUG: Print first 16 bytes before byte-swap
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("DEBUG: IFPAGE raw bytes before byte-swap:\n", .{});
        for (0..16) |i| {
            std.debug.print("0x{x:0>2} ", .{ifpage_buffer[i]});
        }
        std.debug.print("\n", .{});
    }

    // Byte-swap all DLword fields (sysout is big-endian, we're little-endian)
    // C: word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4)
    // We need to swap all DLword (u16) fields in the IFPAGE struct
    // CRITICAL: Swap the buffer first, then copy to struct
    const endianness_utils = @import("../utils/endianness.zig");
    endianness_utils.swapIFPAGE(ifpage_buffer[0..@sizeOf(IFPAGE)]);

    // DEBUG: Print first 16 bytes after byte-swap
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("DEBUG: IFPAGE raw bytes after byte-swap:\n", .{});
        for (0..16) |i| {
            std.debug.print("0x{x:0>2} ", .{ifpage_buffer[i]});
        }
        std.debug.print("\n", .{});
    }

    // Copy the swapped buffer bytes into a properly aligned IFPAGE struct
    var ifpage_struct: IFPAGE = undefined;
    std.mem.copyForwards(u8, @as([*]u8, @ptrCast(&ifpage_struct))[0..@sizeOf(IFPAGE)], ifpage_buffer[0..@sizeOf(IFPAGE)]);
    const ifpage: *IFPAGE = &ifpage_struct;

    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("IFPAGE read: key=0x{x}, lversion={}, minbversion={}, process_size={}\n", .{ ifpage.key, ifpage.lversion, ifpage.minbversion, ifpage.process_size });
        std.debug.print("DEBUG: IFPAGE fields after byte-swap:\n", .{});
        std.debug.print("  currentfxp=0x{x} (DLword offset from Stackspace)\n", .{ifpage.currentfxp});
        std.debug.print("  stackbase=0x{x}, endofstack=0x{x}\n", .{ ifpage.stackbase, ifpage.endofstack });
        std.debug.print("  fptovpstart={} (file page number)\n", .{ifpage.fptovpstart});
    }

    // Validate sysout
    if (!validateSysout(ifpage)) {
        std.debug.print("ERROR: Sysout validation failed for file '{s}'\n", .{filename});
        std.debug.print("  IFPAGE key: 0x{x} (expected: 0x{x})\n", .{ ifpage.key, IFPAGE_KEYVAL });
        std.debug.print("  IFPAGE lversion: {} (minimum: {})\n", .{ ifpage.lversion, LVERSION });
        std.debug.print("  IFPAGE minbversion: {} (maximum: {})\n", .{ ifpage.minbversion, MINBVERSION });
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File is not a valid sysout file (wrong key)\n", .{});
        std.debug.print("    - Sysout version is incompatible (lversion too old or minbversion too new)\n", .{});
        std.debug.print("    - File is corrupted or from a different system\n", .{});
        return error.SysoutLoadFailed;
    }
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("Sysout validation passed\n", .{});
    }

    // Allocate virtual memory (process_size in MB)
    // C: if (ifpage.process_size == 0) sys_size = DEFAULT_MAX_SYSOUTSIZE (64MB) else sys_size = ifpage.process_size
    const DEFAULT_MAX_SYSOUTSIZE: u16 = 64; // Default 64MB for pure SYSOUT
    const process_size_mb = if (ifpage.process_size == 0) DEFAULT_MAX_SYSOUTSIZE else ifpage.process_size;
    const virtual_memory_size = @as(u64, process_size_mb) * 1024 * 1024;
    const virtual_memory = allocator.alloc(u8, virtual_memory_size) catch {
        std.debug.print("ERROR: Failed to allocate virtual memory for sysout file '{s}'\n", .{filename});
        std.debug.print("  Requested size: {} bytes ({} MB)\n", .{ virtual_memory_size, virtual_memory_size / (1024 * 1024) });
        std.debug.print("  IFPAGE process_size: {} MB\n", .{process_size_mb});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Insufficient system memory\n", .{});
        std.debug.print("    - Memory allocation limit exceeded\n", .{});
        std.debug.print("    - Allocator failure\n", .{});
        return error.AllocationFailed;
    };
    // CRITICAL: Zero virtual memory to ensure sparse pages are initialized correctly
    // C emulator initializes memory to zeros, so sparse/unmapped pages should be zero
    @memset(virtual_memory, 0);

    // Load FPtoVP table
    var file_mut = file;
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("DEBUG: Loading FPtoVP table...\n", .{});
    }
    const fptovp = try loadFPtoVPTable(allocator, &file_mut, ifpage, file_size);
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("DEBUG: FPtoVP table loaded: {} entries (BIGVM format)\n", .{fptovp.entries.len});
    }
    if (fptovp.entries.len > 0) {
        std.debug.print("  First 10 entries (GETFPTOVP/GETPAGEOK): ", .{});
        const print_count = @min(10, fptovp.entries.len);
        for (0..print_count) |i| {
            const vpage = fptovp.getFPtoVP(i);
            const pageok = fptovp.getPageOK(i);
            if (fptovp.isSparse(i)) {
                std.debug.print("SPARSE ", .{});
            } else {
                std.debug.print("{}/{} ", .{ vpage, pageok });
            }
        }
        std.debug.print("\n", .{});
    }

    // Load memory pages
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("DEBUG: Loading memory pages...\n", .{});
    }
    try loadMemoryPages(allocator, &file_mut, &fptovp, virtual_memory);

    const elapsed = timer.elapsed();
    if (DEBUG_SYSOUT_LOADING) {
        std.debug.print("DEBUG: Memory pages loaded into virtual memory ({} bytes)\n", .{virtual_memory.len});
        std.debug.print("Performance: Sysout loading took {d:.3} seconds\n", .{elapsed});
    }

    // T103: Check if performance target is met (< 5 seconds)
    if (elapsed > 5.0) {
        std.debug.print("WARNING: Sysout loading took {d:.3} seconds (target: < 5 seconds)\n", .{elapsed});
    }

    // CRITICAL VERIFICATION: Check memory at PC 0x60f131 (C emulator's actual starting PC)
    const verify_pc: usize = 0x60f131;
    if (verify_pc < virtual_memory.len and verify_pc + 8 <= virtual_memory.len) {
        std.debug.print("DEBUG VERIFICATION: Memory at PC 0x60f131 after loading (C emulator's starting PC):\n", .{});
        std.debug.print("  Expected (C emulator trace): 00 60 bf c9 12 0a 02 68\n", .{});
        std.debug.print("  Actual (Zig emulator): ", .{});
        for (0..8) |i| {
            std.debug.print("{x:0>2} ", .{virtual_memory[verify_pc + i]});
        }
        std.debug.print("\n", .{});

        // Check if this matches expected (from C trace: [PC+0]=0x00, [PC+1]=0x60, [PC+2]=0xbf, etc.)
        const expected_bytes = [_]u8{ 0x00, 0x60, 0xbf, 0xc9, 0x12, 0x0a, 0x02, 0x68 };
        var matches = true;
        for (0..8) |i| {
            if (virtual_memory[verify_pc + i] != expected_bytes[i]) {
                matches = false;
                break;
            }
        }
        if (!matches) {
            std.debug.print("  WARNING: Memory at PC 0x60f131 does NOT match C emulator!\n", .{});
            std.debug.print("  This indicates a byte-swapping or page loading issue.\n", .{});
        } else {
            std.debug.print("  SUCCESS: Memory at PC 0x60f131 matches C emulator!\n", .{});
        }

        // Also verify XOR addressing reads correctly
        const memory_access_module = @import("../utils/memory_access.zig");
        std.debug.print("  XOR addressing verification (Get_Pointer(PC+1)):\n", .{});
        std.debug.print("    Reading 4 bytes from PC+1 (0x{x}) with XOR addressing:\n", .{verify_pc + 1});
        var xor_bytes: [4]u8 = undefined;
        for (0..4) |i| {
            const addr = verify_pc + 1 + i;
            const xor_addr = memory_access_module.applyXORAddressingByte(addr);
            xor_bytes[i] = if (xor_addr < virtual_memory.len) virtual_memory[xor_addr] else 0xFF;
            std.debug.print("      base+{} = 0x{x}, XOR 3 = 0x{x}, byte = 0x{x:0>2}\n", .{ i, addr, xor_addr, xor_bytes[i] });
        }
        const xor_result = (@as(u32, xor_bytes[0]) << 24) | (@as(u32, xor_bytes[1]) << 16) | (@as(u32, xor_bytes[2]) << 8) | @as(u32, xor_bytes[3]);
        std.debug.print("    Result: 0x{x:0>8} (expected: 0x0000020a from C trace)\n", .{xor_result});
        if (xor_result == 0x0000020a) {
            std.debug.print("    SUCCESS: XOR addressing matches C emulator!\n", .{});
        } else {
            std.debug.print("    WARNING: XOR addressing result does NOT match C emulator!\n", .{});
        }
    } else {
        std.debug.print("WARNING: Cannot verify PC 0x60f131 - address out of bounds\n", .{});
        std.debug.print("  PC: 0x{x}, virtual_memory.len: 0x{x}\n", .{ verify_pc, virtual_memory.len });
    }

    // Copy IFPAGE struct from buffer (since we used a pointer)
    const ifpage_copy: IFPAGE = ifpage.*;

    return SysoutLoadResult{
        .ifpage = ifpage_copy,
        .virtual_memory = virtual_memory,
        .fptovp = fptovp,
        .allocator = allocator,
    };
}

// Removed swapIFPAGEBytes - now swapping directly on buffer before casting to struct

/// Version constants (from C implementation maiko/inc/version.h)
/// LVERSION: Minimum Lisp version required (21000)
/// MINBVERSION: Maximum bytecode version supported (21001)
const LVERSION: u16 = 21000;
const MINBVERSION: u16 = 21001;

/// Validate sysout
/// Per contracts/memory-interface.zig
/// Checks validation key and version compatibility
pub fn validateSysout(ifpage: *const IFPAGE) bool {
    // Check validation key (CRITICAL: must be 0x15e3)
    if (ifpage.key != IFPAGE_KEYVAL) {
        std.debug.print("Sysout validation failed: key mismatch. Expected 0x{x}, got 0x{x}\n", .{ IFPAGE_KEYVAL, ifpage.key });
        return false;
    }

    // Check version compatibility
    // The sysout's ifpage.lversion must be >= LVERSION
    if (ifpage.lversion < LVERSION) {
        std.debug.print("Sysout validation failed: lversion too old. Required >= {}, got {}\n", .{ LVERSION, ifpage.lversion });
        return false;
    }

    // The sysout's ifpage.minbversion must be <= MINBVERSION
    if (ifpage.minbversion > MINBVERSION) {
        std.debug.print("Sysout validation failed: minbversion too new. Required <= {}, got {}\n", .{ MINBVERSION, ifpage.minbversion });
        return false;
    }

    return true;
}

/// Load FPtoVP (File Page to Virtual Page) table
/// Per contracts/sysout-loading-api.md
/// CRITICAL: All implementations MUST use BIGVM format (32-bit entries)
pub fn loadFPtoVPTable(
    allocator: std.mem.Allocator,
    file: *std.fs.File,
    ifpage: *const IFPAGE,
    file_size: u64,
) errors.MemoryError!FPtoVPTable {
    // Calculate sysout size in half-pages (256-byte pages)
    // C code: sysout_size = (stat_buf.st_size / BYTESPER_PAGE * 2)
    const sysout_size_halfpages = @as(u32, @intCast((file_size / BYTESPER_PAGE) * 2));
    // Number of file pages (sysout_size / 2)
    const num_file_pages = sysout_size_halfpages / 2;

    // Calculate FPtoVP table offset (BIGVM format)
    // BIGVM: (fptovpstart - 1) * BYTESPER_PAGE + 4
    // C code: maiko/src/ldsout.c:287-290 (BIGVM path)
    const offset_adjust: u64 = 4; // BIGVM format
    const fptovp_offset = (@as(u64, ifpage.fptovpstart) - 1) * BYTESPER_PAGE + offset_adjust;

    std.debug.print("DEBUG FPtoVP: ifpage.fptovpstart = {} (0x{x})\n", .{ ifpage.fptovpstart, ifpage.fptovpstart });
    std.debug.print("DEBUG FPtoVP: Calculated offset = {} (0x{x})\n", .{ fptovp_offset, fptovp_offset });

    // ENHANCED TRACING: Match C emulator's CT000-CT002 tracing
    // C: CT000 - Log FPtoVP entries for critical pages
    const ENHANCED_TRACING = @import("builtin").mode == .Debug;

    // Calculate table size before using it in debug output
    const entry_size: usize = 4; // BIGVM: 32-bit entries
    const table_size = num_file_pages * entry_size;

    if (ENHANCED_TRACING) {
        std.debug.print("\n=== ENHANCED TRACING: FPtoVP Table Loading ===\n", .{});
        std.debug.print("DEBUG FPtoVP: Reading {} bytes ({} entries * 4 bytes, BIGVM format)\n", .{ table_size, num_file_pages });
    }
    // Seek to FPtoVP table location
    file.seekTo(fptovp_offset) catch {
        std.debug.print("ERROR: Failed to seek to FPtoVP table offset {} (0x{x}) in sysout file\n", .{ fptovp_offset, fptovp_offset });
        std.debug.print("  IFPAGE fptovpstart: {} (file page number)\n", .{ifpage.fptovpstart});
        std.debug.print("  Calculated offset: {} bytes\n", .{fptovp_offset});
        std.debug.print("  File size: {} bytes\n", .{file_size});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - FPtoVP table offset exceeds file size\n", .{});
        std.debug.print("    - File is truncated or corrupted\n", .{});
        std.debug.print("    - IFPAGE fptovpstart value is invalid\n", .{});
        return error.SysoutLoadFailed;
    };

    // Allocate and read FPtoVP table (BIGVM format: 32-bit entries)
    // BIGVM: read sysout_size * 2 bytes (num_file_pages * 4 bytes, each entry is 32-bit)
    // C code: read(sysout, fptovp, sysout_size * 2)
    std.debug.print("DEBUG FPtoVP: Reading {} bytes (BIGVM format, {} entries * 4 bytes)\n", .{ table_size, num_file_pages });

    const table_buffer = allocator.alloc(u8, table_size) catch {
        return error.AllocationFailed;
    };
    errdefer allocator.free(table_buffer);

    const bytes_read = file.read(table_buffer) catch |err| {
        std.debug.print("ERROR: Failed to read FPtoVP table from sysout file: {}\n", .{err});
        std.debug.print("  Expected size: {} bytes ({} entries * 4 bytes, BIGVM format)\n", .{ table_size, num_file_pages });
        std.debug.print("  File offset: {} bytes\n", .{fptovp_offset});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File read error (disk I/O failure)\n", .{});
        std.debug.print("    - File is truncated or corrupted\n", .{});
        std.debug.print("    - FPtoVP table extends beyond file end\n", .{});
        allocator.free(table_buffer);
        return error.SysoutLoadFailed;
    };
    if (bytes_read != table_size) {
        std.debug.print("ERROR: Incomplete FPtoVP table read from sysout file\n", .{});
        std.debug.print("  Read: {} bytes\n", .{bytes_read});
        std.debug.print("  Expected: {} bytes ({} entries * 4 bytes, BIGVM format)\n", .{ table_size, num_file_pages });
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - File is truncated or corrupted\n", .{});
        std.debug.print("    - FPtoVP table size calculation is incorrect\n", .{});
        allocator.free(table_buffer);
        return error.SysoutLoadFailed;
    }

    // Convert buffer to u32 array (BIGVM format)
    // Sysout stores entries as big-endian u32, need to byte-swap
    const entries = allocator.alloc(u32, num_file_pages) catch {
        std.debug.print("ERROR: Failed to allocate memory for FPtoVP table entries\n", .{});
        std.debug.print("  Requested: {} entries * {} bytes = {} bytes\n", .{ num_file_pages, @sizeOf(u32), num_file_pages * @sizeOf(u32) });
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Insufficient system memory\n", .{});
        std.debug.print("    - Memory allocation limit exceeded\n", .{});
        allocator.free(table_buffer);
        return error.AllocationFailed;
    };
    errdefer allocator.free(entries);

    // CRITICAL: Match C emulator's FPtoVP table byte-swapping exactly
    // C: read(sysout, fptovp, sysout_size * 2) - reads raw bytes
    // C: word_swap_page((unsigned short *)fptovp, (sysout_size / 2) + 1) - swaps 32-bit longwords
    //
    // C's process:
    // 1. Read raw bytes from file (big-endian u32 entries)
    // 2. Store in buffer as raw bytes (no conversion)
    // 3. Treat buffer as u32 array (native byte order = little-endian on x86_64)
    // 4. Swap each u32 with ntohl() (network to host = big-endian to little-endian)
    //
    // Zig equivalent:
    // Read as big-endian u32 (which does the conversion directly)
    // This should be equivalent to C's read + swap

    // BUT: File page 2937 maps to virtual page 11850 in Zig, not 6204
    // This suggests the byte-swapping is wrong!

    // CRITICAL: C's FPtoVP byte-swapping is INCOMPLETE!
    // C swaps only first half: word_swap_page(..., (sysout_size / 4) + 1)
    // For BIGVM: (sysout_size / 4) + 1 = only ~50% of entries
    // Second half (file page 5178+) is NOT swapped - read as big-endian
    //
    // CONFIDENCE LEVEL: HIGH (90%)
    // - Based on exhaustive analysis of maiko/src/ldsout.c:437
    // - Verified: (sysout_size / 4) + 1 only covers ~50% of entries
    // - File page 5178 is in second half (NOT swapped)
    //
    // HOW THIS CONCLUSION WAS REACHED:
    // 1. Analyzed C code: maiko/src/ldsout.c:437
    //    - word_swap_page(..., (sysout_size / 4) + 1)
    // 2. Calculated: (33270 / 4) + 1 = 8318 longwords
    // 3. Total entries: 16635
    // 4. Coverage: 8318/16635 = 50%
    // 5. File page 5178 >= 8318 (NOT in swapped range)
    //
    // HOW TO TEST:
    // - Verify file page 2937 (swapped) vs 5178 (not swapped) mappings
    // - Compare with C emulator's FPtoVP table interpretation
    //
    // HOW TO ENSURE NOT REVERTED:
    // - Use endianness.zig functions for all byte-swapping
    // - Unit test: Verify swap boundary calculation
    // - Integration test: Compare FPtoVP mappings with C emulator

    const endianness_utils = @import("../utils/endianness.zig");
    const swap_boundary = endianness_utils.calculateFPtoVPSwapBoundary(sysout_size_halfpages);

    // ENHANCED TRACING: Log byte-swapping process
    if (ENHANCED_TRACING) {
        std.debug.print("DEBUG FPtoVP: Swap boundary = {} entries (first {} entries swapped)\n", .{ swap_boundary, swap_boundary });
    }

    for (0..num_file_pages) |i| {
        const entry_bytes = table_buffer[i * 4 ..][0..4];
        const entry_array: [4]u8 = entry_bytes[0..4].*;
        const before_swap = std.mem.readInt(u32, entry_array[0..4], .big); // Read as big-endian
        entries[i] = endianness_utils.swapFPtoVPEntry(entry_array, i, swap_boundary);

        // ENHANCED TRACING: Log critical entries (file page 5178, 2937, 9427)
        if (ENHANCED_TRACING and (i == 5178 or i == 2937 or i == 9427)) {
            const after_swap = entries[i];
            const vpage = @as(u16, @truncate(after_swap));
            const pageok = @as(u16, @truncate(after_swap >> 16));
            std.debug.print("DEBUG FPtoVP: Entry {} - BEFORE swap: 0x{x:0>8}, AFTER swap: 0x{x:0>8}\n", .{ i, before_swap, after_swap });
            std.debug.print("  GETFPTOVP = {} (0x{x}), GETPAGEOK = 0x{x:0>4}\n", .{ vpage, vpage, pageok });
        }
    }

    if (ENHANCED_TRACING) {
        std.debug.print("=== END ENHANCED TRACING: FPtoVP Table Loading ===\n\n", .{});
    }

    allocator.free(table_buffer);

    // Debug: Check for virtual page 302 (frame page)
    const frame_vpage: u16 = 302;
    std.debug.print("DEBUG loadFPtoVPTable: Checking file pages that should map to virtual page {d} (frame)\n", .{frame_vpage});
    for (0..num_file_pages) |file_page| {
        const vpage = @as(u16, @truncate(entries[file_page])); // GETFPTOVP: low 16 bits
        if (vpage == frame_vpage) {
            const pageok = @as(u16, @truncate(entries[file_page] >> 16)); // GETPAGEOK: high 16 bits
            std.debug.print("  FPtoVP[{d}] = {d} (0x{x}), GETPAGEOK = 0x{x}\n", .{ file_page, vpage, vpage, pageok });
        }
    }

    return FPtoVPTable{
        .entries = entries,
        .allocator = allocator,
    };
}

/// Valspace byte offset 0xC0000 (lispmap.h) => virtual page 0xC0000/512 = 768.
/// Load initial segment (identity mapping) so valspace/defspace pages are present.
const VALSPACE_VP_END: u32 = 832; // (0xC0000 + 0x20000) / 512

/// Load memory pages from sysout file into virtual memory
/// Per contracts/sysout-loading-api.md
/// Maps file pages to virtual pages using FPtoVP table
pub fn loadMemoryPages(
    allocator: std.mem.Allocator,
    file: *std.fs.File,
    fptovp: *const FPtoVPTable,
    virtual_memory: []u8,
) errors.MemoryError!void {
    _ = allocator; // May be needed for temporary buffers

    // Iterate through file pages
    const num_file_pages = fptovp.entries.len;
    var page_buffer: [BYTESPER_PAGE]u8 = undefined;

    // Load initial segment (identity mapping) so valspace (vp 384 = 0xC0000/512) and nearby pages exist.
    // C emulator has full Lisp_world; FPtoVP loop below may overwrite some of these.
    const endianness_utils = @import("../utils/endianness.zig");
    const initial_pages = @min(VALSPACE_VP_END, num_file_pages);
    for (0..initial_pages) |vp_u| {
        const vp: u32 = @intCast(vp_u);
        const virtual_address = @as(u64, vp) * BYTESPER_PAGE;
        if (virtual_address + BYTESPER_PAGE > virtual_memory.len) break;
        const file_offset = @as(u64, vp) * BYTESPER_PAGE;
        file.seekTo(file_offset) catch break;
        const n = file.read(virtual_memory[@intCast(virtual_address)..][0..BYTESPER_PAGE]) catch break;
        if (n != BYTESPER_PAGE) break;
        const page_slice = virtual_memory[@intCast(virtual_address)..][0..BYTESPER_PAGE];
        endianness_utils.swapMemoryPage(page_slice);
    }

    // T103: Performance optimization - make debug passes conditional
    if (DEBUG_SYSOUT_LOADING) {
        // DEBUG: Search for file pages that map to virtual page 302 (frame location)
        const frame_vpage: u16 = 302;
        var found_frame_pages: [10]usize = undefined;
        var found_count: usize = 0;
        for (0..num_file_pages) |file_page| {
            // Use GETFPTOVP to get virtual page number (low 16 bits)
            const virtual_page = fptovp.getFPtoVP(file_page);
            if (virtual_page == frame_vpage and found_count < found_frame_pages.len) {
                found_frame_pages[found_count] = file_page;
                found_count += 1;
            }
        }
        std.debug.print("DEBUG loadMemoryPages: Found {d} file pages mapping to virtual page {d} (frame):\n", .{ found_count, frame_vpage });
        for (0..found_count) |i| {
            std.debug.print("  File page {d} -> virtual page {d}\n", .{ found_frame_pages[i], frame_vpage });
        }
    }

    // T103: Performance optimization - only do debug passes in debug mode
    //
    // IMPORTANT: The C emulator trace prints PC as a BYTE offset (e.g. 0x60f130),
    // and also shows the DLword offset (/2: 0x307898). `virtual_memory` is
    // byte-addressed, and FPtoVP virtual page numbers are in *byte pages*
    // (512-byte pages), matching C's `[vpage:...]` field.
    //
    // PC (bytes) 0x60f130 / 512 = vpage 12408, offset 0x130 within that page.
    const target_vpage = if (DEBUG_SYSOUT_LOADING) 12408 else 0;
    var found_target_page = if (DEBUG_SYSOUT_LOADING) false else false;
    var target_page_count: usize = 0;

    if (DEBUG_SYSOUT_LOADING) {
        // DEBUG: Also check file page 2937 (where C emulator's expected bytes are)
        const c_emulator_file_page: usize = 2937;
        var c_file_page_vpage: ?u16 = null;

        // First pass: count how many file pages map to target virtual page and list them
        // Also check what virtual page file page 2937 maps to (C emulator's file page)
        var target_file_pages: [10]usize = undefined;
        var target_file_pages_count: usize = 0;
        for (0..num_file_pages) |file_page| {
            if (fptovp.isSparse(file_page)) {
                continue;
            }
            const vpage = fptovp.getFPtoVP(file_page);

            // Check if this is the C emulator's file page
            if (file_page == c_emulator_file_page) {
                c_file_page_vpage = vpage;
                const pageok = fptovp.getPageOK(file_page);
                std.debug.print("DEBUG: C emulator's file page {} maps to virtual page {} (GETPAGEOK=0x{x:0>4})\n", .{ file_page, vpage, pageok });
            }

            if (vpage == target_vpage) {
                if (target_file_pages_count < target_file_pages.len) {
                    target_file_pages[target_file_pages_count] = file_page;
                    target_file_pages_count += 1;
                }
                target_page_count += 1;
            }
        }
        if (target_page_count > 0) {
            std.debug.print("DEBUG: Found {d} file page(s) mapping to virtual page {} (PC page):\n", .{ target_page_count, target_vpage });
            for (0..target_file_pages_count) |i| {
                const fp = target_file_pages[i];
                const pageok = fptovp.getPageOK(fp);
                std.debug.print("  File page {} -> virtual page {} (GETPAGEOK=0x{x:0>4})\n", .{ fp, target_vpage, pageok });
            }
        }
    }

    for (0..num_file_pages) |file_page| {
        // Skip sparse pages (GETPAGEOK returns 0xFFFF for sparse pages)
        if (fptovp.isSparse(file_page)) {
            continue;
        }

        // Use GETFPTOVP to get virtual page number (low 16 bits). This is a byte-page
        // index (512-byte pages), matching C's `[vpage:...]` field.
        const virtual_page_u16 = fptovp.getFPtoVP(file_page);
        const virtual_page: u32 = @as(u32, virtual_page_u16);

        // T103: Performance optimization - conditional debug output
        if (DEBUG_SYSOUT_LOADING and virtual_page == target_vpage) {
            const pageok = fptovp.getPageOK(file_page);
            std.debug.print("DEBUG: Loading file page {} -> virtual page {} (PC page) [{d}/{d}]\n", .{ file_page, virtual_page, target_page_count, target_page_count });
            std.debug.print("  FPtoVP[{}] = {} (GETPAGEOK=0x{x:0>4})\n", .{ file_page, virtual_page, pageok });
            found_target_page = true;
        }

        // Calculate file offset for this page
        const file_offset = @as(u64, file_page) * BYTESPER_PAGE;

        // Seek to file page
        file.seekTo(file_offset) catch |err| {
            std.debug.print("ERROR: Failed to seek to file page {} offset {} (0x{x}): {}\n", .{ file_page, file_offset, file_offset, err });
            std.debug.print("  Virtual page: {}\n", .{virtual_page});
            std.debug.print("  Possible causes:\n", .{});
            std.debug.print("    - File offset exceeds file size\n", .{});
            std.debug.print("    - File is truncated or corrupted\n", .{});
            return error.SysoutLoadFailed;
        };

        // Read page data
        const bytes_read = file.read(&page_buffer) catch |err| {
            std.debug.print("ERROR: Failed to read page data from sysout file: {}\n", .{err});
            std.debug.print("  File page: {}\n", .{file_page});
            std.debug.print("  File offset: {} (0x{x})\n", .{ file_offset, file_offset });
            std.debug.print("  Virtual page: {}\n", .{virtual_page});
            std.debug.print("  Possible causes:\n", .{});
            std.debug.print("    - File read error (disk I/O failure)\n", .{});
            std.debug.print("    - File is truncated or corrupted\n", .{});
            return error.SysoutLoadFailed;
        };
        if (bytes_read != BYTESPER_PAGE) {
            std.debug.print("ERROR: Incomplete page read from sysout file\n", .{});
            std.debug.print("  File page: {}\n", .{file_page});
            std.debug.print("  Read: {} bytes\n", .{bytes_read});
            std.debug.print("  Expected: {} bytes (BYTESPER_PAGE)\n", .{BYTESPER_PAGE});
            std.debug.print("  Virtual page: {}\n", .{virtual_page});
            std.debug.print("  Possible causes:\n", .{});
            std.debug.print("    - File is truncated or corrupted\n", .{});
            std.debug.print("    - Page extends beyond file end\n", .{});
            return error.SysoutLoadFailed;
        }

        // T103: Performance optimization - conditional debug output
        // ENHANCED TRACING: Match C emulator's detailed page loading trace
        const ENHANCED_TRACING = @import("builtin").mode == .Debug;

        // Calculate virtual address before using it in debug output
        const virtual_address = @as(u64, virtual_page) * BYTESPER_PAGE;

        if ((DEBUG_SYSOUT_LOADING or ENHANCED_TRACING) and virtual_page == target_vpage) {
            std.debug.print("\n=== ENHANCED TRACING: Loading PC Page ===\n", .{});
            std.debug.print("DEBUG sysout_loader: Loading file page {} -> virtual page {} (PC PAGE 0x60f130)\n", .{ file_page, virtual_page });
            std.debug.print("  Virtual address = 0x{x} (offset 0x{x})\n", .{ virtual_address, virtual_address });
            std.debug.print("  PC 0x60f130 is at offset 0x130 (0x130 bytes) in this page\n", .{});
            std.debug.print("DEBUG sysout_loader: PC PAGE - Raw bytes from file (BEFORE byte-swap):\n", .{});
            std.debug.print("  File page: {}, file offset: 0x{x} ({} bytes)\n", .{ file_page, file_offset, file_offset });
            std.debug.print("  First 16 bytes: ", .{});
            for (0..16) |i| {
                std.debug.print("0x{x:0>2} ", .{page_buffer[i]});
            }
            std.debug.print("\n", .{});
            const pc_offset_in_page: usize = 0x130; // PC 0x60f130 - base 0x60f000 = 0x130
            std.debug.print("  Bytes at PC offset 0x{x} (0x{x}): ", .{ pc_offset_in_page, pc_offset_in_page });
            for (0..8) |i| {
                if (pc_offset_in_page + i < BYTESPER_PAGE) {
                    std.debug.print("0x{x:0>2} ", .{page_buffer[pc_offset_in_page + i]});
                }
            }
            std.debug.print("\n", .{});
        }

        if (virtual_address + BYTESPER_PAGE > virtual_memory.len) {
            std.debug.print("ERROR: Virtual address exceeds virtual memory bounds during page loading\n", .{});
            std.debug.print("  File page: {}\n", .{file_page});
            std.debug.print("  Virtual page: {}\n", .{virtual_page});
            std.debug.print("  Virtual address: 0x{x} ({} bytes)\n", .{ virtual_address, virtual_address });
            std.debug.print("  Required size: {} bytes (BYTESPER_PAGE)\n", .{BYTESPER_PAGE});
            std.debug.print("  Virtual memory size: {} bytes (0x{x})\n", .{ virtual_memory.len, virtual_memory.len });
            std.debug.print("  End address would be: 0x{x}\n", .{virtual_address + BYTESPER_PAGE});
            std.debug.print("  Possible causes:\n", .{});
            std.debug.print("    - FPtoVP table contains invalid virtual page numbers\n", .{});
            std.debug.print("    - Virtual memory allocation too small\n", .{});
            std.debug.print("    - Sysout file contains pages beyond virtual memory capacity\n", .{});
            return error.SysoutLoadFailed;
        }

        // Write page data to virtual memory
        @memcpy(virtual_memory[virtual_address..][0..BYTESPER_PAGE], &page_buffer);

        // CRITICAL: Byte-swap page data (big-endian sysout -> little-endian native)
        // Uses centralized endianness helper for consistency
        //
        // CONFIDENCE LEVEL: VERY HIGH (99%)
        // - Uses endianness.swapMemoryPage() which matches C exactly
        // - C: word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128)
        // - Parameter 128 = number of 32-bit longwords (128 * 4 = 512 bytes = 1 page)
        //
        // HOW THIS CONCLUSION WAS REACHED:
        // - Analyzed maiko/src/ldsout.c:708 exhaustively
        // - Verified: 128 longwords * 4 bytes = 512 bytes = 1 page
        // - C uses ntohl() to swap each 32-bit longword
        //
        // HOW TO TEST:
        // - Load page from sysout, swap, verify bytes match C emulator
        // - Check memory at PC 0x307898 matches C emulator exactly
        //
        // HOW TO ENSURE NOT REVERTED:
        // - MUST use endianness.swapMemoryPage() - no direct @byteSwap() calls
        const page_slice = virtual_memory[virtual_address..][0..BYTESPER_PAGE];
        endianness_utils.swapMemoryPage(page_slice);

        // T103: Performance optimization - conditional debug output
        // ENHANCED TRACING: Match C emulator's detailed page loading trace
        if ((DEBUG_SYSOUT_LOADING or ENHANCED_TRACING) and virtual_page == target_vpage) {
            std.debug.print("DEBUG sysout_loader: PC PAGE - Bytes AFTER byte-swap (in virtual memory):\n", .{});
            std.debug.print("  First 16 bytes at virtual address 0x{x}: ", .{virtual_address});
            for (0..16) |i| {
                std.debug.print("0x{x:0>2} ", .{virtual_memory[virtual_address + i]});
            }
            std.debug.print("\n", .{});
            const pc_offset_in_page: usize = 0x130; // PC 0x60f130 - base 0x60f000 = 0x130
            const pc_addr = virtual_address + pc_offset_in_page;
            std.debug.print("  Bytes at PC location (offset 0x{x}, address 0x{x}): ", .{ pc_offset_in_page, pc_addr });
            for (0..8) |i| {
                if (pc_offset_in_page + i < BYTESPER_PAGE) {
                    std.debug.print("0x{x:0>2} ", .{virtual_memory[virtual_address + pc_offset_in_page + i]});
                }
            }
            std.debug.print("\n", .{});
            std.debug.print("=== END ENHANCED TRACING: Loading PC Page ===\n\n", .{});
        }
    }

    // T103: Performance optimization - conditional debug output
    if (DEBUG_SYSOUT_LOADING and !found_target_page) {
        std.debug.print("WARNING: Virtual page {} (PC page) was NOT loaded from sysout file!\n", .{target_vpage});
        std.debug.print("  PC 0x307898 is in virtual page {}\n", .{target_vpage});
        std.debug.print("  This page should have been loaded from a file page mapping to virtual page {}\n", .{target_vpage});
    }
}
