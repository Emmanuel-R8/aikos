const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;
const IFPAGE_KEYVAL = types.IFPAGE_KEYVAL;
const IFPAGE_ADDRESS = types.IFPAGE_ADDRESS;
const BYTESPER_PAGE = types.BYTESPER_PAGE;

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
pub fn loadSysout(allocator: std.mem.Allocator, filename: []const u8) errors.MemoryError!SysoutLoadResult {
    std.debug.print("Opening sysout file: {s}\n", .{filename});
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        std.debug.print("Failed to open file: {}\n", .{err});
        return error.SysoutLoadFailed;
    };
    defer file.close();

    const file_size = file.getEndPos() catch |err| {
        std.debug.print("Failed to get file size: {}\n", .{err});
        return error.SysoutLoadFailed;
    };
    std.debug.print("Sysout file size: {} bytes\n", .{file_size});

    // Seek to IFPAGE address (512 bytes from start)
    file.seekTo(IFPAGE_ADDRESS) catch |err| {
        std.debug.print("Failed to seek to IFPAGE address ({}): {}\n", .{ IFPAGE_ADDRESS, err });
        return error.SysoutLoadFailed;
    };

    // Read IFPAGE structure
    // Sysout files store DLwords in big-endian format, but we're on a little-endian machine
    // Need to byte-swap all DLword fields after reading
    var ifpage: IFPAGE = undefined;
    const bytes_read = file.read(std.mem.asBytes(&ifpage)) catch |err| {
        std.debug.print("Failed to read IFPAGE: {}\n", .{err});
        return error.SysoutLoadFailed;
    };
    if (bytes_read != @sizeOf(IFPAGE)) {
        std.debug.print("Failed to read complete IFPAGE: read {} bytes, expected {}\n", .{ bytes_read, @sizeOf(IFPAGE) });
        return error.SysoutLoadFailed;
    }

    // Byte-swap all DLword fields (sysout is big-endian, we're little-endian)
    // C: word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4)
    // We need to swap all DLword (u16) fields in the IFPAGE struct
    swapIFPAGEBytes(&ifpage);

    std.debug.print("IFPAGE read: key=0x{x}, lversion={}, minbversion={}, process_size={}\n", .{ ifpage.key, ifpage.lversion, ifpage.minbversion, ifpage.process_size });
    std.debug.print("DEBUG: IFPAGE fields after byte-swap:\n", .{});
    std.debug.print("  currentfxp=0x{x} (DLword offset from Stackspace)\n", .{ifpage.currentfxp});
    std.debug.print("  stackbase=0x{x}, endofstack=0x{x}\n", .{ ifpage.stackbase, ifpage.endofstack });
    std.debug.print("  fptovpstart={} (file page number)\n", .{ifpage.fptovpstart});

    // Validate sysout
    if (!validateSysout(&ifpage)) {
        std.debug.print("Sysout validation failed\n", .{});
        return error.SysoutLoadFailed;
    }
    std.debug.print("Sysout validation passed\n", .{});

    // Allocate virtual memory (process_size in MB)
    // C: if (ifpage.process_size == 0) sys_size = DEFAULT_MAX_SYSOUTSIZE (64MB) else sys_size = ifpage.process_size
    const DEFAULT_MAX_SYSOUTSIZE: u16 = 64; // Default 64MB for pure SYSOUT
    const process_size_mb = if (ifpage.process_size == 0) DEFAULT_MAX_SYSOUTSIZE else ifpage.process_size;
    const virtual_memory_size = @as(u64, process_size_mb) * 1024 * 1024;
    const virtual_memory = allocator.alloc(u8, virtual_memory_size) catch {
        return error.AllocationFailed;
    };

    // Load FPtoVP table
    var file_mut = file;
    std.debug.print("DEBUG: Loading FPtoVP table...\n", .{});
    const fptovp = try loadFPtoVPTable(allocator, &file_mut, &ifpage, file_size);
    std.debug.print("DEBUG: FPtoVP table loaded: {} entries (BIGVM format)\n", .{fptovp.entries.len});
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
    std.debug.print("DEBUG: Loading memory pages...\n", .{});
    try loadMemoryPages(allocator, &file_mut, &fptovp, virtual_memory);
    std.debug.print("DEBUG: Memory pages loaded into virtual memory ({} bytes)\n", .{virtual_memory.len});

    return SysoutLoadResult{
        .ifpage = ifpage,
        .virtual_memory = virtual_memory,
        .fptovp = fptovp,
        .allocator = allocator,
    };
}

/// Byte-swap IFPAGE structure
/// Sysout files store DLwords in big-endian format, but we're on a little-endian machine
/// C: word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4)
/// NOTE: C word_swap_page uses ntohl() which swaps u32 words, but the parameter suggests u16 words
/// However, the actual file format stores DLwords (u16) in big-endian, so we swap u16 words
/// The C code may be swapping u32 words because it's more efficient, but the effect is the same
/// for a structure where all fields are multiples of 2 bytes (which IFPAGE is - DLword=2, LispPTR=4)
fn swapIFPAGEBytes(ifpage: *IFPAGE) void {
    // Swap all u16 (DLword) fields in the IFPAGE structure
    // Sysout stores DLwords in big-endian: [high_byte, low_byte]
    // We need little-endian: [low_byte, high_byte]
    const ifpage_bytes = std.mem.asBytes(ifpage);
    var i: usize = 0;
    while (i < ifpage_bytes.len - 1) : (i += 2) {
        // Swap bytes in each u16 word (big-endian to little-endian)
        const temp = ifpage_bytes[i];
        ifpage_bytes[i] = ifpage_bytes[i + 1];
        ifpage_bytes[i + 1] = temp;
    }
    // Note: LispPTR fields (u32) will be swapped twice (once per u16), which is correct
    // because they're stored as two big-endian u16 words: [h1, l1, h2, l2]
    // After u16 swapping: [l1, h1, l2, h2] which is correct little-endian u32
}

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

    // Seek to FPtoVP table location
    file.seekTo(fptovp_offset) catch {
        return error.SysoutLoadFailed;
    };

    // Allocate and read FPtoVP table (BIGVM format: 32-bit entries)
    // BIGVM: read sysout_size * 2 bytes (num_file_pages * 4 bytes, each entry is 32-bit)
    // C code: read(sysout, fptovp, sysout_size * 2)
    const entry_size: usize = 4; // BIGVM: 32-bit entries
    const table_size = num_file_pages * entry_size;
    std.debug.print("DEBUG FPtoVP: Reading {} bytes (BIGVM format, {} entries * 4 bytes)\n", .{ table_size, num_file_pages });
    
    const table_buffer = allocator.alloc(u8, table_size) catch {
        return error.AllocationFailed;
    };
    errdefer allocator.free(table_buffer);

    const bytes_read = file.read(table_buffer) catch {
        allocator.free(table_buffer);
        return error.SysoutLoadFailed;
    };
    if (bytes_read != table_size) {
        allocator.free(table_buffer);
        return error.SysoutLoadFailed;
    }

    // Convert buffer to u32 array (BIGVM format)
    // Sysout stores entries as big-endian u32, need to byte-swap
    const entries = allocator.alloc(u32, num_file_pages) catch {
        allocator.free(table_buffer);
        return error.AllocationFailed;
    };
    errdefer allocator.free(entries);

    // Read as big-endian u32 and convert to native (little-endian)
    // C code: #ifdef BYTESWAP word_swap_page((unsigned short *)fptovp, (sysout_size / 2) + 1); #endif
    // This swaps u16 words, but since we have u32, we need to swap u32 words
    for (0..num_file_pages) |i| {
        const entry_bytes = table_buffer[i * 4..][0..4];
        // Read as big-endian u32
        const entry_be = std.mem.readInt(u32, entry_bytes, .big);
        // Store as native (little-endian) - Zig will handle the byte order
        entries[i] = entry_be;
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

    for (0..num_file_pages) |file_page| {
        // Skip sparse pages (GETPAGEOK returns 0xFFFF for sparse pages)
        if (fptovp.isSparse(file_page)) {
            continue;
        }

        // Use GETFPTOVP to get virtual page number (low 16 bits)
        const virtual_page = fptovp.getFPtoVP(file_page);

        // Calculate file offset for this page
        const file_offset = @as(u64, file_page) * BYTESPER_PAGE;

        // Seek to file page
        file.seekTo(file_offset) catch {
            return error.SysoutLoadFailed;
        };

        // Read page data
        const bytes_read = file.read(&page_buffer) catch {
            return error.SysoutLoadFailed;
        };
        if (bytes_read != BYTESPER_PAGE) {
            return error.SysoutLoadFailed;
        }

        // Calculate virtual address
        const virtual_address = @as(u64, virtual_page) * BYTESPER_PAGE;
        if (virtual_address + BYTESPER_PAGE > virtual_memory.len) {
            return error.SysoutLoadFailed;
        }

        // Write page data to virtual memory
        @memcpy(virtual_memory[virtual_address..][0..BYTESPER_PAGE], &page_buffer);
    }
}