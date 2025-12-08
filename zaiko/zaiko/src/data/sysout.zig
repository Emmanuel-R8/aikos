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

/// FPtoVP table structure
pub const FPtoVPTable = struct {
    entries: []u16, // Array of virtual page numbers (or 0xFFFF for sparse pages)
    allocator: std.mem.Allocator,
    is_bigvm: bool,

    pub fn deinit(self: *FPtoVPTable) void {
        self.allocator.free(self.entries);
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
    const fptovp = try loadFPtoVPTable(allocator, &file_mut, &ifpage, file_size);

    // Load memory pages
    try loadMemoryPages(allocator, &file_mut, &fptovp, virtual_memory);

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
/// Supports both BIGVM (32-bit entries) and non-BIGVM (16-bit entries) formats
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

    // Calculate FPtoVP table offset
    // For non-BIGVM: (fptovpstart - 1) * BYTESPER_PAGE + 2
    // For BIGVM: (fptovpstart - 1) * BYTESPER_PAGE + 4
    // We'll use non-BIGVM format for now (most common)
    const is_bigvm = false; // TODO: Detect BIGVM from build config
    const offset_adjust = if (is_bigvm) 4 else 2;
    const fptovp_offset = (@as(u64, ifpage.fptovpstart) - 1) * BYTESPER_PAGE + offset_adjust;

    // Seek to FPtoVP table location
    file.seekTo(fptovp_offset) catch {
        return error.SysoutLoadFailed;
    };

    // Allocate and read FPtoVP table
    // For non-BIGVM: read sysout_size bytes (num_file_pages * 2 bytes, each entry is 16-bit)
    // For BIGVM: read sysout_size * 2 bytes (num_file_pages * 4 bytes, each entry is 32-bit)
    const entry_size = if (is_bigvm) 4 else 2;
    const table_size = num_file_pages * entry_size;
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

    // Convert buffer to u16 array (for non-BIGVM)
    const entries = allocator.alloc(u16, num_file_pages) catch {
        allocator.free(table_buffer);
        return error.AllocationFailed;
    };
    errdefer allocator.free(entries);

    if (is_bigvm) {
        // For BIGVM, read as u32 and convert
        // TODO: Implement BIGVM support
        @panic("BIGVM format not yet implemented");
    } else {
        // For non-BIGVM, read as u16 (little-endian)
        for (0..num_file_pages) |i| {
            entries[i] = std.mem.readInt(u16, table_buffer[i * 2..][0..2], .little);
        }
    }

    allocator.free(table_buffer);

    return FPtoVPTable{
        .entries = entries,
        .allocator = allocator,
        .is_bigvm = is_bigvm,
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

    for (0..num_file_pages) |file_page| {
        const virtual_page = fptovp.entries[file_page];

        // Skip sparse pages (marked with 0xFFFF)
        if (virtual_page == 0xFFFF) {
            continue;
        }

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