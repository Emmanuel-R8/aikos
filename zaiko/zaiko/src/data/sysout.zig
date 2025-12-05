const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Interface page (matches C IFPAGE)
/// Per data-model.md
pub const IFPAGE = packed struct {
    keyval: u32, // Validation key
    version: u32, // Version number
    currentfxp: LispPTR, // Current frame pointer
    endofstack: LispPTR, // End of stack
    // ... other VM state fields
};

/// Sysout validation key
pub const SYSOUT_KEYVAL: u32 = 0x12345678;

/// Load sysout file
/// Per contracts/memory-interface.zig
pub fn loadSysout(allocator: std.mem.Allocator, filename: []const u8) errors.MemoryError!IFPAGE {
    const file = std.fs.cwd().openFile(filename, .{}) catch {
        return error.SysoutLoadFailed;
    };
    defer file.close();

    const file_size = file.getEndPos() catch {
        return error.SysoutLoadFailed;
    };
    const file_contents = allocator.alloc(u8, file_size) catch {
        return error.AllocationFailed;
    };
    defer allocator.free(file_contents);

    _ = file.readAll(file_contents) catch {
        return error.SysoutLoadFailed;
    };

    // Parse IFPAGE from file
    if (file_contents.len < @sizeOf(IFPAGE)) {
        return error.SysoutLoadFailed;
    }

    // Ensure proper alignment for IFPAGE
    // For now, assume file contents are properly aligned
    // In production, would need to copy to aligned buffer
    const aligned_ptr = @as([*]align(@alignOf(IFPAGE)) u8, @alignCast(file_contents.ptr));
    const ifpage: *IFPAGE = @as(*IFPAGE, @ptrCast(aligned_ptr));

    // Validate sysout
    if (!validateSysout(ifpage)) {
        return error.SysoutLoadFailed;
    }

    return ifpage.*;
}

/// Validate sysout
/// Per contracts/memory-interface.zig
pub fn validateSysout(ifpage: *IFPAGE) bool {
    // Check validation key
    if (ifpage.keyval != SYSOUT_KEYVAL) {
        return false;
    }

    // Check version (basic validation)
    if (ifpage.version == 0) {
        return false;
    }

    return true;
}