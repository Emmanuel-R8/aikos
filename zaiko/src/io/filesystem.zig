const std = @import("std");
const errors = @import("../utils/errors.zig");

/// File mode
pub const FileMode = enum {
    READ,
    WRITE,
    APPEND,
    READ_WRITE,
};

/// File handle
pub const FileHandle = struct {
    file: std.fs.File,

    pub fn close(self: *FileHandle) void {
        self.file.close();
    }
};

/// Translate Lisp pathname to platform path
/// Per contracts/io-interface.zig
pub fn lispToPlatformPathname(lisp_path: []const u8, versionp: bool) errors.IOError![]u8 {
    _ = versionp;
    // TODO: Implement pathname translation
    // Lisp pathnames use different format (e.g., "DSK:>foo>bar")
    // Need to translate to platform-specific paths
    return lisp_path; // Placeholder
}

/// Translate platform path to Lisp pathname
/// Per contracts/io-interface.zig
pub fn platformToLispPathname(platform_path: []const u8, dirp: bool) errors.IOError![]u8 {
    _ = dirp;
    // TODO: Implement reverse pathname translation
    return platform_path; // Placeholder
}

/// Open file
/// Per contracts/io-interface.zig
pub fn openFile(pathname: []const u8, mode: FileMode) errors.IOError!FileHandle {
    const flags: std.fs.File.OpenFlags = switch (mode) {
        .READ => .{ .mode = .read_only },
        .WRITE => .{ .mode = .write_only },
        .APPEND => .{ .mode = .write_only },
        .READ_WRITE => .{ .mode = .read_write },
    };

    const file = try std.fs.cwd().openFile(pathname, flags);

    return FileHandle{ .file = file };
}

/// Read file
/// Per contracts/io-interface.zig
pub fn readFile(handle: *FileHandle, buffer: []u8) errors.IOError!usize {
    return try handle.file.read(buffer);
}

/// Write file
/// Per contracts/io-interface.zig
pub fn writeFile(handle: *FileHandle, buffer: []const u8) errors.IOError!usize {
    return try handle.file.writeAll(buffer);
}

/// Close file
/// Per contracts/io-interface.zig
pub fn closeFile(handle: *FileHandle) void {
    handle.close();
}
