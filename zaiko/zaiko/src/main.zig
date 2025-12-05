const std = @import("std");
const types = @import("utils/types.zig");
const errors = @import("utils/errors.zig");
const stack = @import("vm/stack.zig");
const dispatch = @import("vm/dispatch.zig");
const sysout = @import("data/sysout.zig");
const storage = @import("memory/storage.zig");
const gc_module = @import("memory/gc.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <sysout_file>\n", .{args[0]});
        return;
    }

    const sysout_path = args[1];
    std.debug.print("Loading sysout file: {s}\n", .{sysout_path});

    // Initialize VM
    var vm = try stack.VM.init(allocator, 1024 * 1024); // 1MB stack
    defer vm.deinit();

    // Initialize memory management
    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024 * 10, 100); // 10MB heap
    defer mem_storage.deinit();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // Load sysout file
    const ifpage = sysout.loadSysout(allocator, sysout_path) catch |err| {
        std.debug.print("Failed to load sysout file: {}\n", .{err});
        return;
    };

    std.debug.print("Sysout loaded: version={}, keyval=0x{x}\n", .{ ifpage.version, ifpage.keyval });

    // TODO: Initialize code segment from sysout
    // TODO: Map memory regions from sysout
    // TODO: Initialize VM state from IFPAGE

    std.debug.print("VM initialized, entering dispatch loop\n", .{});

    // Enter dispatch loop
    // TODO: Uncomment when sysout loading is fully implemented
    // const test_code: []const types.ByteCode = &[_]types.ByteCode{ 0xC0, 0xC1, 0xC2 };
    // try dispatch.dispatch(&vm, test_code);

    std.debug.print("VM execution complete\n", .{});
}