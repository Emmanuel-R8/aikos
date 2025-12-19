const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "APPLYFN - placeholder implementation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // APPLYFN currently does nothing (placeholder)
    // When function application is implemented, this will apply function to arguments
    try opcodes.handleAPPLYFN(&vm);

    // For now, just verify it doesn't crash
    try testing.expect(true);
}

test "CHECKAPPLY - placeholder implementation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // CHECKAPPLY currently does nothing (placeholder)
    // When function validation is implemented, this will validate function application
    try opcodes.handleCHECKAPPLY(&vm);

    // For now, just verify it doesn't crash
    try testing.expect(true);
}

test "APPLYFN instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x0E, // APPLYFN opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .APPLYFN);
        try testing.expect(inst.length == 1);
    }
}

test "CHECKAPPLY instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x0F, // CHECKAPPLY opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .CHECKAPPLY);
        try testing.expect(inst.length == 1);
    }
}
