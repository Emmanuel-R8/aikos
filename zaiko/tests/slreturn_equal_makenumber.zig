const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "SLRETURN - placeholder implementation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // SLRETURN currently does nothing (placeholder)
    // When frame management is implemented, this will return using stack-relative addressing
    try opcodes.handleSLRETURN(&vm);

    // For now, just verify it doesn't crash
    try testing.expect(true);
}

test "SLRETURN instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x3F, // SLRETURN opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .SLRETURN);
        try testing.expect(inst.length == 1);
    }
}

test "EQUAL - deep equality comparison" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test equal values
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 5);
    try opcodes.handleEQUAL(&vm);
    const result_equal = stack.getTopOfStack(&vm);
    try testing.expect(result_equal == 1); // T

    // Test unequal values
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 3);
    try opcodes.handleEQUAL(&vm);
    const result_unequal = stack.getTopOfStack(&vm);
    try testing.expect(result_unequal == 0); // NIL
}

test "EQUAL instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xF4, // EQUAL opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .EQUAL);
        try testing.expect(inst.length == 1);
    }
}

test "MAKENUMBER - number creation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // MAKENUMBER currently preserves value (placeholder)
    // When number objects are implemented, this will create proper number objects
    try stack.pushStack(&vm, 42);
    try opcodes.handleMAKENUMBER(&vm);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 42); // Value preserved for now
}

test "MAKENUMBER instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xF5, // MAKENUMBER opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .MAKENUMBER);
        try testing.expect(inst.length == 1);
    }
}
