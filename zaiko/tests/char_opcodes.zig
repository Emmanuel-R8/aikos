const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "CHARCODE - extract character code from character object" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push character object (character 'A' = 65)
    try stack.pushStack(&vm, 65);

    // Extract character code
    try opcodes.handleCHARCODE(&vm);

    // Should get character code 65
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 65);
}

test "CHARCODE - extract code from 8-bit character" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push character object with high bits set (should mask to low 8 bits)
    try stack.pushStack(&vm, 0x12345678);

    // Extract character code
    try opcodes.handleCHARCODE(&vm);

    // Should get low 8 bits (0x78 = 120)
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x78);
}

test "CHARN - create character object from code" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push character code ('B' = 66)
    try stack.pushStack(&vm, 66);

    // Create character object
    try opcodes.handleCHARN(&vm);

    // Should get character object (66)
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 66);
}

test "CHARN - mask high bits to ensure valid character code" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push character code with high bits set
    try stack.pushStack(&vm, 0x12345678);

    // Create character object
    try opcodes.handleCHARN(&vm);

    // Should get low 8 bits (0x78 = 120)
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x78);
}

test "CHARCODE and CHARN - round trip" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push character code
    const original_code: types.LispPTR = 100;
    try stack.pushStack(&vm, original_code);

    // Create character object
    try opcodes.handleCHARN(&vm);

    // Extract character code back
    try opcodes.handleCHARCODE(&vm);

    // Should get original code
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == original_code);
}

test "CHARCODE instruction decoding" {
    // Test that CHARCODE instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xB4, // CHARCODE opcode
        0x00, // Operand (unused for CHARCODE, but instruction length is 2)
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .CHARCODE);
        try testing.expect(inst.length == 2);
    }
}

test "CHARN instruction decoding" {
    // Test that CHARN instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xB5, // CHARN opcode
        0x41, // Character code operand ('A')
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .CHARN);
        try testing.expect(inst.length == 2);
    }
}
