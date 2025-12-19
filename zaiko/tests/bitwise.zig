const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "LOGOR2 - bitwise OR operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push two values: 0b1010 (10) and 0b1100 (12)
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0b1100);

    // Perform LOGOR2: 0b1010 | 0b1100 = 0b1110 (14)
    try opcodes.handleLOGOR2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1110); // 14
}

test "LOGAND2 - bitwise AND operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push two values: 0b1010 (10) and 0b1100 (12)
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0b1100);

    // Perform LOGAND2: 0b1010 & 0b1100 = 0b1000 (8)
    try opcodes.handleLOGAND2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1000); // 8
}

test "LOGXOR2 - bitwise XOR operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push two values: 0b1010 (10) and 0b1100 (12)
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0b1100);

    // Perform LOGXOR2: 0b1010 ^ 0b1100 = 0b0110 (6)
    try opcodes.handleLOGXOR2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b0110); // 6
}

test "LOGOR2 - with zero" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and zero
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0);

    // Perform LOGOR2: 0b1010 | 0 = 0b1010
    try opcodes.handleLOGOR2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1010);
}

test "LOGAND2 - with zero" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and zero
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0);

    // Perform LOGAND2: 0b1010 & 0 = 0
    try opcodes.handleLOGAND2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0);
}

test "LOGXOR2 - with zero" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and zero
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0);

    // Perform LOGXOR2: 0b1010 ^ 0 = 0b1010
    try opcodes.handleLOGXOR2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1010);
}

test "LOGOR2 - with all ones" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and all ones (0xFFFFFFFF)
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0xFFFFFFFF);

    // Perform LOGOR2: 0b1010 | 0xFFFFFFFF = 0xFFFFFFFF
    try opcodes.handleLOGOR2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0xFFFFFFFF);
}

test "LOGAND2 - with all ones" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and all ones (0xFFFFFFFF)
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0xFFFFFFFF);

    // Perform LOGAND2: 0b1010 & 0xFFFFFFFF = 0b1010
    try opcodes.handleLOGAND2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1010);
}

test "LOGXOR2 - with all ones" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and all ones (0xFFFFFFFF)
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 0xFFFFFFFF);

    // Perform LOGXOR2: 0b1010 ^ 0xFFFFFFFF = ~0b1010 (bitwise NOT)
    try opcodes.handleLOGXOR2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0xFFFFFFF5); // ~0b1010 = 0xFFFFFFF5 (assuming 32-bit)
}

test "LOGOR2 instruction decoding" {
    // Test that LOGOR2 instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE4, // LOGOR2 opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .LOGOR2);
        try testing.expect(inst.length == 1);
    }
}

test "LOGAND2 instruction decoding" {
    // Test that LOGAND2 instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE5, // LOGAND2 opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .LOGAND2);
        try testing.expect(inst.length == 1);
    }
}

test "LOGXOR2 instruction decoding" {
    // Test that LOGXOR2 instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE6, // LOGXOR2 opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .LOGXOR2);
        try testing.expect(inst.length == 1);
    }
}
