const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "FPLUS2 - floating-point addition" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // FPLUS2 currently treats values as integers (placeholder)
    // When floating-point support is added, this will handle FP values correctly
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 3);
    try opcodes.handleFPLUS2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 8); // Simplified integer addition
}

test "FDIFFERENCE - floating-point subtraction" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 3);
    try opcodes.handleFDIFFERENCE(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 7); // Simplified integer subtraction
}

test "FTIMES2 - floating-point multiplication" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 4);
    try opcodes.handleFTIMES2(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 20); // Simplified integer multiplication
}

test "FQUOTIENT - floating-point division" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    try stack.pushStack(&vm, 15);
    try stack.pushStack(&vm, 3);
    try opcodes.handleFQUOTIENT(&vm);

    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 5); // Simplified integer division
}

test "FQUOTIENT - division by zero error" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 0);

    const result = opcodes.handleFQUOTIENT(&vm);
    try testing.expectError(error.DivisionByZero, result);
}

test "FGREATERP - floating-point comparison" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test 5 > 3
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 3);
    try opcodes.handleFGREATERP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1);

    // Test 3 > 5
    try stack.pushStack(&vm, 3);
    try stack.pushStack(&vm, 5);
    try opcodes.handleFGREATERP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0);
}

test "FPLUS2 instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE8, // FPLUS2 opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FPLUS2);
        try testing.expect(inst.length == 1);
    }
}

test "FDIFFERENCE instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE9, // FDIFFERENCE opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FDIFFERENCE);
        try testing.expect(inst.length == 1);
    }
}

test "FTIMES2 instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xEA, // FTIMES2 opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FTIMES2);
        try testing.expect(inst.length == 1);
    }
}

test "FQUOTIENT instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xEB, // FQUOTIENT opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FQUOTIENT);
        try testing.expect(inst.length == 1);
    }
}

test "FGREATERP instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xF2, // FGREATERP opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FGREATERP);
        try testing.expect(inst.length == 1);
    }
}
