const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "PLUS2 - general addition" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 20);
    
    // Perform PLUS2: 10 + 20 = 30
    try opcodes.handlePLUS2(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 30);
}

test "DIFFERENCE - general subtraction" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 20);
    try stack.pushStack(&vm, 10);
    
    // Perform DIFFERENCE: 20 - 10 = 10
    try opcodes.handleDIFFERENCE(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 10);
}

test "TIMES2 - general multiplication" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 6);
    
    // Perform TIMES2: 5 * 6 = 30
    try opcodes.handleTIMES2(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 30);
}

test "QUOTIENT - general division" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 20);
    try stack.pushStack(&vm, 4);
    
    // Perform QUOTIENT: 20 / 4 = 5
    try opcodes.handleQUOTIENT(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 5);
}

test "QUOTIENT - division by zero error" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and zero
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 0);
    
    // Perform QUOTIENT: should error
    const result = opcodes.handleQUOTIENT(&vm);
    try testing.expectError(error.InvalidAddress, result);
}

test "PLUS2 instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xD4, // PLUS2 opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .PLUS2);
        try testing.expect(inst.length == 1);
    }
}

test "DIFFERENCE - negative result" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values (smaller - larger)
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 10);
    
    // Perform DIFFERENCE: 5 - 10 = -5
    try opcodes.handleDIFFERENCE(&vm);
    
    const result = stack.getTopOfStack(&vm);
    const result_signed = @as(i32, @bitCast(@as(u32, result)));
    try testing.expect(result_signed == -5);
}

test "QUOTIENT - integer division (truncation)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 7);
    try stack.pushStack(&vm, 3);
    
    // Perform QUOTIENT: 7 / 3 = 2 (truncated)
    try opcodes.handleQUOTIENT(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 2);
}
