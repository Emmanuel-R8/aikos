const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "LLSH1 - left shift by 1" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value
    try stack.pushStack(&vm, 0b1010); // 10
    
    // Shift left by 1: 0b1010 << 1 = 0b10100 (20)
    try opcodes.handleLLSH1(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b10100); // 20
}

test "LLSH8 - left shift by 8" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value
    try stack.pushStack(&vm, 0x12); // 18
    
    // Shift left by 8: 0x12 << 8 = 0x1200 (4608)
    try opcodes.handleLLSH8(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x1200); // 4608
}

test "LRSH1 - right shift by 1" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value
    try stack.pushStack(&vm, 0b10100); // 20
    
    // Shift right by 1: 0b10100 >> 1 = 0b1010 (10)
    try opcodes.handleLRSH1(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1010); // 10
}

test "LRSH8 - right shift by 8" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value
    try stack.pushStack(&vm, 0x1200); // 4608
    
    // Shift right by 8: 0x1200 >> 8 = 0x12 (18)
    try opcodes.handleLRSH8(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x12); // 18
}

test "LLSH1 - with high bit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value with high bit set
    try stack.pushStack(&vm, 0x80000000);
    
    // Shift left by 1: 0x80000000 << 1 = 0x00000000 (overflow, wraps)
    try opcodes.handleLLSH1(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x00000000);
}

test "LRSH1 - logical shift (zero-fill)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value with high bit set
    try stack.pushStack(&vm, 0x80000000);
    
    // Logical right shift by 1: 0x80000000 >> 1 = 0x40000000 (zero-fill, not sign-extend)
    try opcodes.handleLRSH1(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x40000000);
}

test "LLSH8 instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE1, // LLSH8 opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .LLSH8);
        try testing.expect(inst.length == 1);
    }
}

test "LRSH8 instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE3, // LRSH8 opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .LRSH8);
        try testing.expect(inst.length == 1);
    }
}

test "Shift opcodes - round trip" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value
    try stack.pushStack(&vm, 0x1234);
    
    // Shift left by 8
    try opcodes.handleLLSH8(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0x123400);
    
    // Shift right by 8 (should restore original)
    try opcodes.handleLRSH8(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0x1234);
}