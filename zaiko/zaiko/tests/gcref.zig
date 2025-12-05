const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "GCREF - ADDREF operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push object pointer
    try stack.pushStack(&vm, 0x1000);
    
    // Perform GCREF with ADDREF (ref_type = 0)
    try opcodes.handleGCREF(&vm, 0);
    
    // Object pointer should still be on stack
    try testing.expect(stack.getTopOfStack(&vm) == 0x1000);
}

test "GCREF - DELREF operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push object pointer
    try stack.pushStack(&vm, 0x2000);
    
    // Perform GCREF with DELREF (ref_type = 1)
    try opcodes.handleGCREF(&vm, 1);
    
    // Object pointer should still be on stack
    try testing.expect(stack.getTopOfStack(&vm) == 0x2000);
}

test "GCREF - STKREF operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push object pointer
    try stack.pushStack(&vm, 0x3000);
    
    // Perform GCREF with STKREF (ref_type = 2)
    try opcodes.handleGCREF(&vm, 2);
    
    // Object pointer should still be on stack
    try testing.expect(stack.getTopOfStack(&vm) == 0x3000);
}

test "GCREF instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x15, // GCREF opcode
        0x01, // ref_type = DELREF
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .GCREF);
        try testing.expect(inst.length == 2);
        try testing.expect(inst.getByteOperand(0) == 1);
    }
}

test "GCREF - different ref types" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Test various ref types
    try stack.pushStack(&vm, 0x1000);
    try opcodes.handleGCREF(&vm, 0); // ADDREF
    try testing.expect(stack.getTopOfStack(&vm) == 0x1000);
    
    try stack.setTopOfStack(&vm, 0x2000);
    try opcodes.handleGCREF(&vm, 255); // Max ref_type
    try testing.expect(stack.getTopOfStack(&vm) == 0x2000);
}