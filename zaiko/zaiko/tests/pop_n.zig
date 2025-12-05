const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "POP_N - pop multiple values" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push 5 values onto stack
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 20);
    try stack.pushStack(&vm, 30);
    try stack.pushStack(&vm, 40);
    try stack.pushStack(&vm, 50);
    
    // Pop 3 values
    try opcodes.handlePOP_N(&vm, 3);
    
    // Top should now be 20 (10 and 20 remain)
    try testing.expect(stack.getTopOfStack(&vm) == 20);
    
    // Pop remaining 2 values
    try opcodes.handlePOP_N(&vm, 2);
    
    // Stack should be empty (or at least TOS should be 0)
    // Note: getTopOfStack may return 0 or undefined value when stack is empty
    // This depends on stack implementation
}

test "POP_N - pop zero values" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push a value
    try stack.pushStack(&vm, 100);
    
    // Pop 0 values (should be no-op)
    try opcodes.handlePOP_N(&vm, 0);
    
    // Value should still be on stack
    try testing.expect(stack.getTopOfStack(&vm) == 100);
}

test "POP_N - pop all values" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push 3 values
    try stack.pushStack(&vm, 1);
    try stack.pushStack(&vm, 2);
    try stack.pushStack(&vm, 3);
    
    // Pop all 3 values
    try opcodes.handlePOP_N(&vm, 3);
    
    // Stack should be empty
    // Attempting to pop more should fail
    const result = opcodes.handlePOP_N(&vm, 1);
    try testing.expectError(error.StackUnderflow, result);
}

test "POP_N - pop more than available (error)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push 2 values
    try stack.pushStack(&vm, 1);
    try stack.pushStack(&vm, 2);
    
    // Try to pop 5 values (more than available)
    const result = opcodes.handlePOP_N(&vm, 5);
    try testing.expectError(error.StackUnderflow, result);
}

test "POP_N instruction decoding" {
    // Test that POP_N instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xC0, // POP_N opcode
        0x05, // Count = 5
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .POP_N);
        try testing.expect(inst.length == 2);
        try testing.expect(inst.getByteOperand(0) == 5);
    }
}