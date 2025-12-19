const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "SWAP - swap top two values" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 20);
    
    // Swap them
    try opcodes.handleSWAP(&vm);
    
    // Top should now be 10 (was 20)
    try testing.expect(stack.getTopOfStack(&vm) == 10);
    
    // Pop and check second value
    const top = try stack.popStack(&vm);
    try testing.expect(top == 10);
    
    // Second should now be 20 (was 10)
    try testing.expect(stack.getTopOfStack(&vm) == 20);
}

test "SWAP - multiple swaps" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 200);
    
    // Swap them
    try opcodes.handleSWAP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 100);
    
    // Swap again (should restore original order)
    try opcodes.handleSWAP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 200);
}

test "SWAP - error on insufficient values" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push only one value
    try stack.pushStack(&vm, 50);
    
    // Try to swap (should fail - need 2 values)
    const result = opcodes.handleSWAP(&vm);
    try testing.expectError(error.StackUnderflow, result);
}

test "SWAP - error on empty stack" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Try to swap on empty stack (should fail)
    const result = opcodes.handleSWAP(&vm);
    try testing.expectError(error.StackUnderflow, result);
}

test "SWAP instruction decoding" {
    // Test that SWAP instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xFD, // SWAP opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .SWAP);
        try testing.expect(inst.length == 1);
    }
}

test "SWAP - with different value types" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push NIL and a pointer
    try stack.pushStack(&vm, 0); // NIL
    try stack.pushStack(&vm, 0x1000); // Pointer
    
    // Swap them
    try opcodes.handleSWAP(&vm);
    
    // Top should now be NIL
    try testing.expect(stack.getTopOfStack(&vm) == 0);
    
    // Pop and check second value
    const top = try stack.popStack(&vm);
    try testing.expect(top == 0);
    try testing.expect(stack.getTopOfStack(&vm) == 0x1000);
}
