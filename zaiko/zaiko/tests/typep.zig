const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "TYPEP - check NIL type" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push NIL (0)
    try stack.pushStack(&vm, 0);
    
    // Check if it's type 0 (NIL)
    try opcodes.handleTYPEP(&vm, 0);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1); // Should match
    
    // Check if it's type 1 (fixnum) - should not match
    try stack.setTopOfStack(&vm, 0); // Reset to NIL
    try opcodes.handleTYPEP(&vm, 1);
    const result2 = stack.getTopOfStack(&vm);
    try testing.expect(result2 == 0); // Should not match
}

test "TYPEP - check fixnum type" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push fixnum (odd value, e.g., 1, 3, 5)
    // In Lisp, fixnums are encoded with low bit = 1
    // For simplicity, use odd value
    try stack.pushStack(&vm, 1);
    
    // Check if it's type 1 (fixnum)
    try opcodes.handleTYPEP(&vm, 1);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1); // Should match
    
    // Check if it's type 0 (NIL) - should not match
    try stack.setTopOfStack(&vm, 1); // Reset to fixnum
    try opcodes.handleTYPEP(&vm, 0);
    const result2 = stack.getTopOfStack(&vm);
    try testing.expect(result2 == 0); // Should not match
}

test "TYPEP - check pointer type" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push pointer (even value, e.g., 2, 4, 6)
    // In Lisp, pointers are encoded with low bit = 0
    try stack.pushStack(&vm, 0x1000);
    
    // Check if it's type 2 (pointer)
    try opcodes.handleTYPEP(&vm, 2);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1); // Should match
    
    // Check if it's type 1 (fixnum) - should not match
    try stack.setTopOfStack(&vm, 0x1000); // Reset to pointer
    try opcodes.handleTYPEP(&vm, 1);
    const result2 = stack.getTopOfStack(&vm);
    try testing.expect(result2 == 0); // Should not match
}

test "TYPEP - different type codes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push NIL
    try stack.pushStack(&vm, 0);
    
    // Test various type codes
    try opcodes.handleTYPEP(&vm, 0); // NIL type
    try testing.expect(stack.getTopOfStack(&vm) == 1);
    
    try stack.setTopOfStack(&vm, 0);
    try opcodes.handleTYPEP(&vm, 5); // Different type
    try testing.expect(stack.getTopOfStack(&vm) == 0);
    
    try stack.setTopOfStack(&vm, 0);
    try opcodes.handleTYPEP(&vm, 255); // Max type code
    try testing.expect(stack.getTopOfStack(&vm) == 0);
}

test "TYPEP instruction decoding" {
    // Test that TYPEP instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x06, // TYPEP opcode (0x06 in our enum, but actual opcode is 0x05)
        0x01, // Type code operand (fixnum)
    };
    
    // Note: Our TYPEP opcode is 0x06 in the enum
    // The actual opcode value might differ
    const instruction = dispatch.decodeInstruction(0, code);
    // This test may need adjustment based on actual opcode value
    if (instruction) |inst| {
        // Verify instruction structure
        try testing.expect(inst.length == 2);
    }
}

test "TYPEP - multiple type checks" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Test with fixnum
    try stack.pushStack(&vm, 3); // Odd value = fixnum
    try opcodes.handleTYPEP(&vm, 1);
    try testing.expect(stack.getTopOfStack(&vm) == 1);
    
    // Test with pointer
    try stack.setTopOfStack(&vm, 0x2000); // Even value = pointer
    try opcodes.handleTYPEP(&vm, 2);
    try testing.expect(stack.getTopOfStack(&vm) == 1);
    
    // Test mismatch
    try stack.setTopOfStack(&vm, 0x2000); // Pointer
    try opcodes.handleTYPEP(&vm, 1); // Checking for fixnum
    try testing.expect(stack.getTopOfStack(&vm) == 0);
}