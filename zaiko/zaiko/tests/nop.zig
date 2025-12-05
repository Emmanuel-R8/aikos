const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "NOP - no operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push a value
    try stack.pushStack(&vm, 42);
    
    // Execute NOP
    try opcodes.handleNOP(&vm);
    
    // Stack should be unchanged
    try testing.expect(stack.getTopOfStack(&vm) == 42);
}

test "NOP - multiple NOPs" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push a value
    try stack.pushStack(&vm, 100);
    
    // Execute multiple NOPs
    try opcodes.handleNOP(&vm);
    try opcodes.handleNOP(&vm);
    try opcodes.handleNOP(&vm);
    
    // Stack should still be unchanged
    try testing.expect(stack.getTopOfStack(&vm) == 100);
}

test "NOP instruction decoding" {
    // Test that NOP instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xFE, // NOP opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .NOP);
        try testing.expect(inst.length == 1);
    }
}

test "NOP - with empty stack" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Execute NOP on empty stack (should work fine)
    try opcodes.handleNOP(&vm);
    
    // Stack should still be empty (or TOS should be 0)
    // This depends on stack implementation
}