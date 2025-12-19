const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "NTYPX - get type code" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push NIL
    try stack.pushStack(&vm, 0);
    
    // Get type code: NIL should be type 0
    try opcodes.handleNTYPX(&vm);
    const type_code = stack.getTopOfStack(&vm);
    try testing.expect(type_code == 0);
}

test "NTYPX - get type code for fixnum" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push fixnum (odd value)
    try stack.pushStack(&vm, 1);
    
    // Get type code: fixnum should be type 1
    try opcodes.handleNTYPX(&vm);
    const type_code = stack.getTopOfStack(&vm);
    try testing.expect(type_code == 1);
}

test "NTYPX - get type code for pointer" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push pointer (even value)
    try stack.pushStack(&vm, 0x1000);
    
    // Get type code: pointer should be type 2
    try opcodes.handleNTYPX(&vm);
    const type_code = stack.getTopOfStack(&vm);
    try testing.expect(type_code == 2);
}

test "NTYPX instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x04, // NTYPX opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .NTYPX);
        try testing.expect(inst.length == 1);
    }
}

test "DTEST - test atom equality" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value matching atom_index (placeholder behavior)
    try stack.pushStack(&vm, 100);
    
    // Test if value equals atom at index 100
    try opcodes.handleDTEST(&vm, 100);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1); // Should match
}

test "DTEST - test atom inequality" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value not matching atom_index
    try stack.pushStack(&vm, 200);
    
    // Test if value equals atom at index 100
    try opcodes.handleDTEST(&vm, 100);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0); // Should not match
}

test "DTEST instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x06, // DTEST opcode
        0x12, // Atom index low byte
        0x34, // Atom index high byte
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .DTEST);
        try testing.expect(inst.length == 3);
        try testing.expect(inst.getWordOperand(0) == 0x3412); // Little-endian
    }
}
