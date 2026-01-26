const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "GVAR opcode handler" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // GVAR currently returns NIL (placeholder)
    // When atom tables are implemented, this will access global variables
    try opcodes.handleGVAR(&vm, 0);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0); // NIL (placeholder)
}

test "GVAR instruction decoding" {
    // Test that GVAR instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x60, // GVAR opcode
        0x12, // Atom index low byte
        0x34, // Atom index high byte
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .GVAR);
        try testing.expect(inst.length == 3);
        try testing.expect(inst.getWordOperand(0) == 0x3412); // Little-endian
    }
}

test "GVAR with different atom indices" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Test with different atom indices
    try opcodes.handleGVAR(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 0);
    
    try opcodes.handleGVAR(&vm, 100);
    try testing.expect(stack.getTopOfStack(&vm) == 0);
    
    try opcodes.handleGVAR(&vm, 65535);
    try testing.expect(stack.getTopOfStack(&vm) == 0);
}