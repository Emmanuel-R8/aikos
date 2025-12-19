const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "UNWIND - unwind stack frames" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // UNWIND currently does nothing (placeholder)
    // When unwinding is implemented, this will unwind stack frames
    try opcodes.handleUNWIND(&vm, 0);
    
    // For now, just verify it doesn't crash
    try testing.expect(true);
}

test "UNWIND instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x07, // UNWIND opcode
        0x12, // Unwind params low byte
        0x34, // Unwind params high byte
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .UNWIND);
        try testing.expect(inst.length == 3);
        try testing.expect(inst.getWordOperand(0) == 0x3412); // Little-endian
    }
}

test "UNWIND - different unwind parameters" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Test with different unwind parameters
    try opcodes.handleUNWIND(&vm, 0);
    try opcodes.handleUNWIND(&vm, 1);
    try opcodes.handleUNWIND(&vm, 65535);
    
    // For now, just verify it doesn't crash
    try testing.expect(true);
}
