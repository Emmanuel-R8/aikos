const std = @import("std");
const testing = std.testing;
const dispatch_module = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "instruction fetch" {
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0xC0, 0xC1, 0xC2 };
    
    const opcode0 = dispatch_module.fetchInstruction(0, code);
    try testing.expect(opcode0 == 0xC0);
    
    const opcode1 = dispatch_module.fetchInstruction(1, code);
    try testing.expect(opcode1 == 0xC1);
}

test "opcode decode" {
    const opcode = dispatch_module.decodeOpcode(0xC0);
    try testing.expect(opcode == .IPLUS2);
}

test "decode instruction with operands" {
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0x20, 0x05 }; // JUMP with offset 5
    const instruction = dispatch_module.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .JUMP);
        try testing.expect(inst.length == 2);
        try testing.expect(inst.operands.len == 1);
        try testing.expect(inst.getByteOperand(0) == 0x05);
    }
}

test "instruction operand extraction" {
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0xB0, 0x12, 0x34 }; // JUMPX with 16-bit offset
    const instruction = dispatch_module.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.getWordOperand(0) == 0x3412); // Little-endian
    }
}

test "constant opcodes NIL and T" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const stack_module = @import("../src/vm/stack.zig");
    
    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Test NIL opcode
    const nil_code: []const types.ByteCode = &[_]types.ByteCode{ 0x68 }; // NIL
    try dispatch_module.dispatch(&vm, nil_code);
    try testing.expect(stack_module.getTopOfStack(&vm) == 0);
    
    // Test T opcode
    const t_code: []const types.ByteCode = &[_]types.ByteCode{ 0x69 }; // T
    try dispatch_module.dispatch(&vm, t_code);
    try testing.expect(stack_module.getTopOfStack(&vm) == 1);
}

test "dispatch loop - simple program" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const stack_module = @import("../src/vm/stack.zig");
    
    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Simple program: push NIL, push T
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x68, // NIL
        0x69, // T
    };
    
    // Should execute without error
    try dispatch_module.dispatch(&vm, code);
    try testing.expect(stack_module.getTopOfStack(&vm) == 1); // T is on top
}

test "dispatch loop - arithmetic program" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const stack_module = @import("../src/vm/stack.zig");
    
    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Program: push 10, push 20, add
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x68, // NIL (we'll use as placeholder - actual implementation needs PUSH with value)
        // For now, test with manual stack setup
    };
    
    // Manual setup for testing
    try stack_module.pushStack(&vm, 10);
    try stack_module.pushStack(&vm, 20);
    
    // Execute IPLUS2 directly
    const opcodes_module = @import("../src/vm/opcodes.zig");
    try opcodes_module.handleIPLUS2(&vm);
    
    try testing.expect(stack_module.getTopOfStack(&vm) == 30);
}

test "instruction length calculation" {
    // Test that instruction lengths are correct
    try testing.expect(dispatch_module.getInstructionLength(.NIL) == 1);
    try testing.expect(dispatch_module.getInstructionLength(.T) == 1);
    try testing.expect(dispatch_module.getInstructionLength(.JUMP) == 2);
    try testing.expect(dispatch_module.getInstructionLength(.JUMPX) == 3);
    try testing.expect(dispatch_module.getInstructionLength(.IPLUS2) == 1);
    try testing.expect(dispatch_module.getInstructionLength(.IVARX) == 2);
}