const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "ACONST - push atom constant" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // ACONST currently returns atom_index as placeholder
    // When atom tables are implemented, this will return actual atom object
    try opcodes.handleACONST(&vm, 100);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 100); // Placeholder: atom_index
}

test "GCONST - push global constant" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // GCONST currently returns atom_index as placeholder
    // When atom tables are implemented, this will return actual atom object
    try opcodes.handleGCONST(&vm, 200);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 200); // Placeholder: atom_index
}

test "ACONST - different atom indices" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test with different atom indices
    try opcodes.handleACONST(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 0);

    try opcodes.handleACONST(&vm, 65535);
    try testing.expect(stack.getTopOfStack(&vm) == 65535);
}

test "GCONST - different atom indices" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test with different atom indices
    try opcodes.handleGCONST(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 0);

    try opcodes.handleGCONST(&vm, 65535);
    try testing.expect(stack.getTopOfStack(&vm) == 65535);
}

test "ACONST instruction decoding" {
    // Test that ACONST instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x67, // ACONST opcode
        0x12, // Atom index low byte
        0x34, // Atom index high byte
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .ACONST);
        try testing.expect(inst.length == 3);
        try testing.expect(inst.getWordOperand(0) == 0x3412); // Little-endian
    }
}

test "GCONST instruction decoding" {
    // Test that GCONST instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x6F, // GCONST opcode
        0x56, // Atom index low byte
        0x78, // Atom index high byte
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .GCONST);
        try testing.expect(inst.length == 3);
        try testing.expect(inst.getWordOperand(0) == 0x7856); // Little-endian
    }
}