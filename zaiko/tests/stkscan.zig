const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "STKSCAN - scan stack for variable" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push atom index on stack
    try stack.pushStack(&vm, 100); // Atom index

    // STKSCAN currently returns NIL (placeholder)
    // When name table lookup is implemented, this will scan frames for variable
    try opcodes.handleSTKSCAN(&vm);

    // Check result is NIL (placeholder behavior)
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0); // NIL
}

test "STKSCAN - different atom indices" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test with different atom indices
    try stack.pushStack(&vm, 0);
    try opcodes.handleSTKSCAN(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0);

    try stack.pushStack(&vm, 65535);
    try opcodes.handleSTKSCAN(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0);
}

test "STKSCAN instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x2F, // STKSCAN opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .STKSCAN);
        try testing.expect(inst.length == 1);
    }
}
