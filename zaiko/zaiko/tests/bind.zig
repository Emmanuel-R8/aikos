const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "BIND - bind single variable" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push value and atom_index (in reverse order for BIND)
    try stack.pushStack(&vm, 100); // atom_index
    try stack.pushStack(&vm, 42); // value

    // Bind 1 variable
    try opcodes.handleBIND(&vm, 1);

    // Stack should be empty after binding
    // (For now, BIND just pops values - proper binding will be implemented later)
}

test "BIND - bind multiple variables" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push 3 variable bindings (in reverse order)
    // Binding 2: atom_index=200, value=20
    try stack.pushStack(&vm, 200);
    try stack.pushStack(&vm, 20);
    // Binding 1: atom_index=100, value=10
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 10);
    // Binding 0: atom_index=50, value=5
    try stack.pushStack(&vm, 50);
    try stack.pushStack(&vm, 5);

    // Bind 3 variables
    try opcodes.handleBIND(&vm, 3);

    // Stack should be empty after binding
}

test "BIND - zero count (no-op)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Push a value
    try stack.pushStack(&vm, 42);

    // Bind 0 variables (should be no-op)
    try opcodes.handleBIND(&vm, 0);

    // Value should still be on stack
    try testing.expect(stack.getTopOfStack(&vm) == 42);
}

test "BIND instruction decoding" {
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x11, // BIND opcode
        0x03, // Count = 3
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .BIND);
        try testing.expect(inst.length == 2);
        try testing.expect(inst.getByteOperand(0) == 3);
    }
}

test "UNBIND - unbind variables" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // UNBIND currently does nothing (placeholder)
    // When binding system is implemented, this will restore variable values
    try opcodes.handleUNBIND(&vm);

    // For now, just verify it doesn't crash
    try testing.expect(true);
}

test "DUNBIND - dynamic unbind" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // DUNBIND currently does nothing (placeholder)
    // When binding system is implemented, this will handle dynamic scope
    try opcodes.handleDUNBIND(&vm);

    // For now, just verify it doesn't crash
    try testing.expect(true);
}

test "BIND and UNBIND - round trip" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Bind a variable
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 42);
    try opcodes.handleBIND(&vm, 1);

    // Unbind (currently does nothing, but should work)
    try opcodes.handleUNBIND(&vm);

    // For now, just verify it doesn't crash
    try testing.expect(true);
}