const std = @import("std");
const testing = std.testing;
const stack_module = @import("../src/vm/stack.zig");
const types = @import("../src/utils/types.zig");

test "stack frame allocation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test frame allocation
    const frame = try stack_module.allocateStackFrame(&vm, 10);
    try testing.expect(frame != null);

    // Test activation link
    const link = stack_module.getActivationLink(frame);
    try testing.expect(link == 0); // First frame has no link
}

test "stack push and pop" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test push
    try stack_module.pushStack(&vm, 42);
    try testing.expect(stack_module.getTopOfStack(&vm) == 42);

    // Test push multiple values
    try stack_module.pushStack(&vm, 100);
    try testing.expect(stack_module.getTopOfStack(&vm) == 100);

    // Test pop
    const value = try stack_module.popStack(&vm);
    try testing.expect(value == 100);
    try testing.expect(stack_module.getTopOfStack(&vm) == 42);

    // Test pop again
    const value2 = try stack_module.popStack(&vm);
    try testing.expect(value2 == 42);
}

// T033: Test stack operations matching C emulator results
// Tests 32-bit LispPTR storage, overflow/underflow detection
test "T033: stack operations - 32-bit LispPTR storage matching C" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test that LispPTR values are stored as 2 DLwords (32-bit)
    // C: PushStack stores LispPTR as 2 DLwords (low word, high word)
    const test_value: types.LispPTR = 0x12345678; // 32-bit value
    try stack_module.pushStack(&vm, test_value);

    // Verify value can be retrieved correctly
    const retrieved = stack_module.getTopOfStack(&vm);
    try testing.expect(retrieved == test_value);

    // Test pop retrieves full 32-bit value
    const popped = try stack_module.popStack(&vm);
    try testing.expect(popped == test_value);

    // Test multiple pushes/pops maintain correct order
    try stack_module.pushStack(&vm, 0x11111111);
    try stack_module.pushStack(&vm, 0x22222222);
    try stack_module.pushStack(&vm, 0x33333333);

    try testing.expect(stack_module.getTopOfStack(&vm) == 0x33333333);
    try testing.expect(try stack_module.popStack(&vm) == 0x33333333);
    try testing.expect(try stack_module.popStack(&vm) == 0x22222222);
    try testing.expect(try stack_module.popStack(&vm) == 0x11111111);

    // Test stack underflow detection
    const underflow = stack_module.popStack(&vm);
    try testing.expectError(error.StackUnderflow, underflow);
}

test "stack overflow detection" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack_module.VM.init(allocator, 10); // Small stack
    defer vm.deinit();

    // Fill stack
    var i: u32 = 0;
    while (i < 10) : (i += 1) {
        try stack_module.pushStack(&vm, i);
    }

    // Next push should fail
    const result = stack_module.pushStack(&vm, 999);
    try testing.expectError(error.StackOverflow, result);
}

test "stack underflow detection" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack_module.VM.init(allocator, 1024);
    defer vm.deinit();

    // Pop from empty stack should fail
    const result = stack_module.popStack(&vm);
    try testing.expectError(error.StackOverflow, result);
}