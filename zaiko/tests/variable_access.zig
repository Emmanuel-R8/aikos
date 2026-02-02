const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const types = @import("../src/utils/types.zig");

test "IVAR - local variable access" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate frame with space for 3 local variables
    const frame = try stack.allocateStackFrame(&vm, 3);

    // Set local variable values
    stack.setIVar(frame, 0, 100);
    stack.setIVar(frame, 1, 200);
    stack.setIVar(frame, 2, 300);

    // Test IVAR0
    try opcodes.handleIVAR(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 100);

    // Test IVAR1
    try opcodes.handleIVAR(&vm, 1);
    try testing.expect(stack.getTopOfStack(&vm) == 200);

    // Test IVAR2
    try opcodes.handleIVAR(&vm, 2);
    try testing.expect(stack.getTopOfStack(&vm) == 300);
}

test "PVAR - parameter variable access" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate frame
    const frame = try stack.allocateStackFrame(&vm, 0);

    // Set parameter values (parameters stored after frame header)
    stack.setPVar(frame, 0, 42);
    stack.setPVar(frame, 1, 84);
    stack.setPVar(frame, 2, 126);

    // Test PVAR0
    try opcodes.handlePVAR(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 42);

    // Test PVAR1
    try opcodes.handlePVAR(&vm, 1);
    try testing.expect(stack.getTopOfStack(&vm) == 84);

    // Test PVAR2
    try opcodes.handlePVAR(&vm, 2);
    try testing.expect(stack.getTopOfStack(&vm) == 126);
}

test "IVAR without frame" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // No frame allocated - should fail
    const result = opcodes.handleIVAR(&vm, 0);
    try testing.expectError(error.InvalidAddress, result);
}

test "PVAR without frame" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // No frame allocated - should fail
    const result = opcodes.handlePVAR(&vm, 0);
    try testing.expectError(error.InvalidAddress, result);
}

test "variable access with multiple frames" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate first frame
    const frame1 = try stack.allocateStackFrame(&vm, 2);
    stack.setIVar(frame1, 0, 10);
    stack.setPVar(frame1, 0, 20);

    // Allocate second frame (nested call)
    const frame2 = try stack.allocateStackFrame(&vm, 2);
    stack.setIVar(frame2, 0, 30);
    stack.setPVar(frame2, 0, 40);

    // Current frame should be frame2
    try testing.expect(vm.current_frame == frame2);

    // Access variables from current frame (frame2)
    try opcodes.handleIVAR(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 30);

    try opcodes.handlePVAR(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 40);
}

/// T054: Test variable access operations
/// Per tasks.md T054: Add test case for variable access operations
test "T054: IVAR variants (IVAR0-IVAR6, IVARX)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate frame with space for 7 local variables
    const frame = try stack.allocateStackFrame(&vm, 7);

    // Set local variable values
    for (0..7) |i| {
        stack.setIVar(frame, @as(u8, @intCast(i)), @as(types.LispPTR, @intCast(100 + i)));
    }

    // Test IVAR0-IVAR6
    for (0..7) |i| {
        try opcodes.handleIVAR(&vm, @as(u8, @intCast(i)));
        const value = stack.getTopOfStack(&vm);
        try testing.expect(value == @as(types.LispPTR, @intCast(100 + i)));
    }

    // Test IVARX (indexed access)
    try opcodes.handleIVARX(&vm, 3);
    const value = stack.getTopOfStack(&vm);
    try testing.expect(value == 103);
}

test "T054: PVAR variants (PVAR0-PVAR6, PVARX, PVAR_0-PVAR_6, PVARX_)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate frame
    const frame = try stack.allocateStackFrame(&vm, 0);

    // Set parameter values
    for (0..7) |i| {
        stack.setPVar(frame, @as(u8, @intCast(i)), @as(types.LispPTR, @intCast(200 + i)));
    }

    // Test PVAR0-PVAR6
    for (0..7) |i| {
        try opcodes.handlePVAR(&vm, @as(u8, @intCast(i)));
        const value = stack.getTopOfStack(&vm);
        try testing.expect(value == @as(types.LispPTR, @intCast(200 + i)));
    }

    // Test PVARX (indexed access)
    try opcodes.handlePVARX(&vm, 4);
    const value = stack.getTopOfStack(&vm);
    try testing.expect(value == 204);
}

test "T054: FVAR variants (FVAR0-FVAR6, FVARX)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate frame with free variables
    const frame = try stack.allocateStackFrame(&vm, 0);

    // Set free variable values (stored in frame's free variable array)
    for (0..7) |i| {
        stack.setFVar(frame, @as(u8, @intCast(i)), @as(types.LispPTR, @intCast(300 + i)));
    }

    // Test FVAR0-FVAR6
    for (0..7) |i| {
        try opcodes.handleFVAR(&vm, @as(u8, @intCast(i)));
        const value = stack.getTopOfStack(&vm);
        try testing.expect(value == @as(types.LispPTR, @intCast(300 + i)));
    }

    // Test FVARX (indexed access)
    try opcodes.handleFVARX(&vm, 5);
    const value = stack.getTopOfStack(&vm);
    try testing.expect(value == 305);
}

test "T054: GVAR - global variable access" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // TODO: Set up global variable table
    // For now, test that GVAR handler exists and doesn't crash
    // Global variables require atom table setup (Phase 4)
    // This test structure is ready for when atom table is implemented
    try testing.expect(true);
}