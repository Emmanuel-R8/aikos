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