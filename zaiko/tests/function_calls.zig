const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const function_module = @import("../src/vm/function.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const types = @import("../src/utils/types.zig");

test "function call - basic frame setup" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Create a function header
    var func_header = function_module.FunctionHeader{
        .stkmin = 0,
        .na = 0,
        .pv = 2, // 3 parameters (pv + 1)
        .startpc = 100,
        .framename = 0,
        .ntsize = 0,
        .nlocals = 2, // 2 local variables
        .fvaroffset = 0,
    };

    // Call function
    try function_module.callFunction(&vm, &func_header, 0);

    // Check that new frame was created
    try testing.expect(vm.current_frame != null);
    try testing.expect(vm.pc == 100); // PC set to function start

    // Check frame has function header set
    const frame = vm.current_frame.?;
    try testing.expect(frame.fnheader != 0);
}

// T034: Test function call and return execution matching C emulator
// Tests FN0-FN4 handlers, frame management, and RETURN opcode
test "T034: function call - FN0-FN4 frame setup matching C" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Simulate FN1 call (1 argument)
    // C: OPFN sets up frame, saves PC, pushes TOS, sets up BF/FX markers
    vm.pc = 100; // Current PC

    // Create function header (placeholder - atom table lookup deferred)
    var func_header = function_module.FunctionHeader{
        .stkmin = 0,
        .na = 1, // 1 argument
        .pv = 0, // 1 parameter (pv + 1)
        .startpc = 200, // Function code starts at 200
        .framename = 1234, // Atom index
        .ntsize = 0,
        .nlocals = 2,
        .fvaroffset = 0,
    };

    // Call function (simulating FN1 handler)
    try function_module.callFunction(&vm, &func_header, 1);

    // Verify frame was created
    try testing.expect(vm.current_frame != null);
    const new_frame = vm.current_frame.?;

    // Verify PC was saved in previous frame (if existed) and set to function start
    try testing.expect(vm.pc == 200); // PC set to function start

    // Verify function header is set in frame
    try testing.expect(new_frame.fnheader != 0);
}

test "T034: function return - frame restoration matching C" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Set up caller frame
    const caller_frame = try stack.allocateStackFrame(&vm, 2);
    caller_frame.pcoffset = 150; // Save PC in caller
    vm.pc = 200; // Current PC in callee

    // Set return value on stack
    const return_value: types.LispPTR = 42;
    stack.setTopOfStack(&vm, return_value);

    // Return from function (simulating RETURN handler)
    const returned = try function_module.returnFromFunction(&vm);

    // Verify return value
    try testing.expect(returned == return_value);

    // Verify frame was restored
    try testing.expect(vm.current_frame == caller_frame);

    // Verify PC was restored
    try testing.expect(vm.pc == 150);
}

test "function return - basic" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Set up initial frame
    const frame1 = try stack.allocateStackFrame(&vm, 2);
    frame1.pcoffset = 50; // Save PC
    vm.pc = 100; // Current PC

    // Set return value on stack
    try stack.pushStack(&vm, 42);

    // Return from function
    const return_value = try function_module.returnFromFunction(&vm);

    // Check return value
    try testing.expect(return_value == 42);

    // Check PC was restored
    try testing.expect(vm.pc == 50);
}

test "function call and return - nested" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Set up outer frame
    const outer_frame = try stack.allocateStackFrame(&vm, 1);
    outer_frame.pcoffset = 10;
    vm.pc = 20;

    // Create function header for inner function
    var func_header = function_module.FunctionHeader{
        .stkmin = 0,
        .na = 0,
        .pv = 0,
        .startpc = 200,
        .framename = 0,
        .ntsize = 0,
        .nlocals = 1,
        .fvaroffset = 0,
    };

    // Call inner function
    try function_module.callFunction(&vm, &func_header, 0);

    // Check inner frame is active
    try testing.expect(vm.current_frame != null);
    try testing.expect(vm.current_frame != outer_frame);
    try testing.expect(vm.pc == 200);

    // Set return value
    try stack.pushStack(&vm, 99);

    // Return from inner function
    const return_value = try function_module.returnFromFunction(&vm);

    // Check return value
    try testing.expect(return_value == 99);

    // Check outer frame restored
    try testing.expect(vm.current_frame == outer_frame);
    try testing.expect(vm.pc == 10); // PC restored from outer frame
}

test "function call with arguments" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Create function header expecting 2 parameters
    var func_header = function_module.FunctionHeader{
        .stkmin = 0,
        .na = 2,
        .pv = 1, // 2 parameters (pv + 1)
        .startpc = 300,
        .framename = 0,
        .ntsize = 0,
        .nlocals = 0,
        .fvaroffset = 0,
    };

    // Set up arguments (would normally be on stack)
    const args = [_]types.LispPTR{ 10, 20 };

    // Setup frame with arguments
    const frame = try function_module.setupFunctionFrame(&vm, &func_header, &args);

    // Check arguments were set
    try testing.expect(stack.getPVar(frame, 0) == 10);
    try testing.expect(stack.getPVar(frame, 1) == 20);
}

test "RETURN opcode handler" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Set up frame
    const frame = try stack.allocateStackFrame(&vm, 0);
    frame.pcoffset = 5;
    vm.pc = 10;

    // Set return value on stack
    try stack.pushStack(&vm, 777);

    // Execute RETURN opcode
    try opcodes.handleRETURN(&vm);

    // Check return value is still on stack
    try testing.expect(stack.getTopOfStack(&vm) == 777);

    // Check PC was restored
    try testing.expect(vm.pc == 5);
}

test "function return - top level" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Set up frame with no link (top level)
    const frame = try stack.allocateStackFrame(&vm, 0);
    frame.link = 0; // No previous frame
    frame.pcoffset = 0;

    // Set return value
    try stack.pushStack(&vm, 123);

    // Return from top level function
    const return_value = try function_module.returnFromFunction(&vm);

    // Check return value
    try testing.expect(return_value == 123);

    // Check frame cleared (top level return)
    try testing.expect(vm.current_frame == null);
    try testing.expect(vm.pc == 0);
}
