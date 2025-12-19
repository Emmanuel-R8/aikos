const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const function_header = @import("../src/data/function_header.zig");
const types = @import("../src/utils/types.zig");

test "FVAR - free variable access" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Create a function header with 2 PVars and 1 free variable
    var fnheader = function_header.FunctionHeader{
        .stkmin = 100,
        .na = 2,
        .pv = 2,
        .startpc = 0,
        .framename = 0,
        .ntsize = 0,
        .nlocals = 0,
        .fvaroffset = 0,
    };

    // Allocate frame
    const frame = try stack.allocateStackFrame(&vm, 0);
    frame.fnheader = @as(types.LispPTR, @intFromPtr(&fnheader));

    // Set up PVars (2 PVars = indices 0, 1)
    stack.setPVar(frame, 0, 10);
    stack.setPVar(frame, 1, 20);

    // Free variables start after PVars (pv + 1 = 3 slots)
    // Each free variable is 2 words (low word and high word)
    // FVAR0 should be at offset (pv + 1) * sizeof(LispPTR) = 3 * 4 = 12 bytes
    // But we need to access it as 2 DLwords (low and high)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base_addr = frame_addr + frame_size;
    
    // Set free variable 0: value = 0x12345678
    // Low word = 0x5678, High word = 0x1234
    const fvar0_low_addr = pvar_base_addr + (3 * @sizeOf(types.LispPTR)); // After 3 PVars
    const fvar0_high_addr = fvar0_low_addr + @sizeOf(types.DLword);
    
    const fvar0_low_ptr: *types.DLword = @as(*types.DLword, @ptrFromInt(fvar0_low_addr));
    const fvar0_high_ptr: *types.DLword = @as(*types.DLword, @ptrFromInt(fvar0_high_addr));
    
    fvar0_low_ptr.* = 0x5678;
    fvar0_high_ptr.* = 0x1234;

    // Test FVAR0
    try opcodes.handleFVAR(&vm, 0);
    const value = stack.getTopOfStack(&vm);
    // Expected: (0x1234 << 16) | 0x5678 = 0x12345678, masked = 0x12345678 & 0xFFFFFFFE = 0x12345678
    try testing.expect(value == 0x12345678);
}

test "FVAR - multiple free variables" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Create function header with 1 PVar and 2 free variables
    var fnheader = function_header.FunctionHeader{
        .stkmin = 100,
        .na = 1,
        .pv = 1,
        .startpc = 0,
        .framename = 0,
        .ntsize = 0,
        .nlocals = 0,
        .fvaroffset = 0,
    };

    const frame = try stack.allocateStackFrame(&vm, 0);
    frame.fnheader = @as(types.LispPTR, @intFromPtr(&fnheader));

    // Set PVar
    stack.setPVar(frame, 0, 100);

    // Set free variables
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base_addr = frame_addr + frame_size;
    
    // FVAR0: value = 0x11111111
    const fvar0_low_addr = pvar_base_addr + (2 * @sizeOf(types.LispPTR)); // After 2 PVars (pv + 1)
    const fvar0_high_addr = fvar0_low_addr + @sizeOf(types.DLword);
    const fvar0_low_ptr: *types.DLword = @as(*types.DLword, @ptrFromInt(fvar0_low_addr));
    const fvar0_high_ptr: *types.DLword = @as(*types.DLword, @ptrFromInt(fvar0_high_addr));
    fvar0_low_ptr.* = 0x1111;
    fvar0_high_ptr.* = 0x1111;

    // FVAR1: value = 0x22222222
    const fvar1_low_addr = fvar0_high_addr + @sizeOf(types.DLword);
    const fvar1_high_addr = fvar1_low_addr + @sizeOf(types.DLword);
    const fvar1_low_ptr: *types.DLword = @as(*types.DLword, @ptrFromInt(fvar1_low_addr));
    const fvar1_high_ptr: *types.DLword = @as(*types.DLword, @ptrFromInt(fvar1_high_addr));
    fvar1_low_ptr.* = 0x2222;
    fvar1_high_ptr.* = 0x2222;

    // Test FVAR0
    try opcodes.handleFVAR(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 0x11111111);

    // Test FVAR1
    try opcodes.handleFVAR(&vm, 1);
    try testing.expect(stack.getTopOfStack(&vm) == 0x22222222);
}

test "FVAR without frame" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // No frame allocated - should fail
    const result = opcodes.handleFVAR(&vm, 0);
    try testing.expectError(error.InvalidAddress, result);
}

test "FVAR without function header" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Allocate frame but don't set function header
    _ = try stack.allocateStackFrame(&vm, 0);
    // Frame has fnheader = 0 by default

    // Should fail because no function header
    const result = opcodes.handleFVAR(&vm, 0);
    try testing.expectError(error.InvalidAddress, result);
}

test "FVAR instruction decoding" {
    const dispatch = @import("../src/vm/dispatch.zig");
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x50, // FVAR0 opcode
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FVAR0);
        try testing.expect(inst.length == 1);
    }
}

test "FVARX instruction decoding" {
    const dispatch = @import("../src/vm/dispatch.zig");
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0x57, // FVARX opcode
        0x05, // Index = 5
    };

    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .FVARX);
        try testing.expect(inst.length == 2);
        try testing.expect(inst.getByteOperand(0) == 5);
    }
}
