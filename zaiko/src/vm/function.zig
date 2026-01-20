const std = @import("std");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const types = @import("../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const FX = stack.FX;
const DLword = types.DLword;

/// Function header structure
/// Per data-model.md
pub const FunctionHeader = packed struct {
    stkmin: types.DLword, // Minimum stack size
    na: types.DLword, // Number of arguments
    pv: types.DLword, // Number of PVars
    startpc: types.DLword, // Starting PC
    framename: LispPTR, // Frame name atom
    ntsize: types.DLword, // Name table size
    nlocals: types.DLword, // Number of local variables
    fvaroffset: types.DLword, // FVar offset
    // Name table and code follow in memory
};

/// Call function with arguments
/// Per contracts/vm-core-interface.zig and rewrite documentation vm-core/function-calls.md
pub fn callFunction(vm: *VM, func_header: *FunctionHeader, arg_count: u8) errors.VMError!void {
    // Save current PC (will be restored on return)
    if (vm.current_frame) |frame| {
        // Save PC in current frame before calling new function
        // CRITICAL: PC is stored in frame.pc field (DLword)
        frame.pc = @as(DLword, @truncate(vm.pc));
    }

    // Allocate new frame for called function
    const frame_size = @as(u32, @intCast(func_header.nlocals));
    const new_frame = try stack.allocateStackFrame(vm, frame_size);

    // Set function header in frame
    // CRITICAL: fnheader is 24-bit (non-BIGVM): hi2fnheader (8 bits) + lofnheader (16 bits)
    const fnheader_addr = @as(LispPTR, @truncate(@intFromPtr(func_header)));
    new_frame.lofnheader = @as(DLword, @truncate(fnheader_addr));
    new_frame.hi1fnheader_hi2fnheader = @as(u16, @truncate(fnheader_addr >> 16));

    // Set up arguments in PVar area
    // Arguments are popped from stack and stored as parameters
    var i: u8 = 0;
    while (i < arg_count and i < func_header.pv + 1) : (i += 1) {
        // Arguments are already on stack, we'll access them via PVar
        // For now, we'll set them to 0 and they'll be set by the caller
        stack.setPVar(new_frame, i, 0);
    }

    // Set PC to function start (will be set by dispatch loop)
    vm.pc = func_header.startpc;
}

/// Return from function
/// Per contracts/vm-core-interface.zig and rewrite documentation vm-core/function-calls.md
pub fn returnFromFunction(vm: *VM) errors.VMError!LispPTR {
    const stack_module = @import("stack.zig");

    // Get return value from stack
    const return_value = stack_module.getTopOfStack(vm);

    // Get current frame
    const current_frame = vm.current_frame orelse {
        // No frame - just return the value
        return return_value;
    };

    // Get previous frame from activation link
    const previous_frame_addr = stack.getAlink(current_frame);
    if (previous_frame_addr == 0) {
        // No previous frame - this is top level return
        vm.current_frame = null;
        vm.pc = 0; // End execution
        return return_value;
    }

    // Restore previous frame from stack allocation
    // CRITICAL: alink is a DLword offset from stack_base, not from virtual_memory
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_end_addr = @intFromPtr(vm.stack_end);

    // alink is DLword offset from stack_base
    const previous_frame_byte_offset = stack_base_addr - (@as(usize, @intCast(previous_frame_addr)) * 2);

    // Check bounds
    if (previous_frame_byte_offset < stack_end_addr or previous_frame_byte_offset + @sizeOf(FX) > stack_base_addr) {
        // Invalid frame address - just return
        vm.current_frame = null;
        vm.pc = 0;
        return return_value;
    }

    const previous_frame: *align(1) FX = @ptrFromInt(previous_frame_byte_offset);
    vm.current_frame = previous_frame;

    // Restore PC from previous frame
    vm.pc = stack.getPC(previous_frame);

    return return_value;
}

/// Setup function frame
/// Per contracts/vm-core-interface.zig
pub fn setupFunctionFrame(vm: *VM, func_header: *FunctionHeader, args: []LispPTR) errors.VMError!*FX {
    // Allocate stack frame
    const frame_size = @as(usize, func_header.nlocals);
    const frame = try stack.allocateStackFrame(vm, frame_size);

    // Set function header
    frame.fnheader = @as(LispPTR, @intFromPtr(func_header));

    // Set up arguments
    var i: usize = 0;
    while (i < args.len and i < func_header.pv + 1) : (i += 1) {
        stack.setPVar(frame, i, args[i]);
    }

    return frame;
}
