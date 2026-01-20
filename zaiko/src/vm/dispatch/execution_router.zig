const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const std = @import("std");

const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;

// Import split execution modules
const execution_constants = @import("execution_constants.zig");
const execution_control = @import("execution_control.zig");
const execution_data = @import("execution_data.zig");
const execution_arithmetic = @import("execution_arithmetic.zig");

/// Route opcode to appropriate handler module
/// Returns jump offset if instruction is a jump, null otherwise
pub fn routeOpcode(vm: *VM, opcode: Opcode, instruction: Instruction) errors.VMError!?i64 {
    // Handle invalid/unknown opcodes
    if (@intFromEnum(opcode) == 0xFF) {
        return error.InvalidOpcode;
    }

    // Try constants first
    if (execution_constants.handleConstants(vm, opcode, instruction)) |result| {
        return result;
    } else |err| {
        if (err != error.NotHandled) return err;
    }

    // Try control flow
    if (execution_control.handleControlFlow(vm, opcode, instruction)) |result| {
        return result;
    } else |err| {
        if (err != error.NotHandled) return err;
    }

    // Try data operations
    std.debug.print("DEBUG ROUTER: Trying data operations for opcode 0x{x}\n", .{@intFromEnum(opcode)});
    if (execution_data.handleDataOperations(vm, opcode, instruction)) |result| {
        std.debug.print("DEBUG ROUTER: Data operations succeeded\n", .{});
        return result;
    } else |err| {
        std.debug.print("DEBUG ROUTER: Data operations returned error: {}\n", .{err});
        if (err != error.NotHandled) return err;
    }

    // Try arithmetic and remaining
    return execution_arithmetic.handleArithmeticAndRemaining(vm, opcode, instruction);
}
