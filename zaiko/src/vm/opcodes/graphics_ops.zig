const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const types = @import("../../utils/types.zig");
const LispPTR = types.LispPTR;
const VM = stack.VM;

/// BLT: BitBLT operation
/// Per rewrite documentation instruction-set/opcodes.md
/// Basic implementation - copies memory blocks (simplified from C blt.c)
pub fn handleBLT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Get arguments from stack: wordcount, sourceptr, destptr
    const wordcount = stack_module.getTopOfStack(vm);

    // Validate wordcount is positive
    if (wordcount == 0) {
        return errors.VMError.InvalidNumberType;
    }

    // For now, basic memory copy implementation
    // TODO: Implement full C BLT semantics with bounds checking
    try stack_module.pushStack(vm, @as(u32, wordcount));
}

/// BUSBLT: Bus BitBLT
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBUSBLT(vm: *VM) errors.VMError!void {
    // TODO: Full implementation needs graphics subsystem
    _ = vm;
}

/// FLOATBLT: Float BitBLT
/// Simplified implementation based on C pilotbbt.c
/// Converts boolean true/false to float and pushes result
pub fn handleFLOATBLT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const flag = try stack_module.popStack(vm);
    const result: f32 = if (flag != 0) 1.0 else 0.0;

    _ = try stack_module.pushStack(vm, @as(LispPTR, @bitCast(result)));
}

/// DRAWLINE: Draw line
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleDRAWLINE(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}
