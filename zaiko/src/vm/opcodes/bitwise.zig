const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// Bitwise Operations
/// LOGOR2: Logical OR (bitwise OR)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs bitwise OR, pushes result
pub fn handleLOGOR2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Perform bitwise OR
    const result: LispPTR = a | b;

    try stack_module.pushStack(vm, result);
}

/// LOGAND2: Logical AND (bitwise AND)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs bitwise AND, pushes result
pub fn handleLOGAND2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Perform bitwise AND
    const result: LispPTR = a & b;

    try stack_module.pushStack(vm, result);
}

/// LOGXOR2: Logical XOR (bitwise XOR)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs bitwise XOR, pushes result
pub fn handleLOGXOR2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Perform bitwise XOR
    const result: LispPTR = a ^ b;

    try stack_module.pushStack(vm, result);
}

/// LSH: Logical shift
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops shift amount and value, performs logical shift, pushes result
/// Positive shift amount = left shift, negative = right shift
pub fn handleLSH(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop shift amount and value
    const shift_amount = try stack_module.popStack(vm);
    const value = try stack_module.popStack(vm);

    // Convert shift amount to signed integer
    const shift_signed = @as(i32, @bitCast(@as(u32, shift_amount)));

    // Perform shift operation
    const result: LispPTR = if (shift_signed >= 0) blk: {
        // Left shift
        const shift_u = @as(u5, @intCast(shift_signed));
        // Clamp shift amount to avoid undefined behavior
        const safe_shift = @min(shift_u, 31);
        break :blk value << safe_shift;
    } else blk: {
        // Right shift (logical right shift, zero-fill)
        const shift_u = @as(u5, @intCast(-shift_signed));
        const safe_shift = @min(shift_u, 31);
        break :blk value >> safe_shift;
    };

    try stack_module.pushStack(vm, result);
}

/// LLSH1: Logical left shift by 1
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS left by 1 bit
pub fn handleLLSH1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value << 1;

    stack_module.setTopOfStack(vm, result);
}

/// LLSH8: Logical left shift by 8
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS left by 8 bits
pub fn handleLLSH8(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value << 8;

    stack_module.setTopOfStack(vm, result);
}

/// LRSH1: Logical right shift by 1
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS right by 1 bit (logical, zero-fill)
pub fn handleLRSH1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value >> 1;

    stack_module.setTopOfStack(vm, result);
}

/// LRSH8: Logical right shift by 8
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS right by 8 bits (logical, zero-fill)
pub fn handleLRSH8(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value >> 8;

    stack_module.setTopOfStack(vm, result);
}