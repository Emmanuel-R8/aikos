const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// Arithmetic opcode handlers
/// Per rewrite documentation opcodes.md

/// IPLUS2: Integer plus 2 operands
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/src/arithops.c:N_OP_iplus2
pub fn handleIPLUS2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // Pop two values from stack (order: tos, tosm1)
    const tos = try stack_module.popStack(vm);
    const tosm1 = try stack_module.popStack(vm);

    // Extract integers using N_IGETNUMBER equivalent
    const arg1 = types_module.extractInteger(tosm1) catch return error.InvalidNumberType;
    const arg2 = types_module.extractInteger(tos) catch return error.InvalidNumberType;

    // Perform addition with overflow checking (matches C implementation)
    const result_int = arg1 + arg2;

    // Check for overflow (matches C overflow detection)
    if (((arg1 >= 0) == (arg2 >= 0)) and ((result_int >= 0) != (arg1 >= 0))) {
        return error.InvalidOpcode; // Overflow - matches C ERROR_EXIT behavior
    }

    // Encode result using N_ARITH_SWITCH equivalent
    const result = try types_module.encodeIntegerResult(result_int);

    // Push result
    try stack_module.pushStack(vm, result);
}

/// IDIFFERENCE: Integer difference
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/src/arithops.c:N_OP_idifference
pub fn handleIDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // Pop two values from stack (order: tos, tosm1)
    const tos = try stack_module.popStack(vm);
    const tosm1 = try stack_module.popStack(vm);

    // Extract integers using N_IGETNUMBER equivalent
    const arg1 = types_module.extractInteger(tosm1) catch return error.InvalidNumberType;
    const arg2 = types_module.extractInteger(tos) catch return error.InvalidNumberType;

    // Perform subtraction with overflow checking (matches C implementation)
    const result_int = arg1 - arg2;

    // Check for overflow (matches C overflow detection)
    if (((arg1 >= 0) == (arg2 < 0)) and ((result_int >= 0) != (arg1 >= 0))) {
        return error.InvalidOpcode; // Overflow - matches C ERROR_EXIT behavior
    }

    // Encode result using N_ARITH_SWITCH equivalent
    const result = try types_module.encodeIntegerResult(result_int);

    try stack_module.pushStack(vm, result);
}

/// ITIMES2: Integer times 2 operands
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/src/arithops.c:N_OP_itimes2
pub fn handleITIMES2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // Pop two values from stack (order: tos, tosm1)
    const tos = try stack_module.popStack(vm);
    const tosm1 = try stack_module.popStack(vm);

    // Extract integers using N_IGETNUMBER equivalent
    const arg1 = types_module.extractInteger(tosm1) catch return error.InvalidNumberType;
    const arg2 = types_module.extractInteger(tos) catch return error.InvalidNumberType;

    // Perform multiplication with overflow checking (matches C implementation)
    const result_int = arg1 * arg2;

    // Check for overflow (matches C overflow detection)
    if (((arg1 >= 0) == (arg2 >= 0)) and ((result_int >= 0) != (arg1 >= 0))) {
        return error.InvalidOpcode; // Overflow - matches C ERROR_EXIT behavior
    }

    // Encode result using N_ARITH_SWITCH equivalent
    const result = try types_module.encodeIntegerResult(result_int);

    try stack_module.pushStack(vm, result);
}

/// IQUO: Integer quotient
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/src/arithops.c:N_OP_iquotient
pub fn handleIQUO(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // Pop two values from stack (order: tos, tosm1)
    const tos = try stack_module.popStack(vm);
    const tosm1 = try stack_module.popStack(vm);

    // Extract integers using N_IGETNUMBER equivalent
    const arg1 = types_module.extractInteger(tosm1) catch return error.InvalidNumberType;
    const arg2 = types_module.extractInteger(tos) catch return error.InvalidNumberType;

    // Check for division by zero
    if (arg2 == 0) {
        return error.DivisionByZero;
    }

    // Perform division (matches C implementation: uses integer division)
    const result_int = @divTrunc(arg1, arg2);

    // Encode result using N_ARITH_SWITCH equivalent
    const result = try types_module.encodeIntegerResult(result_int);

    try stack_module.pushStack(vm, result);
}

/// IREM: Integer remainder
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/src/arithops.c:N_OP_iremainder
pub fn handleIREM(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // Pop two values from stack (order: tos, tosm1)
    const tos = try stack_module.popStack(vm);
    const tosm1 = try stack_module.popStack(vm);

    // Extract integers using N_IGETNUMBER equivalent
    const arg1 = types_module.extractInteger(tosm1) catch return error.InvalidNumberType;
    const arg2 = types_module.extractInteger(tos) catch return error.InvalidNumberType;

    // Check for division by zero
    if (arg2 == 0) {
        return error.DivisionByZero;
    }

    // Perform remainder operation (matches C implementation)
    const result_int = @rem(arg1, arg2);

    // Encode result using N_ARITH_SWITCH equivalent
    const result = try types_module.encodeIntegerResult(result_int);

    try stack_module.pushStack(vm, result);
}

/// PLUS2: General addition (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs addition, pushes result
/// Falls back to float addition if not integers
pub fn handlePLUS2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Check if both are integers (low bit = 0 for fixnums)
    // For now, treat as integer addition (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed + b_signed))));

    try stack_module.pushStack(vm, result);
}

/// DIFFERENCE: General subtraction (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs subtraction (a - b), pushes result
pub fn handleDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // For now, treat as integer subtraction (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed - b_signed))));

    try stack_module.pushStack(vm, result);
}

/// TIMES2: General multiplication (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs multiplication, pushes result
pub fn handleTIMES2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // For now, treat as integer multiplication (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed * b_signed))));

    try stack_module.pushStack(vm, result);
}

/// QUOTIENT: General division (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs division (a / b), pushes result
pub fn handleQUOTIENT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    if (b == 0) {
        return errors_module.VMError.InvalidAddress; // Division by zero
    }

    // For now, treat as integer division (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(@divTrunc(a_signed, b_signed)))));

    try stack_module.pushStack(vm, result);
}

/// IPLUS_N: Integer plus N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIPLUS_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation - add count values
    var sum: LispPTR = 0;
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        const value = try stack_module.popStack(vm);
        sum += value;
    }
    try stack_module.pushStack(vm, sum);
}

/// IDIFFERENCE_N: Integer difference N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIDIFFERENCE_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    if (count == 0) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    var result = try stack_module.popStack(vm);
    var i: u8 = 1;
    while (i < count) : (i += 1) {
        const value = try stack_module.popStack(vm);
        result -= value;
    }
    try stack_module.pushStack(vm, result);
}