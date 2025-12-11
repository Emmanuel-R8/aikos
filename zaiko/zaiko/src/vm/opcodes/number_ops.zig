const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// MAKENUMBER: Create number object
/// Per rewrite documentation instruction-set/opcodes.md
/// Creates a number object from value on stack
/// For small integers, encodes as fixnum (odd address)
pub fn handleMAKENUMBER(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Get value from stack
    const value = stack_module.getTopOfStack(vm);

    // Check if value is already a fixnum (odd address)
    if ((value & 1) != 0) {
        // Already a fixnum, no conversion needed
        return;
    }

    // Check if value is NIL (0)
    if (value == 0) {
        // NIL is not a number, keep as-is
        return;
    }

    // Try to encode as fixnum if it's a small integer
    // Fixnums are encoded as (value << 1) | 1
    // Small integers typically fit in 15 bits (signed: -16384 to 16383)
    const value_signed = @as(i32, @bitCast(@as(u32, value)));

    // Check if value fits in fixnum range
    // Fixnum range: typically -16384 to 16383 (15 bits signed)
    if (value_signed >= -16384 and value_signed <= 16383) {
        // Encode as fixnum: (value << 1) | 1
        const fixnum_value = (@as(u32, @bitCast(@as(i32, value_signed))) << 1) | 1;
        stack_module.setTopOfStack(vm, @as(LispPTR, fixnum_value));
        return;
    }

    // For larger integers or floats, would need bignum or float object creation
    // TODO: Implement bignum and float object creation
    // For now, keep value as-is (may be a pointer to number object)
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

/// BOXIPLUS: Box integer plus
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBOXIPLUS(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result = a + b;
    try stack_module.pushStack(vm, result);
}

/// BOXIDIFFERENCE: Box integer difference
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBOXIDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result = a - b;
    try stack_module.pushStack(vm, result);
}