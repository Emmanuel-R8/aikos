const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const std = @import("std");

const VM = stack.VM;

/// INSTANCEP: Instance predicate
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleINSTANCEP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
    stack_module.setTopOfStack(vm, 0); // Return NIL
}

/// SIC: Set instance cell (actually Small Integer Constant)
/// Per C implementation: PUSH(S_POSITIVE | Get_BYTE_PCMAC1);
/// Pushes a small positive integer constant from the 1-byte operand
pub fn handleSIC(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // SIC pushes S_POSITIVE | operand_byte as a small integer constant
    // The index parameter is actually the operand byte value (0-255)
    const value = types_module.S_POSITIVE | @as(types_module.LispPTR, index);
    std.debug.print("SIC: pushing value 0x{x} (S_POSITIVE | 0x{x})\n", .{ value, index });
    try stack_module.pushStack(vm, value);
}

/// SNIC: Set named instance cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSNIC(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // SNIC requires:
    // 1. Value on stack
    // 2. Set named instance cell at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// SICX: Set instance cell X
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSICX(vm: *VM, index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // SICX requires:
    // 1. Value on stack
    // 2. Set instance cell at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}
