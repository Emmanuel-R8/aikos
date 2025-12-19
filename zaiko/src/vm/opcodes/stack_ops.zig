const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// Stack manipulation opcodes
/// PUSH: Push value onto stack
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PUSH typically pushes a constant value from instruction operand
/// For now, simplified version pushes 0
pub fn handlePUSH(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // PUSH typically pushes a constant value from instruction operand
    // TODO: Extract constant value from instruction operand
    // For now, push 0 (will need to get value from instruction operand)
    try stack_module.pushStack(vm, 0);
}

/// POP: Pop value from stack
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePOP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    _ = try stack_module.popStack(vm); // Discard value
}

/// POP_N: Pop N values from stack
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops count values from stack, discarding them
pub fn handlePOP_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop count values from stack
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        _ = stack_module.popStack(vm) catch |err| {
            // If we can't pop enough values, return error
            return switch (err) {
                errors_module.VMError.StackUnderflow => err,
                else => errors_module.VMError.StackUnderflow,
            };
        };
    }
}

/// SWAP: Swap top two stack values
/// Per rewrite documentation instruction-set/opcodes.md
/// Swaps the top two values on the stack
pub fn handleSWAP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop top two values
    const top = try stack_module.popStack(vm);
    const second = try stack_module.popStack(vm);

    // Push them back in swapped order
    try stack_module.pushStack(vm, top);
    try stack_module.pushStack(vm, second);
}

/// NOP: No operation
/// Per rewrite documentation instruction-set/opcodes.md
/// Does nothing, just advances PC
pub fn handleNOP(vm: *VM) errors.VMError!void {
    // No operation - just advance PC (handled by dispatch loop)
    _ = vm;
}
