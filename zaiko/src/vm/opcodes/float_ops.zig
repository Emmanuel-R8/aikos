const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// UBFLOAT1: Unbox float 1
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUBFLOAT1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper floating-point implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
}

/// UBFLOAT2: Unbox float 2
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUBFLOAT2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper floating-point implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
}

/// UBFLOAT3: Unbox float 3
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUBFLOAT3(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper floating-point implementation
    _ = arg;
    const value = stack_module.getTopOfStack(vm);
    _ = value;
}
