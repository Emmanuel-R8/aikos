const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// TYPECHECK: Type check
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleTYPECHECK(vm: *VM, type_code: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
    _ = type_code;
    stack_module.setTopOfStack(vm, 0); // Return NIL
}

/// TYPEMASK_N: Type mask N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleTYPEMASK_N(vm: *VM, mask: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
    _ = mask;
    stack_module.setTopOfStack(vm, 0); // Return NIL
}