const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

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

/// SIC: Set instance cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSIC(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // SIC requires:
    // 1. Value on stack
    // 2. Set instance cell at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
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