const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// BIN: Binary input
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBIN(vm: *VM) errors.VMError!void {
    // BIN requires:
    // 1. Stream on stack
    // 2. Read binary data
    // 3. Push value

    // TODO: Proper implementation needs I/O subsystem
    // Placeholder: return NIL
    _ = vm;
}

/// BOUT: Binary output
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBOUT(vm: *VM) errors.VMError!void {
    // BOUT requires:
    // 1. Value on stack
    // 2. Stream on stack
    // 3. Write binary data

    // TODO: Proper implementation needs I/O subsystem
    // Placeholder: pop values
    const stack_module = @import("../stack.zig");
    const value = try stack_module.popStack(vm);
    const stream = try stack_module.popStack(vm);
    _ = value;
    _ = stream;
}

/// RAID: RAID operation
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleRAID(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs I/O subsystem
    _ = vm;
}