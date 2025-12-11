const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// BLT: BitBLT operation
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBLT(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// BUSBLT: Bus BitBLT
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBUSBLT(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// FLOATBLT: Float BitBLT
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFLOATBLT(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// DRAWLINE: Draw line
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleDRAWLINE(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}