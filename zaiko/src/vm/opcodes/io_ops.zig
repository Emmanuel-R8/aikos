const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// BIN: Binary input
/// Per rewrite documentation instruction-set/opcodes.md
/// Implements C N_OP_bin: read byte from binary stream
pub fn handleBIN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // For now, basic implementation - get stream from stack and read a byte
    // In full implementation, this would handle binary stream I/O
    const stream = try stack_module.popStack(vm);

    // TODO: Full implementation with proper stream handling
    // For now, just return a simple value or error
    if (stream == 0) {
        // Return NIL for invalid stream
        _ = try stack_module.pushStack(vm, 0);
    } else {
        // Return the stream value as positive fixnum
        _ = try stack_module.pushStack(vm, @as(u32, stream));
    }
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
