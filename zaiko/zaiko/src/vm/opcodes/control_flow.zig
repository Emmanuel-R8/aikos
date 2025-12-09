const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// Control flow opcodes

/// JUMP: Unconditional jump
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on returned offset
pub fn handleJUMP(vm: *VM) errors.VMError!void {
    // JUMP is unconditional - dispatch loop handles PC update
    _ = vm;
}

/// FJUMP: False jump (jump if NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleFJUMP(vm: *VM, offset: i8) errors.VMError!void {
    // FJUMP logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// TJUMP: True jump (jump if not NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleTJUMP(vm: *VM, offset: i8) errors.VMError!void {
    // TJUMP logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// JUMPX: Extended jump (16-bit offset)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on returned offset
pub fn handleJUMPX(vm: *VM) errors.VMError!void {
    // JUMPX is unconditional - dispatch loop handles PC update
    _ = vm;
}

/// FJUMPX: Extended false jump (16-bit offset, jump if NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleFJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    // FJUMPX logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// TJUMPX: Extended true jump (16-bit offset, jump if not NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleTJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    // TJUMPX logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// JUMPXX: Extended jump XX
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleJUMPXX(vm: *VM) errors.VMError!void {
    // Similar to JUMPX but with different semantics
    // Placeholder: same as JUMPX
    _ = vm;
}

/// NFJUMPX: Not false jump extended
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNFJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // NFJUMPX jumps if TOS is not false (not NIL)
    // Similar to TJUMPX but inverted logic
    const tos = stack_module.getTopOfStack(vm);
    _ = tos;
    _ = offset;
}

/// NTJUMPX: Not true jump extended
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNTJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // NTJUMPX jumps if TOS is not true (is NIL)
    // Similar to FJUMPX but inverted logic
    const tos = stack_module.getTopOfStack(vm);
    _ = tos;
    _ = offset;
}