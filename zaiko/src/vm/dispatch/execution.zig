const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;

// Import router
const execution_router = @import("execution_router.zig");

pub fn executeInstruction(vm: *VM, instruction: Instruction) errors.VMError!?i64 {
    return executeOpcodeWithOperands(vm, instruction.opcode, instruction);
}

/// Execute opcode handler with operands
/// Per contracts/vm-core-interface.zig
/// Returns jump offset if instruction is a jump, null otherwise
pub fn executeOpcodeWithOperands(vm: *VM, opcode: Opcode, instruction: Instruction) errors.VMError!?i64 {
    return execution_router.routeOpcode(vm, opcode, instruction);
}