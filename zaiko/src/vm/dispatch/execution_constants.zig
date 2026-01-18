const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const opcodes = @import("../opcodes.zig");

const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;

// ============================================================================
// Execution - Constants Opcodes
// ============================================================================

/// Handle constants opcodes
/// Returns jump offset if instruction is a jump, null if handled with no jump
/// Returns error.NotHandled if opcode doesn't match this category
pub fn handleConstants(vm: *VM, opcode: @import("instruction.zig").Opcode, instruction: Instruction) errors.VMError!?i64 {
    switch (opcode) {
        // Constants
        .NIL => {
            const stack_module = @import("../stack.zig");
            try stack_module.tosPush(vm, 0); // PUSH(NIL)
            return null;
        },
        .T => {
            const stack_module = @import("../stack.zig");
            try stack_module.tosPush(vm, 1); // PUSH(T) (placeholder ATOM_T)
            return null;
        },
        .CONST_0 => {
            // C: case 0152: PUSHATOM(S_POSITIVE); /* '0 */
            // S_POSITIVE = 0xE0000 (small positive integer base)
            const stack_module = @import("../stack.zig");
            const LispPTR = @import("../../utils/types.zig").LispPTR;
            const S_POSITIVE: LispPTR = 0xE0000;
            try stack_module.tosPush(vm, S_POSITIVE); // PUSHATOM(S_POSITIVE)
            return null;
        },
        .CONST_1 => {
            // C: case 0153: PUSHATOM(0xE0001); /* '1 */
            // 0xE0001 = S_POSITIVE | 1 (small positive integer 1)
            const stack_module = @import("../stack.zig");
            try stack_module.tosPush(vm, 0xE0001); // PUSHATOM(0xE0001)
            return null;
        },
        .ACONST => {
            try opcodes.handleACONST(vm, instruction.getWordOperand(0));
            return null;
        },
        .SIC => {
            _ = try opcodes.handleSIC(vm, instruction.getByteOperand(0));
            return null;
        },
        .SNIC => {
            _ = try opcodes.handleSNIC(vm, instruction.getByteOperand(0));
            return null;
        },
        .SICX => {
            _ = try opcodes.handleSICX(vm, instruction.getWordOperand(0));
            return null;
        },
        .GCONST => {
            try opcodes.handleGCONST(vm, instruction.getWordOperand(0));
            return null;
        },
        else => return error.NotHandled, // Not a constants opcode
    }
}
