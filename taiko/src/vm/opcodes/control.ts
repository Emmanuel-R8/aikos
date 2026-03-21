// Control flow opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { popStack } from './stack_helpers';

/**
 * JUMPX opcode handler
 * Unconditional jump with 2-byte signed offset
 */
function handleJUMPX(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 1) return null;
    // Operand 0 is treated as the signed 16-bit jump offset in tests
    return instruction.operands[0];
}

/**
 * FJUMPX opcode handler
 * Conditional jump if false (NIL)
 * Per C: FJUMPX macro - jumps if TOPOFSTACK is NIL
 */
function handleFJUMPX(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 1) return null;

    const shouldJump = vm.topOfStack === 0;
    popStack(vm);
    return shouldJump ? instruction.operands[0] : null;
}

/**
 * TJUMPX opcode handler
 * Conditional jump if true (non-NIL)
 * Per C: TJUMPX macro - jumps if TOPOFSTACK is non-NIL
 */
function handleTJUMPX(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 1) return null;

    const shouldJump = vm.topOfStack !== 0;
    popStack(vm);
    return shouldJump ? instruction.operands[0] : null;
}

// Function call handlers are now in function_calls.ts
// Import them to register handlers
import './function_calls';

// Register jump handlers
registerOpcodeHandler(Opcode.JUMPX, handleJUMPX);
registerOpcodeHandler(Opcode.FJUMPX, handleFJUMPX);
registerOpcodeHandler(Opcode.TJUMPX, handleTJUMPX);
