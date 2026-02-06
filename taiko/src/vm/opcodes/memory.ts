// Memory operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';

/**
 * GETBASE_N opcode handler
 * Get base address
 */
function handleGETBASE_N(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement GETBASE_N
    return null;
}

/**
 * PUTBASE_N opcode handler
 * Put base address
 */
function handlePUTBASE_N(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement PUTBASE_N
    return null;
}

/**
 * GETBASEPTR_N opcode handler
 * Get base pointer
 */
function handleGETBASEPTR_N(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement GETBASEPTR_N
    return null;
}

/**
 * PUTBASEPTR_N opcode handler
 * Put base pointer
 */
function handlePUTBASEPTR_N(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement PUTBASEPTR_N
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.GETBASE_N, handleGETBASE_N);
registerOpcodeHandler(Opcode.PUTBASE_N, handlePUTBASE_N);
registerOpcodeHandler(Opcode.GETBASEPTR_N, handleGETBASEPTR_N);
registerOpcodeHandler(Opcode.PUTBASEPTR_N, handlePUTBASEPTR_N);
