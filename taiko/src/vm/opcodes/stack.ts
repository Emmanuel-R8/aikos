// Stack operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { popStack } from './stack_helpers';
import { MemoryManager } from '../memory/manager';

/**
 * POP opcode handler
 * Pops value from stack
 * Per C: POP macro in maiko/inc/tosfns.h
 */
function handlePOP(vm: VM, instruction: Instruction): number | null {
    popStack(vm);
    return null;
}

/**
 * POP_N opcode handler
 * Pops N values from stack
 * Per C: POPN macro
 */
function handlePOP_N(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Read N from operand (PC+1 with XOR addressing)
    const nByte = MemoryManager.Access.readByteXor(vm.virtualMemory, vm.pc + 1);
    if (nByte === null) return null;

    const n = nByte;

    // Pop N values
    for (let i = 0; i < n; i++) {
        popStack(vm);
    }

    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.POP, handlePOP);
registerOpcodeHandler(Opcode.POP_N, handlePOP_N);
