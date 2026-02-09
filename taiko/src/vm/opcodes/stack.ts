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

/**
 * SWAP opcode handler
 * Swap top two stack values
 * Per C: SWAP macro in maiko/inc/inlineC.h
 *
 * Stack: [tos, tos1] -> [tos1, tos]
 *
 * Algorithm:
 * 1. Get TOPOFSTACK (tos)
 * 2. Get GET_TOS_1 (tos1) - value below tos
 * 3. Swap them: TOPOFSTACK = tos1, GET_TOS_1 = tos
 */
function handleSWAP(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Get TOPOFSTACK
    const tos = vm.topOfStack;

    // Get GET_TOS_1 (value below TOPOFSTACK on stack)
    // This is at CSTKPTRL - 4 (one LispPTR below current)
    const tos1Offset = vm.cstkptrl - 4;
    if (tos1Offset < 0 || tos1Offset + 4 > vm.virtualMemory.length) {
        return null; // Invalid stack position
    }

    const tos1 = MemoryManager.Access.readLispPTR(vm.virtualMemory, tos1Offset);

    // Swap: TOPOFSTACK = tos1, GET_TOS_1 = tos
    vm.topOfStack = tos1;
    MemoryManager.Access.writeLispPTR(vm.virtualMemory, tos1Offset, tos);

    return null;
}

/**
 * NOP opcode handler
 * No operation - just advances PC
 * Per C: NOP macro (no-op)
 *
 * Stack: [] -> []
 */
function handleNOP(vm: VM, instruction: Instruction): number | null {
    // No operation - just advance PC
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.POP, handlePOP);
registerOpcodeHandler(Opcode.POP_N, handlePOP_N);
registerOpcodeHandler(Opcode.SWAP, handleSWAP);
registerOpcodeHandler(Opcode.NOP, handleNOP);