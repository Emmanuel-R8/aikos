// Instruction decoder
// Decodes instructions from virtual memory with XOR addressing

import type { VM } from '../vm';
import { Opcode, type Instruction, getInstructionLength, decodeOpcode } from './opcode';
import { MemoryManager } from '../memory/manager';

/**
 * Fetch instruction byte from virtual memory with XOR addressing
 * Per C: Get_BYTE_PCMAC0 = Get_BYTE(PCMAC) = GETBYTE(PCMAC) = *(unsigned char *)(3 ^ (UNSIGNED)(PCMAC))
 */
export function fetchInstructionByte(vm: VM, pc: number): number | null {
    if (vm.virtualMemory === null) {
        return null;
    }
    return MemoryManager.Access.readByteXor(vm.virtualMemory, pc);
}

/**
 * Decode full instruction with operands from virtual memory
 * Reads bytecode from virtual_memory at PC address with XOR addressing (BYTESWAP mode)
 */
export function decodeInstructionFromMemory(vm: VM, pc: number): Instruction | null {
    if (vm.virtualMemory === null) {
        return null;
    }

    const memory = vm.virtualMemory;

    // Fetch opcode byte with XOR addressing
    const opcodeByte = MemoryManager.Access.readByteXor(memory, pc);
    if (opcodeByte === null) {
        return null;
    }

    const opcode = decodeOpcode(opcodeByte);
    if (opcode === null) {
        return null; // Unknown opcode
    }

    const length = getInstructionLength(opcode);

    // Check if we have enough bytes
    if (pc + length > memory.length) {
        return null; // Not enough bytes
    }

    // Read operands (if any) with XOR addressing
    const operands = new Uint8Array(Math.max(0, length - 1));
    for (let i = 0; i < operands.length; i++) {
        const operandOffset = pc + i + 1;
        const byte = MemoryManager.Access.readByteXor(memory, operandOffset);
        if (byte === null) {
            return null; // Invalid address
        }
        operands[ i ] = byte;
    }

    return {
        opcode,
        operands,
        length,
    };
}
