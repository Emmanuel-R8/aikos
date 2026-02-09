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

    // Debug: Check bounds and XOR address
    const xorAddr = pc ^ 3;
    if (pc >= memory.length) {
        console.error(`decodeInstructionFromMemory: PC=0x${pc.toString(16)} out of bounds (memory length=0x${memory.length.toString(16)})`);
        return null;
    }
    if (xorAddr >= memory.length) {
        // XOR address out of bounds - this means we're trying to read from a sparse/unloaded page
        // This can happen when code spans multiple pages and some are sparse
        // In a real sysout, this shouldn't happen for valid code, but we handle it gracefully
        console.error(`decodeInstructionFromMemory: XOR address=0x${xorAddr.toString(16)} out of bounds (memory length=0x${memory.length.toString(16)})`);
        console.error(`decodeInstructionFromMemory: Sparse page detected - code page not loaded`);
        console.error(`decodeInstructionFromMemory: Execution stopped at PC=0x${pc.toString(16)} - sparse page encountered`);
        return null;
    }

    // Check if the page containing the XOR address is sparse (all zeros)
    const pageStart = Math.floor(xorAddr / 512) * 512;
    const pageEnd = Math.min(pageStart + 512, memory.length);
    if (pageEnd > pageStart) {
        const pageBytes = memory.slice(pageStart, pageEnd);
        const isSparse = pageBytes.every(b => b === 0);
        if (isSparse) {
            console.error(`decodeInstructionFromMemory: Page at 0x${pageStart.toString(16)} is sparse (all zeros)`);
            console.error(`decodeInstructionFromMemory: Execution stopped at PC=0x${pc.toString(16)} - sparse page encountered`);
            return null;
        }
    }

    // Fetch opcode byte with XOR addressing
    const opcodeByte = MemoryManager.Access.readByteXor(memory, pc);
    if (opcodeByte === null) {
        console.error(`decodeInstructionFromMemory: Failed to read opcode byte at PC=0x${pc.toString(16)}, XOR addr=0x${xorAddr.toString(16)}`);
        console.error(`decodeInstructionFromMemory: Raw byte at PC=0x${memory[pc]?.toString(16) ?? 'undefined'}, at XOR addr=0x${memory[xorAddr]?.toString(16) ?? 'undefined'}`);
        return null;
    }

    const opcode = decodeOpcode(opcodeByte);
    if (opcode === null) {
        console.error(`decodeInstructionFromMemory: Unknown opcode 0x${opcodeByte.toString(16)} at PC=0x${pc.toString(16)} (XOR addr=0x${xorAddr.toString(16)}, raw byte=0x${memory[xorAddr]?.toString(16) ?? 'undefined'})`);
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
