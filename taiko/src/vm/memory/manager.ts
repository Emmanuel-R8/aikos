// Centralized memory management module
// Provides unified interface for all memory operations

import type { LispPTR, DLword } from '../../utils/types';
import { AddressManager } from './address';
import { FPtoVPManager } from './fptovp';
import { EndiannessManager } from './endianness';
import { BYTESPER_PAGE } from '../../utils/constants';

/**
 * Memory access utilities with bounds checking
 */
export class MemoryAccessManager {
    /**
     * Safe memory read with bounds checking
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @returns Byte value or null if out of bounds
     */
    static readByte(memory: Uint8Array, offset: number): number | null {
        if (offset >= 0 && offset < memory.length) {
            return memory[ offset ];
        }
        return null;
    }

    /**
     * Safe memory read with XOR addressing
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @returns Byte value or null if out of bounds
     */
    static readByteXor(memory: Uint8Array, offset: number): number | null {
        return EndiannessManager.readByteXor(memory, offset);
    }

    /**
     * Read instruction bytes (no XOR, matches C trace format)
     *
     * @param memory Memory array
     * @param pc Program counter (byte offset)
     * @param count Number of bytes to read
     * @returns Array of bytes
     */
    static readInstructionBytes(memory: Uint8Array, pc: number, count: number): Uint8Array {
        const end = Math.min(pc + count, memory.length);
        if (pc < end && pc >= 0) {
            return memory.slice(pc, end);
        }
        return new Uint8Array(0);
    }

    /**
     * Read 16-bit DLword from memory (little-endian)
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @returns DLword value
     */
    static readDLword(memory: Uint8Array, offset: number): DLword {
        if (offset >= 0 && offset + 1 < memory.length) {
            return memory[ offset ] | (memory[ offset + 1 ] << 8);
        }
        return 0;
    }

    /**
     * Read 16-bit DLword from memory with XOR addressing
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @returns DLword value
     */
    static readDLwordXor(memory: Uint8Array, offset: number): DLword {
        return EndiannessManager.readWordXor(memory, offset);
    }

    /**
     * Read 32-bit LispPTR from memory (little-endian, two DLwords)
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @returns LispPTR value
     */
    static readLispPTR(memory: Uint8Array, offset: number): LispPTR {
        const lo = this.readDLword(memory, offset);
        const hi = this.readDLword(memory, offset + 2);
        return (hi << 16) | lo;
    }

    /**
     * Write 16-bit DLword to memory (little-endian)
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @param value DLword value to write
     */
    static writeDLword(memory: Uint8Array, offset: number, value: DLword): void {
        if (offset >= 0 && offset + 1 < memory.length) {
            memory[ offset ] = value & 0xFF;
            memory[ offset + 1 ] = (value >> 8) & 0xFF;
        }
    }

    /**
     * Write 32-bit LispPTR to memory (little-endian, two DLwords)
     *
     * @param memory Memory array
     * @param offset Byte offset
     * @param value LispPTR value to write
     */
    static writeLispPTR(memory: Uint8Array, offset: number, value: LispPTR): void {
        this.writeDLword(memory, offset, value & 0xFFFF);
        this.writeDLword(memory, offset + 2, (value >> 16) & 0xFFFF);
    }

    /**
     * Read JUMPX offset from memory with XOR addressing
     * JUMPX opcode is at PC, operands at PC+1 and PC+2
     * Per C emulator: Get_BYTE_PCMAC1 and Get_BYTE_PCMAC2 apply XOR addressing
     *
     * @param memory Memory array
     * @param pc Program counter (byte offset)
     * @returns Signed 16-bit jump offset
     */
    static readJumpOffset(memory: Uint8Array, pc: number): number {
        const word = EndiannessManager.readWordXor(memory, pc + 1);
        // Convert to signed 16-bit
        return (word << 16) >> 16;
    }

    /**
     * Read JUMPX offset without XOR addressing (raw bytes)
     *
     * @param memory Memory array
     * @param pc Program counter (byte offset)
     * @returns Signed 16-bit jump offset
     */
    static readJumpOffsetRaw(memory: Uint8Array, pc: number): number {
        const word = EndiannessManager.readWordLittleEndian(memory, pc + 1);
        // Convert to signed 16-bit
        return (word << 16) >> 16;
    }
}

/**
 * Centralized memory manager
 * Provides unified interface combining all memory utilities
 */
export class MemoryManager {
    static readonly Address = AddressManager;
    static readonly FPtoVP = FPtoVPManager;
    static readonly Endianness = EndiannessManager;
    static readonly Access = MemoryAccessManager;
}
