// Endianness and byte-swapping utilities
// Handles byte swapping for little-endian hosts and XOR addressing

/**
 * Endianness and byte-swapping utilities
 * Handles conversion between big-endian sysout files and little-endian host
 */
export class EndiannessManager {
    /**
     * Determine if file page needs byte swapping
     *
     * @param file_page File page number
     * @param sysout_size Total sysout file size in bytes
     * @returns true if page needs byte swapping
     */
    static needsByteSwap(file_page: number, sysout_size: number): boolean {
        const swap_boundary = Math.floor(sysout_size / 4) + 1;
        return file_page < swap_boundary;
    }

    /**
     * Byte-swap 32-bit value (FPtoVP entry)
     * Converts big-endian to little-endian
     */
    static swapU32(value: number): number {
        return ((value & 0xFF) << 24) |
            ((value & 0xFF00) << 8) |
            ((value & 0xFF0000) >> 8) |
            ((value & 0xFF000000) >>> 24);
    }

    /**
     * Byte-swap 16-bit value
     */
    static swapU16(value: number): number {
        return ((value & 0xFF) << 8) | ((value & 0xFF00) >> 8);
    }

    /**
     * Apply XOR addressing for byte access
     * Used for instruction decode on little-endian systems
     *
     * @param address Base address
     * @returns XOR-addressed address (address ^ 3)
     */
    static applyXorAddressing(address: number): number {
        return address ^ 3;
    }

    /**
     * Read 16-bit value from memory with XOR addressing (little-endian)
     * Per C emulator: GETBYTE applies XOR addressing, then construct word from bytes
     *
     * @param memory Memory array
     * @param address Byte address
     * @returns 16-bit value
     */
    static readWordXor(memory: Uint8Array, address: number): number {
        const xor_addr0 = address ^ 3;
        const xor_addr1 = (address + 1) ^ 3;
        const byte0 = (xor_addr0 < memory.length) ? memory[ xor_addr0 ] : 0;
        const byte1 = (xor_addr1 < memory.length) ? memory[ xor_addr1 ] : 0;
        return byte0 | (byte1 << 8);
    }

    /**
     * Read 16-bit value from memory without XOR addressing (little-endian)
     *
     * @param memory Memory array
     * @param address Byte address
     * @returns 16-bit value
     */
    static readWordLittleEndian(memory: Uint8Array, address: number): number {
        const byte0 = (address < memory.length) ? memory[ address ] : 0;
        const byte1 = (address + 1 < memory.length) ? memory[ address + 1 ] : 0;
        return byte0 | (byte1 << 8);
    }

    /**
     * Read byte with XOR addressing
     *
     * @param memory Memory array
     * @param address Byte address
     * @returns Byte value or null if out of bounds
     */
    static readByteXor(memory: Uint8Array, address: number): number | null {
        if (address < memory.length) {
            const xor_addr = address ^ 3;
            if (xor_addr < memory.length) {
                return memory[ xor_addr ];
            }
        }
        return null;
    }
}
