/// Memory access utilities with XOR addressing for BYTESWAP mode
/// Per ENDIANNESS_FINDINGS.md: C emulator uses XOR addressing in BYTESWAP mode
/// - Bytes: base ^ 3
/// - Words: base ^ 2
/// This compensates for byte-swapped memory layout after 32-bit longword swapping

const types = @import("types.zig");
const errors = @import("errors.zig");

const DLword = types.DLword;
const ByteCode = types.ByteCode;

/// Apply XOR addressing for byte access (BYTESWAP mode)
/// C: GETBYTE(base) = *(unsigned char *)(3 ^ (UNSIGNED)(base))
/// For address 0x307898, reads from 0x30789b (base ^ 3)
pub fn applyXORAddressingByte(address: usize) usize {
    return address ^ 3;
}

/// Apply XOR addressing for word access (BYTESWAP mode)
/// C: GETWORD(base) = *(DLword *)(2 ^ (UNSIGNED)(base))
/// For address 0x307898, reads from 0x30789a (base ^ 2)
pub fn applyXORAddressingWord(address: usize) usize {
    return address ^ 2;
}

/// Read byte from memory with XOR addressing (BYTESWAP mode)
/// C: GETBYTE(base) = *(unsigned char *)(3 ^ (UNSIGNED)(base))
pub fn getByte(virtual_memory: []const u8, address: usize) errors.VMError!ByteCode {
    const xor_address = applyXORAddressingByte(address);
    if (xor_address >= virtual_memory.len) {
        return error.InvalidAddress;
    }
    return virtual_memory[xor_address];
}

/// Read word (DLword) from memory with XOR addressing (BYTESWAP mode)
/// C: GETWORD(base) = *(DLword *)(2 ^ (UNSIGNED)(base))
/// Note: This reads a single byte at the XOR address, not a full word
/// For full word reading, use Get_DLword which manually constructs from bytes
pub fn getWordAddress(virtual_memory: []const u8, address: usize) errors.VMError!usize {
    const xor_address = applyXORAddressingWord(address);
    if (xor_address >= virtual_memory.len) {
        return error.InvalidAddress;
    }
    return xor_address;
}

/// Read DLword manually from bytes (matching C Get_DLword)
/// C: Get_DLword(ptr) = (Get_BYTE(ptr) << 8) | Get_BYTE((ptr) + 1)
/// This manually constructs the 16-bit value from bytes with XOR addressing
pub fn getDLword(virtual_memory: []const u8, address: usize) errors.VMError!DLword {
    const byte0 = try getByte(virtual_memory, address);
    const byte1 = try getByte(virtual_memory, address + 1);
    return (@as(DLword, byte0) << 8) | byte1;
}
