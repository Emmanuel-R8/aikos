const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const ByteCode = types.ByteCode;

/// Constants
pub const BYTES_PER_PAGE = 512;

/// Address translation utilities
pub const AddressManager = struct {
    /// Convert LispPTR (DLword offset) to byte offset
    pub fn lispPtrToByte(lisp_ptr: LispPTR) usize {
        return @as(usize, lisp_ptr) * 2;
    }

    /// Convert byte offset to LispPTR (DLword offset)
    pub fn byteToLispPtr(byte_offset: usize) LispPTR {
        return @intCast(byte_offset / 2);
    }

    /// Calculate virtual page for byte offset
    pub fn getVirtualPage(byte_offset: usize) usize {
        return byte_offset / BYTES_PER_PAGE;
    }

    /// Calculate offset within virtual page
    pub fn getPageOffset(byte_offset: usize) usize {
        return byte_offset % BYTES_PER_PAGE;
    }

    /// Calculate virtual address base for page
    pub fn getVirtualAddressBase(vpage: usize) usize {
        return vpage * BYTES_PER_PAGE;
    }
};

/// FPtoVP table management
pub const FPtoVPManager = struct {
    /// Lookup file page for virtual page
    pub fn getFilePageForVirtualPage(fptovp_table: []const u32, vpage: usize) ?u16 {
        for (fptovp_table, 0..) |entry, i| {
            const entry_vpage = entry & 0xFFFF;
            if (entry_vpage == vpage) {
                return @intCast(i);
            }
        }
        return null;
    }

    /// Get page OK flag for file page
    pub fn getPageOK(fptovp_table: []const u32, file_page: usize) u16 {
        if (file_page < fptovp_table.len) {
            return @intCast((fptovp_table[file_page] >> 16) & 0xFFFF);
        }
        return 0;
    }

    /// Calculate file offset for file page
    pub fn getFileOffset(file_page: usize) usize {
        return file_page * BYTES_PER_PAGE;
    }
};

/// Endianness and byte-swapping utilities
pub const EndiannessManager = struct {
    /// Determine if file page needs byte swapping
    pub fn needsByteSwap(file_page: usize, sysout_size: usize) bool {
        const swap_boundary = (sysout_size / 4) + 1;
        return file_page < swap_boundary;
    }

    /// Byte-swap 32-bit value (FPtoVP entry)
    pub fn swapU32(value: u32) u32 {
        return ((value & 0xFF) << 24) |
            ((value & 0xFF00) << 8) |
            ((value & 0xFF0000) >> 8) |
            ((value & 0xFF000000) >> 24);
    }

    /// Apply XOR addressing for byte access
    pub fn applyXorAddressing(base_ptr: [*]u8, offset: usize) [*]u8 {
        const base_addr = @intFromPtr(base_ptr);
        const xor_addr = base_addr ^ 3; // XOR with 3 for byte addressing
        return @ptrFromInt(xor_addr + offset);
    }

    /// Get XOR address for byte access
    pub fn getXorAddress(address: usize) usize {
        return address ^ 3;
    }

    /// Read 16-bit value from memory with XOR addressing (little-endian)
    /// Per C emulator: GETBYTE applies XOR addressing, then construct word from bytes
    pub fn readWordXor(memory: []const u8, address: usize) u16 {
        const xor_addr0 = address ^ 3;
        const xor_addr1 = (address + 1) ^ 3;
        const byte0 = if (xor_addr0 < memory.len) memory[xor_addr0] else 0;
        const byte1 = if (xor_addr1 < memory.len) memory[xor_addr1] else 0;
        return @as(u16, byte0) | (@as(u16, byte1) << 8);
    }

    /// Read 16-bit value from memory without XOR addressing (little-endian)
    pub fn readWordLittleEndian(memory: []const u8, address: usize) u16 {
        const byte0 = if (address < memory.len) memory[address] else 0;
        const byte1 = if (address + 1 < memory.len) memory[address + 1] else 0;
        return @as(u16, byte0) | (@as(u16, byte1) << 8);
    }
};

/// Memory access utilities
pub const MemoryAccessManager = struct {
    /// Safe memory read with bounds checking
    pub fn readByte(memory: []const u8, offset: usize) ?u8 {
        if (offset < memory.len) {
            return memory[offset];
        }
        return null;
    }

    /// Safe memory read with XOR addressing
    pub fn readByteXor(memory: []const u8, offset: usize) ?u8 {
        if (offset < memory.len) {
            const xor_addr = offset ^ 3;
            if (xor_addr < memory.len) {
                return memory[xor_addr];
            }
        }
        return null;
    }

    /// Read instruction bytes (no XOR, matches C trace format)
    pub fn readInstructionBytes(memory: []const u8, pc: usize, count: usize) []const u8 {
        const end = @min(pc + count, memory.len);
        if (pc < end) {
            return memory[pc..end];
        }
        return &[_]u8{};
    }

    /// Read JUMPX offset from memory with XOR addressing
    /// JUMPX opcode is at PC, operands at PC+1 and PC+2
    /// Per C emulator: Get_BYTE_PCMAC1 and Get_BYTE_PCMAC2 apply XOR addressing
    pub fn readJumpOffset(memory: []const u8, pc: usize) i16 {
        const byte0 = EndiannessManager.readWordXor(memory, pc + 1);
        return @as(i16, @bitCast(byte0));
    }

    /// Read JUMPX offset without XOR addressing (raw bytes)
    pub fn readJumpOffsetRaw(memory: []const u8, pc: usize) i16 {
        const byte0 = EndiannessManager.readWordLittleEndian(memory, pc + 1);
        return @as(i16, @bitCast(byte0));
    }
};
