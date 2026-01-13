const types = @import("../../utils/types.zig");
const opcode_module = @import("opcode.zig");

const ByteCode = types.ByteCode;
const LispPTR = types.LispPTR;
const Opcode = opcode_module.Opcode;

/// Decoded instruction with operands
/// Note: operands are stored inline to avoid lifetime issues with slices
pub const Instruction = struct {
    opcode: Opcode,
    operands: [4]ByteCode, // Max 4 operands (for GVAR in BIGATOMS mode)
    operands_len: u32, // Actual number of operands
    length: u32,

    /// Extract single-byte operand
    pub fn getByteOperand(self: Instruction, index: usize) u8 {
        if (index >= self.operands_len) return 0;
        return self.operands[index];
    }

    /// Extract 2-byte operand (little-endian)
    pub fn getWordOperand(self: Instruction, index: usize) u16 {
        if (index + 1 >= self.operands_len) return 0;
        return @as(u16, self.operands[index]) | (@as(u16, self.operands[index + 1]) << 8);
    }

    /// Extract 4-byte pointer operand (BIGVM mode for BIGATOMS)
    /// C: For BIGVM, Get_Pointer(ptr) reads 4 bytes:
    ///   (Get_BYTE(ptr) << 24) | (Get_BYTE(ptr+1) << 16) | (Get_BYTE(ptr+2) << 8) | Get_BYTE(ptr+3)
    ///   With XOR addressing: [ptr XOR 3], [(ptr+1) XOR 3], [(ptr+2) XOR 3], [(ptr+3) XOR 3]
    /// For GVAR: Get_AtomNo_PCMAC1 = Get_Pointer_PCMAC1
    /// But Get_Pointer_PCMAC1fn reads only 3 bytes (non-BIGVM)
    /// For BIGVM, we need to read 4 bytes from pccache = PC + 1
    /// So: Read from [PC+1], [PC+2], [PC+3], [PC+4] with XOR addressing
    /// Byte order: [PC+1]<<24 | [PC+2]<<16 | [PC+3]<<8 | [PC+4]
    pub fn getPointerOperand(self: Instruction, index: usize) LispPTR {
        if (index + 3 >= self.operands_len) return 0;
        // BIGVM byte order: [index]<<24 | [index+1]<<16 | [index+2]<<8 | [index+3]
        return (@as(LispPTR, self.operands[index]) << 24) |
               (@as(LispPTR, self.operands[index + 1]) << 16) |
               (@as(LispPTR, self.operands[index + 2]) << 8) |
               (@as(LispPTR, self.operands[index + 3]));
    }

    /// Extract signed byte operand
    pub fn getSignedByteOperand(self: Instruction, index: usize) i8 {
        return @as(i8, @bitCast(self.getByteOperand(index)));
    }

    /// Extract signed word operand
    pub fn getSignedWordOperand(self: Instruction, index: usize) i16 {
        return @as(i16, @bitCast(self.getWordOperand(index)));
    }
};
