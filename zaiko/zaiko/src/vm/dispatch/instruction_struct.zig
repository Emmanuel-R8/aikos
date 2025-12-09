const types = @import("../../utils/types.zig");
const opcode_module = @import("opcode.zig");

const ByteCode = types.ByteCode;
const Opcode = opcode_module.Opcode;

/// Decoded instruction with operands
pub const Instruction = struct {
    opcode: Opcode,
    operands: []const ByteCode,
    length: u32,

    /// Extract single-byte operand
    pub fn getByteOperand(self: Instruction, index: usize) u8 {
        if (index >= self.operands.len) return 0;
        return self.operands[index];
    }

    /// Extract 2-byte operand (little-endian)
    pub fn getWordOperand(self: Instruction, index: usize) u16 {
        if (index + 1 >= self.operands.len) return 0;
        return @as(u16, self.operands[index]) | (@as(u16, self.operands[index + 1]) << 8);
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