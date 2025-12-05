const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const opcodes = @import("opcodes.zig");

const ByteCode = types.ByteCode;
const LispPTR = types.LispPTR;
const VM = stack.VM;

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

/// Opcode enumeration
/// Per rewrite documentation instruction-set/opcodes.md
pub const Opcode = enum(u8) {
    // Constants
    NIL = 0x68,
    T = 0x69,
    CONST_0 = 0x6A,
    CONST_1 = 0x6B,
    ACONST = 0x67,
    GCONST = 0x6F,

    // Control flow
    FN0 = 0x08,
    FN1 = 0x09,
    FN2 = 0x0A,
    FN3 = 0x0B,
    FN4 = 0x0C,
    FNX = 0x0D,
    RETURN = 0x10,
    BIND = 0x11,
    UNBIND = 0x12,
    DUNBIND = 0x13,
    GCREF = 0x15,
    JUMP = 0x20,
    FJUMP = 0x30,
    TJUMP = 0x31,
    JUMPX = 0xB0,
    FJUMPX = 0xB2,
    TJUMPX = 0xB3,

    // Variable access
    IVAR0 = 0x40,
    IVAR1 = 0x41,
    IVAR2 = 0x42,
    IVAR3 = 0x43,
    IVAR4 = 0x44,
    IVAR5 = 0x45,
    IVAR6 = 0x46,
    IVARX = 0x47,
    PVAR0 = 0x48,
    PVAR1 = 0x49,
    PVAR2 = 0x4A,
    PVAR3 = 0x4B,
    PVAR4 = 0x4C,
    PVAR5 = 0x4D,
    PVAR6 = 0x4E,
    PVARX = 0x4F,
    FVAR0 = 0x50,
    FVAR1 = 0x51,
    FVAR2 = 0x52,
    FVAR3 = 0x53,
    FVAR4 = 0x54,
    FVAR5 = 0x55,
    FVAR6 = 0x56,
    FVARX = 0x57,
    GVAR = 0x60,
    POP = 0xBF,
    POP_N = 0xC0,

    // Data operations
    CAR = 0x01,
    CDR = 0x02,
    CONS = 0x03,
    NTYPX = 0x04,
    TYPEP = 0x05,
    DTEST = 0x06,
    RPLACA = 0x18,
    RPLACD = 0x19,
    UNWIND = 0x07,

    // Array operations
    GETAEL1 = 0x80,
    GETAEL2 = 0x81,
    SETAEL1 = 0x82,
    SETAEL2 = 0x83,

    // Comparison
    EQ = 0x90,
    EQL = 0x91,
    LESSP = 0x92,
    GREATERP = 0x93,
    IGREATERP = 0x94,

    // Type checking
    FIXP = 0xA0,
    SMALLP = 0xA1,
    LISTP = 0xA2,

    // String/character
    CHARCODE = 0xB4,
    CHARN = 0xB5,

    // Arithmetic (integer-specific)
    IPLUS2 = 0xC0,
    IDIFFERENCE = 0xC1,
    ITIMES2 = 0xC2,
    IQUO = 0xC3,
    IREM = 0xC4,

    // Arithmetic (general - handles integers and floats)
    PLUS2 = 0xD4,
    DIFFERENCE = 0xD5,
    TIMES2 = 0xD6,
    QUOTIENT = 0xD7,

    // Bitwise operations
    LOGOR2 = 0xE4,
    LOGAND2 = 0xE5,
    LOGXOR2 = 0xE6,
    LSH = 0xE7,

    // Shift operations
    LLSH1 = 0xE0,
    LLSH8 = 0xE1,
    LRSH1 = 0xE2,
    LRSH8 = 0xE3,

    // Stack manipulation
    PUSH = 0xD0,
    SWAP = 0xFD,
    NOP = 0xFE,

    // Add more opcodes as needed
    _,
};

/// Fetch instruction byte
/// Per contracts/vm-core-interface.zig
pub fn fetchInstruction(pc: LispPTR, code: []const ByteCode) ByteCode {
    if (pc >= code.len) {
        return 0; // Invalid
    }
    return code[@as(usize, @intCast(pc))];
}

/// Decode full instruction with operands
/// Per rewrite documentation instruction-set/instruction-format.md
pub fn decodeInstruction(pc: LispPTR, code: []const ByteCode) ?Instruction {
    if (pc >= code.len) {
        return null;
    }

    const opcode_byte = code[@as(usize, @intCast(pc))];
    const opcode = decodeOpcode(opcode_byte);
    const length = getInstructionLength(opcode);

    if (pc + length > code.len) {
        return null; // Not enough bytes
    }

    const operand_start = @as(usize, @intCast(pc + 1));
    const operand_end = @as(usize, @intCast(pc + length));
    const operands = code[operand_start..operand_end];

    return Instruction{
        .opcode = opcode,
        .operands = operands,
        .length = length,
    };
}

/// Decode opcode
/// Per contracts/vm-core-interface.zig
pub fn decodeOpcode(byte: ByteCode) Opcode {
    // Try to decode as known opcode
    if (@intFromEnum(@as(Opcode, @enumFromInt(byte))) == byte) {
        return @as(Opcode, @enumFromInt(byte));
    }
    // Unknown opcode - return catch-all variant
    return ._;
}

/// Main dispatch loop
/// Per contracts/vm-core-interface.zig and execution-model.md
pub fn dispatch(vm: *VM, code: []const ByteCode) errors.VMError!void {
    var pc: LispPTR = 0;
    const interrupt_module = @import("interrupt.zig");

    while (pc < code.len) {
        // Check interrupts before execution
        if (interrupt_module.checkInterrupts(vm)) {
            // TODO: Handle interrupts
        }

        // Decode full instruction with operands
        const instruction = decodeInstruction(pc, code) orelse {
            // Invalid instruction or end of code
            break;
        };

        // Execute opcode handler with instruction
        const jump_offset = executeInstruction(vm, instruction) catch |err| {
            // Handle opcode execution errors
            switch (err) {
                error.InvalidOpcode => {
                    // Unknown opcode - could be UFN (Undefined Function Name)
                    // For now, stop execution
                    return;
                },
                error.StackOverflow => {
                    return err;
                },
                else => return err,
            }
        };

        // Update program counter
        if (jump_offset) |offset| {
            // Jump instruction - update PC by offset
            const new_pc = @as(i64, @intCast(pc)) + offset;
            if (new_pc < 0 or new_pc >= code.len) {
                return error.InvalidOpcode; // Invalid jump target
            }
            pc = @as(LispPTR, @intCast(new_pc));
        } else {
            // Normal instruction - advance by instruction length
            pc += instruction.length;
        }

        // Check interrupts after execution
        if (interrupt_module.checkInterrupts(vm)) {
            // TODO: Handle interrupts
        }
    }
}

/// Execute instruction with operands
/// Per rewrite documentation vm-core/execution-model.md
/// Returns jump offset if instruction is a jump, null otherwise
pub fn executeInstruction(vm: *VM, instruction: Instruction) errors.VMError!?i64 {
    return executeOpcodeWithOperands(vm, instruction.opcode, instruction);
}

/// Execute opcode handler with operands
/// Per contracts/vm-core-interface.zig
/// Returns jump offset if instruction is a jump, null otherwise
pub fn executeOpcodeWithOperands(vm: *VM, opcode: Opcode, instruction: Instruction) errors.VMError!?i64 {
    // Handle invalid/unknown opcodes
    if (@intFromEnum(opcode) == 0xFF) {
        return error.InvalidOpcode;
    }
    switch (opcode) {
        // Constants
        .NIL => {
            const stack_module = @import("stack.zig");
            try stack_module.pushStack(vm, 0); // Push NIL
            return null;
        },
        .T => {
            const stack_module = @import("stack.zig");
            try stack_module.pushStack(vm, 1); // Push T
            return null;
        },
        .ACONST => {
            try opcodes.handleACONST(vm, instruction.getWordOperand(0));
            return null;
        },
        .GCONST => {
            try opcodes.handleGCONST(vm, instruction.getWordOperand(0));
            return null;
        },

        // Control flow
        .FN0, .FN1, .FN2, .FN3, .FN4, .FNX => {
            try opcodes.handleCALL(vm);
            return null;
        },
        .RETURN => {
            try opcodes.handleRETURN(vm);
            return null;
        },
        .BIND => {
            try opcodes.handleBIND(vm, instruction.getByteOperand(0));
            return null;
        },
        .UNBIND => {
            try opcodes.handleUNBIND(vm);
            return null;
        },
        .DUNBIND => {
            try opcodes.handleDUNBIND(vm);
            return null;
        },
        .GCREF => {
            try opcodes.handleGCREF(vm, instruction.getByteOperand(0));
            return null;
        },
        .JUMP => {
            try opcodes.handleJUMP(vm);
            return @as(i64, instruction.getSignedByteOperand(0));
        },
        .FJUMP => {
            const offset = instruction.getSignedByteOperand(0);
            try opcodes.handleFJUMP(vm, offset);
            const stack_module = @import("stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .TJUMP => {
            const offset = instruction.getSignedByteOperand(0);
            try opcodes.handleTJUMP(vm, offset);
            const stack_module = @import("stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos != 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .JUMPX => {
            try opcodes.handleJUMPX(vm);
            return @as(i64, instruction.getSignedWordOperand(0));
        },
        .FJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleFJUMPX(vm, offset);
            const stack_module = @import("stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .TJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleTJUMPX(vm, offset);
            const stack_module = @import("stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos != 0) {
                return @as(i64, offset);
            }
            return null;
        },

        // Variable access
        .IVAR0 => try opcodes.handleIVAR(vm, 0),
        .IVAR1 => try opcodes.handleIVAR(vm, 1),
        .IVAR2 => try opcodes.handleIVAR(vm, 2),
        .IVAR3 => try opcodes.handleIVAR(vm, 3),
        .IVAR4 => try opcodes.handleIVAR(vm, 4),
        .IVAR5 => try opcodes.handleIVAR(vm, 5),
        .IVAR6 => try opcodes.handleIVAR(vm, 6),
        .IVARX => try opcodes.handleIVAR(vm, instruction.getByteOperand(0)),
        .PVAR0 => try opcodes.handlePVAR(vm, 0),
        .PVAR1 => try opcodes.handlePVAR(vm, 1),
        .PVAR2 => try opcodes.handlePVAR(vm, 2),
        .PVAR3 => try opcodes.handlePVAR(vm, 3),
        .PVAR4 => try opcodes.handlePVAR(vm, 4),
        .PVAR5 => try opcodes.handlePVAR(vm, 5),
        .PVAR6 => try opcodes.handlePVAR(vm, 6),
        .PVARX => try opcodes.handlePVAR(vm, instruction.getByteOperand(0)),
        .FVAR0 => try opcodes.handleFVAR(vm, 0),
        .FVAR1 => try opcodes.handleFVAR(vm, 1),
        .FVAR2 => try opcodes.handleFVAR(vm, 2),
        .FVAR3 => try opcodes.handleFVAR(vm, 3),
        .FVAR4 => try opcodes.handleFVAR(vm, 4),
        .FVAR5 => try opcodes.handleFVAR(vm, 5),
        .FVAR6 => try opcodes.handleFVAR(vm, 6),
        .FVARX => try opcodes.handleFVAR(vm, instruction.getByteOperand(0)),
        .GVAR => try opcodes.handleGVAR(vm, instruction.getWordOperand(0)),
        .POP => try opcodes.handlePOP(vm),
        .POP_N => try opcodes.handlePOP_N(vm, instruction.getByteOperand(0)),
        .SWAP => try opcodes.handleSWAP(vm),
        .NOP => try opcodes.handleNOP(vm),

        // Data operations
        .CAR => try opcodes.handleCAR(vm),
        .CDR => try opcodes.handleCDR(vm),
        .CONS => try opcodes.handleCONS(vm),
        .RPLACA => try opcodes.handleRPLACA(vm),
        .RPLACD => try opcodes.handleRPLACD(vm),
        .NTYPX => try opcodes.handleNTYPX(vm),
        .TYPEP => try opcodes.handleTYPEP(vm, instruction.getByteOperand(0)),
        .DTEST => try opcodes.handleDTEST(vm, instruction.getWordOperand(0)),
        .UNWIND => try opcodes.handleUNWIND(vm, instruction.getWordOperand(0)),

        // Array operations
        .GETAEL1 => try opcodes.handleGETAEL1(vm, instruction.getByteOperand(0)),
        .GETAEL2 => try opcodes.handleGETAEL2(vm, instruction.getWordOperand(0)),
        .SETAEL1 => try opcodes.handleSETAEL1(vm, instruction.getByteOperand(0)),
        .SETAEL2 => try opcodes.handleSETAEL2(vm, instruction.getWordOperand(0)),

        // Comparison
        .EQ => try opcodes.handleEQ(vm),
        .EQL => try opcodes.handleEQL(vm),
        .LESSP => try opcodes.handleLESSP(vm),
        .GREATERP => try opcodes.handleGREATERP(vm),
        .IGREATERP => try opcodes.handleIGREATERP(vm),

        // Type checking
        .FIXP => try opcodes.handleFIXP(vm),
        .SMALLP => try opcodes.handleSMALLP(vm),
        .LISTP => try opcodes.handleLISTP(vm),

        // String/character
        .CHARCODE => try opcodes.handleCHARCODE(vm),
        .CHARN => try opcodes.handleCHARN(vm),

        // Arithmetic (integer-specific)
        .IPLUS2 => try opcodes.handleIPLUS2(vm),
        .IDIFFERENCE => try opcodes.handleIDIFFERENCE(vm),
        .ITIMES2 => try opcodes.handleITIMES2(vm),
        .IQUO => try opcodes.handleIQUO(vm),
        .IREM => try opcodes.handleIREM(vm),

        // Arithmetic (general)
        .PLUS2 => try opcodes.handlePLUS2(vm),
        .DIFFERENCE => try opcodes.handleDIFFERENCE(vm),
        .TIMES2 => try opcodes.handleTIMES2(vm),
        .QUOTIENT => try opcodes.handleQUOTIENT(vm),

        // Bitwise operations
        .LOGOR2 => try opcodes.handleLOGOR2(vm),
        .LOGAND2 => try opcodes.handleLOGAND2(vm),
        .LOGXOR2 => try opcodes.handleLOGXOR2(vm),
        .LSH => try opcodes.handleLSH(vm),

        // Shift operations
        .LLSH1 => try opcodes.handleLLSH1(vm),
        .LLSH8 => try opcodes.handleLLSH8(vm),
        .LRSH1 => try opcodes.handleLRSH1(vm),
        .LRSH8 => try opcodes.handleLRSH8(vm),

        // Stack manipulation
        .PUSH => try opcodes.handlePUSH(vm),

        else => {
            // Unknown opcode - could be UFN or unimplemented
            // For now, return error
            return error.InvalidOpcode;
        },
    }
    return null; // Default: no jump
}

/// Get instruction length for opcode
/// Per rewrite documentation instruction-set/instruction-format.md
pub fn getInstructionLength(opcode: Opcode) u32 {
    return switch (opcode) {
        // Constants
        .NIL, .T, .CONST_0, .CONST_1 => 1,
        .ACONST, .GCONST => 3, // Opcode + 2-byte atom index (BIGATOMS)

        // 1-byte opcodes (no operands)
        .FN0, .FN1, .FN2, .FN3, .FN4 => 1,
        .RETURN, .UNBIND, .DUNBIND => 1,
        .CAR, .CDR, .CONS, .NTYPX, .RPLACA, .RPLACD => 1, // Data operations
        .BIND, .GCREF => 2, // Opcode + 1-byte operand
        .DTEST => 3, // Opcode + 2-byte atom index (BIGATOMS)
        .IPLUS2, .IDIFFERENCE, .ITIMES2, .IQUO, .IREM => 1,
        .EQ, .EQL, .LESSP, .GREATERP, .IGREATERP => 1,
        .FIXP, .SMALLP, .LISTP => 1,
        .POP => 1,
        .IVAR0, .IVAR1, .IVAR2, .IVAR3, .IVAR4, .IVAR5, .IVAR6 => 1,
        .PVAR0, .PVAR1, .PVAR2, .PVAR3, .PVAR4, .PVAR5, .PVAR6 => 1,
        .FVAR0, .FVAR1, .FVAR2, .FVAR3, .FVAR4, .FVAR5, .FVAR6 => 1,
        .PUSH => 1,

        // 2-byte opcodes (1 operand)
        .FNX => 2, // Opcode + atom index
        .JUMP, .FJUMP, .TJUMP => 2, // Opcode + 1-byte offset
        .IVARX, .PVARX, .FVARX => 2, // Opcode + variable index
        .TYPEP => 2, // Opcode + type code
        .UNWIND => 3, // Opcode + 2-byte unwind parameters
        .GETAEL1, .SETAEL1 => 2, // Opcode + 1-byte index
        .CHARCODE, .CHARN => 2, // Opcode + character code
        .POP_N => 2, // Opcode + 1-byte count
        .SWAP => 1, // Opcode only
        .NOP => 1, // Opcode only
        .LOGOR2, .LOGAND2, .LOGXOR2, .LSH => 1, // Opcode only
        .LLSH1, .LLSH8, .LRSH1, .LRSH8 => 1, // Opcode only
        .PLUS2, .DIFFERENCE, .TIMES2, .QUOTIENT => 1, // Opcode only

        // 3-byte opcodes (2 operands)
        .JUMPX, .FJUMPX, .TJUMPX => 3, // Opcode + 2-byte offset
        .GVAR => 3, // Opcode + 2-byte atom index (BIGATOMS)
        .GETAEL2, .SETAEL2 => 3, // Opcode + 2-byte index

        else => 1, // Default to 1 byte
    };
}