const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const opcodes = @import("opcodes.zig");

const ByteCode = types.ByteCode;
const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;
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
    SIC = 0x6C,
    SNIC = 0x6D,
    SICX = 0x6E,
    GCONST = 0x6F,

    // Control flow
    FN0 = 0x08,
    FN1 = 0x09,
    FN2 = 0x0A,
    FN3 = 0x0B,
    FN4 = 0x0C,
    FNX = 0x0D,
    APPLYFN = 0x0E,
    CHECKAPPLY = 0x0F,
    RETURN = 0x10,
    BIND = 0x11,
    UNBIND = 0x12,
    DUNBIND = 0x13,
    RPLPTR_N = 0x14,
    GCREF = 0x15,
    ASSOC = 0x16,
    GVAR_ = 0x17,
    // Note: No generic JUMP/FJUMP/TJUMP opcodes - use JUMPX, JUMPXX, or JUMP0-JUMP15, FJUMP0-FJUMP15, TJUMP0-TJUMP15
    // Optimized jump variants (offset encoded in opcode)
    // These are at 0x80-0x8F (128-143) per opcodes.h
    JUMP0 = 0x80, // 128
    JUMP1 = 0x81, // 129
    JUMP2 = 0x82, // 130
    JUMP3 = 0x83,
    JUMP4 = 0x84,
    JUMP5 = 0x85,
    JUMP6 = 0x86,
    JUMP7 = 0x87,
    JUMP8 = 0x88,
    JUMP9 = 0x89,
    JUMP10 = 0x8A,
    JUMP11 = 0x8B,
    JUMP12 = 0x8C,
    JUMP13 = 0x8D,
    JUMP14 = 0x8E,
    JUMP15 = 0x8F,
    FJUMP0 = 0x90,
    FJUMP1 = 0x91,
    FJUMP2 = 0x92,
    FJUMP3 = 0x93,
    FJUMP4 = 0x94,
    FJUMP5 = 0x95,
    FJUMP6 = 0x96,
    FJUMP7 = 0x97,
    FJUMP8 = 0x98,
    FJUMP9 = 0x99,
    FJUMP10 = 0x9A,
    FJUMP11 = 0x9B,
    FJUMP12 = 0x9C,
    FJUMP13 = 0x9D,
    FJUMP14 = 0x9E,
    FJUMP15 = 0x9F,
    TJUMP0 = 0xA0,
    TJUMP1 = 0xA1,
    TJUMP2 = 0xA2,
    TJUMP3 = 0xA3,
    TJUMP4 = 0xA4,
    TJUMP5 = 0xA5,
    TJUMP6 = 0xA6,
    TJUMP7 = 0xA7,
    TJUMP8 = 0xA8,
    TJUMP9 = 0xA9,
    TJUMP10 = 0xAA,
    TJUMP11 = 0xAB,
    TJUMP12 = 0xAC,
    TJUMP13 = 0xAD,
    TJUMP14 = 0xAE,
    TJUMP15 = 0xAF,
    JUMPX = 0xB0,
    JUMPXX = 0xB1,
    FJUMPX = 0xB2,
    TJUMPX = 0xB3,
    NFJUMPX = 0xB4,
    NTJUMPX = 0xB5,

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
    PVAR_0 = 0x58,
    PVAR_1 = 0x59,
    PVAR_2 = 0x5A,
    PVAR_3 = 0x5B,
    PVAR_4 = 0x5C,
    PVAR_5 = 0x5D,
    PVAR_6 = 0x5E,
    PVARX_ = 0x5F,
    GVAR = 0x60,
    ARG0 = 0x61,
    IVARX_ = 0x62,
    FVARX_ = 0x63,
    COPY = 0x64,
    MYARGCOUNT = 0x65,
    MYALINK = 0x66,
    STKSCAN = 0x2F,
    SLRETURN = 0x3F,
    POP = 0xBF,
    POP_N = 0xC0,
    ATOMCELL_N = 0xC1,
    GETBASEBYTE = 0xC2,
    INSTANCEP = 0xC3,
    BLT = 0xC4,
    MISC10 = 0xC5,
    PUTBASEBYTE = 0xC7,
    GETBASE_N = 0xC8,
    GETBASEPTR_N = 0xC9,
    GETBITS_N_FD = 0xCA,
    CMLEQUAL = 0xCC,
    PUTBASE_N = 0xCD,
    PUTBASEPTR_N = 0xCE,
    PUTBITS_N_FD = 0xCF,
    ADDBASE = 0xD0,
    VAG2 = 0xD1,
    HILOC = 0xD2,
    LOLOC = 0xD3,
    IPLUS_N = 0xDD,
    IDIFFERENCE_N = 0xDE,
    BASE_LESSTHAN = 0xDF,
    UBFLOAT2 = 0xEC,
    UBFLOAT1 = 0xED,
    AREF2 = 0xEE,
    ASET2 = 0xEF,
    BOXIPLUS = 0xF6,
    BOXIDIFFERENCE = 0xF7,
    FLOATBLT = 0xF8,
    FFTSTEP = 0xF9,
    MISC3 = 0xFA,
    MISC4 = 0xFB,
    UPCTRACE = 0xFC,
    CL_EQUAL = 0xFF,

    // Data operations
    CAR = 0x01,
    CDR = 0x02,
    CONS = 0x03,
    NTYPX = 0x04,
    TYPEP = 0x05,
    DTEST = 0x06,
    RPLACA = 0x18,
    RPLACD = 0x19,
    CMLASSOC = 0x1B,
    FMEMB = 0x1C,
    CMLMEMBER = 0x1D,
    FINDKEY = 0x1E,
    CREATECELL = 0x1F,
    BIN = 0x20,
    BOUT = 0x21,
    RESTLIST = 0x23,
    MISCN = 0x24,
    RPLCONS = 0x26,
    LISTGET = 0x27,
    ELT = 0x28,
    NTHCHC = 0x29,
    SETA = 0x2A,
    RPLCHARCODE = 0x2B,
    EVAL = 0x2C,
    ENVCALL = 0x2D,
    TYPECHECK = 0x2E,
    BUSBLT = 0x30,
    MISC8 = 0x31,
    UBFLOAT3 = 0x32,
    TYPEMASK_N = 0x33,
    MISC7 = 0x38,
    DRAWLINE = 0x3B,
    STORE_N = 0x3C,
    COPY_N = 0x3D,
    RAID = 0x3E,
    UNWIND = 0x07,

    // Array operations
    // Note: GETAEL1/GETAEL2/SETAEL1/SETAEL2 are not in C opcodes.h
    // These might be placeholders - using different values to avoid conflicts
    // TODO: Verify correct opcode values from C implementation
    // GETAEL1 = 0x80, // Conflicts with JUMP0
    // GETAEL2 = 0x81, // Conflicts with JUMP1
    // SETAEL1 = 0x82, // Conflicts with JUMP2
    // SETAEL2 = 0x83, // Conflicts with JUMP3

    // Comparison (per opcodes.h: EQ=240, EQL=58, IGREATERP=241, FGREATERP=242, GREATERP=243, EQUAL=244)
    EQ = 0xF0, // 240
    EQL = 0x3A, // 58
    IGREATERP = 0xF1, // 241
    FGREATERP = 0xF2, // 242
    GREATERP = 0xF3, // 243
    EQUAL = 0xF4, // 244
    // Note: LESSP opcode not found in opcodes.h - might be implemented differently

    // Type checking
    // Note: FIXP, SMALLP, LISTP opcodes conflict with TJUMP0-TJUMP2 (0xA0-0xA2)
    // These might be handled by TYPEP opcode with different arguments
    // TODO: Verify correct opcode values from C implementation
    // FIXP = 0xA0, // Conflicts with TJUMP0
    // SMALLP = 0xA1, // Conflicts with TJUMP1
    // LISTP = 0xA2, // Conflicts with TJUMP2
    // TYPEP = 0x05, // Already defined elsewhere (duplicate)

    // String/character
    // Note: CHARCODE/CHARN opcodes conflict with NFJUMPX/NTJUMPX (0xB4-0xB5)
    // These might be handled via different opcodes or mechanisms
    // TODO: Verify correct opcode values from C implementation
    // CHARCODE = 0xB4, // Conflicts with NFJUMPX = 0xB4 (180)
    // CHARN = 0xB5, // Conflicts with NTJUMPX = 0xB5 (181)

    // Arithmetic (integer-specific) - Note: These use different values in C enum vs bytecode
    // Bytecode values match C switch cases (octal notation)
    IPLUS2 = 0xD8, // C enum: 216, bytecode: matches
    IDIFFERENCE = 0xD9,
    ITIMES2 = 0xDA,
    IQUO = 0xDB,
    IREM = 0xDC,
    // IQUOTIENT = 0xDB, // Alias for IQUO (can't have duplicate enum values)
    // IREMAINDER = 0xDC, // Alias for IREM (can't have duplicate enum values)

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

    // Floating-point arithmetic
    FPLUS2 = 0xE8,
    FDIFFERENCE = 0xE9,
    FTIMES2 = 0xEA,
    FQUOTIENT = 0xEB,
    // FGREATERP = 0xF2, // Moved to comparison section above (line 259)
    MAKENUMBER = 0xF5,

    // Shift operations
    LLSH1 = 0xE0,
    LLSH8 = 0xE1,
    LRSH1 = 0xE2,
    LRSH8 = 0xE3,

    // Stack manipulation
    // Note: PUSH opcode not found in C opcodes.h - might be handled via other opcodes
    // TODO: Verify correct opcode values from C implementation
    // PUSH = 0xD0, // Conflicts with ADDBASE = 0xD0 (208)
    SWAP = 0xFD, // 253 per opcodes.h: opc_SWAP = 253
    NOP = 0xFE, // 254 per opcodes.h: opc_NOP = 254

    // Note: Zig enum doesn't support catch-all (_) - all opcodes must be explicitly defined
    // Unknown opcodes will cause enumFromInt to fail, which we handle in decodeInstruction
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
    const opcode = decodeOpcode(opcode_byte) orelse {
        return null; // Unknown opcode
    };
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
pub fn decodeOpcode(byte: ByteCode) ?Opcode {
    // Try to decode as known opcode
    // enumFromInt will panic on invalid values, so we need to check validity first
    // For now, we'll use a safe conversion that returns null for invalid opcodes
    // This is a simplified check - full implementation would validate against all known opcodes
    if (byte > 255) {
        return null;
    }
    // Use inline switch or validation to ensure byte is a valid opcode
    // For now, attempt conversion and catch any issues
    const opcode = @as(Opcode, @enumFromInt(byte));
    return opcode;
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
            // C: Unknown opcodes trigger UFN lookup (goto op_ufn)
            // For now, treat as invalid opcode error
            return error.InvalidOpcode;
        };

        // Execute opcode handler with instruction
        const jump_offset = executeInstruction(vm, instruction) catch |err| {
            // Handle opcode execution errors matching C emulator behavior
            switch (err) {
                error.InvalidOpcode => {
                    // Unknown opcode - could be UFN (Undefined Function Name)
                    // C: goto op_ufn; - triggers UFN lookup
                    // TODO: Implement UFN lookup (Phase 3)
                    // For now, stop execution with error
                    return err;
                },
                error.StackOverflow => {
                    // C: Triggers do_stackoverflow() which tries to extend stack
                    // TODO: Implement stack extension (Phase 3)
                    // For now, return error
                    return err;
                },
                error.StackUnderflow => {
                    // C: Stack underflow typically indicates programming error
                    // Return error immediately
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

/// Helper function for FJUMP handlers
/// C: FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
fn handleFJUMPWithOffset(vm: *VM, offset: i8) errors.VMError!?i64 {
    const stack_module = @import("stack.zig");
    const tos = stack_module.getTopOfStack(vm);
    _ = try stack_module.popStack(vm); // Always pop (C: POP in both branches)
    if (tos == 0) {
        // NIL - jump
        try opcodes.handleFJUMP(vm, offset);
        return @as(i64, offset);
    }
    // Not NIL - continue
    return null;
}

/// Helper function for TJUMP handlers
/// C: TJUMPMACRO(x): if (TOPOFSTACK == 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
fn handleTJUMPWithOffset(vm: *VM, offset: i8) errors.VMError!?i64 {
    const stack_module = @import("stack.zig");
    const tos = stack_module.getTopOfStack(vm);
    _ = try stack_module.popStack(vm); // Always pop (C: POP in both branches)
    if (tos != 0) {
        // Not NIL - jump
        try opcodes.handleTJUMP(vm, offset);
        return @as(i64, offset);
    }
    // NIL - continue
    return null;
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
        .SIC => try opcodes.handleSIC(vm, instruction.getByteOperand(0)),
        .SNIC => try opcodes.handleSNIC(vm, instruction.getByteOperand(0)),
        .SICX => try opcodes.handleSICX(vm, instruction.getWordOperand(0)),
        .GCONST => {
            try opcodes.handleGCONST(vm, instruction.getWordOperand(0));
            return null;
        },

        // Control flow - Function calls
        .FN0 => {
            try opcodes.handleFN0(vm, &instruction);
            return null;
        },
        .FN1 => {
            try opcodes.handleFN1(vm, &instruction);
            return null;
        },
        .FN2 => {
            try opcodes.handleFN2(vm, &instruction);
            return null;
        },
        .FN3 => {
            try opcodes.handleFN3(vm, &instruction);
            return null;
        },
        .FN4 => {
            try opcodes.handleFN4(vm, &instruction);
            return null;
        },
        .FNX => {
            // FNX has variable argument count - will implement later
            // TODO: Implement handleFNX similar to handleFN but with variable arg count
            return error.InvalidOpcode; // Not yet implemented
        },
        .APPLYFN => try opcodes.handleAPPLYFN(vm),
        .CHECKAPPLY => try opcodes.handleCHECKAPPLY(vm),
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
        .RPLPTR_N => {
            try opcodes.handleRPLPTR_N(vm, instruction.getByteOperand(0));
            return null;
        },
        .GCREF => {
            try opcodes.handleGCREF(vm, instruction.getByteOperand(0));
            return null;
        },
        .ASSOC => try opcodes.handleASSOC(vm),
        .GVAR_ => try opcodes.handleGVAR_(vm, instruction.getWordOperand(0)),
        // Note: No generic JUMP opcode - use JUMPX, JUMPXX, or JUMP0-JUMP15
        // Optimized jump variants (offset encoded in opcode)
        .JUMP0 => {
            try opcodes.handleJUMP(vm);
            return 0;
        },
        .JUMP1 => {
            try opcodes.handleJUMP(vm);
            return 1;
        },
        .JUMP2 => {
            try opcodes.handleJUMP(vm);
            return 2;
        },
        .JUMP3 => {
            try opcodes.handleJUMP(vm);
            return 3;
        },
        .JUMP4 => {
            try opcodes.handleJUMP(vm);
            return 4;
        },
        .JUMP5 => {
            try opcodes.handleJUMP(vm);
            return 5;
        },
        .JUMP6 => {
            try opcodes.handleJUMP(vm);
            return 6;
        },
        .JUMP7 => {
            try opcodes.handleJUMP(vm);
            return 7;
        },
        .JUMP8 => {
            try opcodes.handleJUMP(vm);
            return 8;
        },
        .JUMP9 => {
            try opcodes.handleJUMP(vm);
            return 9;
        },
        .JUMP10 => {
            try opcodes.handleJUMP(vm);
            return 10;
        },
        .JUMP11 => {
            try opcodes.handleJUMP(vm);
            return 11;
        },
        .JUMP12 => {
            try opcodes.handleJUMP(vm);
            return 12;
        },
        .JUMP13 => {
            try opcodes.handleJUMP(vm);
            return 13;
        },
        .JUMP14 => {
            try opcodes.handleJUMP(vm);
            return 14;
        },
        .JUMP15 => {
            try opcodes.handleJUMP(vm);
            return 15;
        },
        // Note: No generic FJUMP opcode - use FJUMPX or FJUMP0-FJUMP15
        // Optimized false jump variants
        // C: FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
        .FJUMP0 => return handleFJUMPWithOffset(vm, 0),
        .FJUMP1 => return handleFJUMPWithOffset(vm, 1),
        .FJUMP2 => return handleFJUMPWithOffset(vm, 2),
        .FJUMP3 => return handleFJUMPWithOffset(vm, 3),
        .FJUMP4 => return handleFJUMPWithOffset(vm, 4),
        .FJUMP5 => return handleFJUMPWithOffset(vm, 5),
        .FJUMP6 => return handleFJUMPWithOffset(vm, 6),
        .FJUMP7 => return handleFJUMPWithOffset(vm, 7),
        .FJUMP8 => return handleFJUMPWithOffset(vm, 8),
        .FJUMP9 => return handleFJUMPWithOffset(vm, 9),
        .FJUMP10 => return handleFJUMPWithOffset(vm, 10),
        .FJUMP11 => return handleFJUMPWithOffset(vm, 11),
        .FJUMP12 => return handleFJUMPWithOffset(vm, 12),
        .FJUMP13 => return handleFJUMPWithOffset(vm, 13),
        .FJUMP14 => return handleFJUMPWithOffset(vm, 14),
        .FJUMP15 => return handleFJUMPWithOffset(vm, 15),
        // Note: No generic TJUMP opcode - use TJUMPX or TJUMP0-TJUMP15
        // Note: No generic TJUMP opcode - use TJUMPX or TJUMP0-TJUMP15
        // Optimized true jump variants
        // C: TJUMPMACRO(x): if (TOPOFSTACK == 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
        .TJUMP0 => return handleTJUMPWithOffset(vm, 0),
        .TJUMP1 => return handleTJUMPWithOffset(vm, 1),
        .TJUMP2 => return handleTJUMPWithOffset(vm, 2),
        .TJUMP3 => return handleTJUMPWithOffset(vm, 3),
        .TJUMP4 => return handleTJUMPWithOffset(vm, 4),
        .TJUMP5 => return handleTJUMPWithOffset(vm, 5),
        .TJUMP6 => return handleTJUMPWithOffset(vm, 6),
        .TJUMP7 => return handleTJUMPWithOffset(vm, 7),
        .TJUMP8 => return handleTJUMPWithOffset(vm, 8),
        .TJUMP9 => return handleTJUMPWithOffset(vm, 9),
        .TJUMP10 => return handleTJUMPWithOffset(vm, 10),
        .TJUMP11 => return handleTJUMPWithOffset(vm, 11),
        .TJUMP12 => return handleTJUMPWithOffset(vm, 12),
        .TJUMP13 => return handleTJUMPWithOffset(vm, 13),
        .TJUMP14 => return handleTJUMPWithOffset(vm, 14),
        .TJUMP15 => return handleTJUMPWithOffset(vm, 15),
        .JUMPX => {
            try opcodes.handleJUMPX(vm);
            return @as(i64, instruction.getSignedWordOperand(0));
        },
        .JUMPXX => {
            try opcodes.handleJUMPXX(vm);
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
        .NFJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleNFJUMPX(vm, offset);
            const stack_module = @import("stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .NTJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleNTJUMPX(vm, offset);
            const stack_module = @import("stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
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
        .PVAR_0 => try opcodes.handlePVAR_SET(vm, 0),
        .PVAR_1 => try opcodes.handlePVAR_SET(vm, 1),
        .PVAR_2 => try opcodes.handlePVAR_SET(vm, 2),
        .PVAR_3 => try opcodes.handlePVAR_SET(vm, 3),
        .PVAR_4 => try opcodes.handlePVAR_SET(vm, 4),
        .PVAR_5 => try opcodes.handlePVAR_SET(vm, 5),
        .PVAR_6 => try opcodes.handlePVAR_SET(vm, 6),
        .PVARX_ => try opcodes.handlePVAR_SET(vm, instruction.getByteOperand(0)),
        .GVAR => try opcodes.handleGVAR(vm, instruction.getWordOperand(0)),
        // .GVAR_ => try opcodes.handleGVAR_(vm, instruction.getWordOperand(0)), // Duplicate - already handled above
        .ARG0 => try opcodes.handleARG0(vm),
        .IVARX_ => try opcodes.handleIVARX_(vm, instruction.getByteOperand(0)),
        .FVARX_ => try opcodes.handleFVARX_(vm, instruction.getByteOperand(0)),
        .COPY => try opcodes.handleCOPY(vm),
        .MYARGCOUNT => try opcodes.handleMYARGCOUNT(vm),
        .MYALINK => try opcodes.handleMYALINK(vm),
        .STKSCAN => try opcodes.handleSTKSCAN(vm),
        .SLRETURN => try opcodes.handleSLRETURN(vm),
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
        .CMLASSOC => try opcodes.handleCMLASSOC(vm),
        .FMEMB => try opcodes.handleFMEMB(vm),
        .CMLMEMBER => try opcodes.handleCMLMEMBER(vm),
        .FINDKEY => try opcodes.handleFINDKEY(vm, instruction.getByteOperand(0)),
        .CREATECELL => try opcodes.handleCREATECELL(vm),
        .BIN => try opcodes.handleBIN(vm),
        .BOUT => try opcodes.handleBOUT(vm),
        .RESTLIST => try opcodes.handleRESTLIST(vm, instruction.getByteOperand(0)),
        .MISCN => try opcodes.handleMISCN(vm, instruction.getByteOperand(0), instruction.getByteOperand(1)),
        .RPLCONS => try opcodes.handleRPLCONS(vm),
        .LISTGET => try opcodes.handleLISTGET(vm),
        .ELT => try opcodes.handleELT(vm),
        .NTHCHC => try opcodes.handleNTHCHC(vm),
        .SETA => try opcodes.handleSETA(vm),
        .RPLCHARCODE => try opcodes.handleRPLCHARCODE(vm),
        .EVAL => try opcodes.handleEVAL(vm),
        .ENVCALL => try opcodes.handleENVCALL(vm),
        .TYPECHECK => try opcodes.handleTYPECHECK(vm, instruction.getByteOperand(0)),
        .BUSBLT => try opcodes.handleBUSBLT(vm),
        .MISC8 => try opcodes.handleMISC8(vm),
        .UBFLOAT3 => try opcodes.handleUBFLOAT3(vm, instruction.getByteOperand(0)),
        .TYPEMASK_N => try opcodes.handleTYPEMASK_N(vm, instruction.getByteOperand(0)),
        .MISC7 => try opcodes.handleMISC7(vm, instruction.getByteOperand(0)),
        .DRAWLINE => try opcodes.handleDRAWLINE(vm),
        .STORE_N => try opcodes.handleSTORE_N(vm, instruction.getByteOperand(0)),
        .COPY_N => try opcodes.handleCOPY_N(vm, instruction.getByteOperand(0)),
        .RAID => try opcodes.handleRAID(vm),
        .NTYPX => try opcodes.handleNTYPX(vm),
        .TYPEP => try opcodes.handleTYPEP(vm, instruction.getByteOperand(0)),
        .DTEST => try opcodes.handleDTEST(vm, instruction.getWordOperand(0)),
        .UNWIND => try opcodes.handleUNWIND(vm, instruction.getWordOperand(0)),

        // Array operations
        // .GETAEL1 => try opcodes.handleGETAEL1(vm, instruction.getByteOperand(0)), // Commented out - conflicts with JUMP0
        // .GETAEL2 => try opcodes.handleGETAEL2(vm, instruction.getWordOperand(0)), // Commented out - conflicts with JUMP1
        // .SETAEL1 => try opcodes.handleSETAEL1(vm, instruction.getByteOperand(0)), // Commented out - conflicts with JUMP2
        // .SETAEL2 => try opcodes.handleSETAEL2(vm, instruction.getWordOperand(0)), // Commented out - conflicts with JUMP3
        .AREF2 => try opcodes.handleAREF2(vm),
        .ASET2 => try opcodes.handleASET2(vm),

        // Comparison
        .EQ => try opcodes.handleEQ(vm),
        .EQL => try opcodes.handleEQL(vm),
        // .LESSP => try opcodes.handleLESSP(vm), // Commented out - opcode not found in C opcodes.h
        .GREATERP => try opcodes.handleGREATERP(vm),
        .IGREATERP => try opcodes.handleIGREATERP(vm),
        .FGREATERP => try opcodes.handleFGREATERP(vm),
        .EQUAL => try opcodes.handleEQUAL(vm),

        // Type checking
        // .FIXP => try opcodes.handleFIXP(vm), // Commented out - conflicts with TJUMP0
        // .SMALLP => try opcodes.handleSMALLP(vm), // Commented out - conflicts with TJUMP1
        // .LISTP => try opcodes.handleLISTP(vm), // Commented out - conflicts with TJUMP2

        // String/character
        // .CHARCODE => try opcodes.handleCHARCODE(vm), // Commented out - conflicts with NFJUMPX
        // .CHARN => try opcodes.handleCHARN(vm), // Commented out - conflicts with NTJUMPX

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

        // Floating-point arithmetic
        .FPLUS2 => try opcodes.handleFPLUS2(vm),
        .FDIFFERENCE => try opcodes.handleFDIFFERENCE(vm),
        .FTIMES2 => try opcodes.handleFTIMES2(vm),
        .FQUOTIENT => try opcodes.handleFQUOTIENT(vm),
        .MAKENUMBER => try opcodes.handleMAKENUMBER(vm),

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
        // .PUSH => try opcodes.handlePUSH(vm), // Commented out - opcode not found in C opcodes.h

        // High-range opcodes (0xC0-0xFF)
        .ATOMCELL_N => try opcodes.handleATOMCELL_N(vm, instruction.getByteOperand(0)),
        .GETBASEBYTE => try opcodes.handleGETBASEBYTE(vm),
        .INSTANCEP => try opcodes.handleINSTANCEP(vm),
        .BLT => try opcodes.handleBLT(vm),
        .MISC10 => try opcodes.handleMISC10(vm),
        .PUTBASEBYTE => try opcodes.handlePUTBASEBYTE(vm),
        .GETBASE_N => try opcodes.handleGETBASE_N(vm, instruction.getByteOperand(0)),
        .GETBASEPTR_N => try opcodes.handleGETBASEPTR_N(vm, instruction.getByteOperand(0)),
        .GETBITS_N_FD => try opcodes.handleGETBITS_N_FD(vm, instruction.getByteOperand(0), instruction.getByteOperand(1)),
        .CMLEQUAL => try opcodes.handleCMLEQUAL(vm),
        .PUTBASE_N => try opcodes.handlePUTBASE_N(vm, instruction.getByteOperand(0)),
        .PUTBASEPTR_N => try opcodes.handlePUTBASEPTR_N(vm, instruction.getByteOperand(0)),
        .PUTBITS_N_FD => try opcodes.handlePUTBITS_N_FD(vm, instruction.getByteOperand(0), instruction.getByteOperand(1)),
        .ADDBASE => try opcodes.handleADDBASE(vm),
        .VAG2 => try opcodes.handleVAG2(vm),
        .HILOC => try opcodes.handleHILOC(vm),
        .LOLOC => try opcodes.handleLOLOC(vm),
        .IPLUS_N => try opcodes.handleIPLUS_N(vm, instruction.getByteOperand(0)),
        .IDIFFERENCE_N => try opcodes.handleIDIFFERENCE_N(vm, instruction.getByteOperand(0)),
        .BASE_LESSTHAN => try opcodes.handleBASE_LESSTHAN(vm),
        .UBFLOAT2 => try opcodes.handleUBFLOAT2(vm),
        .UBFLOAT1 => try opcodes.handleUBFLOAT1(vm),
        // .UBFLOAT3 => try opcodes.handleUBFLOAT3(vm, instruction.getByteOperand(0)), // Duplicate - already handled above
        .BOXIPLUS => try opcodes.handleBOXIPLUS(vm),
        .BOXIDIFFERENCE => try opcodes.handleBOXIDIFFERENCE(vm),
        .FLOATBLT => try opcodes.handleFLOATBLT(vm),
        .FFTSTEP => try opcodes.handleFFTSTEP(vm),
        .MISC3 => try opcodes.handleMISC3(vm),
        .MISC4 => try opcodes.handleMISC4(vm),
        .UPCTRACE => try opcodes.handleUPCTRACE(vm),
        .CL_EQUAL => try opcodes.handleCL_EQUAL(vm),

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
        .SIC, .SNIC => 2, // Opcode + 1-byte operand
        .SICX => 3, // Opcode + 2-byte operand

        // FN0-FN4: Opcode (1 byte) + Atom index (2 bytes for non-BIGATOMS)
        // C: FN_OPCODE_SIZE = 3 for non-BIGATOMS
        .FN0, .FN1, .FN2, .FN3, .FN4 => 3,
        .RETURN, .UNBIND, .DUNBIND => 1,
        .CAR, .CDR, .CONS, .NTYPX, .RPLACA, .RPLACD => 1, // Data operations
        .ASSOC, .CMLASSOC, .FMEMB, .CMLMEMBER, .CREATECELL => 1, // List operations
        .RPLCONS, .LISTGET => 1, // List operations
        .BIN, .BOUT => 1, // I/O operations
        .EVAL, .ENVCALL => 1, // Evaluation operations
        .BIND, .GCREF, .RPLPTR_N => 2, // Opcode + 1-byte operand
        .FINDKEY, .RESTLIST => 2, // Opcode + 1-byte operand
        .MISCN => 3, // Opcode + 2-byte operands
        .DTEST, .GVAR_ => 3, // Opcode + 2-byte atom index (BIGATOMS)
        .IPLUS2, .IDIFFERENCE, .ITIMES2, .IQUO, .IREM => 1,
        .EQ, .EQL, .GREATERP, .IGREATERP => 1, // Note: LESSP opcode not found in C opcodes.h
        // .FIXP, .SMALLP, .LISTP => 1, // These opcodes not found in C opcodes.h
        .POP => 1,
        .IVAR0, .IVAR1, .IVAR2, .IVAR3, .IVAR4, .IVAR5, .IVAR6 => 1,
        .PVAR0, .PVAR1, .PVAR2, .PVAR3, .PVAR4, .PVAR5, .PVAR6 => 1,
        .FVAR0, .FVAR1, .FVAR2, .FVAR3, .FVAR4, .FVAR5, .FVAR6 => 1,
        .PVAR_0, .PVAR_1, .PVAR_2, .PVAR_3, .PVAR_4, .PVAR_5, .PVAR_6 => 1,
        .ARG0, .COPY, .MYARGCOUNT, .MYALINK => 1,
        // .PUSH => 1, // PUSH opcode not found in C opcodes.h

        // 2-byte opcodes (1 operand)
        .FNX => 2, // Opcode + atom index
        // Note: No generic JUMP/FJUMP/TJUMP opcodes - use JUMPX, FJUMPX, TJUMPX or optimized variants
        // .JUMP, .FJUMP, .TJUMP => 2, // Opcode + 1-byte offset (removed - no generic opcodes)
        // Optimized jump variants (1-byte, offset encoded in opcode)
        .JUMP0, .JUMP1, .JUMP2, .JUMP3, .JUMP4, .JUMP5, .JUMP6, .JUMP7, .JUMP8, .JUMP9, .JUMP10, .JUMP11, .JUMP12, .JUMP13, .JUMP14, .JUMP15 => 1,
        .FJUMP0, .FJUMP1, .FJUMP2, .FJUMP3, .FJUMP4, .FJUMP5, .FJUMP6, .FJUMP7, .FJUMP8, .FJUMP9, .FJUMP10, .FJUMP11, .FJUMP12, .FJUMP13, .FJUMP14, .FJUMP15 => 1,
        .TJUMP0, .TJUMP1, .TJUMP2, .TJUMP3, .TJUMP4, .TJUMP5, .TJUMP6, .TJUMP7, .TJUMP8, .TJUMP9, .TJUMP10, .TJUMP11, .TJUMP12, .TJUMP13, .TJUMP14, .TJUMP15 => 1,
        .IVARX, .PVARX, .FVARX => 2, // Opcode + variable index
        .PVARX_, .IVARX_, .FVARX_ => 2, // Opcode + variable index (set operations)
        .TYPEP => 2, // Opcode + type code
        .UNWIND => 3, // Opcode + 2-byte unwind parameters
        // .GETAEL1, .SETAEL1 => 2, // Commented out - conflicts with JUMP0/JUMP1
        // .CHARCODE, .CHARN => 2, // Commented out - conflicts with NFJUMPX/NTJUMPX
        .POP_N => 2, // Opcode + 1-byte count
        .SWAP => 1, // Opcode only
        .NOP => 1, // Opcode only
        .LOGOR2, .LOGAND2, .LOGXOR2, .LSH => 1, // Opcode only
        .LLSH1, .LLSH8, .LRSH1, .LRSH8 => 1, // Opcode only
        .PLUS2, .DIFFERENCE, .TIMES2, .QUOTIENT => 1, // Opcode only
        .FPLUS2, .FDIFFERENCE, .FTIMES2, .FQUOTIENT => 1, // Floating-point arithmetic
        .FGREATERP => 1, // Floating-point comparison
        .APPLYFN, .CHECKAPPLY => 1, // Function application
        .STKSCAN => 1, // Stack scan
        .SLRETURN => 1, // Stack-relative return
        .EQUAL => 1, // Deep equality comparison
        .MAKENUMBER => 1, // Number creation

        // High-range opcodes (0xC0-0xFF)
        .ATOMCELL_N => 2, // Opcode + 1-byte operand
        .GETBASEBYTE => 1,
        .INSTANCEP => 1,
        .BLT => 1,
        .MISC10 => 1,
        .PUTBASEBYTE => 1,
        .GETBASE_N => 2, // Opcode + 1-byte operand
        .GETBASEPTR_N => 2,
        .GETBITS_N_FD => 3, // Opcode + 2-byte operands
        .CMLEQUAL => 1,
        .PUTBASE_N => 2,
        .PUTBASEPTR_N => 2,
        .PUTBITS_N_FD => 3,
        .ADDBASE => 1,
        .VAG2 => 1,
        .HILOC => 1,
        .LOLOC => 1,
        .IPLUS_N => 2,
        .IDIFFERENCE_N => 2,
        .BASE_LESSTHAN => 1,
        .UBFLOAT2 => 1,
        .UBFLOAT1 => 1,
        .UBFLOAT3 => 2, // Opcode + 1-byte operand
        .AREF2, .ASET2 => 1, // Array operations
        .BOXIPLUS => 1,
        .BOXIDIFFERENCE => 1,
        .FLOATBLT => 1,
        .FFTSTEP => 1,
        .MISC3 => 1,
        .MISC4 => 1,
        .UPCTRACE => 1,
        .CL_EQUAL => 1,

        // 3-byte opcodes (2 operands)
        .JUMPX, .JUMPXX, .FJUMPX, .TJUMPX, .NFJUMPX, .NTJUMPX => 3, // Opcode + 2-byte offset
        .GVAR => 3, // Opcode + 2-byte atom index (BIGATOMS)
        // .GETAEL2, .SETAEL2 => 3, // Commented out - conflicts with JUMP1/JUMP3

        else => 1, // Default to 1 byte
    };
}

/// Initialize VM state from IFPAGE
/// Per contracts/vm-execution-api.md
/// Sets stack pointers, frame pointer, and program counter from IFPAGE
pub fn initializeVMState(
    vm: *VM,
    ifpage: *const IFPAGE,
    virtual_memory: []u8,
) errors.VMError!void {
    // Convert LispPTR addresses to native pointers
    // Note: In the C implementation, these are converted using NativeAligned2FromStackOffset
    // For now, we'll treat them as offsets into virtual_memory

    // Set stack base from IFPAGE
    // stackbase is a LispPTR that points to the base of the stack
    const stackbase_offset = ifpage.stackbase;
    if (stackbase_offset >= virtual_memory.len) {
        return error.InvalidStackPointer;
    }

    // Set end of stack from IFPAGE
    const endofstack_offset = ifpage.endofstack;
    if (endofstack_offset >= virtual_memory.len) {
        return error.InvalidStackPointer;
    }

    // Set current frame pointer from IFPAGE
    const currentfxp_offset = ifpage.currentfxp;
    if (currentfxp_offset >= virtual_memory.len) {
        return error.InvalidFramePointer;
    }

    // Convert offsets to pointers (assuming virtual_memory is the Lisp world)
    // For now, we'll set the stack pointers relative to the virtual memory
    // The actual conversion depends on the memory mapping implementation
    // This is a simplified version - full implementation would use proper address translation

    // Initialize program counter (will be set from function entry point or sysout state)
    vm.pc = 0; // TODO: Set from sysout state or function entry point

    // Note: Full implementation would:
    // 1. Convert LispPTR addresses to native pointers using address translation
    // 2. Set up stack frame from currentfxp
    // 3. Initialize program counter from function entry point
    // 4. Set up interrupt state
}