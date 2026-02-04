const types = @import("../../utils/types.zig");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const opcode_module = @import("opcode.zig");
const instruction_struct_module = @import("instruction_struct.zig");
const length_module = @import("length.zig");
const memory_access = @import("../../utils/memory_access.zig");
const memory_manager = @import("../../memory/manager.zig");

const ByteCode = types.ByteCode;
const LispPTR = types.LispPTR;
const VM = stack.VM;
const Opcode = opcode_module.Opcode;
const Instruction = instruction_struct_module.Instruction;
const getInstructionLength = length_module.getInstructionLength;
const MemoryAccessManager = memory_manager.MemoryAccessManager;
const EndiannessManager = memory_manager.EndiannessManager;

/// Fetch instruction byte from virtual memory
/// Per contracts/vm-core-interface.zig
/// Reads bytecode from virtual_memory at PC address with XOR addressing (BYTESWAP mode)
/// C: Get_BYTE_PCMAC0 = Get_BYTE(PCMAC) = GETBYTE(PCMAC) = *(unsigned char *)(3 ^ (UNSIGNED)(PCMAC))
pub fn fetchInstructionByte(vm: *VM, pc: LispPTR) errors.VMError!ByteCode {
    if (vm.virtual_memory) |vmem| {
        // CRITICAL: Use XOR addressing for byte access (BYTESWAP mode)
        // C emulator uses: GETBYTE(base) = *(unsigned char *)(3 ^ (UNSIGNED)(base))
        return memory_access.getByte(vmem, @as(usize, @intCast(pc)));
    } else {
        return error.MemoryAccessFailed;
    }
}

/// Decode full instruction with operands from virtual memory
/// Per rewrite documentation instruction-set/instruction-format.md
/// Reads bytecode from virtual_memory at PC address with XOR addressing (BYTESWAP mode)
/// C: Uses Get_BYTE_PCMAC0, Get_BYTE_PCMAC1, etc. which apply XOR addressing
pub fn decodeInstructionFromMemory(vm: *VM, pc: LispPTR) errors.VMError!?Instruction {
    if (vm.virtual_memory) |vmem| {
        // CRITICAL: Fetch opcode byte with XOR addressing (BYTESWAP mode)
        // C: Get_BYTE_PCMAC0 = Get_BYTE(PCMAC) = GETBYTE(PCMAC) = *(unsigned char *)(3 ^ (UNSIGNED)(PCMAC))
        const opcode_byte = memory_access.getByte(vmem, @as(usize, @intCast(pc))) catch {
            return null; // Invalid address - beyond virtual memory
        };

        const opcode = decodeOpcode(opcode_byte) orelse {
            return null; // Unknown opcode
        };
        const length = getInstructionLength(opcode);

        // Check if we have enough bytes (using XOR addressing for bounds check)
        // Note: We check the original PC + length, but read with XOR addressing
        const pc_usize = @as(usize, @intCast(pc));
        if (pc_usize + length > vmem.len) {
            return null; // Not enough bytes
        }

        // Read operands (if any) with XOR addressing
        // C: Get_BYTE_PCMAC1 = Get_BYTE(PCMAC + 1) = GETBYTE(PCMAC + 1)
        var operands_buffer: [4]ByteCode = undefined; // Max instruction length is 5 bytes (opcode + 4 operands for GVAR in BIGATOMS mode)
        var operands_len: u32 = 0;

        if (length > 1) {
            operands_len = @as(u32, @intCast(length - 1));
            // Read operand bytes with XOR addressing (BYTESWAP mode)
            for (0..operands_len) |i| {
                const operand_offset = pc_usize + i + 1;
                const byte = memory_access.getByte(vmem, operand_offset) catch {
                    return null; // Invalid address
                };
                operands_buffer[i] = byte;
            }
            // Zero out remaining bytes
            for (operands_len..4) |i| {
                operands_buffer[i] = 0;
            }
        }

        return Instruction{
            .opcode = opcode,
            .operands = operands_buffer,
            .operands_len = operands_len,
            .length = length,
        };
    } else {
        return error.MemoryAccessFailed;
    }
}

/// Fetch instruction byte (legacy - for compatibility)
/// Per contracts/vm-core-interface.zig
pub fn fetchInstruction(pc: LispPTR, code: []const ByteCode) ByteCode {
    if (pc >= code.len) {
        return 0; // Invalid
    }
    return code[@as(usize, @intCast(pc))];
}

/// Decode full instruction with operands (legacy - for compatibility)
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
/// Returns null for unknown opcodes (which may be UFNs)
/// Uses exhaustive switch to safely convert byte to Opcode enum
pub fn decodeOpcode(byte: ByteCode) ?Opcode {
    // Use exhaustive switch to safely convert byte to Opcode
    // Unknown opcodes return null (they may be UFNs handled elsewhere)
    return switch (byte) {
        // Map all known opcode values from Opcode enum
        0x01 => Opcode.CAR,
        0x02 => Opcode.CDR,
        0x03 => Opcode.CONS,
        0x04 => Opcode.NTYPX,
        0x05 => Opcode.TYPEP,
        0x06 => Opcode.DTEST,
        0x07 => Opcode.UNWIND,
        0x08 => Opcode.FN0,
        0x09 => Opcode.FN1,
        0x0A => Opcode.FN2,
        0x0B => Opcode.FN3,
        0x0C => Opcode.FN4,
        0x0D => Opcode.FNX,
        0x0E => Opcode.APPLYFN,
        0x0F => Opcode.CHECKAPPLY,
        0x10 => Opcode.RETURN,
        0x11 => Opcode.BIND,
        0x12 => Opcode.UNBIND,
        0x13 => Opcode.DUNBIND,
        0x14 => Opcode.RPLPTR_N,
        0x15 => Opcode.GCREF,
        0x16 => Opcode.ASSOC,
        0x17 => Opcode.GVAR_,
        0x18 => Opcode.RPLACA,
        0x19 => Opcode.RPLACD,
        0x1A => Opcode.POPDISP,
        0x1B => Opcode.CMLASSOC,
        0x1C => Opcode.FMEMB,
        0x1D => Opcode.CMLMEMBER,
        0x1E => Opcode.FINDKEY,
        0x1F => Opcode.CREATECELL,
        0x20 => Opcode.BIN,
        0x21 => Opcode.BOUT,
        0x23 => Opcode.RESTLIST,
        0x24 => Opcode.MISCN,
        0x26 => Opcode.RPLCONS,
        0x27 => Opcode.LISTGET,
        0x28 => Opcode.ELT,
        0x29 => Opcode.NTHCHC,
        0x2A => Opcode.SETA,
        0x2B => Opcode.RPLCHARCODE,
        0x2C => Opcode.EVAL,
        0x2D => Opcode.ENVCALL,
        0x2E => Opcode.TYPECHECK,
        0x2F => Opcode.STKSCAN,
        0x30 => Opcode.BUSBLT,
        0x31 => Opcode.MISC8,
        0x32 => Opcode.UBFLOAT3,
        0x33 => Opcode.TYPEMASK_N,
        0x3A => Opcode.EQL,
        0x3B => Opcode.DRAWLINE,
        0x3C => Opcode.STORE_N,
        0x3D => Opcode.COPY_N,
        0x3E => Opcode.RAID,
        0x3F => Opcode.SLRETURN,
        0x40 => Opcode.IVAR0,
        0x41 => Opcode.IVAR1,
        0x42 => Opcode.IVAR2,
        0x43 => Opcode.IVAR3,
        0x44 => Opcode.IVAR4,
        0x45 => Opcode.IVAR5,
        0x46 => Opcode.IVAR6,
        0x47 => Opcode.IVARX,
        0x48 => Opcode.PVAR0,
        0x49 => Opcode.PVAR1,
        0x4A => Opcode.PVAR2,
        0x4B => Opcode.PVAR3,
        0x4C => Opcode.PVAR4,
        0x4D => Opcode.PVAR5,
        0x4E => Opcode.PVAR6,
        0x4F => Opcode.PVARX,
        0x50 => Opcode.FVAR0,
        0x51 => Opcode.FVAR1,
        0x52 => Opcode.FVAR2,
        0x53 => Opcode.FVAR3,
        0x54 => Opcode.FVAR4,
        0x55 => Opcode.FVAR5,
        0x56 => Opcode.FVAR6,
        0x57 => Opcode.FVARX,
        0x58 => Opcode.PVAR_0,
        0x59 => Opcode.PVAR_1,
        0x5A => Opcode.PVAR_2,
        0x5B => Opcode.PVAR_3,
        0x5C => Opcode.PVAR_4,
        0x5D => Opcode.PVAR_5,
        0x5E => Opcode.PVAR_6,
        0x5F => Opcode.PVARX_,
        0x60 => Opcode.GVAR,
        0x61 => Opcode.ARG0,
        0x62 => Opcode.IVARX_,
        0x63 => Opcode.FVARX_,
        0x64 => Opcode.COPY,
        0x65 => Opcode.MYARGCOUNT,
        0x66 => Opcode.MYALINK,
        0x67 => Opcode.ACONST,
        0x68 => Opcode.NIL,
        0x69 => Opcode.T,
        0x6A => Opcode.CONST_0,
        0x6B => Opcode.CONST_1,
        0x6C => Opcode.SIC,
        0x6D => Opcode.SNIC,
        0x6E => Opcode.SICX,
        0x6F => Opcode.GCONST,
        0x7E => Opcode.CONTEXTSWITCH,
        0x7F => Opcode.RETCALL,
        0x80 => Opcode.JUMP0,
        0x81 => Opcode.JUMP1,
        0x82 => Opcode.JUMP2,
        0x83 => Opcode.JUMP3,
        0x84 => Opcode.JUMP4,
        0x85 => Opcode.JUMP5,
        0x86 => Opcode.JUMP6,
        0x87 => Opcode.JUMP7,
        0x88 => Opcode.JUMP8,
        0x89 => Opcode.JUMP9,
        0x8A => Opcode.JUMP10,
        0x8B => Opcode.JUMP11,
        0x8C => Opcode.JUMP12,
        0x8D => Opcode.JUMP13,
        0x8E => Opcode.JUMP14,
        0x8F => Opcode.JUMP15,
        0x90 => Opcode.FJUMP0,
        0x91 => Opcode.FJUMP1,
        0x92 => Opcode.FJUMP2,
        0x93 => Opcode.FJUMP3,
        0x94 => Opcode.FJUMP4,
        0x95 => Opcode.FJUMP5,
        0x96 => Opcode.FJUMP6,
        0x97 => Opcode.FJUMP7,
        0x98 => Opcode.FJUMP8,
        0x99 => Opcode.FJUMP9,
        0x9A => Opcode.FJUMP10,
        0x9B => Opcode.FJUMP11,
        0x9C => Opcode.FJUMP12,
        0x9D => Opcode.FJUMP13,
        0x9E => Opcode.FJUMP14,
        0x9F => Opcode.FJUMP15,
        0xA0 => Opcode.TJUMP0,
        0xA1 => Opcode.TJUMP1,
        0xA2 => Opcode.TJUMP2,
        0xA3 => Opcode.TJUMP3,
        0xA4 => Opcode.TJUMP4,
        0xA5 => Opcode.TJUMP5,
        0xA6 => Opcode.TJUMP6,
        0xA7 => Opcode.TJUMP7,
        0xA8 => Opcode.TJUMP8,
        0xA9 => Opcode.TJUMP9,
        0xAA => Opcode.TJUMP10,
        0xAB => Opcode.TJUMP11,
        0xAC => Opcode.TJUMP12,
        0xAD => Opcode.TJUMP13,
        0xAE => Opcode.TJUMP14,
        0xAF => Opcode.TJUMP15,
        0xB0 => Opcode.JUMPX,
        0xB1 => Opcode.JUMPXX,
        0xB2 => Opcode.FJUMPX,
        0xB3 => Opcode.TJUMPX,
        0xB4 => Opcode.NFJUMPX,
        0xB5 => Opcode.NTJUMPX,
        0xB6 => Opcode.AREF1,
        0xB7 => Opcode.ASET1,
        0xB8 => Opcode.PVARSETPOP0,
        0xB9 => Opcode.PVARSETPOP1,
        0xBA => Opcode.PVARSETPOP2,
        0xBB => Opcode.PVARSETPOP3,
        0xBC => Opcode.PVARSETPOP4,
        0xBD => Opcode.PVARSETPOP5,
        0xBE => Opcode.PVARSETPOP6,
        0xBF => Opcode.POP,
        0xC0 => Opcode.POP_N,
        0xC1 => Opcode.ATOMCELL_N,
        0xC2 => Opcode.GETBASEBYTE,
        0xC3 => Opcode.INSTANCEP,
        0xC4 => Opcode.BLT,
        0xC7 => Opcode.PUTBASEBYTE,
        0xC8 => Opcode.GETBASE_N,
        0xC9 => Opcode.GETBASEPTR_N,
        0xCA => Opcode.GETBITS_N_FD,
        0xCC => Opcode.CMLEQUAL,
        0xCD => Opcode.PUTBASE_N,
        0xCE => Opcode.PUTBASEPTR_N,
        0xCF => Opcode.PUTBITS_N_FD,
        0xD0 => Opcode.ADDBASE,
        0xD2 => Opcode.HILOC,
        0xD3 => Opcode.LOLOC,
        0xD4 => Opcode.PLUS2,
        0xD5 => Opcode.DIFFERENCE,
        0xD6 => Opcode.TIMES2,
        0xD7 => Opcode.QUOTIENT,
        0xD8 => Opcode.IPLUS2,
        0xD9 => Opcode.IDIFFERENCE,
        0xDA => Opcode.ITIMES2,
        0xDB => Opcode.IQUOTIENT,
        0xDC => Opcode.IREMAINDER,
        0xDD => Opcode.IPLUS_N,
        0xDE => Opcode.IDIFFERENCE_N,
        0xDF => Opcode.BASE_LESSTHAN,
        0xE0 => Opcode.LLSH1,
        0xE1 => Opcode.LLSH8,
        0xE2 => Opcode.LRSH1,
        0xE3 => Opcode.LRSH8,
        0xE4 => Opcode.LOGOR2,
        0xE5 => Opcode.LOGAND2,
        0xE6 => Opcode.LOGXOR2,
        0xE7 => Opcode.LSH,
        0xE8 => Opcode.FPLUS2,
        0xE9 => Opcode.FDIFFERENCE,
        0xEA => Opcode.FTIMES2,
        0xEB => Opcode.FQUOTIENT,
        0xEE => Opcode.AREF2,
        0xEF => Opcode.ASET2,
        0xF0 => Opcode.EQ,
        0xF1 => Opcode.IGREATERP,
        0xF2 => Opcode.FGREATERP,
        0xF3 => Opcode.GREATERP,
        0xF4 => Opcode.EQUAL,
        0xF5 => Opcode.MAKENUMBER,
        0xF6 => Opcode.BOXIPLUS,
        0xF7 => Opcode.BOXIDIFFERENCE,
        0xF8 => Opcode.FLOATBLT,
        0xF9 => Opcode.FFTSTEP,
        0xFC => Opcode.UPCTRACE,
        0xFD => Opcode.SWAP,
        0xFE => Opcode.NOP,
        0xFF => Opcode.CL_EQUAL,
        // Add IVAR/PVAR/FVAR variants (0x40-0x46, 0x48-0x4E, 0x50-0x56, etc.)
        // These are handled via opcode ranges in the execution switch
        else => null, // Unknown opcode - may be UFN
    };
}
