const std = @import("std");
const types = @import("../../utils/types.zig");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const opcodes = @import("../opcodes.zig");

const ByteCode = types.ByteCode;
const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;
const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
pub fn executeInstruction(vm: *VM, instruction: Instruction) errors.VMError!?i64 {
    return executeOpcodeWithOperands(vm, instruction.opcode, instruction);
}

/// Helper function for FJUMP handlers
/// C: FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
fn handleFJUMPWithOffset(vm: *VM, offset: i8) errors.VMError!?i64 {
    const stack_module = @import("../stack.zig");
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
    const stack_module = @import("../stack.zig");
    
    // Check stack depth before popping
    const stack_depth = stack_module.getStackDepth(vm);
    std.debug.print("DEBUG: TJUMP: stack_depth={}, offset={}\n", .{ stack_depth, offset });
    
    if (stack_depth == 0) {
        std.debug.print("ERROR: TJUMP: Stack is empty, cannot pop!\n", .{});
        return error.StackUnderflow;
    }
    
    const tos = stack_module.getTopOfStack(vm);
    std.debug.print("DEBUG: TJUMP: TOS=0x{x}, offset={}\n", .{ tos, offset });
    _ = try stack_module.popStack(vm); // Always pop (C: POP in both branches)
    
    const new_stack_depth = stack_module.getStackDepth(vm);
    std.debug.print("DEBUG: TJUMP: After pop, stack_depth={}\n", .{new_stack_depth});
    
    if (tos != 0) {
        // Not NIL - jump
        std.debug.print("DEBUG: TJUMP: TOS is non-NIL, jumping by {}\n", .{offset});
        try opcodes.handleTJUMP(vm, offset);
        return @as(i64, offset);
    }
    // NIL - continue
    std.debug.print("DEBUG: TJUMP: TOS is NIL, continuing\n", .{});
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
            const stack_module = @import("../stack.zig");
            try stack_module.pushStack(vm, 0); // Push NIL
            return null;
        },
        .T => {
            const stack_module = @import("../stack.zig");
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
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .TJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleTJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos != 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .NFJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleNFJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .NTJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleNTJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
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
            // Log the opcode for debugging
            const opcode_byte = @intFromEnum(opcode);
            std.debug.print("ERROR: Unimplemented opcode 0x{x:0>2} in execution switch\n", .{opcode_byte});
            return error.InvalidOpcode;
        },
    }
    return null; // Default: no jump
}