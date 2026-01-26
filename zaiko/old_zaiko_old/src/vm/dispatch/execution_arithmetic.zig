const std = @import("std");
const errors = @import("../../utils/errors.zig");
const opcodes = @import("../opcodes.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;

// ============================================================================
// Execution - Arithmetic and Remaining Opcodes
// ============================================================================

/// Handle arithmetic and remaining opcodes
/// Returns jump offset if instruction is a jump, null if handled with no jump
/// This is the last handler, so it handles all remaining opcodes or returns InvalidOpcode
pub fn handleArithmeticAndRemaining(vm: *VM, opcode: Opcode, instruction: Instruction) errors.VMError!?i64 {
    switch (opcode) {
        // Comparison
        .EQ => try opcodes.handleEQ(vm),
        .EQL => try opcodes.handleEQL(vm),
        .GREATERP => try opcodes.handleGREATERP(vm),
        .IGREATERP => try opcodes.handleIGREATERP(vm),
        .FGREATERP => try opcodes.handleFGREATERP(vm),
        .EQUAL => try opcodes.handleEQUAL(vm),

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
        .BOXIPLUS => try opcodes.handleBOXIPLUS(vm),
        .BOXIDIFFERENCE => try opcodes.handleBOXIDIFFERENCE(vm),
        .FLOATBLT => try opcodes.handleFLOATBLT(vm),
        .FFTSTEP => try opcodes.handleFFTSTEP(vm),
        .MISC3 => try opcodes.handleMISC3(vm),
        .MISC4 => try opcodes.handleMISC4(vm),
        .UPCTRACE => try opcodes.handleUPCTRACE(vm),
        .CL_EQUAL => try opcodes.handleCL_EQUAL(vm),
        .WRTPTRTAG => try opcodes.handleWRTPTRTAG(vm),
        .WRTPTR0TAG => try opcodes.handleWRTPTR0TAG(vm),

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