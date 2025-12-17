const errors = @import("../../utils/errors.zig");
const opcodes = @import("../opcodes.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;

// ============================================================================
// Execution - Data Operations Opcodes
// ============================================================================

/// Handle data operations opcodes
/// Returns jump offset if instruction is a jump, null if handled with no jump
/// Returns error.NotHandled if opcode doesn't match this category
pub fn handleDataOperations(vm: *VM, opcode: Opcode, instruction: Instruction) errors.VMError!?i64 {
    switch (opcode) {
        // Variable access
        .IVAR0 => try opcodes.handleIVAR(vm, 0),
        .IVAR1 => try opcodes.handleIVAR(vm, 1),
        .IVAR2 => try opcodes.handleIVAR(vm, 2),
        .IVAR3 => try opcodes.handleIVAR(vm, 3),
        .IVAR4 => try opcodes.handleIVAR(vm, 4),
        .IVAR5 => try opcodes.handleIVAR(vm, 5),
        .IVAR6 => try opcodes.handleIVAR(vm, 6),
        .IVARX => try opcodes.handleIVARX(vm, instruction.getByteOperand(0)),
        .PVAR0 => try opcodes.handlePVAR(vm, 0),
        .PVAR1 => try opcodes.handlePVAR(vm, 1),
        .PVAR2 => try opcodes.handlePVAR(vm, 2),
        .PVAR3 => try opcodes.handlePVAR(vm, 3),
        .PVAR4 => try opcodes.handlePVAR(vm, 4),
        .PVAR5 => try opcodes.handlePVAR(vm, 5),
        .PVAR6 => try opcodes.handlePVAR(vm, 6),
        .PVARX => try opcodes.handlePVARX(vm, instruction.getByteOperand(0)),
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
        .MISC7 => try opcodes.handleMISC7(vm, instruction.getByteOperand(0)),
        .MISC8 => try opcodes.handleMISC8(vm),
        .DOVEMISC => try opcodes.handleDOVEMISC(vm),
        .PILOTBITBLT => try opcodes.handlePILOTBITBLT(vm),
        .RCLK => try opcodes.handleRCLK(vm),
        .MISC1 => try opcodes.handleMISC1(vm),
        .MISC2 => try opcodes.handleMISC2(vm),
        .UBFLOAT3 => try opcodes.handleUBFLOAT3(vm, instruction.getByteOperand(0)),
        .TYPEMASK_N => try opcodes.handleTYPEMASK_N(vm, instruction.getByteOperand(0)),
        .DRAWLINE => try opcodes.handleDRAWLINE(vm),
        .STORE_N => try opcodes.handleSTORE_N(vm, instruction.getByteOperand(0)),
        .COPY_N => try opcodes.handleCOPY_N(vm, instruction.getByteOperand(0)),
        .RAID => try opcodes.handleRAID(vm),
        .NTYPX => try opcodes.handleNTYPX(vm),
        .TYPEP => try opcodes.handleTYPEP(vm, instruction.getByteOperand(0)),
        .DTEST => try opcodes.handleDTEST(vm, instruction.getWordOperand(0)),
        .UNWIND => try opcodes.handleUNWIND(vm, instruction.getWordOperand(0)),

        // Array operations
        .AREF1 => try opcodes.handleAREF1(vm),
        .AREF2 => try opcodes.handleAREF2(vm),
        .ASET1 => try opcodes.handleASET1(vm),
        .ASET2 => try opcodes.handleASET2(vm),
        .PVARSETPOP0 => try opcodes.handlePVARSETPOP(vm, 0),
        .PVARSETPOP1 => try opcodes.handlePVARSETPOP(vm, 1),
        .PVARSETPOP2 => try opcodes.handlePVARSETPOP(vm, 2),
        .PVARSETPOP3 => try opcodes.handlePVARSETPOP(vm, 3),
        .PVARSETPOP4 => try opcodes.handlePVARSETPOP(vm, 4),
        .PVARSETPOP5 => try opcodes.handlePVARSETPOP(vm, 5),
        .PVARSETPOP6 => try opcodes.handlePVARSETPOP(vm, 6),

        else => return error.NotHandled, // Not a data operations opcode
    }
    return error.NotHandled; // Should never reach here, but satisfy compiler
}