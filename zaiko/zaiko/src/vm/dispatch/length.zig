const opcode_module = @import("opcode.zig");

const Opcode = opcode_module.Opcode;

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