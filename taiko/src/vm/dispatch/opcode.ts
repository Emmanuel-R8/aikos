// Opcode enumeration
// Per rewrite documentation instruction-set/opcodes.md
// Matches zaiko/src/vm/dispatch/opcode.zig

export enum Opcode {
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
    CAR = 0x01,
    CDR = 0x02,
    NTYPX = 0x04,
    CONS = 0x1A,
    CMLASSOC = 0x1B,
    FMEMB = 0x1C,
    CMLMEMBER = 0x1D,
    FINDKEY = 0x1E,
    CREATECELL = 0x1F,
    WRTPTRTAG = 0x36,
    BIN = 0x20,
    BOUT = 0x21,
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
    RPLACA = 0x18,
    RPLACD = 0x19,
    TYPEP = 0x05,
    DTEST = 0x06,
    UNWIND = 0x07,
    MISCN = 0x24,
    POPDISP = 0x22,
    RESTLIST = 0x23,
    GVAR = 0x60,
    ARG0 = 0x61,
    PVARX_ = 0x5F,
    IVARX_ = 0x62,
    FVARX_ = 0x63,
    POP = 0xBF,
    POP_N = 0xC0,
    ATOMCELL_N = 0xC1,
    GETBASEBYTE = 0xC2,
    INSTANCEP = 0xC3,
    BLT = 0xC4,
    MISC10 = 0xC5,
    PUTBASEBYTE = 0xC7,
    GETBASE_N = 0xC8,
    ITIMES2 = 0xC9,
    GETBITS_N_FD = 0xCA,
    PUTBASE_N = 0xCD,
    PUTBASEPTR_N = 0xCE,
    PUTBITS_N_FD = 0xCF,
    ADDBASE = 0xD0,
    VAG2 = 0xD1,
    HILOC = 0xD2,
    LOLOC = 0xD3,
    PLUS2 = 0xD4,
    DIFFERENCE = 0xD5,
    TIMES2 = 0xD6,
    QUOTIENT = 0xD7,
    CMLEQUAL = 0xCC,
    IPLUS2 = 0xD8,
    IDIFFERENCE = 0xD9,
    LLSH8 = 0xDA,
    IQUOTIENT = 0xDB,
    IREMAINDER = 0xDC,
    IPLUS_N = 0xDD,
    IDIFFERENCE_N = 0xDE,
    BASE_LESSTHAN = 0xDF,
    LLSH1 = 0xE0,
    LRSH1 = 0xE2,
    LRSH8 = 0xE3,
    LOGOR2 = 0xE4,
    LOGAND2 = 0xE5,
    LOGXOR2 = 0xE6,
    LSH = 0xE7,
    GETBASEPTR_N = 0xEC,
    AREF2 = 0xEE,
    UBFLOAT1 = 0xED,
    UBFLOAT2 = 0xE1,
    FPLUS2 = 0xE8,
    FDIFFERENCE = 0xE9,
    FTIMES2 = 0xEA,
    FQUOTIENT = 0xEB,
    ASET2 = 0xEF,
    TYPEMASK_N = 0x33,
    MISC7 = 0x38,
    EQL = 0x3A,
    DRAWLINE = 0x3B,
    STORE_N = 0x3C,
    RAID = 0x3E,
    COPY_N = 0x3D,
    RECLAIMCELL = 0x7A,
    GCSCAN1 = 0x7B,
    GCSCAN2 = 0x7C,
    CONTEXTSWITCH = 0x7E,
    RETCALL = 0x7F,
    FGREATERP = 0xF2,
    IGREATERP = 0xF1,
    GREATERP = 0xF3,
    EQ = 0xF0,
    EQUAL = 0xF4,
    MAKENUMBER = 0xF5,
    BOXIPLUS = 0xF6,
    BOXIDIFFERENCE = 0xF7,
    FLOATBLT = 0xF8,
    FFTSTEP = 0xF9,
    MISC3 = 0xFA,
    MISC4 = 0xFB,
    UPCTRACE = 0xFC,
    CL_EQUAL = 0xFF,
    SWAP = 0xFD,
    NOP = 0xFE,

    // Optimized jump variants (offset encoded in opcode)
    JUMP0 = 0x80,
    JUMP1 = 0x81,
    JUMP2 = 0x82,
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
    AREF1 = 0xB6,
    ASET1 = 0xB7,
    PVARSETPOP0 = 0xB8,
    PVARSETPOP1 = 0xB9,
    PVARSETPOP2 = 0xBA,
    PVARSETPOP3 = 0xBB,
    PVARSETPOP4 = 0xBC,
    PVARSETPOP5 = 0xBD,
    PVARSETPOP6 = 0xBE,

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
    COPY = 0x64,
    MYARGCOUNT = 0x65,
    MYALINK = 0x66,
    STKSCAN = 0x2F,
    SLRETURN = 0x3F,
}

/**
 * Instruction structure
 */
export interface Instruction {
    opcode: Opcode;
    operands: Uint8Array;
    length: number;
}

/**
 * Get instruction length for opcode
 * Simplified version - full implementation would check operand requirements
 */
export function getInstructionLength(opcode: Opcode): number {
    // Most opcodes are 1 byte
    // JUMPX variants are 3 bytes (opcode + 2-byte offset)
    // Some opcodes have variable length operands
    if (opcode >= Opcode.JUMP0 && opcode <= Opcode.JUMP15) {
        return 1; // Offset encoded in opcode
    }
    if (opcode >= Opcode.FJUMP0 && opcode <= Opcode.FJUMP15) {
        return 1;
    }
    if (opcode >= Opcode.TJUMP0 && opcode <= Opcode.TJUMP15) {
        return 1;
    }
    if (opcode === Opcode.JUMPX || opcode === Opcode.FJUMPX || opcode === Opcode.TJUMPX ||
        opcode === Opcode.NFJUMPX || opcode === Opcode.NTJUMPX) {
        return 3; // opcode + 2-byte signed offset
    }
    if (opcode === Opcode.JUMPXX) {
        return 5; // opcode + 4-byte offset
    }
    // Opcodes with 1-byte operands
    if (opcode === Opcode.RPLPTR_N ||
        opcode === Opcode.TYPEP ||
        opcode === Opcode.GVAR_ ||
        opcode === Opcode.GCREF ||
        opcode === Opcode.ASSOC ||
        opcode === Opcode.RESTLIST) {
        return 2; // opcode + 1-byte operand
    }
    // Opcodes with 2-byte operands
    if (opcode === Opcode.DTEST || opcode === Opcode.UNWIND) {
        return 3; // opcode + 2-byte operand
    }
    // Default: 1 byte opcode
    return 1;
}

/**
 * Decode opcode from byte
 */
export function decodeOpcode(byte: number): Opcode | null {
    if (byte in Opcode) {
        return byte as Opcode;
    }
    return null;
}
