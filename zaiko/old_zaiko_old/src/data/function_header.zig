const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Function header structure (matches C fnhead)
/// Per data-model.md
pub const FunctionHeader = packed struct {
    stkmin: DLword, // Minimum stack size
    na: DLword, // Number of arguments
    pv: DLword, // Number of PVars
    startpc: DLword, // Starting PC
    framename: LispPTR, // Frame name atom
    ntsize: DLword, // Name table size
    nlocals: DLword, // Number of local variables
    fvaroffset: DLword, // FVar offset
    // Name table and code follow in memory
};

/// Get function start PC
pub fn getStartPC(header: *const FunctionHeader) DLword {
    return header.startpc;
}

/// Get number of arguments
pub fn getNumArgs(header: *const FunctionHeader) DLword {
    return header.na;
}

/// Get number of local variables
pub fn getNumLocals(header: *const FunctionHeader) DLword {
    return header.nlocals;
}