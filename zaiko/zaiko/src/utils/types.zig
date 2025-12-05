// Core types for Maiko VM implementation
// All types must maintain exact compatibility with C implementation

/// 32-bit virtual address (matches C LispPTR)
pub const LispPTR = u32;

/// 16-bit word (matches C DLword)
pub const DLword = u16;

/// Bytecode instruction byte
pub const ByteCode = u8;

/// Address component extraction (matches C macros)
pub fn hiloc(ptr: LispPTR) u16 {
    return @as(u16, @truncate(ptr >> 16));
}

/// Address component extraction (matches C macros)
pub fn loloc(ptr: LispPTR) u16 {
    return @as(u16, @truncate(ptr));
}