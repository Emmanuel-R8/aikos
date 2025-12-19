const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// OneDArray structure (matches C OneDArray from lsptypes.h)
/// Non-BIGVM version (most common)
pub const OneDArray = packed struct {
    nil1: u8, // Unused (8 bits)
    base: u24, // Base address (24 bits for non-BIGVM)
    readonlyp: u1, // Read-only flag
    indirectp: u1, // Indirect flag
    bitp: u1, // Bit array flag
    stringp: u1, // String flag
    ajustablep: u1, // Adjustable flag
    displacedp: u1, // Displaced flag
    fillpointerp: u1, // Fill pointer flag
    extendablep: u1, // Extendable flag
    typenumber: u8, // Element type number
    offset: DLword, // Offset into array
    fillpointer: DLword, // Fill pointer
    totalsize: DLword, // Total size of array
};

/// Array type numbers (from C implementation)
pub const TYPE_POINTER: u8 = 38; // Pointer: 32 bits
pub const TYPE_SIGNED_16: u8 = 20; // Signed: 16 bits
pub const TYPE_SIGNED_32: u8 = 22; // Signed: 32 bits
pub const TYPE_CHARACTER: u8 = 67; // Character: 8 bits
pub const TYPE_UNSIGNED_1BIT: u8 = 0; // Unsigned: 1 bit per element
pub const TYPE_UNSIGNED_8BIT: u8 = 3; // Unsigned: 8 bits per element

/// Array header structure (legacy, kept for compatibility)
pub const ArrayHeader = packed struct {
    type_code: u8, // Array type code
    fill_pointer: DLword, // Fill pointer (for arrays)
    length: DLword, // Array length
    // Data follows in memory
};

/// Get array element
/// Per rewrite documentation data-structures/arrays.md
/// Assumes array elements are LispPTR (pointer type)
pub fn getArrayElement(header: *ArrayHeader, index: usize) LispPTR {
    // Validate index
    if (index >= header.length) {
        return 0; // Out of bounds - return NIL
    }

    // Calculate element address (elements follow header)
    const header_addr = @intFromPtr(header);
    const element_base = header_addr + @sizeOf(ArrayHeader);
    const element_addr = element_base + (index * @sizeOf(LispPTR));

    // Read element value
    const element_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(element_addr));
    return element_ptr.*;
}

/// Set array element
/// Per rewrite documentation data-structures/arrays.md
/// Assumes array elements are LispPTR (pointer type)
pub fn setArrayElement(header: *ArrayHeader, index: usize, value: LispPTR) void {
    // Validate index
    if (index >= header.length) {
        return; // Out of bounds - ignore
    }

    // Calculate element address (elements follow header)
    const header_addr = @intFromPtr(header);
    const element_base = header_addr + @sizeOf(ArrayHeader);
    const element_addr = element_base + (index * @sizeOf(LispPTR));

    // Write element value
    const element_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(element_addr));
    element_ptr.* = value;
}
