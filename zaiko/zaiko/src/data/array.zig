const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Array type enumeration
pub const ArrayType = enum(u8) {
    TYP_ARRAY = 0,
    TYP_STRING = 1,
    TYP_BITBLT = 2,
    TYP_SMALLPCL = 3,
    TYP_BIGNUM = 4,
    // Add more types as needed
};

/// Array header structure
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