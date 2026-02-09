const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Cons cell structure (matches C ConsCell)
/// Per data-model.md
pub const ConsCell = packed struct {
    car_field: LispPTR, // CAR field
    cdr_code: u8, // CDR coding (low 8 bits)
    // CDR coding values: CDR_NIL, CDR_ONPAGE, CDR_INDIRECT
};

/// CDR coding constants
pub const CDR_NIL: u8 = 8; // NEWCDRCODING: CDR is NIL
pub const CDR_INDIRECT: u8 = 0; // CDR stored indirectly
pub const CDR_ONPAGE_MIN: u8 = 8; // Same page encoding start
pub const CDR_ONPAGE_MAX: u8 = 15; // Same page encoding end

/// Decode CDR from cons cell
/// Per rewrite documentation data-structures/cons-cells.md
pub fn decodeCDR(cons_cell: *const ConsCell, cell_address: LispPTR) LispPTR {
    const cdr_code = cons_cell.cdr_code;

    if (cdr_code == CDR_NIL) {
        return 0; // NIL_PTR
    }

    if (cdr_code == CDR_INDIRECT) {
        // Indirect encoding: CAR points to indirect cell
        // TODO: Implement indirect decoding
        return cons_cell.car_field;
    }

    // NEWCDRCODING: Same page encoding (3-bit offset)
    if (cdr_code >= CDR_ONPAGE_MIN and cdr_code <= CDR_ONPAGE_MAX) {
        const offset = (@as(u32, cdr_code) & 7) << 1;
        return cell_address + offset;
    }

    // Different page encoding
    const offset = @as(u32, cdr_code) << 1;
    return cell_address + offset;
}

/// Get CAR value
pub fn getCAR(cons_cell: *const ConsCell) LispPTR {
    return cons_cell.car_field;
}

/// Set CAR value
pub fn setCAR(cons_cell: *ConsCell, value: LispPTR) void {
    cons_cell.car_field = value;
}

/// Get CDR value (decoded)
pub fn getCDR(cons_cell: *const ConsCell, cell_address: LispPTR) LispPTR {
    return decodeCDR(cons_cell, cell_address);
}

/// Set CDR value (with encoding)
/// Per rewrite documentation data-structures/cons-cells.md
pub fn setCDR(cons_cell: *ConsCell, cell_address: LispPTR, cdr_value: LispPTR) void {
    if (cdr_value == 0) {
        // NIL
        cons_cell.cdr_code = CDR_NIL;
        return;
    }

    // Check if same page encoding (NEWCDRCODING)
    const cell_page = cell_address & 0xFFFF00;
    const cdr_page = cdr_value & 0xFFFF00;

    if (cell_page == cdr_page and cdr_value > cell_address and cdr_value <= cell_address + 14) {
        // Same page encoding (3-bit offset)
        const offset = (cdr_value - cell_address) >> 1;
        cons_cell.cdr_code = @as(u8, @intCast(CDR_ONPAGE_MIN + offset));
    } else {
        // Different page or too far - use simple offset encoding for now
        // TODO: Implement proper different-page encoding or indirect cell allocation
        // Check if cdr_value >= cell_address to avoid integer overflow/wrap-around
        if (cdr_value >= cell_address) {
            const offset = (cdr_value - cell_address) >> 1;
            if (offset < 128) {
                cons_cell.cdr_code = @as(u8, @intCast(offset));
            } else {
                cons_cell.cdr_code = CDR_INDIRECT;
                cons_cell.car_field = cdr_value;
            }
        } else {
            // cdr_value < cell_address: use indirect encoding to avoid overflow
            cons_cell.cdr_code = CDR_INDIRECT;
            cons_cell.car_field = cdr_value;
        }
    }
}
