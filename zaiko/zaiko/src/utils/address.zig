const std = @import("std");
const types = @import("types.zig");
const errors = @import("errors.zig");
const layout = @import("../memory/layout.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Translate LispPTR to native pointer (2-byte aligned)
/// Per contracts/vm-core-interface.zig and rewrite documentation
pub fn translateAddress2(ptr: LispPTR, fptovp: []LispPTR) errors.VMError![*]DLword {
    if (fptovp.len == 0) {
        return error.InvalidAddress;
    }

    // Extract page number
    const page_num = layout.getPageNumber(ptr);
    const page_offset = layout.getPageOffset(ptr);

    if (page_num >= fptovp.len) {
        return error.InvalidAddress;
    }

    // Get virtual page base from FPtoVP table
    const virtual_page_base = fptovp[page_num];
    if (virtual_page_base == 0) {
        return error.InvalidAddress;
    }

    // Calculate native address
    // For now, assume direct mapping (will need proper page mapping later)
    const native_base: [*]DLword = @as([*]DLword, @ptrFromInt(@as(usize, virtual_page_base)));
    const native_addr = native_base + (page_offset / @sizeOf(DLword));

    // Align to 2-byte boundary
    const aligned_addr = @as(usize, @intFromPtr(native_addr)) & ~@as(usize, 1);
    return @as([*]DLword, @ptrFromInt(aligned_addr));
}

/// Translate LispPTR to native pointer (4-byte aligned)
/// Per contracts/vm-core-interface.zig
pub fn translateAddress4(ptr: LispPTR, fptovp: []LispPTR) errors.VMError![*]u32 {
    if (fptovp.len == 0) {
        return error.InvalidAddress;
    }

    // Extract page number
    const page_num = layout.getPageNumber(ptr);
    const page_offset = layout.getPageOffset(ptr);

    if (page_num >= fptovp.len) {
        return error.InvalidAddress;
    }

    // Get virtual page base from FPtoVP table
    const virtual_page_base = fptovp[page_num];
    if (virtual_page_base == 0) {
        return error.InvalidAddress;
    }

    // Calculate native address
    const native_base: [*]u8 = @as([*]u8, @ptrFromInt(@as(usize, virtual_page_base)));
    const native_addr = native_base + page_offset;

    // Align to 4-byte boundary
    const aligned_addr = @as(usize, @intFromPtr(native_addr)) & ~@as(usize, 3);
    return @as([*]u32, @ptrFromInt(aligned_addr));
}

/// Reverse translation: native pointer to LispPTR
/// Per contracts/vm-core-interface.zig
pub fn lispAddressFromNative(native: [*]DLword, fptovp: []LispPTR) LispPTR {
    _ = fptovp; // TODO: Implement reverse mapping using FPtoVP table
    // For now, use direct address conversion
    const native_addr = @intFromPtr(native);
    return @as(LispPTR, @intCast(native_addr));
}

/// Translate LispPTR to offset in virtual_memory slice
/// C: NativeAligned4FromLAddr(LAddr) = (void *)(Lisp_world + LAddr)
/// CRITICAL: LispPTR is a DLword offset from Lisp_world, not a byte offset!
/// Per maiko/inc/lspglob.h: "Pointers in Cell or any object means DLword offset from Lisp_world"
/// Since Lisp_world is DLword*, pointer arithmetic: Lisp_world + LAddr = LAddr DLwords = LAddr * 2 bytes
/// Per maiko/inc/adr68k.h and maiko/inc/lspglob.h
/// NOTE: fptovp_table parameter is for compatibility but not actually used (direct offset calculation)
pub fn translateLispPTRToOffset(ptr: LispPTR, fptovp_table: *const @import("../data/sysout.zig").FPtoVPTable, virtual_memory_len: usize) ?usize {
    _ = fptovp_table; // FPtoVP is used during page loading, but address translation is direct offset

    // C: NativeAligned4FromLAddr(LAddr) = (void *)(Lisp_world + LAddr)
    // Lisp_world is DLword*, so Lisp_world + LAddr adds LAddr DLwords = LAddr * 2 bytes
    // Per maiko/inc/lspglob.h: "Pointers in Cell or any object means DLword offset from Lisp_world"
    const byte_offset = @as(usize, @intCast(ptr)) * 2; // Convert DLword offset to byte offset

    if (byte_offset >= virtual_memory_len) {
        return null; // Address out of bounds
    }

    return byte_offset;
}