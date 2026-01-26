// Definition cell structure (matches C DefCell)
// Based on maiko/inc/cell.h

const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("../vm/stack.zig");
const virtual_memory_module = @import("../memory/virtual.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const VM = stack.VM;

// Assume BIGVM for now (most common case)
const BIGVM = true;

/// Definition cell structure (matches C DefCell)
/// For BIGVM: 28-bit defpointer, for non-BIGVM: 24-bit defpointer
pub const DefCell = packed struct {
    ccodep: u1, // C code flag (1 = C function, 0 = Lisp function)
    fastp: u1, // Fast function flag
    argtype: u2, // Argument type
    defpointer: if (BIGVM) u28 else u24, // Pointer to function definition (fnhead)
    // For BIGVM, additional fields follow:
    nil_PL: if (BIGVM) LispPTR else void, // Skip proplist cell (BIGVM only)
    nilpkg: if (BIGVM) u8 else void, // Skip pkg byte (BIGVM only)
    nil2: if (BIGVM) u4 else void, // Padding (BIGVM only)
    pseudocodep: if (BIGVM) u1 else void, // Pseudo code flag (BIGVM only)
    byteswapped: if (BIGVM) u1 else void, // Byte swapped flag (BIGVM only)
    nil_last: if (BIGVM) u18 else void, // Padding (BIGVM only)
};

/// Read DefCell from atom's definition cell
/// C: GetDEFCELL68k(atom_index)
pub fn readDefCell(vm: *VM, atom_index: LispPTR) errors.VMError!DefCell {
    const atom_module = @import("atom.zig");
    const virtual_memory = vm.virtual_memory orelse {
        return errors.VMError.MemoryAccessFailed;
    };
    
    // Get definition cell offset
    std.debug.print("DEBUG readDefCell: Calling getDEFCELL with atom_index=0x{x}\n", .{atom_index});
    const defcell_offset = atom_module.getDEFCELL(vm, atom_index) catch |err| {
        std.debug.print("ERROR readDefCell: getDEFCELL failed with error: {}\n", .{err});
        return err;
    };
    
    std.debug.print("DEBUG readDefCell: defcell_offset=0x{x} ({d}), virtual_memory.len={d} (0x{x})\n", 
        .{ defcell_offset, defcell_offset, virtual_memory.len, virtual_memory.len });
    std.debug.print("  defcell_offset / 2: {d} (0x{x})\n", .{ defcell_offset / 2, defcell_offset / 2 });
    std.debug.print("  defcell_offset * 2: {d} (0x{x})\n", .{ defcell_offset * 2, defcell_offset * 2 });
    
    if (defcell_offset + @sizeOf(DefCell) > virtual_memory.len) {
        std.debug.print("ERROR readDefCell: defcell_offset (0x{x}) + DefCell size ({}) exceeds virtual_memory.len (0x{x})\n", 
            .{ defcell_offset, @sizeOf(DefCell), virtual_memory.len });
        return errors.VMError.InvalidAddress;
    }
    
    std.debug.print("DEBUG readDefCell: Reading DefCell from offset 0x{x}\n", .{defcell_offset});
    // Read DefCell from virtual_memory (already byte-swapped to native little-endian)
    // CRITICAL: Check bounds before slicing to avoid panic
    if (defcell_offset + @sizeOf(DefCell) > virtual_memory.len) {
        std.debug.print("ERROR readDefCell: Bounds check failed! defcell_offset=0x{x}, DefCell size={}, virtual_memory.len={}\n", 
            .{ defcell_offset, @sizeOf(DefCell), virtual_memory.len });
        return errors.VMError.InvalidAddress;
    }
    const defcell_bytes = virtual_memory[defcell_offset..][0..@sizeOf(DefCell)];
    std.debug.print("DEBUG readDefCell: Successfully read DefCell bytes\n", .{});
    std.debug.print("  First 4 bytes: 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", 
        .{ defcell_bytes[0], defcell_bytes[1], defcell_bytes[2], defcell_bytes[3] });
    
    // For BIGVM, DefCell is 4 LispPTRs (16 bytes)
    // Structure layout: [ccodep:1, fastp:1, argtype:2, defpointer:28] in first LispPTR
    // C code: defcell_word = *(int *)fn_defcell; (reads as native little-endian int)
    // Read first LispPTR as little-endian (native byte order, already swapped)
    const first_ptr: LispPTR = std.mem.readInt(LispPTR, defcell_bytes[0..4], .little);
    std.debug.print("  first_ptr (little-endian): 0x{x:0>8}\n", .{first_ptr});
    
    // Extract fields from first LispPTR
    const ccodep = @as(u1, @truncate(first_ptr >> 31));
    const fastp = @as(u1, @truncate(first_ptr >> 30));
    const argtype = @as(u2, @truncate(first_ptr >> 28));
    const defpointer = @as(u28, @truncate(first_ptr & 0x0FFFFFFF)); // Low 28 bits
    
    // For now, return simplified DefCell (BIGVM format)
    return DefCell{
        .ccodep = ccodep,
        .fastp = fastp,
        .argtype = argtype,
        .defpointer = defpointer,
        .nil_PL = 0,
        .nilpkg = 0,
        .nil2 = 0,
        .pseudocodep = 0,
        .byteswapped = 0,
        .nil_last = 0,
    };
}

/// Get function header pointer from DefCell
/// C: NativeAligned4FromLAddr(defcell->defpointer & POINTERMASK)
pub fn getFunctionHeader(defcell: DefCell) LispPTR {
    const defpointer_lisp: LispPTR = @as(LispPTR, defcell.defpointer);
    return types.POINTERMASK & defpointer_lisp;
}

/// Check if DefCell points to C code
/// C: fn_defcell->ccodep
pub fn isCCode(defcell: DefCell) bool {
    return defcell.ccodep != 0;
}