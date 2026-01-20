// Atom table access utilities
// Implements atom table access matching C implementation in maiko/inc/cell.h

const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("../vm/stack.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const VM = stack.VM;

// Constants from maiko/inc/lispemul.h and maiko/inc/cell.h
const SEGMASK: LispPTR = 0x0F000000; // Segment mask for NEWATOM detection
const NEWATOM_VALUE_OFFSET: u32 = 4; // Offset in DLwords (2 bytes each) = 8 bytes
const NEWATOM_DEFN_OFFSET: u32 = 8; // Offset in DLwords = 16 bytes
const NEWATOM_PNAME_OFFSET: u32 = 0; // Offset in DLwords = 0 bytes
const NEWATOM_PLIST_OFFSET: u32 = 12; // Offset in DLwords = 24 bytes

// For BIGVM BIGATOMS: AtomSpace layout (5 LispPTRs per atom)
const NEWATOM_VALUE_PTROFF: usize = 1; // Value cell is at index 1 (after PNAME at 0)
const NEWATOM_DEFN_PTROFF: usize = 2; // Definition cell is at index 2
const NEWATOM_PNAME_PTROFF: usize = 0; // Pname cell is at index 0
const NEWATOM_PLIST_PTROFF: usize = 3; // Plist cell is at index 3

// Memory layout constants from maiko/inc/lispmap.h
// For BIGVM: ATOMS_OFFSET = 0x2c0000 (byte offset)
// For non-BIGVM: Valspace, Defspace, etc. are separate
const ATOMS_OFFSET: u32 = 0x2c0000; // Byte offset for AtomSpace (BIGVM)
const ATOM_OFFSET: u32 = 0x00000; // Byte offset for ATOMSPACE (non-BIGVM)

// Sysout/emulator build configuration.
//
// For Medley starter.sysout in this repo, opcode operands like the initial GVAR
// use 16-bit atom indices (non-BIGATOMS), and the C reference uses VALSPACE/DEFSPACE
// indexing (`GetVALCELL68k(index) ((LispPTR*)Valspace + index)`).
//
// TODO: detect BIGATOMS from build/sysout once we support both.
const BIGVM = true;
const BIGATOMS = false; // Changed to false for non-BIGATOMS mode (16-bit atom indices)

// lispmap.h (BIGVM) offsets are DLword offsets (see `NativeAligned2FromLAddr` in maiko/inc/adr68k.h).
// These are used for non-BIGATOMS Valspace/Defspace addressing.
const DEFS_OFFSET_DLWORDS: usize = 0xA0000;
const VALS_OFFSET_DLWORDS: usize = 0xC0000;

/// Get value cell pointer for an atom (matching GetVALCELL68k)
/// Returns byte offset in virtual memory for the value cell
pub fn getVALCELL(_: *VM, atom_index: LispPTR) errors.VMError!usize {
    if (!BIGATOMS) {
        // Non-BIGATOMS: `GetLongWord(Valspace + ((x) << 1))`.
        // Valspace base is VALS_OFFSET (DLwords); each LispPTR is 2 DLwords.
        const laddr_dlwords: usize = VALS_OFFSET_DLWORDS + (@as(usize, @intCast(atom_index)) << 1);
        return laddr_dlwords * 2;
    }

    // BIGATOMS: Check if NEWATOM or LITATOM
    if ((atom_index & SEGMASK) != 0) {
        // NEWATOM: Use NativeAligned4FromLAddr(atom_index + NEWATOM_VALUE_OFFSET)
        // NEWATOM_VALUE_OFFSET is in DLwords, convert to bytes: * 2
        const value_offset_bytes = NEWATOM_VALUE_OFFSET * 2;
        const value_addr = @as(usize, @intCast(atom_index)) + value_offset_bytes;

        // Translate to virtual memory offset
        // For now, assume atom_index is already a virtual memory offset
        // TODO: Proper address translation if needed
        return value_addr;
    } else {
        // LITATOM
        if (BIGVM) {
            // BIGATOMS+BIGVM (C: `GetLongWord((LispPTR *)AtomSpace + (tx * 5) + NEWATOM_VALUE_PTROFF)`).
            // AtomSpace base is ATOMS_OFFSET in DLwords; indexing is in LispPTR cells (2 DLwords each).
            const cells: usize = (@as(usize, @intCast(atom_index)) * 5) + NEWATOM_VALUE_PTROFF;
            const laddr_dlwords: usize = @as(usize, ATOMS_OFFSET) + (cells * 2);
            return laddr_dlwords * 2;
        }

        // Non-BIGVM: litatoms use Valspace in the C macro.
        const laddr_dlwords: usize = VALS_OFFSET_DLWORDS + (@as(usize, @intCast(atom_index)) << 1);
        return laddr_dlwords * 2;
    }
}

/// Get definition cell pointer for an atom (matching GetDEFCELL68k)
/// Returns byte offset in virtual memory for the definition cell
pub fn getDEFCELL(vm: *VM, atom_index: LispPTR) errors.VMError!usize {
    const virtual_memory = vm.virtual_memory orelse {
        return errors.VMError.MemoryAccessFailed;
    };

    if (!BIGATOMS) {
        // Non-BIGATOMS: Defspace base is `DS_OFFSET` in DLwords (BIGVM layout).
        // Byte offset = (DEFS_OFFSET + (atom_index << 1)) * 2.
        const laddr_dlwords: usize = DEFS_OFFSET_DLWORDS + (@as(usize, @intCast(atom_index)) << 1);
        const byte_off = laddr_dlwords * 2;
        if (byte_off + @sizeOf(LispPTR) > virtual_memory.len) return errors.VMError.InvalidAddress;
        return byte_off;
    }

    // BIGATOMS: Check if NEWATOM or LITATOM
    if ((atom_index & SEGMASK) != 0) {
        // NEWATOM: Use NativeAligned4FromLAddr(atom_index + NEWATOM_DEFN_OFFSET)
        const defn_offset_bytes = NEWATOM_DEFN_OFFSET * 2;
        const defn_addr = @as(usize, @intCast(atom_index)) + defn_offset_bytes;
        if (defn_addr + @sizeOf(LispPTR) > virtual_memory.len) return errors.VMError.InvalidAddress;
        return defn_addr;
    }

    // LITATOM
    if (BIGVM) {
        // BIGATOMS+BIGVM (C: `GetLongWord((LispPTR *)AtomSpace + (tx * 5) + NEWATOM_DEFN_PTROFF)`).
        const cells: usize = (@as(usize, @intCast(atom_index)) * 5) + NEWATOM_DEFN_PTROFF;
        const laddr_dlwords: usize = @as(usize, ATOMS_OFFSET) + (cells * 2);
        const defn_offset = laddr_dlwords * 2;
        if (defn_offset + @sizeOf(LispPTR) > virtual_memory.len) return errors.VMError.InvalidAddress;
        return defn_offset;
    }

    // Non-BIGVM: not yet supported here
    return errors.VMError.InvalidAddress;
}

/// Read value from atom's value cell (matching GVAR macro)
pub fn readAtomValue(vm: *VM, atom_index: LispPTR) errors.VMError!LispPTR {
    const virtual_memory = vm.virtual_memory orelse {
        return error.MemoryAccessFailed;
    };

    const value_cell_offset = try getVALCELL(vm, atom_index);

    if (value_cell_offset + @sizeOf(LispPTR) > virtual_memory.len) {
        return error.InvalidAddress;
    }

    // NOTE: Sysout pages are byte-swapped into native endianness during load.
    const value_bytes = virtual_memory[value_cell_offset..][0..@sizeOf(LispPTR)];
    return std.mem.readInt(LispPTR, value_bytes, .little);
}

/// Write value to atom's value cell (matching GVAR_ opcode)
pub fn writeAtomValue(vm: *VM, atom_index: LispPTR, value: LispPTR) errors.VMError!void {
    const virtual_memory_mut = if (vm.virtual_memory) |vmem| @constCast(vmem) else {
        return error.MemoryAccessFailed;
    };

    const value_cell_offset = try getVALCELL(vm, atom_index);

    if (value_cell_offset + @sizeOf(LispPTR) > virtual_memory_mut.len) {
        return error.InvalidAddress;
    }

    // NOTE: Sysout pages are byte-swapped into native endianness during load,
    // so writes should be native little-endian.
    const dst = virtual_memory_mut[value_cell_offset..][0..@sizeOf(LispPTR)];
    std.mem.writeInt(LispPTR, dst, value, .little);
}

/// Get atom object pointer (for ACONST, GCONST)
/// For LITATOM: returns atom_index directly
/// For NEWATOM: returns atom_index (which is already the pointer)
pub fn getAtomPointer(atom_index: LispPTR) LispPTR {
    // For now, return atom_index as-is
    // In Lisp, atoms are represented by their index (LITATOM) or pointer (NEWATOM)
    return atom_index;
}
