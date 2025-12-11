// Atom table access utilities
// Implements atom table access matching C implementation in maiko/inc/cell.h

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

// Assume BIGVM and BIGATOMS for now (most common case)
// TODO: Detect from build config or sysout
const BIGVM = true;
const BIGATOMS = true;

/// Get value cell pointer for an atom (matching GetVALCELL68k)
/// Returns byte offset in virtual memory for the value cell
pub fn getVALCELL(_: *VM, atom_index: LispPTR) errors.VMError!usize {
    if (!BIGATOMS) {
        // Non-BIGATOMS: Valspace + (atom_index << 1)
        // Valspace is at a fixed offset in virtual memory
        // TODO: Get Valspace offset from memory layout
        const valspace_offset: usize = 0; // TODO: Get from memory layout
        return valspace_offset + (@as(usize, @intCast(atom_index)) << 1);
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
            // BIGVM: AtomSpace + (atom_index * 5) + NEWATOM_VALUE_PTROFF
            // AtomSpace is at ATOMS_OFFSET in virtual memory
            // Each atom is 5 LispPTRs (5 * 4 = 20 bytes)
            const atom_offset = ATOMS_OFFSET + (@as(usize, @intCast(atom_index)) * 20);
            const value_offset = atom_offset + (NEWATOM_VALUE_PTROFF * @sizeOf(LispPTR));
            return value_offset;
        } else {
            // Non-BIGVM: Valspace + (atom_index << 1)
            const valspace_offset: usize = 0; // TODO: Get from memory layout
            return valspace_offset + (@as(usize, @intCast(atom_index)) << 1);
        }
    }
}

/// Get definition cell pointer for an atom (matching GetDEFCELL68k)
/// Returns byte offset in virtual memory for the definition cell
pub fn getDEFCELL(_: *VM, atom_index: LispPTR) errors.VMError!usize {
    if (!BIGATOMS) {
        // Non-BIGATOMS: Defspace + atom_index
        const defspace_offset: usize = 0; // TODO: Get from memory layout
        return defspace_offset + (@as(usize, @intCast(atom_index)) * @sizeOf(LispPTR));
    }

    // BIGATOMS: Check if NEWATOM or LITATOM
    if ((atom_index & SEGMASK) != 0) {
        // NEWATOM: Use NativeAligned4FromLAddr(atom_index + NEWATOM_DEFN_OFFSET)
        const defn_offset_bytes = NEWATOM_DEFN_OFFSET * 2;
        const defn_addr = @as(usize, @intCast(atom_index)) + defn_offset_bytes;
        return defn_addr;
    } else {
        // LITATOM
        if (BIGVM) {
            // BIGVM: AtomSpace + (atom_index * 5) + NEWATOM_DEFN_PTROFF
            const atom_offset = ATOMS_OFFSET + (@as(usize, @intCast(atom_index)) * 20);
            const defn_offset = atom_offset + (NEWATOM_DEFN_PTROFF * @sizeOf(LispPTR));
            return defn_offset;
        } else {
            // Non-BIGVM: Defspace + atom_index
            const defspace_offset: usize = 0; // TODO: Get from memory layout
            return defspace_offset + (@as(usize, @intCast(atom_index)) * @sizeOf(LispPTR));
        }
    }
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

    // Read LispPTR from virtual memory (big-endian from sysout)
    // Convert to native little-endian
    const value_bytes = virtual_memory[value_cell_offset..][0..@sizeOf(LispPTR)];
    const value: LispPTR = (@as(LispPTR, value_bytes[0]) << 24) |
                           (@as(LispPTR, value_bytes[1]) << 16) |
                           (@as(LispPTR, value_bytes[2]) << 8) |
                           (@as(LispPTR, value_bytes[3]));

    return value;
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

    // Write LispPTR to virtual memory (convert to big-endian for sysout format)
    // For now, write in native format (will be converted on save if needed)
    const value_ptr: *LispPTR = @ptrFromInt(@intFromPtr(virtual_memory_mut.ptr) + value_cell_offset);
    value_ptr.* = value;
}

/// Get atom object pointer (for ACONST, GCONST)
/// For LITATOM: returns atom_index directly
/// For NEWATOM: returns atom_index (which is already the pointer)
pub fn getAtomPointer(atom_index: LispPTR) LispPTR {
    // For now, return atom_index as-is
    // In Lisp, atoms are represented by their index (LITATOM) or pointer (NEWATOM)
    return atom_index;
}