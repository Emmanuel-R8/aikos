const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

// ============================================================================
// Basic Variable Access Opcodes
// ============================================================================

/// IVAR0-IVAR6: Local variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handleIVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // IVar access: variables stored at nextblock offset
    // CRITICAL: frame.nextblock is a DLword offset from Stackspace, not a LispPTR
    // C: IVAR = NativeAligned2FromStackOffset(CURRENTFX->nextblock) = Stackspace + nextblock
    // Stackspace is DLword*, so Stackspace + nextblock adds nextblock DLwords = nextblock * 2 bytes
    const nextblock = stack_module.getNextblock(frame);
    if (nextblock == 0) {
        try stack_module.pushStack(vm, 0); // No IVar area - return NIL
        return;
    }

    // Calculate IVAR base address: Stackspace + nextblock (in bytes)
    // Stackspace is at STK_OFFSET * 2 = 0x20000 bytes
    const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
    const stackspace_byte_offset = STK_OFFSET * 2; // Convert DLword offset to byte offset
    const ivar_base_byte_offset = stackspace_byte_offset + (@as(usize, @intCast(nextblock)) * 2); // nextblock is in DLwords

    // Access IVar at index (each IVar is LispPTR = 4 bytes)
    const ivar_offset_bytes = @as(usize, index) * @sizeOf(types.LispPTR);
    const ivar_addr = ivar_base_byte_offset + ivar_offset_bytes;

    // Read from virtual memory (big-endian from sysout)
    if (vm.virtual_memory == null or ivar_addr + 4 > vm.virtual_memory.?.len) {
        try stack_module.pushStack(vm, 0);
        return;
    }

    const virtual_memory = vm.virtual_memory.?;
    const ivar_bytes = virtual_memory[ivar_addr..ivar_addr+4];
    const low_word = (@as(types.DLword, ivar_bytes[0]) << 8) | @as(types.DLword, ivar_bytes[1]);
    const high_word = (@as(types.DLword, ivar_bytes[2]) << 8) | @as(types.DLword, ivar_bytes[3]);
    const ivar_value: types.LispPTR = (@as(types.LispPTR, high_word) << 16) | @as(types.LispPTR, low_word);

    try stack_module.pushStack(vm, ivar_value);
}

/// PVAR0-PVAR6: Parameter variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handlePVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // PVar access: parameters stored right after frame header (FRAMESIZE offset)
    // Use helper function to get parameter value
    const pvar_value = stack_module.getPVar(frame, index);
    try stack_module.pushStack(vm, pvar_value);
}

/// FVAR0-FVAR6: Free variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
/// Free variables are stored in PVar area after regular parameters
/// Each free variable occupies 2 words (low word and high word)
pub fn handleFVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // Get function header from frame
    const fnheader_ptr = stack_module.getFnHeader(frame);
    std.debug.print("DEBUG FVAR: frame.fnheader=0x{x}\n", .{fnheader_ptr});
    if (fnheader_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // No function header
    }

    // Translate function header address to byte offset in virtual memory
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return errors_module.VMError.InvalidAddress;
    }

    const fptovp_table = vm.fptovp.?;
    const virtual_memory = vm.virtual_memory.?;
    std.debug.print("DEBUG FVAR: Translating fnheader_ptr=0x{x}\n", .{fnheader_ptr});

    // Get page number and offset from LispPTR
    // LispPTR format: [page_num:15 bits][offset_in_page:9 bits][unused:8 bits]
    const page_num = (fnheader_ptr >> 9) & 0x7FFF; // Page number (15 bits)
    const page_offset_dlwords = fnheader_ptr & 0x1FF; // Page offset (9 bits, in DLwords)
    const page_offset_bytes = page_offset_dlwords * 2; // Convert DLwords to bytes

    // Get virtual page from FPtoVP table
    if (page_num >= fptovp_table.entries.len) {
        std.debug.print("DEBUG FVAR: Invalid page number {}\n", .{page_num});
        return errors_module.VMError.InvalidAddress;
    }

    const virtual_page = fptovp_table.getFPtoVP(page_num);
    if (virtual_page == 0) {
        std.debug.print("DEBUG FVAR: Page {} is sparse\n", .{page_num});
        return errors_module.VMError.InvalidAddress;
    }

    // Calculate byte offset in virtual memory: virtual_page * 512 + page_offset_bytes
    const BYTESPER_PAGE: usize = 512;
    const fnheader_byte_offset = (@as(usize, virtual_page) * BYTESPER_PAGE) + page_offset_bytes;

    if (fnheader_byte_offset + 8 > virtual_memory.len) {
        std.debug.print("DEBUG FVAR: Function header offset exceeds virtual memory bounds\n", .{});
        return errors_module.VMError.InvalidAddress;
    }

    // Read function header fields manually (big-endian from sysout)
    // Function header layout: stkmin (2), na (2), pv (2), startpc (2), framename (4), ...
    // Per maiko/inc/stack.h:58-66
    if (fnheader_byte_offset + 6 > virtual_memory.len) {
        std.debug.print("DEBUG FVAR: Function header offset exceeds virtual memory bounds\n", .{});
        return errors_module.VMError.InvalidAddress;
    }

    const fnheader_bytes = virtual_memory[fnheader_byte_offset..];
    // Read pv field (bytes 4-5, big-endian)
    const pv_be = (@as(types_module.DLword, fnheader_bytes[4]) << 8) | @as(types_module.DLword, fnheader_bytes[5]);

    // Free variables are stored in PVar area after regular parameters
    // PVars are stored as LispPTR (4 bytes each)
    // Free variables are stored as 2 DLwords (2 bytes each) = 4 bytes total
    // Free variable offset calculation:
    //   PVar area size = (pv + 1) * sizeof(LispPTR) = (pv + 1) * 4 bytes
    //   Free variable i starts at: PVar area size + (i * 4 bytes)
    const pvar_count = pv_be + 1; // PVar count includes return value slot
    const pvar_area_size = @as(usize, pvar_count) * @sizeOf(LispPTR);
    const fvar_offset_bytes = pvar_area_size + (@as(usize, index) * 4); // Each FVAR is 4 bytes (2 DLwords)

    // Access free variable slot (2 words)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base_addr = frame_addr + frame_size;

    // Get low word and high word (each is 2 bytes)
    const low_word_addr = pvar_base_addr + fvar_offset_bytes;
    const high_word_addr = low_word_addr + @sizeOf(types_module.DLword);

    const low_word_ptr: *types_module.DLword = @as(*types_module.DLword, @ptrFromInt(low_word_addr));
    const high_word_ptr: *types_module.DLword = @as(*types_module.DLword, @ptrFromInt(high_word_addr));

    const low_word = low_word_ptr.*;
    const high_word = high_word_ptr.*;

    // Check if unbound (LSB of low word indicates unbound)
    // For now, we'll skip the lookup and just return the value
    // TODO: Implement nfvlookup for unbound variables

    // Construct LispPTR from two words: (high_word << 16) | low_word
    const fvar_value: LispPTR = (@as(LispPTR, high_word) << 16) | @as(LispPTR, low_word);

    // Mask to get pointer (clear tag bits)
    const masked_value = fvar_value & 0xFFFFFFFE; // Clear LSB

    // Push value onto stack
    try stack_module.pushStack(vm, masked_value);
}

/// GVAR: Global variable access
/// Per rewrite documentation instruction-set/opcodes.md
/// Accesses global variable via atom index
/// C: GVAR macro in maiko/inc/inlineC.h
/// BIGATOMS+BIGVM mode: atom_index is a 4-byte LispPTR pointer
/// Non-BIGATOMS mode: atom_index is a 2-byte DLword
pub fn handleGVAR(vm: *VM, atom_index: types.LispPTR) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // GVAR: Read value from atom's value cell and push on stack
    // C: GVAR(x) does PUSH(GetLongWord(Valspace + ((x) << 1))) for non-BIGATOMS
    // C: For BIGATOMS+BIGVM: PUSH(GetLongWord((LispPTR *)AtomSpace + (tx * 5) + NEWATOM_VALUE_PTROFF))
    const value = try atom_module.readAtomValue(vm, atom_index);
    // C uses the TOS-stack macros: PUSH(x) pushes the current TOS to the value stack,
    // then sets TOS to x (tos1defs.h).
    try stack_module.tosPush(vm, value);
}

/// ACONST: Atom constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes atom constant by atom index
/// C: ACONST macro in maiko/inc/inlineC.h
pub fn handleACONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // ACONST: Push atom object (atom_index as LispPTR)
    const atom_index_lisp: types.LispPTR = @as(types.LispPTR, atom_index);
    const atom_ptr = atom_module.getAtomPointer(atom_index_lisp);
    try stack_module.pushStack(vm, atom_ptr);
}
