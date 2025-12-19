const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

// ============================================================================
// Extended Variable Access Opcodes (with DLword offsets)
// ============================================================================

/// PVARX_: Set PVAR X (word offset)
/// C: PVARX_(x) macro in maiko/inc/inlineC.h
/// Sets parameter variable using DLword offset
/// Stack: [value] -> []
/// Operand: x (1B, DLword offset)
pub fn handlePVAR_SET(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: PVARX_(x): *((LispPTR *)((DLword *)PVAR + (x))) = TOPOFSTACK;
    const value = try stack_module.popStack(vm);
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // PVAR area starts after frame header (FRAMESIZE bytes)
    const frame_addr = @intFromPtr(frame);
    const pvar_base_addr = frame_addr + @sizeOf(stack.FX);
    
    // Access PVAR at word_offset (DLword units)
    const pvar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const pvar_addr = pvar_base_addr + pvar_offset_bytes;
    
    // Write LispPTR (2 DLwords, big-endian)
    const pvar_bytes: [*]u8 = @ptrFromInt(pvar_addr);
    const low_word = @as(types_module.DLword, @truncate(value));
    const high_word = @as(types_module.DLword, @truncate(value >> 16));
    pvar_bytes[0] = @as(u8, @truncate(low_word >> 8));
    pvar_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    pvar_bytes[2] = @as(u8, @truncate(high_word >> 8));
    pvar_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
}

/// PVARX: Get PVAR X (word offset)
/// C: PVARX(x) macro in maiko/inc/inlineC.h
/// Gets parameter variable using DLword offset (not LispPTR offset)
/// Stack: [] -> [value]
/// Operand: x (1B, DLword offset)
pub fn handlePVARX(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: PVARX(x): PUSH(GetLongWord((DLword *)PVAR + (x)));
    // x is a DLword offset, not a LispPTR offset
    // GetLongWord reads 2 DLwords (4 bytes) as a LispPTR
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // PVAR area starts after frame header (FRAMESIZE bytes)
    const frame_addr = @intFromPtr(frame);
    const pvar_base_addr = frame_addr + @sizeOf(stack.FX);
    
    // Access PVAR at word_offset (DLword units)
    // Each LispPTR is 2 DLwords, so offset in bytes = word_offset * 2
    const pvar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const pvar_addr = pvar_base_addr + pvar_offset_bytes;
    
    // Read LispPTR (2 DLwords, big-endian)
    const pvar_bytes: [*]const u8 = @ptrFromInt(pvar_addr);
    const low_word = (@as(types_module.DLword, pvar_bytes[0]) << 8) | @as(types_module.DLword, pvar_bytes[1]);
    const high_word = (@as(types_module.DLword, pvar_bytes[2]) << 8) | @as(types_module.DLword, pvar_bytes[3]);
    const value: types.LispPTR = (@as(types.LispPTR, high_word) << 16) | @as(types.LispPTR, low_word);
    
    try stack_module.pushStack(vm, value);
}

/// IVARX: Get IVAR X (word offset)
/// C: IVARX(x) macro in maiko/inc/inlineC.h
/// Gets instance variable using DLword offset (not LispPTR offset)
/// Stack: [] -> [value]
/// Operand: x (1B, DLword offset)
pub fn handleIVARX(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: IVARX(x): PUSH(GetLongWord((DLword *)IVAR + (x)));
    // IVAR is frame.nextblock (LispPTR address)
    // x is a DLword offset, not a LispPTR offset
    // GetLongWord reads 2 DLwords (4 bytes) as a LispPTR
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // IVAR area is at frame.nextblock (DLword offset from Stackspace)
    const nextblock = stack_module.getNextblock(frame);
    if (nextblock == 0) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    
    // Calculate IVAR base address: Stackspace + nextblock (in bytes)
    const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
    const stackspace_byte_offset = STK_OFFSET * 2;
    const ivar_base_byte_offset = stackspace_byte_offset + (@as(usize, @intCast(nextblock)) * 2);
    
    // Access IVAR at word_offset (DLword units)
    const ivar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const ivar_addr = ivar_base_byte_offset + ivar_offset_bytes;
    
    // Read from virtual memory (big-endian from sysout)
    if (vm.virtual_memory == null or ivar_addr + 4 > vm.virtual_memory.?.len) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    
    const virtual_memory = vm.virtual_memory.?;
    const ivar_bytes = virtual_memory[ivar_addr..ivar_addr+4];
    const low_word = (@as(types_module.DLword, ivar_bytes[0]) << 8) | @as(types_module.DLword, ivar_bytes[1]);
    const high_word = (@as(types_module.DLword, ivar_bytes[2]) << 8) | @as(types_module.DLword, ivar_bytes[3]);
    const value: types.LispPTR = (@as(types.LispPTR, high_word) << 16) | @as(types.LispPTR, low_word);
    
    try stack_module.pushStack(vm, value);
}

/// IVARX_: Set IVAR X (word offset)
/// C: IVARX_(x) macro in maiko/inc/inlineC.h
/// Sets instance variable using DLword offset
/// Stack: [value] -> []
/// Operand: x (1B, DLword offset)
pub fn handleIVARX_(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: IVARX_(x): *((LispPTR *)((DLword *)IVAR + (x))) = TOPOFSTACK;
    const value = try stack_module.popStack(vm);
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // IVAR area is at frame.nextblock (DLword offset from Stackspace)
    const nextblock = stack_module.getNextblock(frame);
    if (nextblock == 0) {
        return; // No IVAR area - ignore
    }
    
    // Calculate IVAR base address: Stackspace + nextblock (in bytes)
    const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
    const stackspace_byte_offset = STK_OFFSET * 2;
    const ivar_base_byte_offset = stackspace_byte_offset + (@as(usize, @intCast(nextblock)) * 2);
    
    // Access IVAR at word_offset (DLword units)
    const ivar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const ivar_addr = ivar_base_byte_offset + ivar_offset_bytes;
    
    // Write to virtual memory (big-endian from sysout)
    if (vm.virtual_memory == null or ivar_addr + 4 > vm.virtual_memory.?.len) {
        return;
    }
    
    const virtual_memory_mut: []u8 = @constCast(vm.virtual_memory.?);
    const ivar_bytes = virtual_memory_mut[ivar_addr..ivar_addr+4];
    const low_word = @as(types_module.DLword, @truncate(value));
    const high_word = @as(types_module.DLword, @truncate(value >> 16));
    ivar_bytes[0] = @as(u8, @truncate(low_word >> 8));
    ivar_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    ivar_bytes[2] = @as(u8, @truncate(high_word >> 8));
    ivar_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
}

/// FVARX_: Set FVAR X
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets free variable value
pub fn handleFVARX_(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // FVARX_ requires:
    // 1. Value on stack
    // 2. Set FVAR at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}
