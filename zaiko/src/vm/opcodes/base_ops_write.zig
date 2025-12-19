const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

// ============================================================================
// Base Operations - Write Operations
// ============================================================================

/// PUTBASEBYTE: Put base byte
/// Per rewrite documentation instruction-set/opcodes.md
/// C: PUTBASEBYTE - Write byte to base + byteoffset
/// Stack: [value, byteoffset, base] -> []
pub fn handlePUTBASEBYTE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const type_check_module = @import("../../utils/type_check.zig");
    
    // Validate value is S_POSITIVE and < 256
    const value = stack_module.getTopOfStack(vm);
    if (((value & types.SEGMASK) != types.S_POSITIVE) or ((value & 0xFFFF) >= 256)) {
        // C: goto op_ufn
        return;
    }
    
    // Get byteoffset (must be small integer)
    const byteoffset_value = try stack_module.popStack(vm);
    var byteoffset = byteoffset_value;
    const segment = byteoffset & types.SEGMASK;
    
    if (segment == types.S_POSITIVE) {
        byteoffset &= 0x0000FFFF;
    } else if (segment == types.S_NEGATIVE) {
        byteoffset |= 0xFFFF0000;
    } else {
        // Check if FIXP object - need virtual memory access first
        const fptovp_table_fixp = vm.fptovp orelse {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const type_num = type_check_module.getTypeNumber(vm, byteoffset_value);
        if (type_num) |tn| {
            if (tn == type_check_module.TYPE_FIXP) {
                // FIXP is a boxed integer - read the int32 value from memory
                // C: FIXP_VALUE(dest) = *((int *)NativeAligned4FromLAddr(dest))
                const fixp_ptr = virtual_memory_module.translateAddress(byteoffset_value, fptovp_table_fixp, 4) catch {
                    return errors_module.VMError.InvalidAddress;
                };
                const fixp_value_ptr: *i32 = @as(*i32, @ptrCast(@alignCast(fixp_ptr)));
                const fixp_value = fixp_value_ptr.*;
                // Convert to signed offset
                byteoffset = @as(LispPTR, @bitCast(@as(i32, @intCast(fixp_value))));
            } else {
                // Not a valid offset - trigger UFN
                return;
            }
        } else {
            // Not a valid offset - trigger UFN
            return;
        }
    }
    
    // Pop base
    const base = try stack_module.popStack(vm);
    const base_ptr = types.POINTERMASK & base;
    
    // Translate address to native pointer
    const virtual_memory_mut = if (vm.virtual_memory) |vmem| @constCast(vmem) else {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    
    const native_ptr = virtual_memory_module.translateAddress(base_ptr + @as(LispPTR, @bitCast(@as(i32, @intCast(byteoffset)))), fptovp_table, 1) catch {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Write byte
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory_mut.ptr);
    if (byte_offset >= virtual_memory_mut.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    virtual_memory_mut[byte_offset] = @as(u8, @truncate(value & 0xFF));
    
    // Pop value (already consumed)
    _ = try stack_module.popStack(vm);
}

/// PUTBASE_N: Put base N
/// Per rewrite documentation instruction-set/opcodes.md
/// C: PUTBASE_N(n) - Write DLword to base + offset
/// Stack: [value, base] -> [base]
pub fn handlePUTBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    
    const value = stack_module.getTopOfStack(vm);
    // Validate value is S_POSITIVE (small integer)
    if ((value & types.SEGMASK) != types.S_POSITIVE) {
        // C: goto op_ufn
        return;
    }
    
    const base = try stack_module.popStack(vm);
    const base_ptr = types.POINTERMASK & base;
    
    // Translate address to native pointer
    const virtual_memory_mut = if (vm.virtual_memory) |vmem| @constCast(vmem) else {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    
    const native_ptr = virtual_memory_module.translateAddress(base_ptr + @as(LispPTR, index), fptovp_table, 2) catch {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Write DLword (2 bytes, convert to big-endian for sysout format)
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory_mut.ptr);
    if (byte_offset + 2 > virtual_memory_mut.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    const word_value = types.getLoWord(value); // Extract low word
    virtual_memory_mut[byte_offset] = @as(u8, @truncate(word_value >> 8)); // High byte
    virtual_memory_mut[byte_offset + 1] = @as(u8, @truncate(word_value)); // Low byte
    
    // Push base back on stack
    try stack_module.pushStack(vm, base);
}

/// PUTBASEPTR_N: Put base pointer N
/// Per rewrite documentation instruction-set/opcodes.md
/// C: PUTBASEPTR_N(n) - Write LispPTR to base + offset
/// Stack: [value, base] -> [base]
pub fn handlePUTBASEPTR_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    
    const value = stack_module.getTopOfStack(vm);
    const base = try stack_module.popStack(vm);
    const base_ptr = types.POINTERMASK & base;
    
    // Translate address to native pointer
    const virtual_memory_mut = if (vm.virtual_memory) |vmem| @constCast(vmem) else {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    
    const native_ptr = virtual_memory_module.translateAddress(base_ptr + @as(LispPTR, index), fptovp_table, 4) catch {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Write LispPTR (4 bytes, convert to big-endian for sysout format)
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory_mut.ptr);
    if (byte_offset + 4 > virtual_memory_mut.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    const ptr_value = types.POINTERMASK & value; // Mask to pointer
    virtual_memory_mut[byte_offset] = @as(u8, @truncate(ptr_value >> 24)); // Byte 0
    virtual_memory_mut[byte_offset + 1] = @as(u8, @truncate(ptr_value >> 16)); // Byte 1
    virtual_memory_mut[byte_offset + 2] = @as(u8, @truncate(ptr_value >> 8)); // Byte 2
    virtual_memory_mut[byte_offset + 3] = @as(u8, @truncate(ptr_value)); // Byte 3
    
    // Push base back on stack
    try stack_module.pushStack(vm, base);
}

/// PUTBITS_N_FD: Put bits N FD
/// Per rewrite documentation instruction-set/opcodes.md
/// C: PUTBITS_N_M(a, b) - Write bit field to memory
/// Stack: [value, base] -> [base]
/// arg1 = offset (a), arg2 = field descriptor (b)
/// Field descriptor format: [shift:4][size:4]
pub fn handlePUTBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    
    // Validate value is S_POSITIVE
    const value = stack_module.getTopOfStack(vm);
    if ((value & types.SEGMASK) != types.S_POSITIVE) {
        // C: goto op_ufn
        return;
    }
    
    const base = try stack_module.popStack(vm);
    const base_ptr = types.POINTERMASK & base;
    
    // Translate address to native pointer
    const virtual_memory_mut = if (vm.virtual_memory) |vmem| @constCast(vmem) else {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    
    const native_ptr = virtual_memory_module.translateAddress(base_ptr + @as(LispPTR, arg1), fptovp_table, 2) catch {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Read current DLword (2 bytes, big-endian)
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory_mut.ptr);
    if (byte_offset + 2 > virtual_memory_mut.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    const word_bytes = virtual_memory_mut[byte_offset..][0..2];
    var word_value: DLword = (@as(DLword, word_bytes[0]) << 8) | @as(DLword, word_bytes[1]);
    
    // Parse field descriptor: b = [shift:4][size:4]
    const field_size = 0xF & arg2; // Low 4 bits = field size
    const shift_pos = 0xF & (arg2 >> 4); // High 4 bits = shift position
    
    // Calculate shift: 15 - shift_pos - field_size
    const shift = 15 - (@as(u8, shift_pos) + field_size);
    
    // Create mask for field_size bits at shift position
    const field_mask: DLword = if (field_size == 0) 0 else (@as(DLword, 1) << @as(u4, @intCast(field_size))) - 1;
    const shifted_mask = field_mask << @as(u4, @intCast(shift));
    
    // Extract value to write (low field_size bits)
    const value_to_write = types.getLoWord(value) & field_mask;
    
    // Update word: clear field, then set new value
    word_value = (word_value & ~shifted_mask) | (value_to_write << @as(u4, @intCast(shift)));
    
    // Write back (big-endian)
    virtual_memory_mut[byte_offset] = @as(u8, @truncate(word_value >> 8));
    virtual_memory_mut[byte_offset + 1] = @as(u8, @truncate(word_value));
    
    // Push base back on stack
    try stack_module.pushStack(vm, base);
}
