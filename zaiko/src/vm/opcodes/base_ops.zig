const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");
const storage_module = @import("../../memory/storage.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// n_mask_array: Bit masks for N-bit values (1-16 bits)
/// INDEX: n-1 gives mask for n bits (n_mask_array[0] = 1, [15] = 0xffff)
/// Matches C definition in maiko/src/xc.c:396-398
const n_mask_array: [16]DLword = .{
    1, 3, 7, 0xf, 0x1f, 0x3f, 0x7f, 0xff,
    0x1ff, 0x3ff, 0x7ff, 0xfff, 0x1fff, 0x3fff, 0x7fff, 0xffff,
};

/// GETBASEBYTE: Get base byte
/// Per rewrite documentation instruction-set/opcodes.md
/// C: GETBASEBYTE - Read byte from base + byteoffset
/// Stack: [byteoffset, base] -> [byte_value]
pub fn handleGETBASEBYTE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const type_check_module = @import("../../utils/type_check.zig");

    // Get byteoffset from TOS (must be small integer or fixnum)
    var byteoffset = stack_module.getTopOfStack(vm);
    const segment = byteoffset & types.SEGMASK;

    if (segment == types.S_POSITIVE) {
        byteoffset &= 0x0000FFFF; // Extract low 16 bits
    } else if (segment == types.S_NEGATIVE) {
        byteoffset |= 0xFFFF0000; // Sign extend
    } else {
        // Check if FIXP object - need virtual memory access first
        const fptovp_table_check = vm.fptovp orelse {
            return errors_module.VMError.MemoryAccessFailed;
        };

        const type_num = type_check_module.getTypeNumber(vm, byteoffset);
        if (type_num) |tn| {
            if (tn == type_check_module.TYPE_FIXP) {
                // FIXP is a boxed integer - read the int32 value from memory
                // C: FIXP_VALUE(dest) = *((int *)NativeAligned4FromLAddr(dest))
                const virtual_memory = vm.virtual_memory orelse return error.MemoryAccessFailed;
                const fixp_ptr = virtual_memory_module.translateAddress(virtual_memory, byteoffset, fptovp_table_check, 4) catch {
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
    const virtual_memory = vm.virtual_memory orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };

    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, base_ptr + @as(LispPTR, @bitCast(@as(i32, @intCast(byteoffset)))), fptovp_table, 1) catch {
        return errors_module.VMError.InvalidAddress;
    };

    // Read byte
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory.ptr);
    if (byte_offset >= virtual_memory.len) {
        return errors_module.VMError.InvalidAddress;
    }

    const byte_value = virtual_memory[byte_offset];

    // Push as S_POSITIVE | (0xFF & byte_value)
    const result = types.S_POSITIVE | (@as(LispPTR, byte_value) & 0xFF);
    stack_module.setTopOfStack(vm, result);
}

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
                const virtual_memory = vm.virtual_memory orelse return error.MemoryAccessFailed;
                const fixp_ptr = virtual_memory_module.translateAddress(virtual_memory, byteoffset_value, fptovp_table_fixp, 4) catch {
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

    const virtual_memory = vm.virtual_memory orelse return error.MemoryAccessFailed;
    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, base_ptr + @as(LispPTR, @bitCast(@as(i32, @intCast(byteoffset)))), fptovp_table, 1) catch {
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

/// GETBASE_N: Get base N
/// Per rewrite documentation instruction-set/opcodes.md
/// C: GETBASE_N(N) - Read DLword from base + offset
/// Stack: [base] -> [value]
/// Base may be in sysout or storage heap; use translateAddressExtended when storage is set.
pub fn handleGETBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const base = stack_module.getTopOfStack(vm);
    const base_ptr = types.POINTERMASK & base;
    const target_addr = base_ptr + @as(LispPTR, index);

    const virtual_memory = vm.virtual_memory orelse {
        stack_module.setTopOfStack(vm, types.S_POSITIVE | 0);
        return;
    };
    const fptovp_table = vm.fptovp orelse {
        stack_module.setTopOfStack(vm, types.S_POSITIVE | 0);
        return;
    };

    const native_ptr = if (vm.storage) |storage|
        virtual_memory_module.translateAddressExtended(
            virtual_memory,
            storage,
            target_addr,
            fptovp_table,
            2,
        ) catch {
            stack_module.setTopOfStack(vm, types.S_POSITIVE | 0);
            return;
        }
    else
        virtual_memory_module.translateAddress(virtual_memory, target_addr, fptovp_table, 2) catch {
            stack_module.setTopOfStack(vm, types.S_POSITIVE | 0);
            return;
        };

    const storage_offset_opt = if (vm.storage) |s| storage_module.lispPTRToOffset(s, target_addr) else null;
    const in_storage = storage_offset_opt != null;
    const word_value: DLword = if (in_storage and vm.storage != null) blk: {
        const s = vm.storage.?;
        const offset = storage_offset_opt.?;
        if (offset + 2 > storage_module.getStorageSize(s)) {
            stack_module.setTopOfStack(vm, types.S_POSITIVE | 0);
            return;
        }
        const bytes: *const [2]u8 = @ptrCast(native_ptr);
        break :blk (@as(DLword, bytes[0]) | (@as(DLword, bytes[1]) << 8));
    } else blk: {
        const target_byte_offset: usize = @as(usize, @intCast(types.POINTERMASK & target_addr)) * 2;
        if (target_byte_offset + 2 > virtual_memory.len or target_byte_offset >= virtual_memory.len) {
            stack_module.setTopOfStack(vm, types.S_POSITIVE | 0);
            return;
        }
        const word_bytes = virtual_memory[target_byte_offset..][0..2];
        break :blk (@as(DLword, word_bytes[0]) << 8) | @as(DLword, word_bytes[1]);
    };

    const result = types.S_POSITIVE | @as(LispPTR, word_value);
    stack_module.setTopOfStack(vm, result);
}

/// GETBASEPTR_N: Get base pointer N
/// Per rewrite documentation instruction-set/opcodes.md
/// C: GETBASEPTR_N(N) - Read LispPTR from base + offset
/// Stack: [base] -> [pointer]
pub fn handleGETBASEPTR_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const base = stack_module.getTopOfStack(vm);
    const base_ptr = types.POINTERMASK & base;

    std.debug.print("DEBUG GETBASEPTR_N: index=0x{x}, base=0x{x}, base_ptr=0x{x}\n", .{ index, base, base_ptr });

    // Translate address to native pointer
    const virtual_memory = vm.virtual_memory orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };

    const target_addr = base_ptr + @as(LispPTR, index);
    std.debug.print("DEBUG GETBASEPTR_N: target_addr=0x{x}\n", .{target_addr});

    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, target_addr, fptovp_table, 4) catch {
        return errors_module.VMError.InvalidAddress;
    };

    // Read LispPTR (4 bytes) from native pointer.
    // Pages are already byte-swapped into native endianness on load, but the address may be
    // unaligned (C tolerates this on x86). Avoid `@alignCast` and read via bytes.
    const ptr_bytes: *const [4]u8 = @ptrCast(native_ptr);
    const ptr_value: LispPTR = std.mem.readInt(LispPTR, ptr_bytes, .little);
    std.debug.print("DEBUG GETBASEPTR_N: ptr_value=0x{x}\n", .{ptr_value});

    // Push as POINTERMASK & ptr_value
    const result = types.POINTERMASK & ptr_value;
    std.debug.print("DEBUG GETBASEPTR_N: result=0x{x}\n", .{result});
    stack_module.setTopOfStack(vm, result);
}

/// GETBITS_N_FD: Get bits N FD
/// Per rewrite documentation instruction-set/opcodes.md
/// C: GETBITS_N_M(a, b) - Extract bit field from memory
/// Stack: [base] -> [bit_field]
/// arg1 = offset (a), arg2 = field descriptor (b)
/// Field descriptor format: [shift:4][size:4]
pub fn handleGETBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const base = stack_module.getTopOfStack(vm);
    const base_ptr = types.POINTERMASK & base;
    const target_addr = base_ptr + @as(LispPTR, arg1);

    // Calculate byte offset in virtual memory
    const virtual_memory = vm.virtual_memory orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };

    // Calculate byte offset from LispPTR
    const page_num = (target_addr >> 9) & 0x7FFF; // Page number (15 bits)
    const page_offset_dlwords = target_addr & 0x1FF; // Page offset (9 bits, in DLwords)
    const page_offset_bytes = page_offset_dlwords * 2; // Convert DLwords to bytes

    // Get virtual page from FPtoVP table
    if (page_num >= fptovp_table.entries.len) {
        return errors_module.VMError.InvalidAddress;
    }

    const virtual_page = fptovp_table.getFPtoVP(page_num);
    if (virtual_page == 0) {
        return errors_module.VMError.InvalidAddress;
    }

    // Calculate byte offset in virtual memory: virtual_page * 512 + page_offset_bytes
    const BYTESPER_PAGE: usize = 512;
    const byte_offset = (@as(usize, virtual_page) * BYTESPER_PAGE) + page_offset_bytes;

    if (byte_offset + 2 > virtual_memory.len) {
        return errors_module.VMError.InvalidAddress;
    }

    // Read DLword (2 bytes, big-endian)
    const word_bytes = virtual_memory[byte_offset .. byte_offset + 2];
    const word_value: DLword = (@as(DLword, word_bytes[0]) << 8) | @as(DLword, word_bytes[1]);

    // Parse field descriptor: b = [shift:4][size:4]
    const field_size = 0xF & arg2; // Low 4 bits = field size
    const shift_pos = 0xF & (arg2 >> 4); // High 4 bits = shift position

    // Calculate shift: 16 - (shift_pos + field_size + 1)
    const shift = 16 - (@as(u8, shift_pos) + field_size + 1);

    // Create mask for field_size bits
    const field_mask: DLword = if (field_size == 0) 0 else (@as(DLword, 1) << @as(u4, @intCast(field_size))) - 1;

    // Extract bit field
    const bit_field = (word_value >> @as(u4, @intCast(shift))) & field_mask;

    // Push as S_POSITIVE | bit_field
    const result = types.S_POSITIVE | @as(LispPTR, bit_field);
    stack_module.setTopOfStack(vm, result);
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

    const virtual_memory = vm.virtual_memory orelse return error.MemoryAccessFailed;
    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, base_ptr + @as(LispPTR, index), fptovp_table, 2) catch {
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

    const virtual_memory = vm.virtual_memory orelse return error.MemoryAccessFailed;
    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, base_ptr + @as(LispPTR, index), fptovp_table, 4) catch {
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
/// Exact translation of maiko/inc/inlineC.h:585-600 PUTBITS_N_M macro
pub fn handlePUTBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // C: if ((SEGMASK & TOPOFSTACK) != S_POSITIVE) { goto op_ufn; };
    const value = stack_module.getTopOfStack(vm);
    if ((value & types.SEGMASK) != types.S_POSITIVE) {
        // C: goto op_ufn
        return;
    }

    // C: base = POINTERMASK & POP_TOS_1;
    const base = try stack_module.popStack(vm);
    const base_ptr = types.POINTERMASK & base;

    // C: pword = NativeAligned2FromLAddr(base + (a));
    // Translate address to native pointer (DLword aligned)
    const virtual_memory_mut = if (vm.virtual_memory) |vmem| @constCast(vmem) else {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };

    const virtual_memory = vm.virtual_memory orelse return error.MemoryAccessFailed;
    const target_addr = base_ptr + @as(LispPTR, arg1);
    const native_ptr = if (vm.storage) |storage|
        virtual_memory_module.translateAddressExtended(
            virtual_memory,
            storage,
            target_addr,
            fptovp_table,
            2,
        ) catch {
            return errors_module.VMError.InvalidAddress;
        }
    else
        virtual_memory_module.translateAddress(virtual_memory, target_addr, fptovp_table, 2) catch {
            return errors_module.VMError.InvalidAddress;
        };

    // C: field_size = 0xF & bb;
    const field_size = 0xF & arg2;

    // C: shift_size = 15 - (0xF & (bb >> 4)) - field_size;
    const shift_pos = 0xF & (arg2 >> 4);
    const shift_size = 15 - shift_pos - field_size;

    // C: fmask = n_mask_array[field_size] << shift_size;
    const fmask = n_mask_array[field_size] << @as(u4, @intCast(shift_size));

    // C: GETWORD(pword) = ((TOPOFSTACK << shift_size) & fmask) | (GETWORD(pword) & (~fmask));
    // Read current DLword (big-endian format from sysout)
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory_mut.ptr);
    if (byte_offset + 2 > virtual_memory_mut.len) {
        return errors_module.VMError.InvalidAddress;
    }

    const word_bytes = virtual_memory_mut[byte_offset..][0..2];
    const current_word: DLword = (@as(DLword, word_bytes[0]) << 8) | @as(DLword, word_bytes[1]);

    // C uses TOPOFSTACK directly (which is value, still on TOS before pop)
    // TOPOFSTACK is a LispPTR, but we need the low word for bit operations
    const value_word = types.getLoWord(value);

    // Apply bit field write: ((value << shift_size) & fmask) | (current_word & (~fmask))
    const new_word: DLword = ((value_word << @as(u4, @intCast(shift_size))) & fmask) | (current_word & ~fmask);

    // Write back (big-endian format)
    virtual_memory_mut[byte_offset] = @as(u8, @truncate(new_word >> 8));
    virtual_memory_mut[byte_offset + 1] = @as(u8, @truncate(new_word));

    // C: TOPOFSTACK = base;
    // Set TOS directly (don't push) to match C semantics
    stack_module.setTopOfStack(vm, base);
}

/// ADDBASE: Add base
/// Per rewrite documentation instruction-set/opcodes.md
/// C: ADDBASE - Add two base addresses
/// Stack: [b, a] -> [a + b]
pub fn handleADDBASE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Apply POINTERMASK to both operands
    const a_ptr = types.POINTERMASK & a;
    const b_ptr = types.POINTERMASK & b;
    const result = a_ptr + b_ptr;

    // Result is masked pointer
    const masked_result = types.POINTERMASK & result;
    try stack_module.pushStack(vm, masked_result);
}

/// HILOC: High location
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_HILOC - TOPOFSTACK = GetHiWord(TOPOFSTACK) | S_POSITIVE
pub fn handleHILOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const value = stack_module.getTopOfStack(vm);
    const hi_word = types.getHiWord(value);
    const result = types.S_POSITIVE | @as(LispPTR, hi_word);
    stack_module.setTopOfStack(vm, result);
}

/// LOLOC: Low location
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_LOLOC - TOPOFSTACK = GetLoWord(TOPOFSTACK) | S_POSITIVE
pub fn handleLOLOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const value = stack_module.getTopOfStack(vm);
    const lo_word = types.getLoWord(value);
    const result = types.S_POSITIVE | @as(LispPTR, lo_word);
    stack_module.setTopOfStack(vm, result);
}

/// BASE_LESSTHAN: Base less than
/// Per rewrite documentation instruction-set/opcodes.md
/// C: BASE_LESSTHAN - Compare two base addresses
/// Stack: [b, a] -> [a < b ? T : NIL]
pub fn handleBASE_LESSTHAN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Apply POINTERMASK to both operands
    const a_ptr = types.POINTERMASK & a;
    const b_ptr = types.POINTERMASK & b;

    const result: LispPTR = if (a_ptr < b_ptr) types.S_POSITIVE | 1 else 0; // T or NIL
    try stack_module.pushStack(vm, result);
}
