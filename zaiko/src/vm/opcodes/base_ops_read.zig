const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

// ============================================================================
// Base Operations - Read Operations
// ============================================================================

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
                const virtual_memory_check = vm.virtual_memory orelse return errors_module.VMError.MemoryAccessFailed;
                const fixp_ptr = virtual_memory_module.translateAddress(virtual_memory_check, byteoffset, fptovp_table_check, 4) catch {
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

/// GETBASE_N: Get base N
/// Per rewrite documentation instruction-set/opcodes.md
/// C: GETBASE_N(N) - Read DLword from base + offset
/// Stack: [base] -> [value]
pub fn handleGETBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const base = stack_module.getTopOfStack(vm);
    const base_ptr = types.POINTERMASK & base;

    // Translate address to native pointer
    const virtual_memory = vm.virtual_memory orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };

    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, base_ptr + @as(LispPTR, index), fptovp_table, 2) catch {
        return errors_module.VMError.InvalidAddress;
    };

    // Read DLword (2 bytes, big-endian from sysout)
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory.ptr);
    if (byte_offset + 2 > virtual_memory.len) {
        return errors_module.VMError.InvalidAddress;
    }

    const word_bytes = virtual_memory[byte_offset..][0..2];
    const word_value: DLword = (@as(DLword, word_bytes[0]) << 8) | @as(DLword, word_bytes[1]);

    // Push as S_POSITIVE | word_value
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

    // Translate address to native pointer
    const virtual_memory = vm.virtual_memory orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };

    const target_addr = base_ptr + @as(LispPTR, index);

    // DEBUG: Check if this is the problematic case
    if (base_ptr == 0x140000 and index == 0x36) {
        std.debug.print("DEBUG GETBASEPTR_N: base=0x{x}, index=0x{x}, target_addr=0x{x}\n", .{ base_ptr, index, target_addr });
    }

    const native_ptr = virtual_memory_module.translateAddress(virtual_memory, target_addr, fptovp_table, 4) catch {
        if (base_ptr == 0x140000 and index == 0x36) {
            std.debug.print("DEBUG GETBASEPTR_N: translateAddress failed for 0x{x}\n", .{target_addr});
        }
        return errors_module.VMError.InvalidAddress;
    };

    // Read LispPTR (4 bytes) directly from native pointer in native byte order
    // C: *((LispPTR *)NativeAligned4FromLAddr(...)) reads in native byte order (little-endian on x86_64)
    // The native_ptr points to memory that has already been byte-swapped during page loading
    // Ensure 4-byte alignment: calculate byte offset and read as aligned LispPTR
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory.ptr);
    if (byte_offset + 4 > virtual_memory.len) {
        return errors_module.VMError.InvalidAddress;
    }

    // Read 4 bytes and interpret as LispPTR in native byte order (little-endian)
    const bytes = virtual_memory[byte_offset..][0..4];
    const ptr_value = std.mem.readInt(LispPTR, bytes, .little);

    // DEBUG: Check if this is the problematic case
    if (base_ptr == 0x140000 and index == 0x36) {
        std.debug.print("DEBUG GETBASEPTR_N: byte_offset=0x{x}, bytes=0x{x}{x}{x}{x}, ptr_value=0x{x}\n", .{ byte_offset, bytes[0], bytes[1], bytes[2], bytes[3], ptr_value });
    }

    // Push as POINTERMASK & ptr_value
    const result = types.POINTERMASK & ptr_value;
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
