const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const array = @import("../../data/array.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

// ============================================================================
// Array Access Opcodes
// ============================================================================

/// GETAEL1: Get array element (1-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETAEL1(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop array pointer from stack
    const array_ptr = try stack_module.popStack(vm);

    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }

    // Get array header from memory
    if (vm.virtual_memory) |_| {
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(array_ptr, fptovp_table, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        } else {
            return errors_module.VMError.MemoryAccessFailed;
        };

        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

        // Get array element
        const element_value = array.getArrayElement(header, index);

        // Push element value
        try stack_module.pushStack(vm, element_value);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// GETAEL2: Get array element (2-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETAEL2(vm: *VM, index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop array pointer from stack
    const array_ptr = try stack_module.popStack(vm);

    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }

    // Get array header from memory
    if (vm.virtual_memory) |_| {
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(array_ptr, fptovp_table, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        } else {
            return errors_module.VMError.MemoryAccessFailed;
        };
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

        // Get array element
        const element_value = array.getArrayElement(header, index);

        // Push element value
        try stack_module.pushStack(vm, element_value);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// SETAEL1: Set array element (1-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSETAEL1(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop value and array pointer from stack
    const value = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);

    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }

    // Get array header from memory
    if (vm.virtual_memory) |_| {
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(array_ptr, fptovp_table, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        } else {
            return errors_module.VMError.MemoryAccessFailed;
        };
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

        // Set array element
        array.setArrayElement(header, index, value);

        // Push array pointer back
        try stack_module.pushStack(vm, array_ptr);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// SETAEL2: Set array element (2-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSETAEL2(vm: *VM, index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop value and array pointer from stack
    const value = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);

    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }

    // Get array header from memory
    if (vm.virtual_memory) |_| {
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(array_ptr, fptovp_table, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        } else {
            return errors_module.VMError.MemoryAccessFailed;
        };
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

        // Set array element
        array.setArrayElement(header, index, value);

        // Push array pointer back
        try stack_module.pushStack(vm, array_ptr);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}
/// AREF1: Array reference 1-dimensional
/// C: N_OP_aref1 in maiko/src/arrayops.c, AREF1 macro in maiko/inc/inlineC.h
/// Stack: [index, array_ptr] -> [element_value]
pub fn handleAREF1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const type_check_module = @import("../../utils/type_check.zig");
    const types_module = @import("../../utils/types.zig");
    const array_module = @import("../../data/array.zig");

    // C: arrayarg = POP_TOS_1; (pop array pointer)
    const arrayarg = try stack_module.popStack(vm);

    // C: if (GetTypeNumber(arrayarg) != TYPE_ONED_ARRAY) goto aref_ufn;
    const type_num = type_check_module.getTypeNumber(vm, arrayarg);
    if (type_num == null or type_num.? != 14) { // TYPE_ONED_ARRAY = 14
        // Invalid type - trigger UFN (for now, push NIL)
        try stack_module.pushStack(vm, 0);
        return;
    }

    // C: arrayblk = (OneDArray *)NativeAligned4FromLAddr(arrayarg);
    if (vm.virtual_memory == null or vm.fptovp == null) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    
    const fptovp_table = vm.fptovp.?;
    const native_ptr = virtual_memory_module.translateAddress(arrayarg, fptovp_table, 4) catch {
        try stack_module.pushStack(vm, 0);
        return;
    };
    
    const arrayblk: *array_module.OneDArray = @as(*array_module.OneDArray, @ptrCast(@alignCast(native_ptr)));

    // C: if ((TOPOFSTACK & SEGMASK) != S_POSITIVE) goto aref_ufn;
    const index_value = stack_module.getTopOfStack(vm);
    if ((index_value & types_module.SEGMASK) != types_module.S_POSITIVE) {
        try stack_module.pushStack(vm, 0);
        return;
    }

    // C: index = TOPOFSTACK & 0xFFFF;
    var index = @as(u16, @truncate(index_value));
    
    // C: if (index >= arrayblk->totalsize) goto aref_ufn;
    if (index >= arrayblk.totalsize) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    
    // C: index += arrayblk->offset;
    index = @as(u16, @intCast(@as(u32, index) + arrayblk.offset));
    
    // C: baseL = arrayblk->base;
    const baseL = arrayblk.base;
    
    // C: switch (arrayblk->typenumber) { ... }
    // Dispatch on type number
    const element_value = switch (arrayblk.typenumber) {
        array_module.TYPE_POINTER => blk: {
            // Pointer: 32 bits
            // C: TOPOFSTACK = *(NativeAligned4FromLAddr(baseL) + index);
            const base_native = virtual_memory_module.translateAddress(@as(LispPTR, baseL), fptovp_table, 4) catch {
                try stack_module.pushStack(vm, 0);
                return;
            };
            const element_ptr: *LispPTR = @as(*LispPTR, @ptrCast(@alignCast(base_native + (@as(usize, index) * 4))));
            break :blk element_ptr.*;
        },
        array_module.TYPE_SIGNED_16 => blk: {
            // Signed: 16 bits
            // C: TOPOFSTACK = (GETWORD(((DLword *)NativeAligned2FromLAddr(baseL)) + index)) & 0xFFFF;
            const base_native = virtual_memory_module.translateAddress(@as(LispPTR, baseL), fptovp_table, 2) catch {
                try stack_module.pushStack(vm, 0);
                return;
            };
            const word_ptr: *DLword = @as(*DLword, @ptrCast(@alignCast(base_native + (@as(usize, index) * 2))));
            const word_value = word_ptr.*;
            // C: if (TOPOFSTACK & 0x8000) TOPOFSTACK |= S_NEGATIVE; else TOPOFSTACK |= S_POSITIVE;
            break :blk if ((word_value & 0x8000) != 0)
                types_module.S_NEGATIVE | word_value
            else
                types_module.S_POSITIVE | word_value;
        },
        array_module.TYPE_CHARACTER => blk: {
            // Character: 8 bits
            // C: TOPOFSTACK = S_CHARACTER | ((GETBYTE(((char *)NativeAligned2FromLAddr(baseL)) + index)) & 0xFF);
            const base_native = virtual_memory_module.translateAddress(@as(LispPTR, baseL), fptovp_table, 2) catch {
                try stack_module.pushStack(vm, 0);
                return;
            };
            const byte_ptr: *u8 = @as(*u8, @ptrCast(base_native + index));
            const S_CHARACTER: LispPTR = 0x70000;
            break :blk S_CHARACTER | byte_ptr.*;
        },
        else => {
            // Other types not yet implemented - for now, return NIL
            // TODO: Implement other array element types as needed
            try stack_module.pushStack(vm, 0);
            return;
        },
    };
    
    // C: TOPOFSTACK = element_value; (replace index with element)
    stack_module.setTopOfStack(vm, element_value);
}

/// ASET1: Array set 1-dimensional
/// C: N_OP_aset1 in maiko/src/arrayops.c, ASET1 macro in maiko/inc/inlineC.h
/// Stack: [value, index, array_ptr] -> [array_ptr]
pub fn handleASET1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const type_check_module = @import("../../utils/type_check.zig");
    const types_module = @import("../../utils/types.zig");
    const array_module = @import("../../data/array.zig");

    // C: Pop value, index, array_ptr from stack
    const value = try stack_module.popStack(vm);
    const index_value = try stack_module.popStack(vm);
    const arrayarg = try stack_module.popStack(vm);

    // C: if (GetTypeNumber(arrayarg) != TYPE_ONED_ARRAY) goto aset_ufn;
    const type_num = type_check_module.getTypeNumber(vm, arrayarg);
    if (type_num == null or type_num.? != 14) { // TYPE_ONED_ARRAY = 14
        try stack_module.pushStack(vm, arrayarg);
        return;
    }

    // C: arrayblk = (OneDArray *)NativeAligned4FromLAddr(arrayarg);
    if (vm.virtual_memory == null or vm.fptovp == null) {
        try stack_module.pushStack(vm, arrayarg);
        return;
    }
    
    const fptovp_table = vm.fptovp.?;
    const native_ptr = virtual_memory_module.translateAddress(arrayarg, fptovp_table, 4) catch {
        try stack_module.pushStack(vm, arrayarg);
        return;
    };
    
    const arrayblk: *array_module.OneDArray = @as(*array_module.OneDArray, @ptrCast(@alignCast(native_ptr)));

    // C: if ((index_value & SEGMASK) != S_POSITIVE) goto aset_ufn;
    if ((index_value & types_module.SEGMASK) != types_module.S_POSITIVE) {
        try stack_module.pushStack(vm, arrayarg);
        return;
    }

    // C: index = index_value & 0xFFFF;
    var index = @as(u16, @truncate(index_value));
    
    // C: if (index >= arrayblk->totalsize) goto aset_ufn;
    if (index >= arrayblk.totalsize) {
        try stack_module.pushStack(vm, arrayarg);
        return;
    }
    
    // C: index += arrayblk->offset;
    index = @as(u16, @intCast(@as(u32, index) + arrayblk.offset));
    
    // C: base = arrayblk->base;
    const base = arrayblk.base;
    
    // C: aset_switch(arrayblk->typenumber, inx);
    // Dispatch on type number
    switch (arrayblk.typenumber) {
        array_module.TYPE_POINTER => {
            // Pointer: 32 bits
            // C: *(NativeAligned4FromLAddr(base) + index) = value;
            const base_native = virtual_memory_module.translateAddress(@as(LispPTR, base), fptovp_table, 4) catch {
                try stack_module.pushStack(vm, arrayarg);
                return;
            };
            const element_ptr: *LispPTR = @as(*LispPTR, @ptrCast(@alignCast(base_native + (@as(usize, index) * 4))));
            element_ptr.* = value;
        },
        array_module.TYPE_SIGNED_16 => {
            // Signed: 16 bits
            // C: GETWORD(((DLword *)NativeAligned2FromLAddr(base)) + index) = GetLoWord(value);
            const base_native = virtual_memory_module.translateAddress(@as(LispPTR, base), fptovp_table, 2) catch {
                try stack_module.pushStack(vm, arrayarg);
                return;
            };
            const word_ptr: *DLword = @as(*DLword, @ptrCast(@alignCast(base_native + (@as(usize, index) * 2))));
            word_ptr.* = types_module.getLoWord(value);
        },
        array_module.TYPE_CHARACTER => {
            // Character: 8 bits
            // C: GETBYTE(((char *)NativeAligned2FromLAddr(base)) + index) = value & 0xFF;
            const base_native = virtual_memory_module.translateAddress(@as(LispPTR, base), fptovp_table, 2) catch {
                try stack_module.pushStack(vm, arrayarg);
                return;
            };
            const byte_ptr: *u8 = @as(*u8, @ptrCast(base_native + index));
            byte_ptr.* = @as(u8, @truncate(value & 0xFF));
        },
        else => {
            // Other types not yet implemented - push array_ptr back and return
            // TODO: Implement other array element types as needed
            try stack_module.pushStack(vm, arrayarg);
            return;
        },
    }
    
    // C: Push array_ptr back
    try stack_module.pushStack(vm, arrayarg);
}

/// AREF2: Array reference 2
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleAREF2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // AREF2 requires:
    // 1. Array and indices on stack
    // 2. Get element at indices

    // TODO: Proper implementation needs:
    // 1. Pop indices and array
    // 2. Access multi-dimensional array element

    // Placeholder: return NIL
    const index2 = try stack_module.popStack(vm);
    const index1 = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);
    _ = index2;
    _ = index1;
    _ = array_ptr;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// ASET2: Array set 2
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleASET2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // ASET2 requires:
    // 1. Value, indices, and array on stack
    // 2. Set element at indices

    // TODO: Proper implementation needs:
    // 1. Pop value, indices, and array
    // 2. Set multi-dimensional array element

    // Placeholder: pop values
    const value = try stack_module.popStack(vm);
    const index2 = try stack_module.popStack(vm);
    const index1 = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);
    _ = value;
    _ = index2;
    _ = index1;
    _ = array_ptr;
    try stack_module.pushStack(vm, 0); // Return NIL
}