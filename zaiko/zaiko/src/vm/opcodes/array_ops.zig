const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const array = @import("../../data/array.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

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
/// C: inlineC.h AREF1 macro
/// Stack: [index, array_ptr] -> [element_value]
pub fn handleAREF1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const address_module = @import("../../utils/address.zig");
    
    // C: arrayarg = POP_TOS_1; (pop array pointer)
    const arrayarg = try stack_module.popStack(vm);
    
    // C: if (GetTypeNumber(arrayarg) != TYPE_ONED_ARRAY) goto aref_ufn;
    // For now, skip type check (will implement later)
    
    // C: arrayblk = (OneDArray *)NativeAligned4FromLAddr(arrayarg);
    // Translate array pointer to native address
    if (vm.virtual_memory) |vmem| {
        if (vm.fptovp) |fptovp_table| {
            const array_offset = address_module.translateLispPTRToOffset(arrayarg, fptovp_table, vmem.len) orelse {
                // Invalid address - could call UFN, but for now just push NIL
                try stack_module.pushStack(vm, 0);
                return;
            };
            
            // C: if ((TOPOFSTACK & SEGMASK) != S_POSITIVE) goto aref_ufn;
            const index_value = stack_module.getTopOfStack(vm);
            const S_POSITIVE: types.LispPTR = 0x00000000; // Positive segment mask
            const SEGMASK: types.LispPTR = 0xFFFF0000;
            
            if ((index_value & SEGMASK) != S_POSITIVE) {
                // Invalid index - could call UFN, but for now just push NIL
                try stack_module.pushStack(vm, 0);
                return;
            }
            
            // C: index = TOPOFSTACK & 0xFFFF;
            const index = @as(u16, @truncate(index_value));
            
            // Read array header (OneDArray structure)
            // For now, simplified implementation - just read element at index
            // TODO: Properly implement OneDArray structure access
            const element_offset = array_offset + (@as(usize, index) * 4); // Assume 32-bit elements
            
            if (element_offset + 4 > vmem.len) {
                try stack_module.pushStack(vm, 0);
                return;
            }
            
            // Read element (32-bit, big-endian)
            const element_be: types.LispPTR = (@as(types.LispPTR, vmem[element_offset]) << 24) |
                (@as(types.LispPTR, vmem[element_offset + 1]) << 16) |
                (@as(types.LispPTR, vmem[element_offset + 2]) << 8) |
                (@as(types.LispPTR, vmem[element_offset + 3]));
            
            // C: TOPOFSTACK = element_value; (replace index with element)
            stack_module.setTopOfStack(vm, element_be);
        } else {
            try stack_module.pushStack(vm, 0);
        }
    } else {
        try stack_module.pushStack(vm, 0);
    }
}

/// ASET1: Array set 1-dimensional
/// C: inlineC.h ASET1 macro (similar to AREF1 but sets value)
/// Stack: [value, index, array_ptr] -> [array_ptr]
pub fn handleASET1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const address_module = @import("../../utils/address.zig");
    
    // C: Pop value, index, array_ptr from stack
    const value = try stack_module.popStack(vm);
    const index_value = try stack_module.popStack(vm);
    const arrayarg = try stack_module.popStack(vm);
    
    // C: if (GetTypeNumber(arrayarg) != TYPE_ONED_ARRAY) goto aset_ufn;
    // For now, skip type check (will implement later)
    
    // C: arrayblk = (OneDArray *)NativeAligned4FromLAddr(arrayarg);
    // Translate array pointer to native address
    if (vm.virtual_memory) |vmem| {
        if (vm.fptovp) |fptovp_table| {
            const array_offset = address_module.translateLispPTRToOffset(arrayarg, fptovp_table, vmem.len) orelse {
                // Invalid address - push array_ptr back and return
                try stack_module.pushStack(vm, arrayarg);
                return;
            };
            
            // C: if ((index_value & SEGMASK) != S_POSITIVE) goto aset_ufn;
            const S_POSITIVE: types.LispPTR = 0x00000000;
            const SEGMASK: types.LispPTR = 0xFFFF0000;
            
            if ((index_value & SEGMASK) != S_POSITIVE) {
                try stack_module.pushStack(vm, arrayarg);
                return;
            }
            
            // C: index = index_value & 0xFFFF;
            const index = @as(u16, @truncate(index_value));
            
            // Write element at index (32-bit, big-endian)
            const element_offset = array_offset + (@as(usize, index) * 4); // Assume 32-bit elements
            
            if (element_offset + 4 > vmem.len) {
                try stack_module.pushStack(vm, arrayarg);
                return;
            }
            
            // Write element (32-bit, big-endian)
            const vmem_mut: []u8 = @constCast(vmem);
            vmem_mut[element_offset] = @as(u8, @truncate(value >> 24));
            vmem_mut[element_offset + 1] = @as(u8, @truncate(value >> 16));
            vmem_mut[element_offset + 2] = @as(u8, @truncate(value >> 8));
            vmem_mut[element_offset + 3] = @as(u8, @truncate(value));
            
            // C: Push array_ptr back
            try stack_module.pushStack(vm, arrayarg);
        } else {
            try stack_module.pushStack(vm, arrayarg);
        }
    } else {
        try stack_module.pushStack(vm, arrayarg);
    }
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