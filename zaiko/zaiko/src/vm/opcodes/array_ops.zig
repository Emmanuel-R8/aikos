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