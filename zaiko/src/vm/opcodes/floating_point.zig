const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const float_conv = @import("../../utils/float_conversion.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// Floating-point Operations

/// FPLUS2: Floating-point addition
/// Matches C N_OP_fplus2 (maiko/src/fp.c:46-60)
/// Per rewrite documentation instruction-set/opcodes.md
/// Adds two floating-point values
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_fplus2 implementation exactly
/// - Uses makeFloat for type conversion
/// - Uses fpClear/fpTest for error handling
/// - Uses allocateFloatCell for result storage
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/fp.c lines 46-60 (N_OP_fplus2)
/// - Identified pattern: convert args, clear FP, add, test, allocate cell, store result
/// - Verified error handling matches C behavior
///
/// HOW TO TEST:
/// - Test with small positive integers
/// - Test with small negative integers
/// - Test with float cells (when VM available)
/// - Test overflow (should return error)
/// - Test NaN propagation (should return error)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_fplus2
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleFPLUS2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const storage_module = @import("../../memory/storage.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1);
    const arg2 = try float_conv.makeFloat(parg2);

    // Clear FP error status
    float_conv.fpClear();

    // Perform addition
    const result = arg1 + arg2;

    // Check for FP errors
    if (float_conv.fpTest(result)) {
        return errors.VMError.FloatingPointError;
    }

    // Allocate float cell and store result
    if (vm.storage) |storage| {
        const cell_offset = try storage_module.allocateFloatCell(storage, vm.gc);
        const cell_ptr: [*]u8 = @ptrFromInt(storage_module.getNativeBase(storage) + cell_offset);
        const float_ptr: *f32 = @alignCast(@ptrCast(cell_ptr));
        float_ptr.* = result;

        // Push result onto stack
        try stack_module.pushStack(vm, storage_module.offsetToLispPTR(storage, cell_offset));
    } else {
        return errors.VMError.MemoryAccessFailed;
    }
}

/// FDIFFERENCE: Floating-point subtraction
/// Matches C N_OP_fdifference (maiko/src/fp.c:70-83)
/// Per rewrite documentation instruction-set/opcodes.md
/// Subtracts two floating-point values
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_fdifference implementation exactly
/// - Uses makeFloat for type conversion
/// - Uses fpClear/fpTest for error handling
/// - Uses allocateFloatCell for result storage
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/fp.c lines 70-83 (N_OP_fdifference)
/// - Identified pattern: convert args, clear FP, subtract, test, allocate cell, store result
/// - Verified error handling matches C behavior
///
/// HOW TO TEST:
/// - Test with small positive integers
/// - Test with small negative integers
/// - Test with float cells (when VM available)
/// - Test overflow (should return error)
/// - Test NaN propagation (should return error)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_fdifference
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleFDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const storage_module = @import("../../memory/storage.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1);
    const arg2 = try float_conv.makeFloat(parg2);

    // Clear FP error status
    float_conv.fpClear();

    // Perform subtraction
    const result = arg1 - arg2;

    // Check for FP errors
    if (float_conv.fpTest(result)) {
        return errors.VMError.FloatingPointError;
    }

    // Allocate float cell and store result
    if (vm.storage) |storage| {
        const cell_offset = try storage_module.allocateFloatCell(storage, vm.gc);
        const cell_ptr: [*]u8 = @ptrFromInt(storage_module.getNativeBase(storage) + cell_offset);
        const float_ptr: *f32 = @alignCast(@ptrCast(cell_ptr));
        float_ptr.* = result;

        // Push result onto stack
        try stack_module.pushStack(vm, storage_module.offsetToLispPTR(storage, cell_offset));
    } else {
        return errors.VMError.MemoryAccessFailed;
    }
}

/// FTIMES2: Floating-point multiplication
/// Matches C N_OP_ftimes2 (maiko/src/fp.c:93-106)
/// Per rewrite documentation instruction-set/opcodes.md
/// Multiplies two floating-point values
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_ftimes2 implementation exactly
/// - Uses makeFloat for type conversion
/// - Uses fpClear/fpTest for error handling
/// - Uses allocateFloatCell for result storage
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/fp.c lines 93-106 (N_OP_ftimes2)
/// - Identified pattern: convert args, clear FP, multiply, test, allocate cell, store result
/// - Verified error handling matches C behavior
///
/// HOW TO TEST:
/// - Test with small positive integers
/// - Test with small negative integers
/// - Test with float cells (when VM available)
/// - Test overflow (should return error)
/// - Test NaN propagation (should return error)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_ftimes2
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleFTIMES2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const storage_module = @import("../../memory/storage.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1);
    const arg2 = try float_conv.makeFloat(parg2);

    // Clear FP error status
    float_conv.fpClear();

    // Perform multiplication
    const result = arg1 * arg2;

    // Check for FP errors
    if (float_conv.fpTest(result)) {
        return errors.VMError.FloatingPointError;
    }

    // Allocate float cell and store result
    if (vm.storage) |storage| {
        const cell_offset = try storage_module.allocateFloatCell(storage, vm.gc);
        const cell_ptr: [*]u8 = @ptrFromInt(storage_module.getNativeBase(storage) + cell_offset);
        const float_ptr: *f32 = @alignCast(@ptrCast(cell_ptr));
        float_ptr.* = result;

        // Push result onto stack
        try stack_module.pushStack(vm, storage_module.offsetToLispPTR(storage, cell_offset));
    } else {
        return errors.VMError.MemoryAccessFailed;
    }
}

/// FQUOTIENT: Floating-point division
/// Matches C N_OP_fquotient (maiko/src/fp.c:116-130)
/// Per rewrite documentation instruction-set/opcodes.md
/// Divides two floating-point values
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_fquotient implementation exactly
/// - Uses makeFloat for type conversion
/// - Uses fpClear/fpTest for error handling
/// - Uses allocateFloatCell for result storage
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/fp.c lines 116-130 (N_OP_fquotient)
/// - Identified pattern: convert args, clear FP, divide, test, allocate cell, store result
/// - Verified error handling matches C behavior
///
/// HOW TO TEST:
/// - Test with small positive integers
/// - Test with small negative integers
/// - Test with float cells (when VM available)
/// - Test division by zero (should return error)
/// - Test overflow (should return error)
/// - Test NaN propagation (should return error)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_fquotient
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleFQUOTIENT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const storage_module = @import("../../memory/storage.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1);
    const arg2 = try float_conv.makeFloat(parg2);

    // Clear FP error status
    float_conv.fpClear();

    // Perform division
    const result = arg1 / arg2;

    // Check for FP errors
    if (float_conv.fpTest(result)) {
        return errors.VMError.FloatingPointError;
    }

    // Allocate float cell and store result
    if (vm.storage) |storage| {
        const cell_offset = try storage_module.allocateFloatCell(storage, vm.gc);
        const cell_ptr: [*]u8 = @ptrFromInt(storage_module.getNativeBase(storage) + cell_offset);
        const float_ptr: *f32 = @alignCast(@ptrCast(cell_ptr));
        float_ptr.* = result;

        // Push result onto stack
        try stack_module.pushStack(vm, storage_module.offsetToLispPTR(storage, cell_offset));
    } else {
        return errors.VMError.MemoryAccessFailed;
    }
}

/// FGREATERP: Floating-point greater-than comparison
/// Matches C N_OP_fgreaterp (maiko/src/fp.c:140-149)
/// Per rewrite documentation instruction-set/opcodes.md
/// Compares two floating-point values
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_fgreaterp implementation exactly
/// - Uses makeFloat for type conversion
/// - Returns ATOM_T or NIL_PTR (no cell allocation)
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/fp.c lines 140-149 (N_OP_fgreaterp)
/// - Identified pattern: convert args, compare, return ATOM_T or NIL_PTR
/// - Verified that no cell allocation is needed
///
/// HOW TO TEST:
/// - Test with arg1 > arg2 (returns ATOM_T)
/// - Test with arg1 <= arg2 (returns NIL_PTR)
/// - Test with equal values (returns NIL_PTR)
/// - Test with NaN (should handle gracefully)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_fgreaterp
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleFGREATERP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1);
    const arg2 = try float_conv.makeFloat(parg2);

    // Compare and return ATOM_T or NIL_PTR
    const type_check = @import("../../utils/type_check.zig");
    const result: LispPTR = if (arg1 > arg2) type_check.ATOM_T else type_check.NIL_PTR;

    // Push result onto stack
    try stack_module.pushStack(vm, result);
}
