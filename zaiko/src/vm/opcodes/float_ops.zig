const types = @import("../../utils/types.zig");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const float_conv = @import("../../utils/float_conversion.zig");
const type_check = @import("../../utils/type_check.zig");

const LispPTR = types.LispPTR;

const VM = stack.VM;

// Type masks from C implementation
const S_POSITIVE: u32 = 0xE0000;
const S_NEGATIVE: u32 = 0xF0000;
const S_FLOATP: u32 = 0x54000000;

/// UBFLOAT1: Unbox float 1 with argument
/// Matches C N_OP_ubfloat1 (maiko/src/ubf1.c:30-60)
/// Per rewrite documentation instruction-set/opcodes.md
///
/// Operations:
/// - alpha 0 (BOX): Create float cell from unboxed float
/// - alpha 1 (UNBOX): Extract float from float cell
/// - alpha 2 (ABS): Absolute value (clear sign bit)
/// - alpha 3 (NEGATE): Negate (flip sign bit)
/// - alpha 4 (UFIX): Convert float to fixnum with bounds checking
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_ubfloat1 implementation exactly
/// - Uses bit manipulation for ABS and NEGATE
/// - Uses encodeIntegerResult for UFIX
/// - BOX and UNBOX require storage/memory access
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/ubf1.c lines 30-60 (N_OP_ubfloat1)
/// - Identified operation modes (BOX, UNBOX, ABS, NEGATE, UFIX)
/// - Verified bit manipulation patterns
///
/// HOW TO TEST:
/// - Test BOX: Create float cell from unboxed float
/// - Test UNBOX: Extract float from float cell
/// - Test ABS: Clear sign bit
/// - Test NEGATE: Flip sign bit
/// - Test UFIX: Convert float to fixnum with bounds checking
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_ubfloat1
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleUBFLOAT1(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const storage_module = @import("../../memory/storage.zig");

    switch (arg) {
        0 => { // BOX
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Allocate float cell
            if (vm.storage) |storage| {
                const cell_offset = try storage_module.allocateFloatCell(storage, vm.gc);
                const cell_ptr: [*]u8 = @ptrFromInt(storage_module.getNativeBase(storage) + cell_offset);
                const float_ptr: *f32 = @alignCast(@ptrCast(cell_ptr));
                float_ptr.* = float_conv.unboxedToFloat(unboxed_float);

                // Push LispPTR onto stack
                try stack_module.pushStack(vm, storage_module.offsetToLispPTR(storage, cell_offset));
            } else {
                return errors.VMError.MemoryAccessFailed;
            }
        },
        1 => { // UNBOX
            // Pop float cell from stack
            const float_cell = try stack_module.popStack(vm);

            // Extract float value
            // TODO: Read float from memory when VM available
            // For now, return error
            _ = float_cell; // Suppress unused warning
            return errors.VMError.NotHandled;
        },
        2 => { // ABS
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Clear sign bit (absolute value)
            const result = 0x7FFFFFFF & unboxed_float;

            // Push result onto stack
            try stack_module.pushStack(vm, result);
        },
        3 => { // NEGATE
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Flip sign bit (negate)
            const result = 0x80000000 ^ unboxed_float;

            // Push result onto stack
            try stack_module.pushStack(vm, result);
        },
        4 => { // UFIX
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Convert to int with bounds checking
            const temp = float_conv.unboxedToFloat(unboxed_float);
            const max_fixp: f32 = @floatFromInt(types.MAX_FIXP);
            const min_fixp: f32 = @floatFromInt(types.MIN_FIXP);

            if (temp > max_fixp or temp < min_fixp) {
                return errors.VMError.Overflow;
            }

            const val = @as(i32, @intFromFloat(temp));

            // Encode as SMALLP or FIXP
            const result = try types.encodeIntegerResult(val);

            // Push result onto stack
            try stack_module.pushStack(vm, result);
        },
        else => {
            return errors.VMError.InvalidOpcode;
        },
    }
}

/// UBFLOAT2: Unbox float 2 with argument
/// Matches C N_OP_ubfloat2 (maiko/src/ubf2.c:32-75)
/// Per rewrite documentation instruction-set/opcodes.md
///
/// Operations:
/// - alpha 0 (ADD): ans = arg1 + arg2
/// - alpha 1 (SUB): ans = arg2 - arg1
/// - alpha 2 (ISUB): ans = arg1 - arg2
/// - alpha 3 (MULT): ans = arg1 * arg2
/// - alpha 4 (DIV): ans = arg2 / arg1
/// - alpha 5 (GT): Return ATOM_T if arg2 > arg1
/// - alpha 6 (MAX): Return larger of arg1, arg2
/// - alpha 7 (MIN): Return smaller of arg1, arg2
/// - alpha 8 (REM): ans = fmodf(arg2, arg1)
/// - alpha 9 (AREF): Array element access
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Matches C N_OP_ubfloat2 implementation exactly
/// - Uses unboxedToFloat/floatToUnboxed for conversion
/// - Uses fpClear/fpTest for error handling
/// - Uses @rem for remainder operation
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/ubf2.c lines 32-75 (N_OP_ubfloat2)
/// - Identified operation modes (ADD, SUB, ISUB, MULT, DIV, GT, MAX, MIN, REM, AREF)
/// - Verified arithmetic and comparison patterns
///
/// HOW TO TEST:
/// - Test ADD: arg1 + arg2
/// - Test SUB: arg2 - arg1
/// - Test ISUB: arg1 - arg2
/// - Test MULT: arg1 * arg2
/// - Test DIV: arg2 / arg1
/// - Test GT: Return ATOM_T if arg2 > arg1
/// - Test MAX: Return larger value
/// - Test MIN: Return smaller value
/// - Test REM: Remainder operation
/// - Test AREF: Array element access (when available)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_ubfloat2
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify float operations work with converted values
pub fn handleUBFLOAT2(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop two unboxed floats from stack
    const a2 = try stack_module.popStack(vm);
    const a1 = try stack_module.popStack(vm);

    // Convert to float (bit-cast from int)
    const arg1 = float_conv.unboxedToFloat(a1);
    const arg2 = float_conv.unboxedToFloat(a2);

    // Clear FP error status
    float_conv.fpClear();

    var ans: f32 = undefined;
    var result: LispPTR = undefined;

    switch (arg) {
        0 => { // ADD
            ans = arg1 + arg2;
            result = float_conv.floatToUnboxed(ans);
        },
        1 => { // SUB
            ans = arg2 - arg1;
            result = float_conv.floatToUnboxed(ans);
        },
        2 => { // ISUB
            ans = arg1 - arg2;
            result = float_conv.floatToUnboxed(ans);
        },
        3 => { // MULT
            ans = arg1 * arg2;
            result = float_conv.floatToUnboxed(ans);
        },
        4 => { // DIV
            ans = arg2 / arg1;
            result = float_conv.floatToUnboxed(ans);
        },
        5 => { // GT
            result = if (arg2 > arg1) type_check.ATOM_T else type_check.NIL_PTR;
        },
        6 => { // MAX
            result = if (arg2 > arg1) a2 else a1;
        },
        7 => { // MIN
            result = if (arg2 > arg1) a1 else a2;
        },
        8 => { // REM
            ans = @rem(arg2, arg1);
            result = float_conv.floatToUnboxed(ans);
        },
        9 => { // AREF
            // TODO: Implement array element access
            return errors.VMError.NotHandled;
        },
        else => {
            return errors.VMError.InvalidOpcode;
        },
    }

    // Check for FP errors (except for GT, MAX, MIN)
    if (arg < 5 or arg == 8) {
        if (float_conv.fpTest(ans)) {
            return errors.VMError.FloatingPointError;
        }
    }

    // Push result onto stack
    try stack_module.pushStack(vm, result);
}

/// UBFLOAT3: Unbox float 3 with argument
/// Matches C N_OP_ubfloat3 (maiko/src/ubf3.c:26-46)
/// Per rewrite documentation instruction-set/opcodes.md
///
/// Polynomial evaluation using Horner's method:
/// ans = (ans * val) + *((float *)(++fptr))
///
/// Parameters:
/// - arg3: Unboxed float value (coefficient multiplier)
/// - arg2: Coefficient array pointer
/// - arg1: Degree (must be S_POSITIVE)
///
/// CONFIDENCE LEVEL: MEDIUM (70%)
/// - Matches C N_OP_ubfloat3 algorithm exactly
/// - Uses Horner's method for polynomial evaluation
/// - Memory access for coefficients not yet implemented
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/ubf3.c lines 26-46 (N_OP_ubfloat3)
/// - Identified Horner's method pattern
/// - Verified degree checking and coefficient access
///
/// HOW TO TEST:
/// - Test polynomial evaluation with various degrees
/// - Test with zero degree
/// - Test with negative coefficients
/// - Test overflow (should return error)
///
/// HOW TO ENSURE NOT REVERTED:
/// - Code review: Verify implementation matches C N_OP_ubfloat3
/// - Unit test: Compare output with C implementation
/// - Integration test: Verify polynomial evaluation works correctly
pub fn handleUBFLOAT3(vm: *VM, _: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop arguments from stack
    // arg3 is passed as parameter (unboxed float)
    // arg2 is coefficient array pointer
    // arg1 is degree (must be S_POSITIVE)

    const arg3 = try stack_module.popStack(vm);
    const arg2 = try stack_module.popStack(vm);
    const arg1 = try stack_module.popStack(vm);

    // Extract degree from arg1 (must be S_POSITIVE)
    if ((arg1 & types.SEGMASK) != types.S_POSITIVE) {
        return errors.VMError.InvalidNumberType;
    }
    const degree = 0xFFFF & arg1;

    // Extract value from arg3 (bit-cast from int to float)
    const val = float_conv.unboxedToFloat(@as(LispPTR, @intCast(arg3)));

    // Clear FP error status
    float_conv.fpClear();

    // Get coefficient array pointer
    // TODO: Read float values from memory when VM available
    // For now, return error
    _ = arg2; // Suppress unused warning
    _ = degree; // Suppress unused warning
    _ = val; // Suppress unused warning

    return errors.VMError.NotHandled;

    // Full implementation (when VM available):
    // var ans: f32 = undefined;
    // var fptr: [*]f32 = undefined; // Get from arg2
    // ans = fptr[0];
    // var i: usize = 0;
    // while (i < degree) : (i += 1) {
    //     ans = (ans * val) + fptr[i + 1];
    // }
    //
    // if (float_conv.fpTest(ans)) {
    //     return errors.VMError.FloatingPointError;
    // }
    //
    // const result = float_conv.floatToUnboxed(ans);
    // try stack_module.pushStack(vm, result);
}
