const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// MISCN: Miscellaneous N
/// Per rewrite documentation instruction-set/opcodes.md
/// MISCN: Miscellaneous operation dispatch
/// Per C implementation: maiko/src/miscn.c:OP_miscn
/// C: opc_MISCN (0x24) in maiko/inc/opcodes.h
/// 
/// **Purpose**: Dispatch opcode for various miscellaneous operations
/// 
/// **Instruction Format**: [opcode:0x24][misc_index:1B][arg_count:1B] (length: 3 bytes)
/// 
/// **Operands**:
/// - misc_index (arg1): Operation selector - determines which misc operation to perform
/// - arg_count (arg2): Number of arguments on stack (0-255)
/// 
/// **C Implementation** (maiko/src/miscn.c):
/// - OP_miscn(int misc_index, int arg_count)
/// - Collects arg_count arguments from stack into args array
/// - Dispatches to operation based on misc_index switch:
///   - miscn_USER_SUBR: Call user subr
///   - miscn_SXHASH: String hash
///   - miscn_STRING_EQUAL_HASHBITS: String equality hash
///   - miscn_STRINGHASHBITS: String hash bits
///   - miscn_VALUES: Return multiple values
///   - miscn_VALUES_LIST: Return values as list
///   - miscn_LCFetchMethod: LispC fetch method
///   - miscn_LCFetchMethodOrHelp: LispC fetch method or help
///   - miscn_LCFindVarIndex: LispC find variable index
///   - miscn_LCGetIVValue: LispC get instance variable value
///   - miscn_LCPutIVValue: LispC put instance variable value
///   - RS232 operations (if RS232 enabled)
///   - miscn_CALL_C: Call C function
/// 
/// **Stack State**:
/// - Before: [arg_N, ..., arg_1, arg_0] (arg_count arguments)
/// - After: [result] (operation result)
/// 
/// **Return Behavior**:
/// - Returns 0 if operation succeeded (normal return)
/// - Returns 1 if operation failed (triggers UFN lookup)
/// 
/// **Current Status**: Basic implementation with operation dispatch
/// TODO: Implement all misc_index operations (USER_SUBR, SXHASH, VALUES, etc.)
pub fn handleMISCN(vm: *VM, misc_index: u8, arg_count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const std = @import("std");
    
    // C: Collect arguments from stack into array
    // C: args[0] = NIL_PTR; stk = CurrentStackPTR + 1;
    // C: while (arg_num > 0) args[--arg_num] = *--stk;
    
    // For now, validate arg_count
    if (arg_count > 255) {
        std.debug.print("ERROR MISCN: arg_count ({}) exceeds maximum (255)\n", .{arg_count});
        return error.InvalidOpcode;
    }
    
    // Collect arguments from stack (in reverse order to match C)
    // C implementation collects args backwards: args[--arg_num] = *--stk
    // This means args[0] is the last argument popped, args[arg_count-1] is TOS
    var args: [255]LispPTR = undefined;
    args[0] = 0; // NIL_PTR placeholder
    
    // Pop arguments from stack (TOS is last argument)
    var i: u8 = 0;
    while (i < arg_count) : (i += 1) {
        args[@as(usize, arg_count - 1 - i)] = try stack_module.popStack(vm);
    }
    
    // Dispatch based on misc_index
    // C: switch (misc_index) { case miscn_XXX: ... }
    std.debug.print("DEBUG MISCN: misc_index=0x{x:0>2} ({d}), arg_count={d}\n", .{ misc_index, misc_index, arg_count });
    
    // TODO: Implement full dispatch switch for all misc_index values
    // For now, return error to trigger UFN (matches C behavior for unknown misc_index)
    // This prevents crashes while allowing the emulator to continue
    
    // Place arguments back on stack (for now, until operations are implemented)
    var j: u8 = 0;
    while (j < arg_count) : (j += 1) {
        try stack_module.pushStack(vm, args[@as(usize, arg_count - 1 - j)]);
    }
    
    // For now, set result to NIL (prevents crash)
    // TODO: Implement proper dispatch for all misc_index operations
    const type_check_module = @import("../../utils/type_check.zig");
    try stack_module.pushStack(vm, type_check_module.NIL_PTR);
    
    // Don't return error - returning error causes router to try next handler
    // Instead, just return NIL for now until operations are implemented
}

/// MISC3: Miscellaneous 3
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC3(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// MISC4: Miscellaneous 4
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC4(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// MISC7: Miscellaneous 7
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC7(vm: *VM, arg: u8) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
    _ = arg;
}

/// MISC8: Miscellaneous 8
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC8(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// MISC10: Miscellaneous 10
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC10(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// UPCTRACE: Up counter trace
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUPCTRACE(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// CL_EQUAL: Case-insensitive equal
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCL_EQUAL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper case-insensitive comparison
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result: LispPTR = if (a == b) 1 else 0; // Simplified
    try stack_module.pushStack(vm, result);
}

/// CMLEQUAL: Case-insensitive member equal
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCMLEQUAL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    _ = a;
    _ = b;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// VAG2: Vector add get 2
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleVAG2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// FFTSTEP: FFT step
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFFTSTEP(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// STORE_N: Store N
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops N values from stack (discards them)
/// Stack: [v1, v2, ..., vN, ...] -> [...]
pub fn handleSTORE_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    
    // Pop count values from stack
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        _ = stack_module.popStack(vm) catch |err| {
            return switch (err) {
                errors_module.VMError.StackUnderflow => err,
                else => errors_module.VMError.StackUnderflow,
            };
        };
    }
}

/// COPY_N: Copy N
/// Per rewrite documentation instruction-set/opcodes.md
/// Copies top stack value N times
/// Stack: [value] -> [value, value, ..., value] (N copies)
pub fn handleCOPY_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // Get top value and duplicate it count times
    const value = stack_module.getTopOfStack(vm);
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        try stack_module.pushStack(vm, value);
    }
}
