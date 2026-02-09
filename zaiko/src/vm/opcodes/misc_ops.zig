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
    _ = vm;
    _ = misc_index;
    _ = arg_count;
    // TODO: Implement MISCN operation
    return error.InvalidOpcode;
}

pub fn handleRAID(vm: *VM) errors.VMError!void {
    _ = vm;
    // TODO: Implement RAID operation
    return error.InvalidOpcode;
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

/// CONTEXTSWITCH: Context switch to new function frame
/// Per C implementation: maiko/src/return.c:OP_contextsw and contextsw
/// C: opc_CONTEXTSWITCH = 126 (0x7E) in maiko/inc/opcodes.h
///
/// **Purpose**: Switch execution context to a new function frame
/// This is used for calling special handlers (keyboard, reset stack, fault)
///
/// **C Implementation**:
/// - OP_contextsw() pops TOS to get frame number, calls contextsw()
/// - contextsw() saves current PC, saves TOS, switches to new frame
/// - The frame number is used to look up the new frame via currentfxp
///
/// **Stack State**:
/// - Before: [frame_number] (TOS contains frame number)
/// - After: Context switched to new frame
///
/// **Current Status**: Basic implementation for emulator continuation
/// TODO: Full implementation matching C contextsw() behavior
pub fn handleCONTEXTSWITCH(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const std = @import("std");

    // C: OP_contextsw() implementation:
    //   EXT;
    //   OP_contextsw();
    //   RET;
    //
    // C: contextsw(TopOfStack & 0xffff, 1, 2);
    // - Pops TOS to get frame number (fxnum)
    // - bytenum = 1 (increment PC by 1 byte after switch)
    // - flags = 2 (call from OP_contextsw)

    // Pop frame number from stack
    const fxnum = try stack_module.popStack(vm);
    const frame_number = fxnum & 0xFFFF; // Use low 16 bits

    std.debug.print("DEBUG CONTEXTSWITCH: frame_number=0x{x:0>4} ({})\n", .{ frame_number, frame_number });

    // TODO: Full context switch implementation
    // - Save CURRENTFX->pc = PC - FuncObj + bytenum
    // - Set CURRENTFX->nopush = T
    // - Switch to new frame via currentfxp lookup
    // - Set up new stack pointer and frame pointer
    //
    // For now, just continue execution (basic stub)
    // This allows the emulator to proceed past CONTEXTSWITCH

    // In C, CONTEXTSWITCH causes a significant context change
    // For emulation, we need to actually implement the frame switching
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
