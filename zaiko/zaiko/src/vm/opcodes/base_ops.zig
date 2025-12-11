const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// GETBASEBYTE: Get base byte
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBASEBYTE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// PUTBASEBYTE: Put base byte
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBASEBYTE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
}

/// GETBASE_N: Get base N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    _ = index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GETBASEPTR_N: Get base pointer N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBASEPTR_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    _ = index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GETBITS_N_FD: Get bits N FD
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    _ = arg1;
    _ = arg2;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// PUTBASE_N: Put base N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// PUTBASEPTR_N: Put base pointer N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBASEPTR_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// PUTBITS_N_FD: Put bits N FD
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = arg1;
    _ = arg2;
}

/// ADDBASE: Add base
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleADDBASE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result = a + b;
    try stack_module.pushStack(vm, result);
}

/// HILOC: High location
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleHILOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// LOLOC: Low location
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleLOLOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// BASE_LESSTHAN: Base less than
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBASE_LESSTHAN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result: LispPTR = if (a < b) 1 else 0;
    try stack_module.pushStack(vm, result);
}