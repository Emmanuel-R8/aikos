const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

// ============================================================================
// Base Operations - Arithmetic and Comparison Operations
// ============================================================================

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