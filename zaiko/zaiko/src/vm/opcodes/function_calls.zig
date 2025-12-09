const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// FN0-FN4: Function call opcodes with fixed argument counts
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/function-calls.md
/// Matches C implementation: maiko/inc/tosfns.h:OPFN
///
/// FN0 (0x08): Call function with 0 arguments
/// FN1 (0x09): Call function with 1 argument
/// FN2 (0x0A): Call function with 2 arguments
/// FN3 (0x0B): Call function with 3 arguments
/// FN4 (0x0C): Call function with 4 arguments
///
/// Instruction format: [opcode][atom_index_byte]
/// Stack: [arg_N, ..., arg_0, function_obj] -> []
pub fn handleFN0(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 0);
}

pub fn handleFN1(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 1);
}

pub fn handleFN2(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 2);
}

pub fn handleFN3(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 3);
}

pub fn handleFN4(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 4);
}

/// Common FN handler implementation
/// Matches C implementation: maiko/inc/tosfns.h:OPFN
fn handleFN(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction, arg_count: u8) errors.VMError!void {
    const function_module = @import("../function.zig");

    // Get atom index from instruction operand (2 bytes after opcode for non-BIGATOMS)
    // C: Get_AtomNo_PCMAC1 - gets atom index as DLword (2 bytes) from PC+1
    // For non-BIGATOMS: Get_AtomNo(ptr) = Get_DLword(ptr) - 2 bytes
    const atom_index: LispPTR = instruction.getWordOperand(0); // DLword (2 bytes) for non-BIGATOMS

    // TODO: Lookup function definition from atom table
    // C: defcell = GetDEFCELL68k(atom_index)
    // For now, we'll create a placeholder function header
    // Full implementation will need atom table lookup (Phase 3)

    // Placeholder: Create a minimal function header
    // This will be replaced with proper atom table lookup
    // Note: na is DLword (u16) in FunctionHeader struct, but C uses short (signed)
    // For now, we store as u16 and will handle signed interpretation when needed
    var func_header = function_module.FunctionHeader{
        .stkmin = 0,
        .na = @as(DLword, @intCast(arg_count)), // Number of arguments (negative if spread, but stored as u16)
        .pv = 0, // Parameter variable count
        .startpc = 0, // Code start offset
        .framename = atom_index, // Frame name atom index
        .ntsize = 0,
        .nlocals = 0,
        .fvaroffset = 0,
    };

    // Call function with argument count
    // C: OPFN sets up frame, pushes TOS, handles spread args, sets up BF/FX markers
    try function_module.callFunction(vm, &func_header, arg_count);
}

/// RETURN: Return from function
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/function-calls.md
/// Matches C implementation: maiko/inc/tosret.h:OPRETURN
pub fn handleRETURN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const function_module = @import("../function.zig");

    // Get return value and restore frame
    // C: OPRETURN gets return value from TOPOFSTACK, restores previous frame via alink
    const return_value = try function_module.returnFromFunction(vm);

    // Set return value on stack (TOS)
    // C: TOPOFSTACK is preserved through frame restoration
    stack_module.setTopOfStack(vm, return_value);
}