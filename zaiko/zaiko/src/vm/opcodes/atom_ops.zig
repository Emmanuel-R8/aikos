const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// GVAR_: Set global variable
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets global variable value
pub fn handleGVAR_(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // GVAR_ requires:
    // 1. Value on stack
    // 2. Atom index
    // 3. Set global variable value

    // TODO: Proper implementation needs:
    // 1. Get value from stack
    // 2. Look up atom in atom table
    // 3. Set DEFCELL value

    // Placeholder: pop value but don't set variable
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = atom_index;
}

/// ATOMCELL_N: Atom cell N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleATOMCELL_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation needs atom table access
    _ = index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GCONST: Global constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes global constant atom by atom index
pub fn handleGCONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // GCONST requires:
    // 1. Atom table lookup (from atom_index)
    // 2. Get global constant atom object
    // 3. Push atom on stack

    // TODO: Proper implementation needs:
    // 1. Atom table access (needs atom table structure)
    // 2. Global constant atom object creation/retrieval
    // For now, push atom_index as placeholder (will be properly implemented with atom tables)

    // Placeholder: push atom_index as atom pointer (will be properly implemented)
    try stack_module.pushStack(vm, @as(LispPTR, atom_index));
}