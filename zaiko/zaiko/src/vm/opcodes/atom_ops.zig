const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// GVAR_: Set global variable
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets global variable value
/// C: N_OP_gvar_ in maiko/src/gvar2.c
pub fn handleGVAR_(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // GVAR_: Pop value from stack, update GC refs, write to atom's value cell
    const value = try stack_module.popStack(vm);
    const atom_index_lisp: types.LispPTR = @as(types.LispPTR, atom_index);

    // Get old value before writing new value (for GC)
    const old_value = atom_module.readAtomValue(vm, atom_index_lisp) catch 0; // Default to 0 if read fails

    // Update GC refs: DELREF old value, ADDREF new value
    // C: FRPLPTR(((struct xpointer *)pslot)->addr, tos);
    // This does: DELREF(old_value), ADDREF(new_value), then set value
    if (vm.gc) |gc| {
        const gc_module = @import("../../memory/gc.zig");
        // DELREF old value
        gc_module.deleteReference(gc, old_value) catch {
            // GC errors are non-fatal
        };
        // ADDREF new value
        gc_module.addReference(gc, value) catch {
            // GC errors are non-fatal
        };
    }

    // Write value to atom's value cell
    try atom_module.writeAtomValue(vm, atom_index_lisp, value);
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
/// Similar to ACONST but for global constants
pub fn handleGCONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // GCONST: Push atom object (same as ACONST)
    const atom_index_lisp: types.LispPTR = @as(types.LispPTR, atom_index);
    const atom_ptr = atom_module.getAtomPointer(atom_index_lisp);
    try stack_module.pushStack(vm, atom_ptr);
}