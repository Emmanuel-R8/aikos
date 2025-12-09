const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const function_header_module = @import("../../data/function_header.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

// ============================================================================
// Variable Access Opcodes
// ============================================================================

/// IVAR0-IVAR6: Local variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handleIVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // IVar access: variables stored at nextblock offset
    const ivar_value = stack_module.getIVar(frame, index);
    try stack_module.pushStack(vm, ivar_value);
}

/// PVAR0-PVAR6: Parameter variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handlePVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // PVar access: parameters stored right after frame header (FRAMESIZE offset)
    // Use helper function to get parameter value
    const pvar_value = stack_module.getPVar(frame, index);
    try stack_module.pushStack(vm, pvar_value);
}

/// FVAR0-FVAR6: Free variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
/// Free variables are stored in PVar area after regular parameters
/// Each free variable occupies 2 words (low word and high word)
pub fn handleFVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // Get function header from frame
    const fnheader_ptr = frame.fnheader;
    if (fnheader_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // No function header
    }

    // Translate function header address to native pointer
    // For now, assume direct mapping (will need proper address translation later)
    const fnheader: *function_header_module.FunctionHeader = @as(*function_header_module.FunctionHeader, @ptrFromInt(@as(usize, fnheader_ptr)));

    // Free variables are stored in PVar area after regular parameters
    // PVars are stored as LispPTR (4 bytes each)
    // Free variables are stored as 2 DLwords (2 bytes each) = 4 bytes total
    // Free variable offset calculation:
    //   PVar area size = (pv + 1) * sizeof(LispPTR) = (pv + 1) * 4 bytes
    //   Free variable i starts at: PVar area size + (i * 4 bytes)
    const pvar_count = fnheader.pv + 1; // PVar count includes return value slot
    const pvar_area_size = @as(usize, pvar_count) * @sizeOf(LispPTR);
    const fvar_offset_bytes = pvar_area_size + (@as(usize, index) * 4); // Each FVAR is 4 bytes (2 DLwords)

    // Access free variable slot (2 words)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base_addr = frame_addr + frame_size;

    // Get low word and high word (each is 2 bytes)
    const low_word_addr = pvar_base_addr + fvar_offset_bytes;
    const high_word_addr = low_word_addr + @sizeOf(types_module.DLword);

    const low_word_ptr: *types_module.DLword = @as(*types_module.DLword, @ptrFromInt(low_word_addr));
    const high_word_ptr: *types_module.DLword = @as(*types_module.DLword, @ptrFromInt(high_word_addr));

    const low_word = low_word_ptr.*;
    const high_word = high_word_ptr.*;

    // Check if unbound (LSB of low word indicates unbound)
    // For now, we'll skip the lookup and just return the value
    // TODO: Implement nfvlookup for unbound variables

    // Construct LispPTR from two words: (high_word << 16) | low_word
    const fvar_value: LispPTR = (@as(LispPTR, high_word) << 16) | @as(LispPTR, low_word);

    // Mask to get pointer (clear tag bits)
    const masked_value = fvar_value & 0xFFFFFFFE; // Clear LSB

    // Push value onto stack
    try stack_module.pushStack(vm, masked_value);
}

/// GVAR: Global variable access
/// Per rewrite documentation instruction-set/opcodes.md
/// Accesses global variable via atom index
pub fn handleGVAR(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // GVAR access requires:
    // 1. Atom table lookup (from atom_index)
    // 2. Get DEFCELL from atom
    // 3. Get global variable value from DEFCELL
    // 4. Push value on stack

    // TODO: Proper implementation needs:
    // 1. Atom table access (needs atom table structure)
    // 2. DEFCELL lookup
    // 3. Global variable value extraction
    // For now, return NIL (will be properly implemented with atom tables)

    // Placeholder: push NIL for now
    try stack_module.pushStack(vm, 0);
    _ = atom_index; // Use atom_index when atom tables are implemented
}

/// ACONST: Atom constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes atom constant by atom index
pub fn handleACONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // ACONST requires:
    // 1. Atom table lookup (from atom_index)
    // 2. Get atom object
    // 3. Push atom on stack

    // TODO: Proper implementation needs:
    // 1. Atom table access (needs atom table structure)
    // 2. Atom object creation/retrieval
    // For now, push atom_index as placeholder (will be properly implemented with atom tables)

    // Placeholder: push atom_index as atom pointer (will be properly implemented)
    try stack_module.pushStack(vm, @as(LispPTR, atom_index));
}

/// APPLYFN: Apply function
/// Per rewrite documentation instruction-set/opcodes.md
/// Applies a function to arguments on the stack
pub fn handleAPPLYFN(vm: *VM) errors.VMError!void {
    // APPLYFN requires:
    // 1. Function object on stack
    // 2. Arguments on stack
    // 3. Function application mechanism

    // TODO: Proper implementation needs:
    // 1. Get function object from stack
    // 2. Get argument count
    // 3. Apply function with arguments
    // 4. Handle spread arguments if needed

    // Placeholder: for now, this is similar to CALL but handles apply semantics
    // Will be properly implemented with function application system
    _ = vm;
}

/// CHECKAPPLY: Check function application
/// Per rewrite documentation instruction-set/opcodes.md
/// Validates function application arguments
pub fn handleCHECKAPPLY(vm: *VM) errors.VMError!void {
    // CHECKAPPLY requires:
    // 1. Function object on stack
    // 2. Argument count validation
    // 3. Type checking

    // TODO: Proper implementation needs:
    // 1. Get function object
    // 2. Check argument count matches function signature
    // 3. Validate argument types
    // 4. Signal error if validation fails

    // Placeholder: will be properly implemented with function validation
    _ = vm;
}

/// STKSCAN: Scan stack for variable
/// Per rewrite documentation instruction-set/opcodes.md and src/fvar.c
/// Scans stack frames for variable by atom index
pub fn handleSTKSCAN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // STKSCAN requires:
    // 1. Atom index on TOS
    // 2. Scan current and previous frames
    // 3. Look up variable in name tables
    // 4. Return address of found value

    // TODO: Proper implementation needs:
    // 1. Get atom index from TOS
    // 2. Traverse stack frames (current and previous)
    // 3. Look up variable in each frame's name table
    // 4. Return address of variable value
    // 5. Handle unbound variables

    // Placeholder: for now, return NIL
    // Will be properly implemented with name table lookup
    const atom_index = stack_module.getTopOfStack(vm);
    _ = atom_index;
    stack_module.setTopOfStack(vm, 0); // Return NIL for now
}
/// PVAR_SET: Set PVAR value
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets parameter variable value
pub fn handlePVAR_SET(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // PVAR_SET requires:
    // 1. Value on stack
    // 2. Set PVAR at index

    // TODO: Proper implementation needs:
    // 1. Get value from stack
    // 2. Set PVAR at index in current frame

    // Placeholder: pop value but don't set variable
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// ARG0: Argument 0
/// Per rewrite documentation instruction-set/opcodes.md
/// Gets first argument
pub fn handleARG0(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // ARG0 requires:
    // 1. Get argument 0 from current frame

    // TODO: Proper implementation needs:
    // 1. Access argument 0 from current frame

    // Placeholder: return NIL
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// IVARX_: Set IVAR X
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets instance variable value
pub fn handleIVARX_(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // IVARX_ requires:
    // 1. Value on stack
    // 2. Set IVAR at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// FVARX_: Set FVAR X
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets free variable value
pub fn handleFVARX_(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // FVARX_ requires:
    // 1. Value on stack
    // 2. Set FVAR at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// COPY: Copy value
/// Per rewrite documentation instruction-set/opcodes.md
/// Copies value on stack
pub fn handleCOPY(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // COPY requires:
    // 1. Value on stack
    // 2. Push copy of value

    // TODO: Proper implementation needs:
    // 1. Get value from stack
    // 2. Create copy (for GC purposes)
    // 3. Push copy

    // Placeholder: duplicate value
    const value = stack_module.getTopOfStack(vm);
    try stack_module.pushStack(vm, value);
}

/// MYARGCOUNT: My argument count
/// Per rewrite documentation instruction-set/opcodes.md
/// Gets argument count for current function
pub fn handleMYARGCOUNT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // MYARGCOUNT requires:
    // 1. Get argument count from current frame

    // TODO: Proper implementation needs:
    // 1. Access function header
    // 2. Get argument count

    // Placeholder: return 0
    try stack_module.pushStack(vm, 0);
}

/// MYALINK: My argument link
/// Per rewrite documentation instruction-set/opcodes.md
/// Gets argument link for current function
pub fn handleMYALINK(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // MYALINK requires:
    // 1. Get argument link from current frame

    // TODO: Proper implementation needs:
    // 1. Access frame
    // 2. Get argument link

    // Placeholder: return NIL
    try stack_module.pushStack(vm, 0); // Return NIL
}