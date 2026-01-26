const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;

// ============================================================================
// Frame Operations Opcodes
// ============================================================================

/// PVARSETPOP0-PVARSETPOP6: Set parameter variable and pop value
/// C: inlineC.h PVARSETPOPMACRO(x)
/// Stack: [value] -> [] (value stored in PVar[x], then popped)
pub fn handlePVARSETPOP(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // C: PVARSETPOPMACRO(x):
    //   PVAR[x] = TOPOFSTACK;  // Use cached TopOfStack value
    //   POP;                   // Then pop (updates TopOfStack)
    //   nextop1;

    // CRITICAL: Use TopOfStack cached value first (even if it's 0)
    const value = stack_module.getTopOfStack(vm);

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // Store value in PVar[index]
    // PVar access: parameters stored right after frame header (FRAMESIZE offset)
    stack_module.setPVar(frame, index, value);

    // Now pop (updates TopOfStack cached value)
    // CRITICAL: C code does POP unconditionally, but POP checks for underflow
    // If stack is empty (stack_ptr <= stack_base), TopOfStack should already be 0
    // We need to simulate POP: move stack_ptr UP and update TopOfStack
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    
    if (stack_ptr_addr > stack_base_addr) {
        // Stack has data - pop normally (this updates TopOfStack)
        _ = try stack_module.popStack(vm);
    } else {
        // Stack is empty - TopOfStack should already be 0
        // C: POP on empty stack would underflow, but TopOfStack is already 0
        // Just ensure TopOfStack is 0 (it should be already)
        vm.top_of_stack = 0;
        // Don't call popStack as it would return StackUnderflow error
    }
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

/// MYALINK: My activation link
/// C: MYALINK macro in maiko/inc/inlineC.h
/// Pushes activation link address (previous frame pointer)
/// Stack: [] -> [alink_address]
pub fn handleMYALINK(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: MYALINK: PUSH((((CURRENTFX->alink) & 0xfffe) - FRAMESIZE) | S_POSITIVE);
    // FRAMESIZE = 10 DLwords = 20 bytes
    const FRAMESIZE: u32 = 10; // DLwords
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Get alink (activation link to previous frame)
    const alink = stack_module.getAlink(frame);
    
    // C: (alink & 0xfffe) - FRAMESIZE
    // Clear LSB and subtract FRAMESIZE (in DLwords, so multiply by 2 for bytes)
    const alink_cleared = alink & 0xFFFFFFFE; // Clear LSB
    const alink_addr = alink_cleared - (FRAMESIZE * 2); // FRAMESIZE in bytes
    
    // C: | S_POSITIVE
    const result = types_module.S_POSITIVE | alink_addr;
    
    try stack_module.pushStack(vm, result);
}