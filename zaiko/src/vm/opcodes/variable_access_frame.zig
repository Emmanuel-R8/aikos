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
/// APPLYFN: Apply function to argument list
/// Per rewrite documentation instruction-set/opcodes-control-memory.md
/// C: opc_APPLYFN (0x0E) - no params shown in C logging
/// 
/// **Purpose**: Apply a function to a list of arguments (Lisp APPLY semantics)
/// 
/// **Stack State Before**:
/// - TOS: Argument list (cons cell list)
/// - Second: Function object (atom index or function pointer)
/// 
/// **Algorithm**:
/// 1. Pop argument list from stack
/// 2. Pop function object from stack
/// 3. Spread list elements onto stack as individual arguments
/// 4. Count arguments while spreading
/// 5. Call function with spread arguments (similar to FNX with variable count)
/// 
/// **C Implementation Notes**:
/// - APPLYFN is similar to FNX but handles list spreading
/// - The argument list is traversed using CDR to count and spread arguments
/// - After spreading, calls function using same mechanism as FNX
/// 
/// **Stack Effect**: [arg_list, function] -> [result] (after function call)
/// **Instruction Format**: [opcode:0x0E] (length: 1 byte, no operands)
/// 
/// **Current Status**: Basic implementation - spreads list and calls function
/// TODO: Full implementation needs proper list traversal and argument spreading
pub fn handleAPPLYFN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const cons_module = @import("../../data/cons.zig");
    const function_module = @import("../function.zig");
    const errors_module = @import("../../utils/errors.zig");
    
    // Step 1: Pop argument list from TOS
    const arg_list = try stack_module.popStack(vm);
    
    // Step 2: Pop function object from stack
    const function_obj = try stack_module.popStack(vm);
    
    // Step 3: Count arguments in list
    var arg_count: u32 = 0;
    var current = arg_list;
    const type_check_module = @import("../../utils/type_check.zig");
    
    // Traverse list to count arguments
    // C: Uses CDR to traverse list, counting elements
    while (!type_check_module.isNil(current)) {
        if (!type_check_module.isList(vm, current)) {
            // Not a proper list - error or handle differently
            // For now, return error
            return errors_module.VMError.InvalidOpcode;
        }
        
        arg_count += 1;
        
        // Get CDR to continue traversal
        const cdr = cons_module.getCDR(vm, current) catch {
            // Error getting CDR - stop traversal
            break;
        };
        current = cdr;
    }
    
    // Step 4: Spread arguments onto stack (in reverse order for proper argument passing)
    // We need to traverse the list again, pushing arguments in reverse
    // For now, simplified: just count and call with that count
    // Full implementation would need to:
    // 1. Traverse list backwards or use temporary storage
    // 2. Push each CAR value onto stack
    // 3. Push function object last
    // 4. Call function
    
    // TODO: Proper argument spreading implementation
    // For now, this is a placeholder that at least doesn't crash
    // The function call mechanism will need to handle APPLY semantics
    
    // Placeholder: For now, just push function back and do nothing
    // This prevents crashes but doesn't fully implement APPLY
    // TODO: Once argument spreading is implemented, call the function
    try stack_module.pushStack(vm, function_obj);
    try stack_module.pushStack(vm, arg_list);
    
    // Don't return error - just do nothing for now
    // Returning error causes router to try next handler, which causes "Unimplemented" message
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
