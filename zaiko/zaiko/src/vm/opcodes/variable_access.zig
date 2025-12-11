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
/// C: GVAR macro in maiko/inc/inlineC.h
pub fn handleGVAR(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // GVAR: Read value from atom's value cell and push on stack
    const atom_index_lisp: types.LispPTR = @as(types.LispPTR, atom_index);
    const value = try atom_module.readAtomValue(vm, atom_index_lisp);
    try stack_module.pushStack(vm, value);
}

/// ACONST: Atom constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes atom constant by atom index
/// C: ACONST macro in maiko/inc/inlineC.h
pub fn handleACONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // ACONST: Push atom object (atom_index as LispPTR)
    const atom_index_lisp: types.LispPTR = @as(types.LispPTR, atom_index);
    const atom_ptr = atom_module.getAtomPointer(atom_index_lisp);
    try stack_module.pushStack(vm, atom_ptr);
}

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
/// PVARX_: Set PVAR X (word offset)
/// C: PVARX_(x) macro in maiko/inc/inlineC.h
/// Sets parameter variable using DLword offset
/// Stack: [value] -> []
/// Operand: x (1B, DLword offset)
pub fn handlePVAR_SET(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: PVARX_(x): *((LispPTR *)((DLword *)PVAR + (x))) = TOPOFSTACK;
    const value = try stack_module.popStack(vm);
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // PVAR area starts after frame header (FRAMESIZE bytes)
    const frame_addr = @intFromPtr(frame);
    const pvar_base_addr = frame_addr + @sizeOf(stack.FX);
    
    // Access PVAR at word_offset (DLword units)
    const pvar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const pvar_addr = pvar_base_addr + pvar_offset_bytes;
    
    // Write LispPTR (2 DLwords, big-endian)
    const pvar_bytes: [*]u8 = @ptrFromInt(pvar_addr);
    const low_word = @as(types_module.DLword, @truncate(value));
    const high_word = @as(types_module.DLword, @truncate(value >> 16));
    pvar_bytes[0] = @as(u8, @truncate(low_word >> 8));
    pvar_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    pvar_bytes[2] = @as(u8, @truncate(high_word >> 8));
    pvar_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
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

/// PVARX: Get PVAR X (word offset)
/// C: PVARX(x) macro in maiko/inc/inlineC.h
/// Gets parameter variable using DLword offset (not LispPTR offset)
/// Stack: [] -> [value]
/// Operand: x (1B, DLword offset)
pub fn handlePVARX(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: PVARX(x): PUSH(GetLongWord((DLword *)PVAR + (x)));
    // x is a DLword offset, not a LispPTR offset
    // GetLongWord reads 2 DLwords (4 bytes) as a LispPTR
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // PVAR area starts after frame header (FRAMESIZE bytes)
    const frame_addr = @intFromPtr(frame);
    const pvar_base_addr = frame_addr + @sizeOf(stack.FX);
    
    // Access PVAR at word_offset (DLword units)
    // Each LispPTR is 2 DLwords, so offset in bytes = word_offset * 2
    const pvar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const pvar_addr = pvar_base_addr + pvar_offset_bytes;
    
    // Read LispPTR (2 DLwords, big-endian)
    const pvar_bytes: [*]const u8 = @ptrFromInt(pvar_addr);
    const low_word = (@as(types_module.DLword, pvar_bytes[0]) << 8) | @as(types_module.DLword, pvar_bytes[1]);
    const high_word = (@as(types_module.DLword, pvar_bytes[2]) << 8) | @as(types_module.DLword, pvar_bytes[3]);
    const value: types.LispPTR = (@as(types.LispPTR, high_word) << 16) | @as(types.LispPTR, low_word);
    
    try stack_module.pushStack(vm, value);
}

/// IVARX: Get IVAR X (word offset)
/// C: IVARX(x) macro in maiko/inc/inlineC.h
/// Gets instance variable using DLword offset (not LispPTR offset)
/// Stack: [] -> [value]
/// Operand: x (1B, DLword offset)
pub fn handleIVARX(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");
    const virtual_memory_module = @import("../../memory/virtual.zig");

    // C: IVARX(x): PUSH(GetLongWord((DLword *)IVAR + (x)));
    // IVAR is frame.nextblock (LispPTR address)
    // x is a DLword offset, not a LispPTR offset
    // GetLongWord reads 2 DLwords (4 bytes) as a LispPTR
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // IVAR area is at frame.nextblock (LispPTR address)
    const ivar_base_lisp = frame.nextblock;
    if (ivar_base_lisp == 0) {
        try stack_module.pushStack(vm, 0); // No IVAR area - return NIL
        return;
    }
    
    // Translate IVAR base address to native pointer
    if (vm.virtual_memory == null or vm.fptovp == null) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    
    const fptovp_table = vm.fptovp.?;
    const ivar_base_native = virtual_memory_module.translateAddress(ivar_base_lisp, fptovp_table, 2) catch {
        try stack_module.pushStack(vm, 0);
        return;
    };
    
    // Access IVAR at word_offset (DLword units)
    const ivar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const ivar_addr = @intFromPtr(ivar_base_native) + ivar_offset_bytes;
    
    // Read LispPTR (2 DLwords, big-endian from sysout)
    const ivar_bytes: [*]const u8 = @ptrFromInt(ivar_addr);
    const low_word = (@as(types_module.DLword, ivar_bytes[0]) << 8) | @as(types_module.DLword, ivar_bytes[1]);
    const high_word = (@as(types_module.DLword, ivar_bytes[2]) << 8) | @as(types_module.DLword, ivar_bytes[3]);
    const value: types.LispPTR = (@as(types.LispPTR, high_word) << 16) | @as(types.LispPTR, low_word);
    
    try stack_module.pushStack(vm, value);
}

/// IVARX_: Set IVAR X (word offset)
/// C: IVARX_(x) macro in maiko/inc/inlineC.h
/// Sets instance variable using DLword offset
/// Stack: [value] -> []
/// Operand: x (1B, DLword offset)
pub fn handleIVARX_(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");
    const virtual_memory_module = @import("../../memory/virtual.zig");

    // C: IVARX_(x): *((LispPTR *)((DLword *)IVAR + (x))) = TOPOFSTACK;
    const value = try stack_module.popStack(vm);
    
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // IVAR area is at frame.nextblock (LispPTR address)
    const ivar_base_lisp = frame.nextblock;
    if (ivar_base_lisp == 0) {
        return; // No IVAR area - ignore
    }
    
    // Translate IVAR base address to native pointer
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return;
    }
    
    const fptovp_table = vm.fptovp.?;
    const ivar_base_native = virtual_memory_module.translateAddress(ivar_base_lisp, fptovp_table, 2) catch {
        return;
    };
    
    // Access IVAR at word_offset (DLword units)
    const ivar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const ivar_addr = @intFromPtr(ivar_base_native) + ivar_offset_bytes;
    
    // Write LispPTR (2 DLwords, big-endian from sysout)
    const ivar_bytes: [*]u8 = @ptrFromInt(ivar_addr);
    const low_word = @as(types_module.DLword, @truncate(value));
    const high_word = @as(types_module.DLword, @truncate(value >> 16));
    ivar_bytes[0] = @as(u8, @truncate(low_word >> 8));
    ivar_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    ivar_bytes[2] = @as(u8, @truncate(high_word >> 8));
    ivar_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
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