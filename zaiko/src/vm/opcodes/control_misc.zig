const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;

/// SLRETURN: Stack-relative return
/// Per rewrite documentation instruction-set/opcodes-control-memory.md
/// C: opc_SLRETURN (0x3F) in maiko/inc/opcodes.h
/// 
/// **Purpose**: Return from function using stack-relative addressing (soft return)
/// 
/// **C Implementation**: `maiko/src/mvs.c` - handles soft return with different frame handling
/// 
/// **Difference from RETURN**:
/// - RETURN: Standard return, restores frame completely
/// - SLRETURN: Soft return, uses stack-relative addressing for return address
/// - SLRETURN may preserve more stack state or use different frame restoration
/// 
/// **Stack Effect**: [return_value] -> [] (returns to caller)
/// **Instruction Format**: [opcode:0x3F] (length: 1 byte, no operands)
/// 
/// **Current Status**: Basic implementation - delegates to RETURN for now
/// TODO: Implement proper stack-relative return address handling
/// TODO: Check mvs.c for exact SLRETURN behavior differences from RETURN
pub fn handleSLRETURN(vm: *VM) errors.VMError!void {
    // For now, use RETURN implementation as placeholder
    // SLRETURN has different semantics but similar overall behavior
    const function_module = @import("../function.zig");
    const stack_module = @import("../stack.zig");
    
    // Get return value and restore frame (similar to RETURN)
    const return_value = try function_module.returnFromFunction(vm);
    
    // Set return value on stack (TOS)
    stack_module.setTopOfStack(vm, return_value);
    
    // TODO: Implement proper stack-relative return address handling
    // TODO: Check if SLRETURN preserves different stack state than RETURN
}

/// RPLPTR_N: Replace pointer N
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_rplptr in maiko/src/gvar2.c
/// Replaces pointer at offset N with new value, updating GC refs
/// Stack: [new_value, base] -> [base]
pub fn handleRPLPTR_N(vm: *VM, offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const gc_module = @import("../../memory/gc.zig");

    const new_value = try stack_module.popStack(vm);
    const base = try stack_module.popStack(vm);

    // C: RPLPTR(n) - replaces pointer at base + offset
    // Calculate target address: base + offset (in LispPTR units, so offset * 4 bytes)
    // C: pslot = (struct xpointer *)NativeAligned4FromLAddr(tos_m_1 + alpha);
    // alpha is a word offset, so multiply by 4 for bytes
    const base_ptr = types.POINTERMASK & base;
    const target_addr = base_ptr + (@as(types.LispPTR, offset) * 4);

    // Get virtual memory access
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return errors_module.VMError.MemoryAccessFailed;
    }
    
    const fptovp_table = vm.fptovp.?;
    
    // Calculate byte offset in virtual memory
    const page_num = (target_addr >> 9) & 0x7FFF; // Page number (15 bits)
    const page_offset_dlwords = target_addr & 0x1FF; // Page offset (9 bits, in DLwords)
    const page_offset_bytes = page_offset_dlwords * 2; // Convert DLwords to bytes
    
    // Get virtual page from FPtoVP table
    if (page_num >= fptovp_table.entries.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    const virtual_page = fptovp_table.getFPtoVP(page_num);
    if (virtual_page == 0) {
        return errors_module.VMError.InvalidAddress;
    }
    
    // Calculate byte offset in virtual memory: virtual_page * 512 + page_offset_bytes
    const BYTESPER_PAGE: usize = 512;
    const target_byte_offset = (@as(usize, virtual_page) * BYTESPER_PAGE) + page_offset_bytes;
    
    const virtual_memory = vm.virtual_memory.?;
    if (target_byte_offset + 4 > virtual_memory.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    // Read old value for GC (big-endian from sysout)
    const target_bytes = virtual_memory[target_byte_offset..target_byte_offset+4];
    const old_low_word = (@as(types.DLword, target_bytes[0]) << 8) | @as(types.DLword, target_bytes[1]);
    const old_high_word = (@as(types.DLword, target_bytes[2]) << 8) | @as(types.DLword, target_bytes[3]);
    const old_value: types.LispPTR = (@as(types.LispPTR, old_high_word) << 16) | @as(types.LispPTR, old_low_word);
    
    // Update GC refs: DELREF old value, ADDREF new value
    // C: FRPLPTR(old, new) - does GCLOOKUP(new, ADDREF), GCLOOKUP(old, DELREF), then (old) = (new)
    if (vm.gc) |gc| {
        gc_module.deleteReference(gc, old_value) catch {};
        gc_module.addReference(gc, new_value) catch {};
    }
    
    // Write new value (big-endian to sysout format)
    const virtual_memory_mut: []u8 = @constCast(virtual_memory);
    const target_bytes_mut = virtual_memory_mut[target_byte_offset..target_byte_offset+4];
    const new_low_word = @as(types.DLword, @truncate(new_value));
    const new_high_word = @as(types.DLword, @truncate(new_value >> 16));
    target_bytes_mut[0] = @as(u8, @truncate(new_low_word >> 8));
    target_bytes_mut[1] = @as(u8, @truncate(new_low_word & 0xFF));
    target_bytes_mut[2] = @as(u8, @truncate(new_high_word >> 8));
    target_bytes_mut[3] = @as(u8, @truncate(new_high_word & 0xFF));
    
    // Push base back on stack
    try stack_module.pushStack(vm, base);
}

/// EVAL: Evaluate expression
/// Per rewrite documentation instruction-set/opcodes.md
/// C: opc_EVAL in maiko/inc/opcodes.h
/// Evaluates Lisp expression from stack
pub fn handleEVAL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop expression from stack
    const expression = try stack_module.popStack(vm);

    // TODO: Call Lisp evaluator on expression
    // For now, return expression as-is (no evaluation)
    // This maintains stack balance but doesn't perform evaluation

    // Push result back (placeholder: same as input)
    try stack_module.pushStack(vm, expression);
}

/// ENVCALL: Environment call
/// Per rewrite documentation instruction-set/opcodes.md
/// C: opc_ENVCALL in maiko/inc/opcodes.h
/// Calls function in environment context
pub fn handleENVCALL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop function and arguments from stack
    const function = try stack_module.popStack(vm);
    // TODO: Pop additional arguments as needed

    // TODO: Set up environment context
    // For now, treat as regular function call

    // TODO: Call function in environment context
    // Placeholder: push function back as result
    try stack_module.pushStack(vm, function);
}

/// JUMPXX: Extended jump XX
/// Per rewrite documentation instruction-set/opcodes.md
/// C: opc_JUMPXX in maiko/inc/opcodes.h
/// Extended jump with different operand format than JUMPX
pub fn handleJUMPXX(vm: *VM) errors.VMError!void {
    // JUMPXX uses different operand encoding than JUMPX
    // For now, implement as unconditional jump with 0 offset
    // TODO: Implement proper operand extraction for JUMPXX
    vm.pc = @as(types.LispPTR, @intCast(@as(i64, @intCast(vm.pc)) + 0));
}

/// NFJUMPX: Not false jump extended
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNFJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // NFJUMPX jumps if TOS is not false (not NIL)
    // Similar to TJUMPX but inverted logic
    const tos = stack_module.getTopOfStack(vm);
    if (tos != 0) {
        // Not false - perform jump
        vm.pc = @as(types.LispPTR, @intCast(@as(i64, @intCast(vm.pc)) + offset));
    }
    // If false (NIL), continue to next instruction
}

/// NTJUMPX: Not true jump extended
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNTJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // NTJUMPX jumps if TOS is not true (is NIL)
    // Similar to FJUMPX but inverted logic
    const tos = stack_module.getTopOfStack(vm);
    if (tos == 0) {
        // Not true (NIL) - perform jump
        vm.pc = @as(types.LispPTR, @intCast(@as(i64, @intCast(vm.pc)) + offset));
    }
    // If true (non-NIL), continue to next instruction
}
