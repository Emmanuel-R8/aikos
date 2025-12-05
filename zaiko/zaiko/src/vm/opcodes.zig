const std = @import("std");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const types = @import("../utils/types.zig");
const cons = @import("../data/cons.zig");
const array = @import("../data/array.zig");
const virtual_memory_module = @import("../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Arithmetic opcode handlers
/// Per rewrite documentation opcodes.md

/// IPLUS2: Integer plus 2 operands
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIPLUS2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // Pop two values from stack
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // Add them (treating as signed integers)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed + b_signed))));
    
    // Push result
    try stack_module.pushStack(vm, result);
}

/// IDIFFERENCE: Integer difference
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed - b_signed))));
    
    try stack_module.pushStack(vm, result);
}

/// ITIMES2: Integer times 2 operands
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleITIMES2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed * b_signed))));
    
    try stack_module.pushStack(vm, result);
}

/// IQUO: Integer quotient
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIQUO(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    if (b == 0) {
        return error.InvalidOpcode; // Division by zero
    }
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(@divTrunc(a_signed, b_signed)))));
    
    try stack_module.pushStack(vm, result);
}

/// IREM: Integer remainder
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIREM(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    if (b == 0) {
        return error.InvalidOpcode; // Division by zero
    }
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(@rem(a_signed, b_signed)))));
    
    try stack_module.pushStack(vm, result);
}

// ============================================================================
// General Arithmetic Operations
// ============================================================================

/// PLUS2: General addition (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs addition, pushes result
/// Falls back to float addition if not integers
pub fn handlePLUS2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // Check if both are integers (low bit = 0 for fixnums)
    // For now, treat as integer addition (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed + b_signed))));
    
    try stack_module.pushStack(vm, result);
}

/// DIFFERENCE: General subtraction (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs subtraction (a - b), pushes result
pub fn handleDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // For now, treat as integer subtraction (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed - b_signed))));
    
    try stack_module.pushStack(vm, result);
}

/// TIMES2: General multiplication (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs multiplication, pushes result
pub fn handleTIMES2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // For now, treat as integer multiplication (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(a_signed * b_signed))));
    
    try stack_module.pushStack(vm, result);
}

/// QUOTIENT: General division (handles integers and floats)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs division (a / b), pushes result
pub fn handleQUOTIENT(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    if (b == 0) {
        return errors_module.VMError.InvalidAddress; // Division by zero
    }
    
    // For now, treat as integer division (will be extended for floats)
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result = @as(LispPTR, @bitCast(@as(u32, @intCast(@divTrunc(a_signed, b_signed)))));
    
    try stack_module.pushStack(vm, result);
}

// ============================================================================
// Bitwise Operations
// ============================================================================

/// LOGOR2: Logical OR (bitwise OR)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs bitwise OR, pushes result
pub fn handleLOGOR2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // Perform bitwise OR
    const result: LispPTR = a | b;
    
    try stack_module.pushStack(vm, result);
}

/// LOGAND2: Logical AND (bitwise AND)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs bitwise AND, pushes result
pub fn handleLOGAND2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // Perform bitwise AND
    const result: LispPTR = a & b;
    
    try stack_module.pushStack(vm, result);
}

/// LOGXOR2: Logical XOR (bitwise XOR)
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops two values, performs bitwise XOR, pushes result
pub fn handleLOGXOR2(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // Perform bitwise XOR
    const result: LispPTR = a ^ b;
    
    try stack_module.pushStack(vm, result);
}

/// LSH: Logical shift
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops shift amount and value, performs logical shift, pushes result
/// Positive shift amount = left shift, negative = right shift
pub fn handleLSH(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // Pop shift amount and value
    const shift_amount = try stack_module.popStack(vm);
    const value = try stack_module.popStack(vm);
    
    // Convert shift amount to signed integer
    const shift_signed = @as(i32, @bitCast(@as(u32, shift_amount)));
    
    // Perform shift operation
    const result: LispPTR = if (shift_signed >= 0) blk: {
        // Left shift
        const shift_u = @as(u5, @intCast(shift_signed));
        // Clamp shift amount to avoid undefined behavior
        const safe_shift = @min(shift_u, 31);
        break :blk value << safe_shift;
    } else blk: {
        // Right shift (logical right shift, zero-fill)
        const shift_u = @as(u5, @intCast(-shift_signed));
        const safe_shift = @min(shift_u, 31);
        break :blk value >> safe_shift;
    };
    
    try stack_module.pushStack(vm, result);
}

/// LLSH1: Logical left shift by 1
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS left by 1 bit
pub fn handleLLSH1(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value << 1;
    
    stack_module.setTopOfStack(vm, result);
}

/// LLSH8: Logical left shift by 8
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS left by 8 bits
pub fn handleLLSH8(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value << 8;
    
    stack_module.setTopOfStack(vm, result);
}

/// LRSH1: Logical right shift by 1
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS right by 1 bit (logical, zero-fill)
pub fn handleLRSH1(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value >> 1;
    
    stack_module.setTopOfStack(vm, result);
}

/// LRSH8: Logical right shift by 8
/// Per rewrite documentation instruction-set/opcodes.md
/// Shifts TOS right by 8 bits (logical, zero-fill)
pub fn handleLRSH8(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    const result: LispPTR = value >> 8;
    
    stack_module.setTopOfStack(vm, result);
}

/// Stack manipulation opcodes

/// PUSH: Push value onto stack
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PUSH typically pushes a constant value from instruction operand
/// For now, simplified version pushes 0
pub fn handlePUSH(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    // PUSH typically pushes a constant value from instruction operand
    // TODO: Extract constant value from instruction operand
    // For now, push 0 (will need to get value from instruction operand)
    try stack_module.pushStack(vm, 0);
}

/// POP: Pop value from stack
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePOP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    _ = try stack_module.popStack(vm); // Discard value
}

/// POP_N: Pop N values from stack
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops count values from stack, discarding them
pub fn handlePOP_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop count values from stack
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        _ = stack_module.popStack(vm) catch |err| {
            // If we can't pop enough values, return error
            return switch (err) {
                errors_module.VMError.StackUnderflow => err,
                else => errors_module.VMError.StackUnderflow,
            };
        };
    }
}

/// SWAP: Swap top two stack values
/// Per rewrite documentation instruction-set/opcodes.md
/// Swaps the top two values on the stack
pub fn handleSWAP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // Pop top two values
    const top = try stack_module.popStack(vm);
    const second = try stack_module.popStack(vm);
    
    // Push them back in swapped order
    try stack_module.pushStack(vm, top);
    try stack_module.pushStack(vm, second);
}

/// NOP: No operation
/// Per rewrite documentation instruction-set/opcodes.md
/// Does nothing, just advances PC
pub fn handleNOP(vm: *VM) errors.VMError!void {
    // No operation - just advance PC (handled by dispatch loop)
    _ = vm;
}

/// Function call opcodes

/// CALL: Call function
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/function-calls.md
/// Note: This is a simplified version - full implementation needs function object lookup
pub fn handleCALL(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const function_module = @import("function.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // For FN0-FN4, function object and arguments are on stack
    // Pop function object (for now, we'll use a placeholder)
    const func_obj = try stack_module.popStack(vm);
    
    if (func_obj == 0) {
        return errors_module.VMError.InvalidAddress; // Invalid function object
    }
    
    // TODO: Lookup function header from function object
    // For now, create a minimal function header for testing
    // In real implementation, this would come from atom table or function registry
    
    // Placeholder: Create a simple function header
    // This will be replaced with proper function lookup
    var func_header = function_module.FunctionHeader{
        .stkmin = 0,
        .na = 0,
        .pv = 0,
        .startpc = 0,
        .framename = 0,
        .ntsize = 0,
        .nlocals = 0,
        .fvaroffset = 0,
    };
    
    // Call function (arg_count determined by opcode variant)
    try function_module.callFunction(vm, &func_header, 0);
}

/// RETURN: Return from function
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/function-calls.md
pub fn handleRETURN(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const function_module = @import("function.zig");
    
    // Get return value and restore frame
    const return_value = try function_module.returnFromFunction(vm);
    
    // Set return value on stack
    stack_module.setTopOfStack(vm, return_value);
}

// ============================================================================
// Binding Operations
// ============================================================================

/// BIND: Bind variables from stack
/// Per rewrite documentation instruction-set/opcodes.md
/// Binds count variable-value pairs from stack
/// Stack: [value_N, atom_N, ..., value_0, atom_0] -> []
pub fn handleBIND(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // BIND requires:
    // 1. Atom table access (needs atom table structure)
    // 2. Binding frame allocation
    // 3. Variable binding storage
    
    // TODO: Proper implementation needs:
    // 1. Atom table lookup for each atom_index
    // 2. Binding frame (BF) allocation
    // 3. Store bindings in binding frame
    // 4. Link binding frame to current frame
    
    // For now, pop count pairs from stack (atom_index, value)
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        const value = try stack_module.popStack(vm);
        const atom_index = try stack_module.popStack(vm);
        _ = value;
        _ = atom_index; // Will be used when binding system is implemented
    }
}

/// UNBIND: Unbind variables
/// Per rewrite documentation instruction-set/opcodes.md
/// Unbinds variables in reverse bind order
pub fn handleUNBIND(vm: *VM) errors.VMError!void {
    // UNBIND requires:
    // 1. Binding frame access
    // 2. Restore previous variable values
    // 3. Deallocate binding frame
    
    // TODO: Proper implementation needs:
    // 1. Get current binding frame
    // 2. Restore variable values
    // 3. Unlink and deallocate binding frame
    
    _ = vm; // Will be used when binding system is implemented
}

/// DUNBIND: Dynamic unbind
/// Per rewrite documentation instruction-set/opcodes.md
/// Unbind with dynamic scope handling
pub fn handleDUNBIND(vm: *VM) errors.VMError!void {
    // DUNBIND requires:
    // 1. Dynamic scope handling
    // 2. Binding frame traversal
    // 3. Variable value restoration
    
    // TODO: Proper implementation needs:
    // 1. Dynamic scope lookup
    // 2. Binding frame traversal
    // 3. Variable value restoration
    
    _ = vm; // Will be used when binding system is implemented
}

/// Control flow opcodes

/// JUMP: Unconditional jump
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on returned offset
pub fn handleJUMP(vm: *VM) errors.VMError!void {
    // JUMP is unconditional - dispatch loop handles PC update
    _ = vm;
}

// ============================================================================
// Control Flow Opcodes
// ============================================================================

/// FJUMP: False jump (jump if NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleFJUMP(vm: *VM, offset: i8) errors.VMError!void {
    // FJUMP logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// TJUMP: True jump (jump if not NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleTJUMP(vm: *VM, offset: i8) errors.VMError!void {
    // TJUMP logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// JUMPX: Extended jump (16-bit offset)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on returned offset
pub fn handleJUMPX(vm: *VM) errors.VMError!void {
    // JUMPX is unconditional - dispatch loop handles PC update
    _ = vm;
}

/// FJUMPX: Extended false jump (16-bit offset, jump if NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleFJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    // FJUMPX logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

/// TJUMPX: Extended true jump (16-bit offset, jump if not NIL)
/// Per rewrite documentation instruction-set/opcodes.md
/// Note: PC update is handled by dispatch loop based on condition and returned offset
pub fn handleTJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    // TJUMPX logic is handled in dispatch loop
    // This handler exists for consistency and potential side effects
    _ = vm;
    _ = offset;
}

// ============================================================================
// Data Access Opcodes
// ============================================================================

/// CAR: Get CAR of list
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCAR(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    const list_ptr = stack_module.getTopOfStack(vm);
    
    if (list_ptr == 0) {
        // NIL - CAR of NIL is NIL
        return;
    }
    
    // Get cons cell from memory using address translation
    if (vm.virtual_memory) |vmem| {
        // Translate LispPTR to native pointer (4-byte aligned for cons cell)
        const native_ptr = virtual_memory_module.translateAddress(list_ptr, vmem.fptovp, 4) catch {
            // Invalid address - leave value unchanged
            return;
        };
        
        // Cast to cons cell pointer
        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
        
        // Handle indirect CDR encoding
        var car_value = cons.getCAR(cell);
        if (cell.cdr_code == cons.CDR_INDIRECT) {
            // CAR is stored in indirect cell
            const indirect_addr = car_value;
            const indirect_native = virtual_memory_module.translateAddress(indirect_addr, vmem.fptovp, 4) catch {
                return;
            };
            const indirect_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(indirect_native)));
            car_value = cons.getCAR(indirect_cell);
        }
        
        stack_module.setTopOfStack(vm, car_value);
    } else {
        // No virtual memory - can't access memory
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// CDR: Get CDR of list
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCDR(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    const list_ptr = stack_module.getTopOfStack(vm);
    
    if (list_ptr == 0) {
        // NIL - CDR of NIL is NIL
        return;
    }
    
    // Get cons cell from memory using address translation
    if (vm.virtual_memory) |vmem| {
        // Translate LispPTR to native pointer (4-byte aligned for cons cell)
        const native_ptr = virtual_memory_module.translateAddress(list_ptr, vmem.fptovp, 4) catch {
            // Invalid address - leave value unchanged
            return;
        };
        
        // Cast to cons cell pointer
        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
        
        // Decode CDR using CDR coding
        const cdr_value = cons.getCDR(cell, list_ptr);
        
        stack_module.setTopOfStack(vm, cdr_value);
    } else {
        // No virtual memory - can't access memory
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// CONS: Create cons cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCONS(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const storage_module = @import("../memory/storage.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop CDR and CAR values
    const cdr_value = try stack_module.popStack(vm);
    const car_value = try stack_module.popStack(vm);
    
    // Allocate cons cell from storage
    if (vm.storage) |storage| {
        const cell_addr = storage_module.allocateConsCell(storage) catch |err| {
            return switch (err) {
                error.StorageFull => errors_module.VMError.StorageFull,
                else => errors_module.VMError.MemoryAccessFailed,
            };
        };
        
        // Get native pointer to cons cell
        if (vm.virtual_memory) |vmem| {
            const native_ptr = virtual_memory_module.translateAddress(cell_addr, vmem.fptovp, 4) catch {
                return errors_module.VMError.MemoryAccessFailed;
            };
            
            const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
            
            // Set CAR
            cons.setCAR(cell, car_value);
            
            // Encode CDR using helper function
            cons.setCDR(cell, cell_addr, cdr_value);
            
            // Push cons cell address
            try stack_module.pushStack(vm, cell_addr);
        } else {
            // No virtual memory - can't set up cons cell properly
            return errors_module.VMError.MemoryAccessFailed;
        }
    } else {
        // No storage - can't allocate cons cell
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// RPLACA: Replace CAR
/// Per rewrite documentation instruction-set/opcodes.md and data-structures/cons-cells.md
pub fn handleRPLACA(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop new CAR value and cons cell pointer
    const new_car = try stack_module.popStack(vm);
    const cons_cell_ptr = try stack_module.popStack(vm);
    
    if (cons_cell_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not a cons cell
    }
    
    // Get cons cell from memory
    if (vm.virtual_memory) |vmem| {
        const native_ptr = virtual_memory_module.translateAddress(cons_cell_ptr, vmem.fptovp, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
        
        // Handle indirect CDR encoding
        if (cell.cdr_code == cons.CDR_INDIRECT) {
            // CAR is stored in indirect cell
            const indirect_addr = cell.car_field;
            const indirect_native = virtual_memory_module.translateAddress(indirect_addr, vmem.fptovp, 4) catch {
                return errors_module.VMError.MemoryAccessFailed;
            };
            const indirect_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(indirect_native)));
            cons.setCAR(indirect_cell, new_car);
        } else {
            // Normal CAR update
            cons.setCAR(cell, new_car);
        }
        
        // Push cons cell pointer back
        try stack_module.pushStack(vm, cons_cell_ptr);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// RPLACD: Replace CDR
/// Per rewrite documentation instruction-set/opcodes.md and data-structures/cons-cells.md
pub fn handleRPLACD(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop new CDR value and cons cell pointer
    const new_cdr = try stack_module.popStack(vm);
    const cons_cell_ptr = try stack_module.popStack(vm);
    
    if (cons_cell_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not a cons cell
    }
    
    // Get cons cell from memory
    if (vm.virtual_memory) |vmem| {
        const native_ptr = virtual_memory_module.translateAddress(cons_cell_ptr, vmem.fptovp, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
        
        // Encode and set new CDR
        cons.setCDR(cell, cons_cell_ptr, new_cdr);
        
        // Push cons cell pointer back
        try stack_module.pushStack(vm, cons_cell_ptr);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

// ============================================================================
// Array Access Opcodes
// ============================================================================

/// GETAEL1: Get array element (1-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETAEL1(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop array pointer from stack
    const array_ptr = try stack_module.popStack(vm);
    
    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }
    
    // Get array header from memory
    if (vm.virtual_memory) |vmem| {
        const native_ptr = virtual_memory_module.translateAddress(array_ptr, vmem.fptovp, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));
        
        // Get array element
        const element_value = array.getArrayElement(header, index);
        
        // Push element value
        try stack_module.pushStack(vm, element_value);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// GETAEL2: Get array element (2-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETAEL2(vm: *VM, index: u16) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop array pointer from stack
    const array_ptr = try stack_module.popStack(vm);
    
    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }
    
    // Get array header from memory
    if (vm.virtual_memory) |vmem| {
        const native_ptr = virtual_memory_module.translateAddress(array_ptr, vmem.fptovp, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));
        
        // Get array element
        const element_value = array.getArrayElement(header, index);
        
        // Push element value
        try stack_module.pushStack(vm, element_value);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// SETAEL1: Set array element (1-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSETAEL1(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop value and array pointer from stack
    const value = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);
    
    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }
    
    // Get array header from memory
    if (vm.virtual_memory) |vmem| {
        const native_ptr = virtual_memory_module.translateAddress(array_ptr, vmem.fptovp, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));
        
        // Set array element
        array.setArrayElement(header, index, value);
        
        // Push array pointer back
        try stack_module.pushStack(vm, array_ptr);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// SETAEL2: Set array element (2-byte index)
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSETAEL2(vm: *VM, index: u16) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    // Pop value and array pointer from stack
    const value = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);
    
    if (array_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not an array
    }
    
    // Get array header from memory
    if (vm.virtual_memory) |vmem| {
        const native_ptr = virtual_memory_module.translateAddress(array_ptr, vmem.fptovp, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        };
        
        const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));
        
        // Set array element
        array.setArrayElement(header, index, value);
        
        // Push array pointer back
        try stack_module.pushStack(vm, array_ptr);
    } else {
        return errors_module.VMError.MemoryAccessFailed;
    }
}

// ============================================================================
// Comparison Opcodes
// ============================================================================

/// EQ: Equality test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleEQ(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // EQ compares pointer equality
    const result: LispPTR = if (a == b) 1 else 0; // T or NIL
    try stack_module.pushStack(vm, result);
}

/// EQL: Equal test (deep comparison)
/// Per rewrite documentation instruction-set/opcodes.md
/// Recursively compares structures (not just pointer equality)
pub fn handleEQL(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    // EQL does deep comparison
    const result = eqlDeep(vm, a, b) catch |err| {
        return switch (err) {
            error.MemoryAccessFailed => err,
            else => errors_module.VMError.MemoryAccessFailed,
        };
    };
    
    const result_value: LispPTR = if (result) 1 else 0; // T or NIL
    try stack_module.pushStack(vm, result_value);
}

/// Deep equality comparison helper
/// Recursively compares two Lisp values
fn eqlDeep(vm: *VM, a: LispPTR, b: LispPTR) errors.VMError!bool {
    
    // Pointer equality check first (fast path)
    if (a == b) {
        return true;
    }
    
    // Both NIL
    if (a == 0 and b == 0) {
        return true;
    }
    
    // One is NIL, other is not
    if (a == 0 or b == 0) {
        return false;
    }
    
    // Both are fixnums (odd addresses)
    if ((a & 1) != 0 and (b & 1) != 0) {
        return a == b; // Compare as integers
    }
    
    // Both are pointers (even addresses) - need to compare structures
    if ((a & 1) == 0 and (b & 1) == 0) {
        if (vm.virtual_memory) |vmem| {
            // Try to access as cons cells
            const a_native = virtual_memory_module.translateAddress(a, vmem.fptovp, 4) catch {
                return false; // Invalid address
            };
            const b_native = virtual_memory_module.translateAddress(b, vmem.fptovp, 4) catch {
                return false; // Invalid address
            };
            
            const a_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(a_native)));
            const b_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(b_native)));
            
            // Compare CAR recursively
            const car_equal = try eqlDeep(vm, cons.getCAR(a_cell), cons.getCAR(b_cell));
            if (!car_equal) {
                return false;
            }
            
            // Compare CDR recursively
            const a_cdr = cons.getCDR(a_cell, a);
            const b_cdr = cons.getCDR(b_cell, b);
            return try eqlDeep(vm, a_cdr, b_cdr);
        } else {
            // No virtual memory - fall back to pointer equality
            return a == b;
        }
    }
    
    // Different types (one fixnum, one pointer)
    return false;
}

/// LESSP: Less than test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleLESSP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result: LispPTR = if (a_signed < b_signed) 1 else 0;
    
    try stack_module.pushStack(vm, result);
}

/// GREATERP: Greater than test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGREATERP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result: LispPTR = if (a_signed > b_signed) 1 else 0;
    
    try stack_module.pushStack(vm, result);
}

/// IGREATERP: Integer greater than
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIGREATERP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    
    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result: LispPTR = if (a_signed > b_signed) 1 else 0;
    
    try stack_module.pushStack(vm, result);
}

// ============================================================================
// Type Checking Opcodes
// ============================================================================

/// NTYPX: Type check without type code operand
/// Per rewrite documentation instruction-set/opcodes.md
/// Type check using value's type tag
pub fn handleNTYPX(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const value = stack_module.getTopOfStack(vm);
    
    // Get type of value
    const value_type = getValueType(value, vm);
    
    // Push type code on stack
    try stack_module.pushStack(vm, @as(LispPTR, value_type));
}

/// TYPEP: Type predicate
/// Per rewrite documentation instruction-set/opcodes.md
/// Checks if TOS value matches the given type_code
/// Type codes:
///   0: NIL
///   1: Fixnum (small integer)
///   2: Pointer (general pointer)
///   3: Cons cell
///   4: Array
///   ... (other types)
pub fn handleTYPEP(vm: *VM, type_code: u8) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const value = stack_module.getTopOfStack(vm);
    
    // Get type of value
    const value_type = getValueType(value, vm);
    
    // Check if value type matches requested type_code
    const result: LispPTR = if (value_type == type_code) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}

/// DTEST: Test if TOS is specific atom
/// Per rewrite documentation instruction-set/opcodes.md
/// Tests if TOS value equals the atom at given atom_index
pub fn handleDTEST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // DTEST requires:
    // 1. Atom table lookup (from atom_index)
    // 2. Get atom object
    // 3. Compare with TOS value
    
    // TODO: Proper implementation needs:
    // 1. Atom table access (needs atom table structure)
    // 2. Atom object retrieval
    // 3. Value comparison
    
    const value = stack_module.getTopOfStack(vm);
    
    // For now, placeholder: compare with atom_index (will be properly implemented)
    const result: LispPTR = if (value == @as(LispPTR, atom_index)) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}

/// UNWIND: Unwind stack frames
/// Per rewrite documentation instruction-set/opcodes.md
/// Unwinds stack frames based on unwind parameters
pub fn handleUNWIND(vm: *VM, unwind_params: u16) errors.VMError!void {
    // UNWIND requires:
    // 1. Parse unwind parameters (frame count, etc.)
    // 2. Unwind stack frames
    // 3. Restore previous frame state
    
    // TODO: Proper implementation needs:
    // 1. Frame unwinding logic
    // 2. Exception handling integration
    // 3. Cleanup of local variables
    
    _ = vm;
    _ = unwind_params; // Placeholder for now
}

/// Get type code for a LispPTR value
/// Returns type code based on value encoding
fn getValueType(value: LispPTR, vm: *VM) u8 {
    // NIL is type 0
    if (value == 0) {
        return 0; // NIL type
    }
    
    // Fixnums have low bit set (odd addresses)
    if ((value & 1) != 0) {
        return 1; // Fixnum type
    }
    
    // Even addresses are pointers
    // For now, we can't fully determine type without memory access
    // This is a simplified implementation that can be extended
    
    // If we have virtual memory, we could check the type tag
    // For now, return a generic pointer type (2)
    // TODO: When virtual memory is available, check actual type tags
    _ = vm; // Will be used when type tag lookup is implemented
    
    // Basic heuristics:
    // - Very small even values might be special constants
    // - Larger even values are likely pointers to objects
    
    // For now, return generic pointer type
    // This can be extended to check actual type tags from memory
    return 2; // Generic pointer type
}

/// FIXP: Fixnum predicate
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFIXP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    // Fixnum check: low bit should be 0 (even address)
    const result: LispPTR = if ((value & 1) == 0) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}

/// SMALLP: Small integer predicate
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSMALLP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    // Small integer check: value fits in small integer range
    // Small integers are typically in range -32768 to 32767
    const value_signed = @as(i32, @bitCast(@as(u32, value)));
    const result: LispPTR = if (value_signed >= -32768 and value_signed <= 32767) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}

/// LISTP: List predicate
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleLISTP(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    const value = stack_module.getTopOfStack(vm);
    // LISTP: NIL or cons cell (even address, not a fixnum)
    const result: LispPTR = if (value == 0 or ((value & 1) == 0 and (value & 0x3) != 0)) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}

// ============================================================================
// String/Character Opcodes
// ============================================================================

/// CHARCODE: Get character code from character object
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops character object from stack, pushes character code
/// Character objects are typically represented as fixnums with character code
pub fn handleCHARCODE(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // Pop character object from stack
    const char_obj = try stack_module.popStack(vm);
    
    // Extract character code from character object
    // In Lisp, characters are typically represented as fixnums
    // Character code is typically in the low 8 bits (or 16 bits for wide chars)
    // For now, treat as 8-bit character code
    const char_code: LispPTR = char_obj & 0xFF;
    
    // Push character code
    try stack_module.pushStack(vm, char_code);
}

/// CHARN: Create character object from code
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops character code from stack, pushes character object
/// Character code is typically a small integer (0-255 for 8-bit, 0-65535 for 16-bit)
pub fn handleCHARN(vm: *VM) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
    // Pop character code from stack
    const char_code = try stack_module.popStack(vm);
    
    // Create character object from code
    // In Lisp, characters are typically represented as fixnums
    // Character code is stored in the low bits
    // For now, treat as 8-bit character code (mask to ensure valid range)
    const char_obj: LispPTR = char_code & 0xFF;
    
    // Push character object
    try stack_module.pushStack(vm, char_obj);
}

// ============================================================================
// Variable Access Opcodes
// ============================================================================

/// IVAR0-IVAR6: Local variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handleIVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
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
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    
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
    const stack_module = @import("stack.zig");
    const errors_module = @import("../utils/errors.zig");
    const function_header_module = @import("../data/function_header.zig");
    const types_module = @import("../utils/types.zig");
    
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
    const stack_module = @import("stack.zig");
    
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
    const stack_module = @import("stack.zig");
    
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

/// GCONST: Global constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes global constant atom by atom index
pub fn handleGCONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("stack.zig");
    
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