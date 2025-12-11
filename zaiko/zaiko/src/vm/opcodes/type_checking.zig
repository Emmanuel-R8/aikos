const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// Type Checking Opcodes

/// NTYPX: Type check without type code operand
/// Per rewrite documentation instruction-set/opcodes.md
/// Type check using value's type tag
pub fn handleNTYPX(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
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
    const stack_module = @import("../stack.zig");
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
    const stack_module = @import("../stack.zig");

    // DTEST: Check if TOS value has type named by atom_index
    // C: DTEST macro in maiko/inc/inlineC.h
    // Walks DTD chain to find matching type name
    
    const value = stack_module.getTopOfStack(vm);
    const type_check_module = @import("../../utils/type_check.zig");
    
    // Get type number of TOS value
    const type_num = type_check_module.getTypeNumber(vm, value) orelse {
        // Can't determine type - return NIL
        stack_module.setTopOfStack(vm, type_check_module.NIL_PTR);
        return;
    };
    
    // TODO: Walk DTD chain to check if atom_index matches type name
    // For now, simplified: check if type_num matches atom_index (basic check)
    // Full implementation needs:
    // 1. Get DTD from type_num: GetDTD(type_num)
    // 2. Walk DTD chain: dtd->dtd_supertype
    // 3. Compare dtd->dtd_name with atom_index
    // 4. Return ATOM_T if match found, NIL_PTR otherwise
    
    // Placeholder: basic type number comparison
    const result: LispPTR = if (type_num == atom_index) type_check_module.ATOM_T else type_check_module.NIL_PTR;
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
    // Use type_check module for type tag lookup
    const type_check_module = @import("../../utils/type_check.zig");
    const type_num = type_check_module.getTypeNumber(vm, value);
    if (type_num) |tn| {
        return @as(u8, @intCast(tn));
    }

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
    const stack_module = @import("../stack.zig");

    const value = stack_module.getTopOfStack(vm);
    // Fixnum check: low bit should be 0 (even address)
    const result: LispPTR = if ((value & 1) == 0) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}

/// SMALLP: Small integer predicate
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSMALLP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

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
    const stack_module = @import("../stack.zig");

    const value = stack_module.getTopOfStack(vm);
    // LISTP: NIL or cons cell (even address, not a fixnum)
    const result: LispPTR = if (value == 0 or ((value & 1) == 0 and (value & 0x3) != 0)) 1 else 0;
    stack_module.setTopOfStack(vm, result);
}