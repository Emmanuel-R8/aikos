// Type checking utilities matching C implementation
// Based on maiko/inc/lsptypes.h

const types = @import("types.zig");
const stack = @import("../vm/stack.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const VM = stack.VM;

// Type numbers from maiko/inc/lsptypes.h
pub const TYPE_SMALLP: u16 = 1; // Small integer (encoded directly in LispPTR)
pub const TYPE_FIXP: u16 = 2; // Boxed integer (pointer to memory)
pub const TYPE_LISTP: u16 = 5;
pub const TYPE_NEWATOM: u16 = 21;

// Special values
pub const ATOM_T: LispPTR = 1; // T atom
pub const NIL_PTR: LispPTR = 0; // NIL

/// Get type entry from type table
/// C: GetTypeEntry(address) = GETWORD(MDStypetbl + ((address) >> 9))
/// For now, simplified version - will need full type table access later
fn getTypeEntry(vm: *VM, address: LispPTR) ?u16 {
    _ = vm; // Will use VM to access type table when available
    
    // Special cases
    if (address == NIL_PTR) return 0; // NIL type
    if (address == ATOM_T) return TYPE_NEWATOM; // T is an atom
    
    // For now, we can't fully implement without type table access
    // This is a placeholder that will be extended
    return null;
}

/// Get type number from address
/// C: GetTypeNumber(address) = GetTypeEntry(address) & 0x7ff
pub fn getTypeNumber(vm: *VM, address: LispPTR) ?u16 {
    const type_entry = getTypeEntry(vm, address) orelse return null;
    return type_entry & 0x7ff; // Low 11 bits
}

/// Check if address is a list (cons cell)
/// C: Listp(address) = (GetTypeNumber(address) == TYPE_LISTP)
pub fn isList(vm: *VM, address: LispPTR) bool {
    // Special cases
    if (address == NIL_PTR) return false; // NIL is not a list (though (cdr nil) = nil)
    if (address == ATOM_T) return false; // T is an atom, not a list
    
    // For addresses that could be cons cells:
    // - Must be even (4-byte aligned)
    // - Must be within virtual memory bounds
    if ((address & 1) != 0) return false; // Odd addresses are fixnums
    
    // Check if we can access the address (basic validation)
    if (vm.virtual_memory) |vmem| {
        if (@as(usize, @intCast(address)) >= vmem.len) return false;
    } else {
        return false;
    }
    
    // For now, assume even addresses within bounds could be cons cells
    // Full implementation would check type table: GetTypeNumber(address) == TYPE_LISTP
    // TODO: Implement full type table lookup when MDStypetbl is available
    return true;
}

/// Check if address is NIL
pub fn isNil(address: LispPTR) bool {
    return address == NIL_PTR;
}

/// Check if address is T
pub fn isT(address: LispPTR) bool {
    return address == ATOM_T;
}