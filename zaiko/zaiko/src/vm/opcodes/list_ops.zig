const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const cons = @import("../../data/cons.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");
const type_check_module = @import("../../utils/type_check.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// ASSOC: Association list lookup
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_assoc in maiko/src/vars3.c
/// Looks up key in association list (list of (key . value) pairs)
/// Stack: [key, alist] -> [pair or NIL]
pub fn handleASSOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const key = try stack_module.popStack(vm);
    var alist = try stack_module.popStack(vm);

    // C: if (list == NIL_PTR) { return (NIL_PTR); }
    if (alist == 0) {
        try stack_module.pushStack(vm, 0); // Return NIL
        return;
    }

    // C: if (!Listp(list)) { return (NIL_PTR); }
    if (!type_check_module.isList(vm, alist)) {
        try stack_module.pushStack(vm, 0); // Return NIL
        return;
    }

    // Traverse association list
    // C: do { ... } while (cdr != NIL_PTR);
    while (type_check_module.isList(vm, alist)) {
        // Get CAR of alist (should be a (key . value) pair)
        const pair = try getCAR(vm, alist);
        
        // Check if pair is a list and its CAR matches key
        // C: if (Listp(cadr1.car_cell) && key == car(cadr1.car_cell))
        if (type_check_module.isList(vm, pair)) {
            const pair_key = try getCAR(vm, pair);
            if (key == pair_key) {
                // Found matching key - return the pair
                try stack_module.pushStack(vm, pair);
                return;
            }
        }
        
        // Get CDR to continue traversal
        alist = try getCDR(vm, alist);
        
        // C: Check for interrupts (we'll skip for now)
    }

    // Not found - return NIL
    try stack_module.pushStack(vm, 0);
}

/// Helper: Get CAR of list (with error handling)
fn getCAR(vm: *VM, list_ptr: LispPTR) errors.VMError!LispPTR {
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return errors.VMError.MemoryAccessFailed;
    }
    
    const fptovp_table = vm.fptovp.?;
    const native_ptr = virtual_memory_module.translateAddress(list_ptr, fptovp_table, 4) catch {
        return errors.VMError.InvalidAddress;
    };
    
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
    return cons.getCAR(cell);
}

/// Helper: Get CDR of list (with error handling)
fn getCDR(vm: *VM, list_ptr: LispPTR) errors.VMError!LispPTR {
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return errors.VMError.MemoryAccessFailed;
    }
    
    const fptovp_table = vm.fptovp.?;
    const native_ptr = virtual_memory_module.translateAddress(list_ptr, fptovp_table, 4) catch {
        return errors.VMError.InvalidAddress;
    };
    
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
    return cons.getCDR(cell, list_ptr);
}

/// CMLASSOC: Case-insensitive association list lookup
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCMLASSOC(vm: *VM) errors.VMError!void {
    // Similar to ASSOC but case-insensitive
    // Placeholder: same as ASSOC
    try handleASSOC(vm);
}

/// FMEMB: Fast member test
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_fmemb in maiko/src/lsthandl.c
/// Tests if item is in list, returns list starting from item if found, NIL otherwise
/// Stack: [item, list] -> [sublist or NIL]
pub fn handleFMEMB(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const item = try stack_module.popStack(vm);
    var list = try stack_module.popStack(vm);

    // C: while (Listp(tos)) { if (item == car(tos)) return tos; tos = cdr(tos); }
    while (type_check_module.isList(vm, list)) {
        const car_value = try getCAR(vm, list);
        if (item == car_value) {
            // Found item - return list starting from this position
            try stack_module.pushStack(vm, list);
            return;
        }
        list = try getCDR(vm, list);
    }

    // C: if (tos) ERROR_EXIT(tos);
    if (list != 0) {
        // Not a list and not NIL - error
        return errors_module.VMError.InvalidAddress;
    }

    // Not found - return NIL
    try stack_module.pushStack(vm, 0);
}

/// CMLMEMBER: Case-insensitive member test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCMLMEMBER(vm: *VM) errors.VMError!void {
    // Similar to FMEMB but case-insensitive
    // Placeholder: same as FMEMB
    try handleFMEMB(vm);
}

/// FINDKEY: Find key in association list
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFINDKEY(vm: *VM, key_index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // FINDKEY requires:
    // 1. Association list on stack
    // 2. Key index
    // 3. Find key-value pair

    // TODO: Proper implementation
    // Placeholder: return NIL
    const alist = try stack_module.popStack(vm);
    _ = alist;
    _ = key_index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// RESTLIST: Rest of list
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_restlist in maiko/src/z2.c
/// Builds list by consing elements from IVar array in reverse order
/// Stack: [tail] -> [result_list]
/// Note: C implementation uses IVar array, but we'll implement a simpler version
pub fn handleRESTLIST(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // C: N_OP_restlist(tail, last, skip)
    // last &= 0xFFFF;
    // while (skip <= last) { tail = cons(GetLongWord(IVar + (--last << 1)), tail); }
    // This is complex - requires IVar access and cons creation
    // For now, implement simplified version: traverse list count times using CDR
    
    var tail = stack_module.getTopOfStack(vm);
    
    // Traverse list count times using CDR
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        if (type_check_module.isList(vm, tail)) {
            tail = try getCDR(vm, tail);
        } else {
            // Not a list - return as-is
            break;
        }
    }
    
    // Push result
    stack_module.setTopOfStack(vm, tail);
}

/// RPLCONS: Replace cons
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_rplcons in maiko/src/rplcons.c
/// Replaces CDR of a cons cell with new value
/// Stack: [new_cdr, list] -> [list]
pub fn handleRPLCONS(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const gc_module = @import("../../memory/gc.zig");

    const new_cdr = try stack_module.popStack(vm);
    const list = try stack_module.popStack(vm);

    // C: if (!Listp(list)) ERROR_EXIT(item);
    if (!type_check_module.isList(vm, list)) {
        return errors_module.VMError.InvalidAddress;
    }

    // Get cons cell
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return errors_module.VMError.MemoryAccessFailed;
    }
    
    const fptovp_table = vm.fptovp.?;
    
    const native_ptr = virtual_memory_module.translateAddress(list, fptovp_table, 4) catch {
        return errors_module.VMError.InvalidAddress;
    };
    
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
    
    // Get old CDR for GC
    const old_cdr = cons.getCDR(cell, list);
    
    // Update GC refs: DELREF old CDR, ADDREF new CDR
    if (vm.gc) |gc| {
        gc_module.deleteReference(gc, old_cdr) catch {};
        gc_module.addReference(gc, new_cdr) catch {};
    }
    
    // Set new CDR
    cons.setCDR(cell, list, new_cdr);
    
    // Push list (unchanged)
    try stack_module.pushStack(vm, list);
}

/// LISTGET: Get from list
/// Per rewrite documentation instruction-set/opcodes.md
/// C: N_OP_listget in maiko/src/lsthandl.c
/// Gets element at index from list
/// Stack: [index, list] -> [element]
pub fn handleLISTGET(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    const index_value = try stack_module.popStack(vm);
    var list = try stack_module.popStack(vm);

    // Extract index (should be small integer)
    const segment = index_value & types.SEGMASK;
    var index: u32 = 0;
    if (segment == types.S_POSITIVE) {
        index = index_value & 0xFFFF;
    } else if (segment == types.S_NEGATIVE) {
        // Negative index - invalid
        try stack_module.pushStack(vm, 0);
        return;
    } else {
        // Not a small integer - return NIL
        try stack_module.pushStack(vm, 0);
        return;
    }

    // Traverse list index times using CDR
    var i: u32 = 0;
    while (i < index) : (i += 1) {
        if (type_check_module.isList(vm, list)) {
            list = try getCDR(vm, list);
        } else {
            // List too short - return NIL
            try stack_module.pushStack(vm, 0);
            return;
        }
    }

    // Get CAR of current position
    if (type_check_module.isList(vm, list)) {
        const element = try getCAR(vm, list);
        try stack_module.pushStack(vm, element);
    } else {
        // Not a list - return NIL
        try stack_module.pushStack(vm, 0);
    }
}