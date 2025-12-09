const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const cons = @import("../../data/cons.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// Comparison Opcodes

/// EQ: Equality test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleEQ(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

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
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

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
/// BUG FIX: Removed duplicate return false code (lines 1126-1132 in original)
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
        if (vm.virtual_memory) |_| {
            // Try to access as cons cells
            // BUG FIX: Removed duplicate return false (original had lines 1126-1132 with duplicate code)
            const a_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(a, fptovp_table, 4) catch return false else return false;
            const b_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(b, fptovp_table, 4) catch return false else return false;

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
    const stack_module = @import("../stack.zig");

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
    const stack_module = @import("../stack.zig");

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
    const stack_module = @import("../stack.zig");

    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    const a_signed = @as(i32, @bitCast(@as(u32, a)));
    const b_signed = @as(i32, @bitCast(@as(u32, b)));
    const result: LispPTR = if (a_signed > b_signed) 1 else 0;

    try stack_module.pushStack(vm, result);
}

/// Helper: Check if LispPTR is NIL
fn isNil(ptr: LispPTR) bool {
    return ptr == 0;
}

/// Helper: Check if LispPTR is a fixnum (odd address)
fn isFixnum(ptr: LispPTR) bool {
    return (ptr & 1) != 0;
}

/// Helper: Check if LispPTR is a cons cell pointer (even address, not NIL, not fixnum)
fn isConsCell(ptr: LispPTR) bool {
    return ptr != 0 and (ptr & 1) == 0;
}

/// Helper: Recursive equality comparison
/// Per rewrite documentation instruction-set/opcodes.md
fn equalRecursive(vm: *VM, a: LispPTR, b: LispPTR) errors.VMError!bool {
    // Same pointer = equal
    if (a == b) return true;

    // NIL comparison
    if (isNil(a) or isNil(b)) return false;

    // Fixnum comparison
    if (isFixnum(a) and isFixnum(b)) {
        // Extract fixnum values (right shift by 1)
        const a_val = @as(i32, @bitCast(@as(u32, a >> 1)));
        const b_val = @as(i32, @bitCast(@as(u32, b >> 1)));
        return a_val == b_val;
    }

    // Type mismatch if one is fixnum and other isn't
    if (isFixnum(a) != isFixnum(b)) return false;

    // Cons cell comparison (recursive)
    if (isConsCell(a) and isConsCell(b)) {
        if (vm.virtual_memory) |_| {
            // Get CAR values
            const a_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(a, fptovp_table, 4) catch return false else return false;
            const b_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(b, fptovp_table, 4) catch return false else return false;

            const a_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(a_native)));
            const b_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(b_native)));

            const a_car = cons.getCAR(a_cell);
            const b_car = cons.getCAR(b_cell);

            // Compare CAR recursively
            if (!try equalRecursive(vm, a_car, b_car)) return false;

            // Get CDR values
            const a_cdr = cons.decodeCDR(a_cell, a);
            const b_cdr = cons.decodeCDR(b_cell, b);

            // Compare CDR recursively
            return try equalRecursive(vm, a_cdr, b_cdr);
        } else {
            // Without virtual memory, can't access cons cells
            // Fall back to pointer comparison
            return a == b;
        }
    }

    // For other types (atoms, arrays, etc.), use pointer comparison for now
    // TODO: Implement proper comparison for atoms and arrays
    return a == b;
}

/// EQUAL: Deep equality comparison
/// Per rewrite documentation instruction-set/opcodes.md
/// Compares two values recursively (handles cons cells, numbers, atoms)
pub fn handleEQUAL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop two values from stack
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);

    // Perform recursive comparison
    const is_equal = try equalRecursive(vm, a, b);

    // Push result: T (1) if equal, NIL (0) if not
    const result: LispPTR = if (is_equal) 1 else 0;
    try stack_module.pushStack(vm, result);
}