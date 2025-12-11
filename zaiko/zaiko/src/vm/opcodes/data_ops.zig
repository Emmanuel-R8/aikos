const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const cons = @import("../../data/cons.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");
const storage_module = @import("../../memory/storage.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

// ============================================================================
// Data Access Opcodes
// ============================================================================

/// CAR: Get CAR of list
/// Per rewrite documentation instruction-set/opcodes.md
/// CAR: Get CAR of list
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/inc/inlineC.h:OPCAR
pub fn handleCAR(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const list_ptr = stack_module.getTopOfStack(vm);

    // C: Check Listp(TOPOFSTACK) first, then handle NIL_PTR and ATOM_T
    // For now, we check NIL first (simpler)
    if (list_ptr == 0) {
        // NIL - CAR of NIL is NIL (C: nextop1, value unchanged)
        return;
    }

    // Check special cases: ATOM_T and NIL
    const type_check_module = @import("../../utils/type_check.zig");
    if (type_check_module.isT(list_ptr)) {
        // CAR of T is T (C: special case)
        stack_module.setTopOfStack(vm, type_check_module.ATOM_T);
        return;
    }
    
    // Check if it's a list (Listp check)
    if (!type_check_module.isList(vm, list_ptr)) {
        // Not a list - should trigger UFN lookup
        // For now, leave value unchanged (C: goto op_ufn)
        return;
    }

    // Get cons cell from memory using address translation
    if (vm.virtual_memory) |_| {
        // Translate LispPTR to native pointer (4-byte aligned for cons cell)
        // C: NativeAligned4FromLAddr(TOPOFSTACK)
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(list_ptr, fptovp_table, 4) catch {
            // Invalid address - trigger UFN (for now, leave value unchanged)
            return;
        } else {
            // No FPtoVP table - can't translate address
            return;
        };

        // Cast to cons cell pointer
        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));

        // Handle indirect CDR encoding
        // C: if (DATUM68K->cdr_code == CDR_INDIRECT) { TOPOFSTACK = indirect_cell->car_field }
        var car_value = cons.getCAR(cell);
        if (cell.cdr_code == cons.CDR_INDIRECT) {
            // CAR is stored in indirect cell
            // C: TOPOFSTACK = ((ConsCell *)NativeAligned4FromLAddr(DATUM68K->car_field))->car_field
            const indirect_addr = car_value;
            const indirect_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(indirect_addr, fptovp_table, 4) catch {
                return;
            } else {
                return;
            };
            const indirect_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(indirect_native)));
            car_value = cons.getCAR(indirect_cell);
        }

        // C: TOPOFSTACK = car_value; nextop1;
        stack_module.setTopOfStack(vm, car_value);
    } else {
        // No virtual memory - can't access memory
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// CDR: Get CDR of list
/// Per rewrite documentation instruction-set/opcodes.md
/// Matches C implementation: maiko/inc/inlineC.h:OPCDR (NEWCDRCODING version)
pub fn handleCDR(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const list_ptr = stack_module.getTopOfStack(vm);

    // C: Check Listp(TOPOFSTACK) first, then handle NIL_PTR
    if (list_ptr == 0) {
        // NIL - CDR of NIL is NIL (C: nextop1, value unchanged)
        return;
    }

    // Check if it's a list (Listp check)
    const type_check_module = @import("../../utils/type_check.zig");
    if (!type_check_module.isList(vm, list_ptr)) {
        // Not a list - should trigger UFN lookup
        // For now, leave value unchanged (C: goto op_ufn)
        return;
    }

    // Get cons cell from memory using address translation
    if (vm.virtual_memory) |_| {
        // Translate LispPTR to native pointer (4-byte aligned for cons cell)
        // C: NativeAligned4FromLAddr(TOPOFSTACK)
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(list_ptr, fptovp_table, 4) catch {
            // Invalid address - trigger UFN (for now, leave value unchanged)
            return;
        } else {
            // No FPtoVP table - can't translate address
            return;
        };

        // Cast to cons cell pointer
        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));

        // Decode CDR using CDR coding (NEWCDRCODING version)
        // C: CDRCODEX = DATUM68K->cdr_code
        const cdr_code = cell.cdr_code;

        var cdr_value: LispPTR = 0;

        // C: NEWCDRCODING version (matches maiko/inc/inlineC.h:122-150)
        if (cdr_code == cons.CDR_NIL) {
            // C: CDR_NIL (8) -> TOPOFSTACK = NIL_PTR
            cdr_value = 0;
        } else if (cdr_code > cons.CDR_ONPAGE_MIN) {
            // C: CDR_ONPAGE (8-15) -> TOPOFSTACK = TOPOFSTACK + ((CDRCODEX & 7) << 1)
            // Same page encoding: 3-bit offset
            const offset = (@as(u32, cdr_code) & 7) << 1;
            cdr_value = list_ptr + offset;
        } else if (cdr_code == cons.CDR_INDIRECT) {
            // C: CDR_INDIRECT (0) -> TOPOFSTACK = cdr(DATUM68K->car_field) (recursive)
            // Recursive CDR call on indirect cell
            const indirect_addr = cons.getCAR(cell);
            const indirect_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(indirect_addr, fptovp_table, 4) catch {
                return;
            } else {
                return;
            };
            const indirect_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(indirect_native)));
            // Recursive call to getCDR
            cdr_value = cons.getCDR(indirect_cell, indirect_addr);
        } else {
            // C: CDR different page (1-7) -> TOPOFSTACK = ((ConsCell *)NativeAligned4FromLAddr(TOPOFSTACK + (CDRCODEX << 1)))->car_field
            // Different page encoding: CDR stored in another cell's CAR field
            const offset = @as(u32, cdr_code) << 1;
            const cdr_addr = list_ptr + offset;
            const cdr_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(cdr_addr, fptovp_table, 4) catch {
                return;
            } else {
                return;
            };
            const cdr_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(cdr_native)));
            cdr_value = cons.getCAR(cdr_cell);
        }

        // C: TOPOFSTACK = cdr_value; nextop1;
        stack_module.setTopOfStack(vm, cdr_value);
    } else {
        // No virtual memory - can't access memory
        return errors_module.VMError.MemoryAccessFailed;
    }
}

/// CONS: Create cons cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCONS(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

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
        if (vm.virtual_memory) |_| {
            const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(cell_addr, fptovp_table, 4) catch {
                return errors_module.VMError.MemoryAccessFailed;
            } else {
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
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop new CAR value and cons cell pointer
    const new_car = try stack_module.popStack(vm);
    const cons_cell_ptr = try stack_module.popStack(vm);

    if (cons_cell_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not a cons cell
    }

    // Get cons cell from memory
    if (vm.virtual_memory) |_| {
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(cons_cell_ptr, fptovp_table, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        } else {
            return errors_module.VMError.MemoryAccessFailed;
        };

        const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));

        // Handle indirect CDR encoding
        if (cell.cdr_code == cons.CDR_INDIRECT) {
            // CAR is stored in indirect cell
            const indirect_addr = cell.car_field;
            const indirect_native = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(indirect_addr, fptovp_table, 4) catch {
                return;
            } else {
                return;
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
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Pop new CDR value and cons cell pointer
    const new_cdr = try stack_module.popStack(vm);
    const cons_cell_ptr = try stack_module.popStack(vm);

    if (cons_cell_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // NIL is not a cons cell
    }

    // Get cons cell from memory
    if (vm.virtual_memory) |_| {
        const native_ptr = if (vm.fptovp) |fptovp_table| virtual_memory_module.translateAddress(cons_cell_ptr, fptovp_table, 4) catch {
            return errors_module.VMError.MemoryAccessFailed;
        } else {
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