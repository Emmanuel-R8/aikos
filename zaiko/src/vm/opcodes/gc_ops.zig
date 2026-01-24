const errors = @import("../../utils/errors.zig");
const std = @import("std");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// GCREF: Garbage collection reference counting
/// Per C implementation: maiko/src/gc.c:OP_gcref
/// C: GCLOOKUPV(TopOfStack, Get_code_BYTE(PC + 1), TopOfStack)
/// alpha operand: ADDREF (0), DELREF (1), or STKREF (2)
/// TopOfStack is the slot address to reference count
/// Per tasks.md: Update GCREF handler to call GC operations
///
/// Behavior (from C implementation):
/// - If stk=0 and refcnt=0 of entry of HashMainTable, TopOfStack left alone
/// - else replace TopOfStack with 0
pub fn handleGCREF(vm: *VM, alpha: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const gc_module = @import("../../memory/gc.zig");

    const ptr = stack_module.getTopOfStack(vm);

    // Get GC instance from VM (if available)
    const gc = vm.gc orelse {
        // No GC available - leave TopOfStack unchanged (GC disabled)
        return;
    };

    // alpha: 0 = ADDREF, 1 = DELREF, 2 = STKREF
    switch (alpha) {
        0 => {
            // ADDREF: Add reference to object
            // C: htfind(ptr, ADDREF) or rec_htfind(ptr, ADDREF)
            gc_module.addReference(gc, ptr) catch {
                // GC errors are non-fatal - continue execution
            };
            // TopOfStack remains unchanged for ADDREF
        },
        1 => {
            // DELREF: Delete reference to object
            // C: htfind(ptr, DELREF) or rec_htfind(ptr, DELREF)
            // Get refcount before deletion to check if it reaches 0
            const refcount_before = gc_module.getReferenceCount(gc, ptr);

            gc_module.deleteReference(gc, ptr) catch {
                // GC errors are non-fatal - continue execution
            };

            // Check if refcount reached 0
            const refcount_after = gc_module.getReferenceCount(gc, ptr);

            // C behavior: If stk=0 and refcnt=0, TopOfStack left alone
            // Otherwise, replace TopOfStack with 0
            // For now, if refcount reached 0, replace TopOfStack with 0
            if (refcount_before > 0 and refcount_after == 0) {
                // Refcount reached 0 - replace TopOfStack with 0 (NIL)
                stack_module.setTopOfStack(vm, 0);
            }
            // Otherwise, TopOfStack remains unchanged
        },
        2 => {
            // STKREF: Mark as stack reference
            // C: htfind(ptr, STKREF) or rec_htfind(ptr, STKREF)
            gc_module.markStackReference(gc, ptr) catch {
                // GC errors are non-fatal - continue execution
            };
            // TopOfStack remains unchanged for STKREF
        },
        else => {
            // Invalid alpha value
            return errors_module.VMError.InvalidOpcode;
        },
    }
}

/// RECLAIMCELL: Manual GC trigger
/// Per C implementation: maiko/src/xc_dispatch_part_04.inc
/// Opcode 0x72 (114) - Manual garbage collection trigger
pub fn handleRECLAIMCELL(vm: *VM) errors.VMError!void {
    const gc = vm.gc orelse {
        // No GC available - ignore manual trigger
        return;
    };

    if (!gc.gc_enabled) {
        // GC disabled - ignore manual trigger
        return;
    }

    // Manual GC trigger - run garbage collection
    const gc_module = @import("../../memory/gc.zig");
    gc_module.runGC(gc) catch |err| {
        // GC errors are non-fatal - continue execution
        std.debug.print("GC error during manual RECLAIMCELL: {}\n", .{err});
    };
    gc.gc_run_count += 1;

    // Reset countdown to reclaim_min after manual GC
    gc.reclaim_countdown = gc.reclaim_min;
}

/// GCSCAN1: GC scan phase 1
/// Per C implementation: maiko/src/gc2.c
/// Opcode 0x73 (115) - First phase of garbage collection scanning
pub fn handleGCSCAN1(vm: *VM) errors.VMError!void {
    _ = vm;
    // TODO: Implement GC scan phase 1
    // C: OP_gcscan1() - scans for garbage objects
    // For now, just continue execution
}

/// GCSCAN2: GC scan phase 2
/// Per C implementation: maiko/src/gc2.c
/// Opcode 0x74 (116) - Second phase of garbage collection scanning
pub fn handleGCSCAN2(vm: *VM) errors.VMError!void {
    _ = vm;
    // TODO: Implement GC scan phase 2
    // C: OP_gcscan2() - completes garbage collection scanning
    // For now, just continue execution
}
