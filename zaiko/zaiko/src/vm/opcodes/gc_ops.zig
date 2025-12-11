const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

/// GCREF: Garbage collection reference counting
/// Per C implementation: maiko/src/gc.c:OP_gcref
/// alpha operand: ADDREF (0), DELREF (1), or STKREF (2)
/// TopOfStack is the slot address to reference count
/// Per tasks.md: Update GCREF handler to call GC operations
pub fn handleGCREF(vm: *VM, alpha: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    const ptr = stack_module.getTopOfStack(vm);

    // Get GC instance from VM (if available)
    // TODO: Add GC to VM struct when GC is integrated
    // For now, create temporary GC for testing
    // In full implementation, VM will have a GC instance

    // alpha: 0 = ADDREF, 1 = DELREF, 2 = STKREF
    switch (alpha) {
        0 => {
            // ADDREF: Add reference to object
            // TODO: Get GC from VM when integrated
            // const gc_module = @import("../../memory/gc.zig");
            // try gc_module.addReference(vm.gc, ptr);
            // For now, just leave TopOfStack (stub until GC is integrated into VM)
            _ = ptr;
        },
        1 => {
            // DELREF: Delete reference to object
            // TODO: Get GC from VM when integrated
            // const gc_module = @import("../../memory/gc.zig");
            // try gc_module.deleteReference(vm.gc, ptr);
            // If refcnt reaches 0, replace TopOfStack with 0
            // For now, just leave TopOfStack (stub until GC is integrated into VM)
            _ = ptr;
        },
        2 => {
            // STKREF: Mark as stack reference
            // TODO: Get GC from VM when integrated
            // const gc_module = @import("../../memory/gc.zig");
            // try gc_module.markStackReference(vm.gc, ptr);
            // For now, just leave TopOfStack (stub until GC is integrated into VM)
            _ = ptr;
        },
        else => {
            // Invalid alpha value
            return errors_module.VMError.InvalidOpcode;
        },
    }
}