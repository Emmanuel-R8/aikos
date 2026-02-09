const std = @import("std");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const types = @import("../utils/types.zig");

const VM = stack.VM;

// Interrupt flag atoms (matching C implementation)
const IO_INTERRUPT: types.LispPTR = 1; // Atom for I/O interrupts
const TIMER_INTERRUPT: types.LispPTR = 2; // Atom for timer interrupts
const SYSTEM_INTERRUPT: types.LispPTR = 3; // Atom for system interrupts
// Note: ATOM_T (0/NIL) is obtained from atom module, not hardcoded

/// Interrupt types
pub const InterruptType = enum {
    IO,
    Timer,
    System,
    StorageFull,
};

/// Interrupt state structure (matches C: INTSTAT)
/// Per maiko/src/*: Structure contains various interrupt flags
pub const InterruptState = struct {
    stackoverflow: bool,
    log_file_io: bool,
    io_interrupt: bool,
    timer_interrupt: bool,
    system_interrupt: bool,
};

/// Check for interrupts
/// Per contracts/vm-core-interface.zig
/// Matches C: Check for pending interrupts in interrupt state
pub fn checkInterrupts(vm: *VM) bool {
    _ = vm;

    // For now, basic implementation - no interrupt checking
    // TODO: Implement full interrupt state checking when needed
    // This matches the C pattern where interrupt states are checked
    return false;
}

/// Handle interrupt
/// Per contracts/vm-core-interface.zig
pub fn handleInterrupt(vm: *VM, interrupt_type: InterruptType) errors.VMError!void {
    _ = vm;
    _ = interrupt_type;
    // TODO: Implement interrupt handling
    // 1. Save current execution state
    // 2. Process interrupt
    // 3. Restore execution state
}

/// Set stack overflow flag
/// Per C implementation: *STACKOVERFLOW_word = *PENDINGINTERRUPT_word = ATOM_T;
pub fn setStackOverflowFlag(vm: *VM) void {
    if (vm.storage) |storage| {
        // Get the ATOM_T atom for stack overflow
        const atom_module = @import("../data/atom.zig");
        const ATOM_T = atom_module.findAtom("T", storage) catch 0;

        // Set stack overflow flag in storage
        if (ATOM_T != 0) {
            const stack_overflow_ptr: *types.LispPTR = @ptrCast(@alignCast(storage.heap_ptr + @sizeOf(types.LispPTR)));
            stack_overflow_ptr.* = ATOM_T;
        }
    }
}

/// Set interrupt flag
/// Per contracts/vm-core-interface.zig
pub fn setInterruptFlag(vm: *VM, interrupt_type: InterruptType) void {
    if (vm.storage) |storage| {
        // Basic implementation: set interrupt atom in PENDINGINTERRUPT_word
        // This matches C pattern of setting interrupt flags
        const atom_module = @import("../data/atom.zig");
        const interrupt_atom = switch (interrupt_type) {
            .IO => IO_INTERRUPT,
            .Timer => TIMER_INTERRUPT,
            .System => SYSTEM_INTERRUPT,
        };

        // Store interrupt flag (matches C: *PENDINGINTERRUPT_word = interrupt_atom)
        const pending_interrupt_atom = atom_module.findAtom("\\PENDINGINTERRUPT", storage) catch return;
        if (pending_interrupt_atom != 0) {
            const pending_ptr: *types.LispPTR = @ptrCast(@alignCast(storage.heap_ptr + @sizeOf(types.LispPTR)));
            pending_ptr.* = interrupt_atom;
        }
    }
}
