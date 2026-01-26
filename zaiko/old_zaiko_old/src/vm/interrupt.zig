const std = @import("std");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");

const VM = stack.VM;

/// Interrupt types
pub const InterruptType = enum {
    IO,
    Timer,
    System,
    StorageFull,
};

/// Interrupt state
pub const InterruptState = struct {
    pending: bool,
    interrupt_type: ?InterruptType,
};

/// Check for interrupts
/// Per contracts/vm-core-interface.zig
pub fn checkInterrupts(vm: *VM) bool {
    _ = vm;
    // TODO: Check for pending interrupts
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

/// Set interrupt flag
/// Per contracts/vm-core-interface.zig
pub fn setInterruptFlag(vm: *VM, interrupt_type: InterruptType) void {
    _ = vm;
    _ = interrupt_type;
    // TODO: Set interrupt flag
}