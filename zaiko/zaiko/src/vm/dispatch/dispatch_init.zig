const std = @import("std");
const stack = @import("../stack.zig");
const execution_trace = @import("../execution_trace.zig");

const VM = stack.VM;

/// Initialize dispatch loop - print debug info and setup tracer
pub fn initializeDispatch(vm: *VM) execution_trace.ExecutionTrace {
    // PC should already be initialized from initializeVMState()
    // Don't reset it here - use the value that was set during initialization
    std.debug.print("DEBUG: Dispatch loop starting with PC=0x{x}\n", .{vm.pc});
    
    // DEBUG: Print first few bytes at PC to verify we're reading code
    if (vm.virtual_memory) |vmem| {
        if (vm.pc < vmem.len and vm.pc + 16 <= vmem.len) {
            const code_bytes = vmem[vm.pc..][0..16];
            std.debug.print("DEBUG: First 16 bytes at PC: ", .{});
            for (code_bytes) |b| {
                std.debug.print("0x{x:0>2} ", .{b});
            }
            std.debug.print("\n", .{});
        }
    }

    // Initialize execution tracer (matching C emulator format)
    const tracer = execution_trace.ExecutionTrace.init(vm.allocator, "zig_emulator_execution_log.txt") catch |err| {
        std.debug.print("WARNING: Failed to initialize execution tracer: {}\n", .{err});
        // Continue without tracing - return a dummy tracer
        return execution_trace.ExecutionTrace{
            .log_file = null,
            .instruction_count = 0,
            .allocator = vm.allocator,
        };
    };
    
    return tracer;
}