const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const sysout = @import("../data/sysout.zig");

// Import from split modules
const instruction = @import("dispatch/instruction.zig");
const execution = @import("dispatch/execution.zig");
const execution_trace = @import("execution_trace.zig");
const vm_initialization = @import("vm_initialization.zig");
const dispatch_loop = @import("dispatch/dispatch_loop.zig");
const dispatch_init = @import("dispatch/dispatch_init.zig");

const ByteCode = types.ByteCode;
const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;
const VM = stack.VM;

// Re-export types and functions for backward compatibility
pub const Instruction = instruction.Instruction;
pub const Opcode = instruction.Opcode;
pub const decodeInstructionFromMemory = instruction.decodeInstructionFromMemory;
pub const decodeOpcode = instruction.decodeOpcode;
pub const getInstructionLength = instruction.getInstructionLength;
pub const executeInstruction = execution.executeInstruction;
pub const executeOpcodeWithOperands = execution.executeOpcodeWithOperands;
pub const initializeVMState = vm_initialization.initializeVMState;

/// Main dispatch loop
/// Per contracts/vm-core-interface.zig and execution-model.md
/// Reads bytecode from virtual_memory using PC from current frame
pub fn dispatch(vm: *VM) errors.VMError!void {
    const interrupt_module = @import("interrupt.zig");

    // Initialize dispatch (debug output and tracer)
    // CRITICAL: Create tracer only once - reuse existing tracer if available
    // The tracer should persist across multiple dispatch() calls to avoid overwriting the log
    // C emulator uses static FILE *debug_log that persists across calls
    if (vm.execution_tracer == null) {
        vm.execution_tracer = dispatch_init.initializeDispatch(vm);
    }
    const tracer = &vm.execution_tracer.?;

    // Main dispatch loop - continue until error or explicit stop
    // Add instruction counter to prevent infinite loops
    var instruction_count: u64 = 0;
    const max_steps_opt: ?u64 = blk: {
        const env = std.process.getEnvVarOwned(std.heap.page_allocator, "EMULATOR_MAX_STEPS") catch break :blk null;
        defer std.heap.page_allocator.free(env);
        const trimmed = std.mem.trim(u8, env, " \t\r\n");
        if (trimmed.len == 0) break :blk null;
        const parsed = std.fmt.parseInt(u64, trimmed, 10) catch break :blk null;
        if (parsed == 0) break :blk null;
        break :blk parsed;
    };

    while (true) {
        // CRITICAL: Restore CSTKPTRL from CurrentStackPTR before each opcode
        // C: StackPtrRestore is called at the start of nextopcode: label (before each opcode)
        // This ensures CSTKPTRL is correctly aligned with CurrentStackPTR before execution
        stack.initCSTKPTRLFromCurrentStackPTR(vm);

        // Check instruction limit to prevent infinite loops
        instruction_count += 1;
        if (max_steps_opt) |max_steps| {
            if (instruction_count > max_steps) {
                std.debug.print("INFO: EMULATOR_MAX_STEPS reached ({}) - stopping execution\n", .{max_steps});
                std.debug.print("  Current PC: 0x{x}\n", .{vm.pc});
                vm.stop_requested = true;
                return;
            }
        }

        // Check interrupts before execution
        if (interrupt_module.checkInterrupts(vm)) {
            // TODO: Handle interrupts
        }

        // Decode instruction
        const inst_opt = dispatch_loop.decodeInstruction(vm, instruction_count) catch |err| {
            return err;
        };

        if (inst_opt) |inst| {
            // Execute instruction
            const should_continue = dispatch_loop.executeInstructionInLoop(vm, inst, tracer) catch |err| {
                return err;
            };
            if (!should_continue) {
                return; // Stop execution
            }
        } else {
            // Invalid instruction or unknown opcode
            // CRITICAL: Read opcode byte with XOR addressing (matching decodeInstructionFromMemory)
            const opcode_byte = if (vm.virtual_memory) |vmem| blk: {
                const memory_access_module = @import("../utils/memory_access.zig");
                break :blk memory_access_module.getByte(vmem, @as(usize, @intCast(vm.pc))) catch 0xFF;
            } else 0xFF;

            const should_continue = dispatch_loop.handleUnknownOpcode(vm, opcode_byte, instruction_count) catch |err| {
                return err;
            };
            if (!should_continue) {
                return; // Stop execution
            }
        }

        // Check interrupts after execution
        if (interrupt_module.checkInterrupts(vm)) {
            // TODO: Handle interrupts
        }

        // Debug: Print progress every 10000 instructions
        if (instruction_count % 10000 == 0) {
            std.debug.print("DEBUG: Executed {} instructions, PC=0x{x}\n", .{ instruction_count, vm.pc });
        }
    }
}
