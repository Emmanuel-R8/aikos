const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");

// Import from split modules with unique names to avoid conflicts
const vm_types = @import("../utils/types.zig");
const vm_errors = @import("../utils/errors.zig");
const stack_module = @import("stack.zig");
const instruction_module = @import("dispatch/instruction.zig");
const execution_module = @import("dispatch/execution.zig");
const ufn_module = @import("vm/ufn.zig");

// Use unique type names
const VM = stack_module.VM;
const Instruction = instruction_module.Instruction;
// Rename the Opcode type to avoid conflict
pub const InstructionOpcode = instruction_module.Opcode;
// Update references to use InstructionOpcode where needed

// Rename the Opcode type to avoid conflict
pub const InstructionOpcode = instruction_module.Opcode;

// Clean up duplicate declarations
const Instruction = instruction_module.Instruction;
const Opcode = instruction_module.Opcode;
// const ufn_module = @import("vm/ufn.zig");

// Re-export types and functions for backward compatibility
pub const Instruction = instruction.Instruction;
pub const Opcode = instruction.Opcode;
pub const decodeInstructionFromMemory = instruction.decodeInstructionFromMemory;
pub const decodeOpcode = instruction.decodeOpcode;
pub const getInstructionLength = instruction.getInstructionLength;
pub const executeInstruction = execution.executeInstruction;
pub const executeOpcodeWithOperands = execution.executeOpcodeWithOperands;

/// Main dispatch loop
/// Per contracts/vm-core-interface.zig and execution-model.md
/// Reads bytecode from virtual_memory using PC from current frame
pub fn dispatch(vm: *VM) errors.VMError!void {
    const interrupt_module = @import("interrupt.zig");

    // Initialize PC from current frame if available
    // PC is stored as offset in frame.pcoffset
    // For now, start at 0 - full implementation will restore PC from frame
    vm.pc = 0;

    // Main dispatch loop - continue until error or explicit stop
    while (true) {
        // Check interrupts before execution
        if (interrupt_module.checkInterrupts(vm)) {
            // TODO: Handle interrupts
        }

        // Decode full instruction with operands from virtual memory
        const inst_result = instruction.decodeInstructionFromMemory(vm, vm.pc) catch |err| {
            switch (err) {
                error.InvalidAddress, error.MemoryAccessFailed => {
                    // Invalid address - could be end of code or invalid PC
                    // C: Unknown opcodes trigger UFN lookup (goto op_ufn)
                    // For now, treat as invalid opcode error
                    return error.InvalidOpcode;
                },
                else => return err,
            }
        };

        if (inst_result == null) {
            // Invalid instruction or end of code
            // C: Unknown opcodes trigger UFN lookup (goto op_ufn)
            // For now, treat as invalid opcode error
            return error.InvalidOpcode;
        }

        const inst = inst_result.?;

        // Execute opcode handler with instruction
        const jump_offset = execution.executeInstruction(vm, inst) catch |err| {
            // Handle opcode execution errors matching C emulator behavior
            switch (err) {
                error.InvalidOpcode => {
                    // Unknown opcode - could be UFN (Undefined Function Name)
                    // C: goto op_ufn; - triggers UFN lookup
                    // TODO: Implement UFN lookup (Phase 3)
                    // For now, stop execution with error
                    return err;
                },
                error.StackOverflow => {
                    // C: Triggers do_stackoverflow() which tries to extend stack
                    // TODO: Implement stack extension (Phase 3)
                    // For now, return error
                    return err;
                },
                error.StackUnderflow => {
                    // C: Stack underflow typically indicates programming error
                    // Return error immediately
                    return err;
                },
                else => return err,
            }
        };

        // Update program counter
        if (jump_offset) |offset| {
            // Jump instruction - update PC by offset
            const new_pc = @as(i64, @intCast(vm.pc)) + offset;
            if (new_pc < 0) {
                return error.InvalidOpcode; // Invalid jump target (negative)
            }
            if (vm.virtual_memory) |vmem| {
                if (new_pc >= vmem.len) {
                    return error.InvalidOpcode; // Invalid jump target (beyond memory)
                }
            }
            vm.pc = @as(LispPTR, @intCast(new_pc));
        } else {
            // Normal instruction - advance by instruction length
            vm.pc += inst.length;
        }

        // Check interrupts after execution
        if (interrupt_module.checkInterrupts(vm)) {
            // TODO: Handle interrupts
        }
    }
}

/// Initialize VM state from IFPAGE
/// Per contracts/vm-execution-api.md
/// Sets stack pointers, frame pointer, and program counter from IFPAGE
pub fn initializeVMState(
    vm: *VM,
    ifpage: *const IFPAGE,
    virtual_memory: []const u8,
    fptovp: []const u16,
) errors.VMError!void {
    // Store virtual memory and FPtoVP table in VM
    vm.virtual_memory = virtual_memory;
    vm.fptovp = fptovp;

    // Convert LispPTR addresses to native pointers
    // Note: In the C implementation, these are converted using NativeAligned2FromStackOffset
    // For now, we'll treat them as offsets into virtual_memory

    // Set stack base from IFPAGE
    // stackbase is a LispPTR that points to the base of the stack
    const stackbase_offset = ifpage.stackbase;
    if (stackbase_offset >= virtual_memory.len) {
        return error.InvalidStackPointer;
    }

    // Set end of stack from IFPAGE
    const endofstack_offset = ifpage.endofstack;
    if (endofstack_offset >= virtual_memory.len) {
        return error.InvalidStackPointer;
    }

    // Set current frame pointer from IFPAGE
    const currentfxp_offset = ifpage.currentfxp;
    if (currentfxp_offset >= virtual_memory.len) {
        return error.InvalidFramePointer;
    }

    // Convert offsets to pointers (assuming virtual_memory is the Lisp world)
    // For now, we'll set the stack pointers relative to the virtual memory
    // The actual conversion depends on the memory mapping implementation
    // This is a simplified version - full implementation would use proper address translation

    // Initialize program counter (will be set from function entry point or sysout state)
    // For now, start at 0 - full implementation will set from sysout state
    vm.pc = 0;

    // Note: Full implementation would:
    // 1. Convert LispPTR addresses to native pointers using address translation
    // 2. Set up stack frame from currentfxp
    // 3. Initialize program counter from function entry point
    // 4. Set up interrupt state
}
