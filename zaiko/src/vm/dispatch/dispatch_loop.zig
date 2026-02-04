const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const instruction = @import("instruction.zig");
const execution = @import("execution.zig");
const execution_trace = @import("../execution_trace.zig");
const dispatch_debug = @import("dispatch_debug.zig");

const VM = stack.VM;
const LispPTR = @import("../../utils/types.zig").LispPTR;

/// Execute a single instruction in the dispatch loop
/// Returns true if execution should continue, false if it should stop
/// Trace is logged BEFORE execution (match C: xc.c logs state before dispatching current opcode).
pub fn executeInstructionInLoop(
    vm: *VM,
    inst: instruction.Instruction,
    tracer: *execution_trace.ExecutionTrace,
) errors.VMError!bool {
    // DEBUG: Verify memory integrity before instruction execution
    dispatch_debug.checkMemoryBeforeInstruction(vm, @as(usize, @intCast(vm.pc)));

    // Sync TOPOFSTACK from memory before logging (CSTKPTRL may have been updated by previous opcode e.g. UNBIND).
    stack.readTopOfStackFromMemory(vm);
    // Log trace BEFORE execution so line N = state before instruction N (match C xc.c timing).
    tracer.logInstruction(vm, inst, null) catch |err| {
        std.debug.print("WARNING: Failed to log instruction: {}\n", .{err});
    };

    // Execute opcode handler with instruction
    const jump_offset = execution.executeInstruction(vm, inst) catch |err| {
        // Handle opcode execution errors matching C emulator behavior
        switch (err) {
            error.InvalidOpcode => {
                // Unknown opcode - could be UFN (Undefined Function Name)
                // C: goto op_ufn; - triggers UFN lookup
                // For now, log and continue (allows identifying missing opcodes)
                // CRITICAL: Use XOR addressing to read opcode byte (matching decodeInstructionFromMemory)
                const opcode_byte = if (vm.virtual_memory) |vmem| blk: {
                    const memory_access_module = @import("../../utils/memory_access.zig");
                    break :blk memory_access_module.getByte(vmem, @as(usize, @intCast(vm.pc))) catch 0xFF;
                } else 0xFF;
                std.debug.print("WARNING: Unknown opcode 0x{x:0>2} at PC=0x{x}\n", .{ opcode_byte, vm.pc });

                // Advance PC by 1 byte (opcode) to continue execution
                // This allows us to see what other opcodes are needed
                vm.pc += 1;
                return true; // Continue loop
            },
            error.StackOverflow => {
                // C: Triggers do_stackoverflow() which tries to extend stack
                // TODO: Implement stack extension (Phase 3)
                // For now, return error
                return err;
            },
            error.StackUnderflow => {
                // C: Stack underflow typically indicates programming error
                // Log the opcode that caused it
                const opcode_byte = @intFromEnum(inst.opcode);
                std.debug.print("ERROR: StackUnderflow at PC=0x{x}, opcode=0x{x:0>2} ({s})\n", .{ vm.pc, opcode_byte, @tagName(inst.opcode) });
                return err;
            },
            else => return err,
        }
    };

    // Update program counter
    const pc_before_update = vm.pc;

    if (jump_offset) |offset| {
        const new_pc = @as(i64, @intCast(vm.pc)) + offset;
        if (new_pc < 0) {
            return error.InvalidOpcode;
        }
        if (vm.virtual_memory) |vmem| {
            if (new_pc >= vmem.len) {
                return error.InvalidOpcode;
            }
        }
        vm.pc = @as(LispPTR, @intCast(new_pc));

        if (offset == 0) {
            vm.pc += inst.length;
        }

        dispatch_debug.printPCUpdate(@as(usize, @intCast(pc_before_update)), @as(usize, @intCast(vm.pc)), @as(i32, @intCast(offset)), @as(u8, @intCast(inst.length)));
    } else {
        vm.pc += inst.length;

        dispatch_debug.printPCUpdate(@as(usize, @intCast(pc_before_update)), @as(usize, @intCast(vm.pc)), null, @as(u8, @intCast(inst.length)));
    }

    return true;
}

/// Handle unknown/invalid opcodes
/// Returns true if execution should continue, false if it should stop
pub fn handleUnknownOpcode(vm: *VM, opcode_byte: u8, instruction_count: u64) errors.VMError!bool {
    // DEBUG: Print opcode for first few skips
    dispatch_debug.printUnknownOpcodeDebug(opcode_byte, @as(usize, @intCast(vm.pc)), instruction_count);

    // Handle unused opcode 0x70 (opc_unused_112) gracefully - skip it
    if (opcode_byte == 0x70) {
        // C: opc_unused_112 - skip this byte and continue
        vm.pc += 1;
        return true; // Continue
    }

    // Unknown opcode - handle gracefully
    // Opcode 0x00 is opc_unused_0, which should be skipped
    if (opcode_byte == 0x00) {
        // Skip unused opcode 0x00 (opc_unused_0)
        vm.pc += 1;
        return true; // Continue
    }

    std.debug.print("ERROR: Failed to decode instruction at PC=0x{x}, opcode=0x{x:0>2}\n", .{ vm.pc, opcode_byte });

    // Unknown opcode - skip and continue (don't crash)
    vm.pc += 1;
    return true; // Continue
}

/// Decode instruction from memory
/// Returns instruction if successful, null if invalid/unknown
pub fn decodeInstruction(vm: *VM, instruction_count: u64) errors.VMError!?instruction.Instruction {
    // Decode full instruction with operands from virtual memory
    // DEBUG: Print PC and first few bytes for first few instructions
    // CRITICAL: Also check PC 0x307898 specifically (known mismatch location)
    dispatch_debug.printInstructionDecodeDebug(vm, @as(usize, @intCast(vm.pc)), instruction_count);

    // ERROR HANDLING: Instruction decode failures
    // - InvalidAddress/MemoryAccessFailed: PC points outside valid memory
    //   Rationale: Indicates end of code or corrupted PC register
    //   Implication: Convert to InvalidOpcode to maintain dispatch loop contract
    const inst_result = instruction.decodeInstructionFromMemory(vm, vm.pc) catch |err| {
        switch (err) {
            error.InvalidAddress, error.MemoryAccessFailed => {
                // Invalid address - could be end of code or invalid PC
                // ERROR: PC corruption detected - cannot safely continue
                std.debug.print("ERROR: Invalid address at PC=0x{x}\n", .{vm.pc});
                return error.InvalidOpcode;
            },
            // OTHER ERRORS: Pass through unchanged
            else => return err,
        }
    };

    if (inst_result == null) {
        return null; // Invalid instruction
    }

    const inst = inst_result.?;

    // DEBUG: Print opcode for first few instructions
    if (instruction_count <= 3) {
        const opcode_val: u8 = @intFromEnum(inst.opcode);
        dispatch_debug.printDecodedInstruction(@as(usize, @intCast(vm.pc)), opcode_val, @tagName(inst.opcode), @as(u8, @intCast(inst.length)), instruction_count);
    }

    return inst;
}
