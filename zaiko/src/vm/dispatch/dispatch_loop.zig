const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const instruction = @import("instruction.zig");
const execution = @import("execution.zig");
const execution_trace = @import("../execution_trace.zig");

const VM = stack.VM;
const LispPTR = @import("../../utils/types.zig").LispPTR;

/// Execute a single instruction in the dispatch loop
/// Returns true if execution should continue, false if it should stop
pub fn executeInstructionInLoop(
    vm: *VM,
    inst: instruction.Instruction,
    tracer: *execution_trace.ExecutionTrace,
) errors.VMError!bool {
    // Log instruction BEFORE execution (matching C emulator format)
    tracer.logInstruction(vm, inst) catch |err| {
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

        std.debug.print("DEBUG PC: Jump from 0x{x} to 0x{x} (offset={})\n", .{ pc_before_update, vm.pc, offset });
    } else {
        vm.pc += inst.length;

        if (pc_before_update < 0x70f000) {
            std.debug.print("DEBUG PC: Advance from 0x{x} to 0x{x} (length={})\n", .{ pc_before_update, vm.pc, inst.length });
        }
    }

    return true;
}

/// Handle unknown/invalid opcodes
/// Returns true if execution should continue, false if it should stop
pub fn handleUnknownOpcode(vm: *VM, opcode_byte: u8, instruction_count: u64) errors.VMError!bool {
    // DEBUG: Print opcode for first few skips
    if (instruction_count <= 3) {
        std.debug.print("DEBUG: Skipping opcode 0x{x:0>2} at PC=0x{x}\n", .{ opcode_byte, vm.pc });
    }

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
    if (instruction_count <= 3 or vm.pc == 0x307898 or vm.pc == 0x60f14f) {
        if (vm.virtual_memory) |vmem| {
            if (vm.pc < vmem.len and vm.pc + 8 <= vmem.len) {
                // CRITICAL: Show BOTH raw bytes and XOR-addressed bytes
                // Raw bytes (what C trace shows, no XOR)
                const raw_bytes_at_pc = vmem[@as(usize, @intCast(vm.pc))..][0..8];
                std.debug.print("DEBUG dispatch: PC=0x{x}, RAW bytes (no XOR): 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", .{ vm.pc, raw_bytes_at_pc[0], raw_bytes_at_pc[1], raw_bytes_at_pc[2], raw_bytes_at_pc[3], raw_bytes_at_pc[4], raw_bytes_at_pc[5], raw_bytes_at_pc[6], raw_bytes_at_pc[7] });

                // XOR-addressed bytes (what decode actually uses)
                const memory_access_module = @import("../../utils/memory_access.zig");
                std.debug.print("DEBUG dispatch: PC=0x{x}, XOR bytes: ", .{vm.pc});
                for (0..8) |i| {
                    const addr = @as(usize, @intCast(vm.pc)) + i;
                    const xor_addr = memory_access_module.applyXORAddressingByte(addr);
                    if (xor_addr < vmem.len) {
                        std.debug.print("0x{x:0>2} ", .{vmem[xor_addr]});
                    } else {
                        std.debug.print("?? ", .{});
                    }
                }
                std.debug.print("\n", .{});

                // CRITICAL DEBUG: For PC 0x307898, dump more context
                if (vm.pc == 0x307898) {
                    std.debug.print("DEBUG CRITICAL: PC=0x307898 memory dump:\n", .{});
                    std.debug.print("  Expected (C emulator reads AFTER XOR): 00 00 60 bf c9 12 0a 02\n", .{});
                    std.debug.print("  Direct memory (no XOR): {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2}\n", .{ raw_bytes_at_pc[0], raw_bytes_at_pc[1], raw_bytes_at_pc[2], raw_bytes_at_pc[3], raw_bytes_at_pc[4], raw_bytes_at_pc[5], raw_bytes_at_pc[6], raw_bytes_at_pc[7] });

                    std.debug.print("  With XOR addressing (what we should read): ", .{});
                    for (0..8) |i| {
                        const addr = @as(usize, @intCast(vm.pc)) + i;
                        const xor_addr = memory_access_module.applyXORAddressingByte(addr);
                        if (xor_addr < vmem.len) {
                            std.debug.print("{x:0>2} ", .{vmem[xor_addr]});
                        } else {
                            std.debug.print("?? ", .{});
                        }
                    }
                    std.debug.print("\n", .{});

                    // Show XOR addresses
                    std.debug.print("  XOR addresses: ", .{});
                    for (0..8) |i| {
                        const addr = @as(usize, @intCast(vm.pc)) + i;
                        const xor_addr = memory_access_module.applyXORAddressingByte(addr);
                        std.debug.print("0x{x} ", .{xor_addr});
                    }
                    std.debug.print("\n", .{});
                }
            } else {
                std.debug.print("DEBUG dispatch: PC=0x{x} is out of bounds (vmem.len=0x{x})\n", .{ vm.pc, vmem.len });
            }
        } else {
            std.debug.print("DEBUG dispatch: PC=0x{x} but virtual_memory is null\n", .{vm.pc});
        }
    }

    const inst_result = instruction.decodeInstructionFromMemory(vm, vm.pc) catch |err| {
        switch (err) {
            error.InvalidAddress, error.MemoryAccessFailed => {
                // Invalid address - could be end of code or invalid PC
                std.debug.print("ERROR: Invalid address at PC=0x{x}\n", .{vm.pc});
                return error.InvalidOpcode;
            },
            else => return err,
        }
    };

    if (inst_result == null) {
        return null; // Invalid instruction
    }

    const inst = inst_result.?;

    // DEBUG: Print opcode for first few instructions
    if (instruction_count <= 3) {
        const opcode_val = @intFromEnum(inst.opcode);
        std.debug.print("DEBUG: Decoded opcode 0x{x:0>2} ({s}) at PC=0x{x}, length={}\n", .{ opcode_val, @tagName(inst.opcode), vm.pc, inst.length });
    }

    return inst;
}
