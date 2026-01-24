const std = @import("std");
const VM = @import("../stack.zig").VM;

/// Debug functions for dispatch loop - only compiled in Debug builds
/// These functions help with systematic debugging and parity checking
/// Check memory at specific addresses before instruction execution
/// Used for targeted debugging of specific instruction sequences
pub fn checkMemoryBeforeInstruction(vm: *const VM, pc: usize) void {
    if (vm.virtual_memory) |vmem| {
        const memory_access_module = @import("../../utils/memory_access.zig");

        switch (pc) {
            0x60f136 => {
                // Check memory at 0x60f14f before executing UNBIND
                const test_addr = 0x60f14f;
                if (test_addr < vmem.len) {
                    const byte = memory_access_module.getByte(vmem, test_addr) catch 0xFF;
                    std.debug.print("DEBUG MEMORY CHECK: Before UNBIND at PC=0x60f136, memory[0x60f14f] = 0x{x:0>2}\n", .{byte});
                }
            },
            0x60f14a => {
                // Check memory at 0x60f14f before executing SIC (JUMPX)
                const test_addr = 0x60f14f;
                if (test_addr < vmem.len) {
                    const byte = memory_access_module.getByte(vmem, test_addr) catch 0xFF;
                    std.debug.print("DEBUG MEMORY CHECK: Before SIC at PC=0x60f14a, memory[0x60f14f] = 0x{x:0>2}\n", .{byte});
                }
            },
            0x60f14c => {
                // Check memory at 0x60f14f before executing EQ
                const test_addr = 0x60f14f;
                if (test_addr < vmem.len) {
                    const byte = memory_access_module.getByte(vmem, test_addr) catch 0xFF;
                    std.debug.print("DEBUG MEMORY CHECK: Before EQ at PC=0x60f14c, memory[0x60f14f] = 0x{x:0>2}\n", .{byte});
                }
            },
            0x60f14d => {
                // Check memory at 0x60f14f before executing FJUMP7
                const test_addr = 0x60f14f;
                if (test_addr < vmem.len) {
                    const byte = memory_access_module.getByte(vmem, test_addr) catch 0xFF;
                    std.debug.print("DEBUG MEMORY CHECK: Before FJUMP7 at PC=0x60f14d, memory[0x60f14f] = 0x{x:0>2}\n", .{byte});
                }
            },
            0x60f14e => {
                // Check memory at 0x60f14f before executing POP
                const test_addr = 0x60f14f;
                if (test_addr < vmem.len) {
                    const byte = memory_access_module.getByte(vmem, test_addr) catch 0xFF;
                    std.debug.print("DEBUG MEMORY CHECK: Before POP at PC=0x60f14e, memory[0x60f14f] = 0x{x:0>2}\n", .{byte});
                }
            },
            else => {
                // No debug check needed for this PC
            },
        }
    }
}

/// Print PC update information for debugging
pub fn printPCUpdate(pc_before: usize, pc_after: usize, offset: ?i64, inst_length: u8) void {
    if (offset) |off| {
        std.debug.print("DEBUG PC: Jump from 0x{x} to 0x{x} (offset={})\n", .{ pc_before, pc_after, off });
    } else {
        if (pc_before < 0x70f000) {
            std.debug.print("DEBUG PC: Advance from 0x{x} to 0x{x} (length={})\n", .{ pc_before, pc_after, inst_length });
        }
    }
}

/// Print instruction decode debug information
/// Shows raw bytes, XOR-addressed bytes, and critical memory context
pub fn printInstructionDecodeDebug(vm: *const VM, pc: usize, instruction_count: u64) void {
    if (instruction_count <= 3 or pc == 0x307898 or pc == 0x60f14f) {
        if (vm.virtual_memory) |vmem| {
            if (pc < vmem.len and pc + 8 <= vmem.len) {
                // CRITICAL: Show BOTH raw bytes and XOR-addressed bytes
                // Raw bytes (what C trace shows, no XOR)
                const raw_bytes_at_pc = vmem[@as(usize, @intCast(pc))..][0..8];
                std.debug.print("DEBUG dispatch: PC=0x{x}, RAW bytes (no XOR): 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", .{ pc, raw_bytes_at_pc[0], raw_bytes_at_pc[1], raw_bytes_at_pc[2], raw_bytes_at_pc[3], raw_bytes_at_pc[4], raw_bytes_at_pc[5], raw_bytes_at_pc[6], raw_bytes_at_pc[7] });

                // XOR-addressed bytes (what decode actually uses)
                const memory_access_module = @import("../../utils/memory_access.zig");
                std.debug.print("DEBUG dispatch: PC=0x{x}, XOR bytes: ", .{pc});
                for (0..8) |i| {
                    const addr = @as(usize, @intCast(pc)) + i;
                    const xor_addr = memory_access_module.applyXORAddressingByte(addr);
                    if (xor_addr < vmem.len) {
                        std.debug.print("0x{x:0>2} ", .{vmem[xor_addr]});
                    } else {
                        std.debug.print("?? ", .{});
                    }
                }
                std.debug.print("\n", .{});

                // CRITICAL DEBUG: For PC 0x307898, dump more context
                if (pc == 0x307898) {
                    std.debug.print("DEBUG CRITICAL: PC=0x307898 memory dump:\n", .{});
                    std.debug.print("  Expected (C emulator reads AFTER XOR): 00 00 60 bf c9 12 0a 02\n", .{});
                    std.debug.print("  Direct memory (no XOR): {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2}\n", .{ raw_bytes_at_pc[0], raw_bytes_at_pc[1], raw_bytes_at_pc[2], raw_bytes_at_pc[3], raw_bytes_at_pc[4], raw_bytes_at_pc[5], raw_bytes_at_pc[6], raw_bytes_at_pc[7] });

                    std.debug.print("  With XOR addressing (what we should read): ", .{});
                    for (0..8) |i| {
                        const addr = @as(usize, @intCast(pc)) + i;
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
                        const addr = @as(usize, @intCast(pc)) + i;
                        const xor_addr = memory_access_module.applyXORAddressingByte(addr);
                        std.debug.print("0x{x} ", .{xor_addr});
                    }
                    std.debug.print("\n", .{});
                }
            } else {
                std.debug.print("DEBUG dispatch: PC=0x{x} is out of bounds (vmem.len=0x{x})\n", .{ pc, vmem.len });
            }
        } else {
            std.debug.print("DEBUG dispatch: PC=0x{x} but virtual_memory is null\n", .{pc});
        }
    }
}

/// Print decoded instruction information for debugging
pub fn printDecodedInstruction(pc: usize, opcode_val: u8, opcode_name: []const u8, inst_length: u8, instruction_count: u64) void {
    if (instruction_count <= 3) {
        std.debug.print("DEBUG: Decoded opcode 0x{x:0>2} ({s}) at PC=0x{x}, length={}\n", .{ opcode_val, opcode_name, pc, inst_length });
    }
}

/// Print unknown opcode information for debugging
pub fn printUnknownOpcodeDebug(opcode_byte: u8, pc: usize, instruction_count: u64) void {
    if (instruction_count <= 3) {
        std.debug.print("DEBUG: Skipping opcode 0x{x:0>2} at PC=0x{x}\n", .{ opcode_byte, pc });
    }
}
