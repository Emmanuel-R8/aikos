const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");

// Import from split modules
const instruction = @import("dispatch/instruction.zig");
const execution = @import("dispatch/execution.zig");

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

/// Main dispatch loop
/// Per contracts/vm-core-interface.zig and execution-model.md
/// Reads bytecode from virtual_memory using PC from current frame
pub fn dispatch(vm: *VM) errors.VMError!void {
    const interrupt_module = @import("interrupt.zig");

    // PC should already be initialized from initializeVMState()
    // Don't reset it here - use the value that was set during initialization
    std.debug.print("DEBUG: Dispatch loop starting with PC=0x{x}\n", .{vm.pc});

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
                    std.debug.print("ERROR: Invalid address at PC=0x{x}\n", .{vm.pc});
                    return error.InvalidOpcode;
                },
                else => return err,
            }
        };

        if (inst_result == null) {
            // Invalid instruction or unknown opcode
            // Log the opcode byte for debugging
            const opcode_byte = if (vm.virtual_memory) |vmem|
                if (vm.pc < vmem.len) vmem[@as(usize, @intCast(vm.pc))] else 0xFF
            else
                0xFF;
            std.debug.print("ERROR: Failed to decode instruction at PC=0x{x}, opcode=0x{x:0>2}\n", .{ vm.pc, opcode_byte });

            // Unknown opcode - log and continue to see what's needed
            vm.pc += 1;
            continue;
        }

        const inst = inst_result.?;

        // Execute opcode handler with instruction
        const jump_offset = execution.executeInstruction(vm, inst) catch |err| {
            // Handle opcode execution errors matching C emulator behavior
            switch (err) {
                error.InvalidOpcode => {
                    // Unknown opcode - could be UFN (Undefined Function Name)
                    // C: goto op_ufn; - triggers UFN lookup
                    // For now, log and continue (allows identifying missing opcodes)
                    const opcode_byte = if (vm.virtual_memory) |vmem|
                        if (vm.pc < vmem.len) vmem[@as(usize, @intCast(vm.pc))] else 0xFF
                    else
                        0xFF;
                    std.debug.print("WARNING: Unknown opcode 0x{x:0>2} at PC=0x{x}\n", .{ opcode_byte, vm.pc });

                    // Advance PC by 1 byte (opcode) to continue execution
                    // This allows us to see what other opcodes are needed
                    vm.pc += 1;
                    continue; // Skip to next iteration
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
/// CRITICAL: Initializes PC from current frame's function entry point
pub fn initializeVMState(
    vm: *VM,
    ifpage: *const IFPAGE,
    virtual_memory: []const u8,
    fptovp: []const u16,
) errors.VMError!void {

    // Store virtual memory and FPtoVP table in VM
    vm.virtual_memory = virtual_memory;
    vm.fptovp = fptovp;

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

    // CRITICAL: Read current frame and initialize PC from function entry point
    // C: start_lisp() -> FastRetCALL -> reads CURRENTFX->fnheader -> gets startpc
    const currentfxp_addr = ifpage.currentfxp;
    if (currentfxp_addr >= virtual_memory.len) {
        return error.InvalidFramePointer;
    }

    // Translate currentfxp to native pointer (2-byte aligned for frame)
    // C: NativeAligned2FromStackOffset(InterfacePage->currentfxp)
    // For now, use simplified direct offset (proper address translation will be added later)
    if (currentfxp_addr >= virtual_memory.len) {
        return error.InvalidFramePointer;
    }

    // Read frame structure fields directly from memory (FX is packed struct)
    // FX structure: nextblock (4 bytes), link (4 bytes), fnheader (4 bytes), pcoffset (2 bytes)
    // NOTE: Sysout files store multi-byte values in BIG-ENDIAN format
    // We need to byte-swap when reading LispPTR values
    const frame_offset = @as(usize, @intCast(currentfxp_addr));
    std.debug.print("DEBUG: Reading frame at offset 0x{x}\n", .{frame_offset});
    if (frame_offset + 14 > virtual_memory.len) {
        std.debug.print("WARNING: Frame offset + 14 exceeds virtual memory\n", .{});
        vm.pc = 0;
        return;
    }

    // Read fnheader field (offset 8 bytes from frame start)
    // LispPTR is 4 bytes, stored BIG-ENDIAN in sysout
    // Byte-swap: read as big-endian and convert to little-endian
    const fnheader_be: LispPTR = (@as(LispPTR, virtual_memory[frame_offset + 8]) << 24) |
        (@as(LispPTR, virtual_memory[frame_offset + 9]) << 16) |
        (@as(LispPTR, virtual_memory[frame_offset + 10]) << 8) |
        (@as(LispPTR, virtual_memory[frame_offset + 11]));
    const fnheader_addr = fnheader_be;

    std.debug.print("DEBUG: Read fnheader_addr=0x{x} (big-endian) from frame\n", .{fnheader_addr});

    if (fnheader_addr == 0) {
        // No function header - can't get entry point
        std.debug.print("WARNING: fnheader_addr is 0, cannot get entry point\n", .{});
        vm.pc = 0;
        return;
    }

    // Read pcoffset from frame (offset 12 bytes from frame start, 2 bytes)
    // pcoffset might contain a saved PC value we can use
    const pcoffset_be: types.DLword = (@as(types.DLword, virtual_memory[frame_offset + 12]) << 8) |
        (@as(types.DLword, virtual_memory[frame_offset + 13]));
    const pcoffset = pcoffset_be;
    std.debug.print("DEBUG: Read pcoffset={} (big-endian) from frame\n", .{pcoffset});

    // CRITICAL: Initialize stack with NIL (TopOfStack = 0) BEFORE setting PC
    // C: start_lisp() -> TopOfStack = 0;
    // This ensures the stack has at least one value (NIL) for conditional jumps
    const stack_module = @import("stack.zig");
    try stack_module.pushStack(vm, 0); // Push NIL
    std.debug.print("DEBUG: Initialized stack with NIL (TopOfStack=0)\n", .{});

    // Translate fnheader_addr (LispPTR) to virtual_memory offset using FPtoVP
    // C: NativeAligned4FromLispPTR(fnheader_addr) -> translates using FPtoVP
    const address_module = @import("../utils/address.zig");
    const fnheader_offset_opt = address_module.translateLispPTRToOffset(fnheader_addr, fptovp, virtual_memory.len);

    if (fnheader_offset_opt) |fnheader_offset| {
        std.debug.print("DEBUG: Translated fnheader_addr=0x{x} to offset=0x{x}\n", .{ fnheader_addr, fnheader_offset });

        // Read function header fields directly from memory
        // FunctionHeader: stkmin (2), na (2), pv (2), startpc (2), framename (4), ntsize (2), nlocals (2), fvaroffset (2)
        // startpc is at offset 6 bytes
        if (fnheader_offset + 8 > virtual_memory.len) {
            std.debug.print("WARNING: Function header offset exceeds virtual memory\n", .{});
            vm.pc = if (pcoffset > 0 and pcoffset < virtual_memory.len) pcoffset else 0x100;
            return;
        }

        // Read startpc field (offset 6 bytes from function header start)
        // DLword is 2 bytes, stored BIG-ENDIAN in sysout
        // Byte-swap: read as big-endian and convert to little-endian
        const startpc_be: types.DLword = (@as(types.DLword, virtual_memory[fnheader_offset + 6]) << 8) |
            (@as(types.DLword, virtual_memory[fnheader_offset + 7]));
        const startpc = startpc_be;

        std.debug.print("DEBUG: Read startpc={} (big-endian) from function header\n", .{startpc});

        // Calculate actual PC: fnheader address + startpc offset
        // C: PC = (ByteCode *)FuncObj + FuncObj->startpc;
        // Since fnheader_addr is a LispPTR, we add startpc to it and translate again
        const code_start_addr = fnheader_addr + startpc;
        const code_start_offset_opt = address_module.translateLispPTRToOffset(code_start_addr, fptovp, virtual_memory.len);

        if (code_start_offset_opt) |code_start_offset| {
            vm.pc = @as(LispPTR, @intCast(code_start_offset));
            std.debug.print("Initialized PC from frame: currentfxp=0x{x}, fnheader=0x{x}, startpc={}, PC=0x{x}\n", .{
                currentfxp_addr,
                fnheader_addr,
                startpc,
                vm.pc,
            });

            // Validate PC is within virtual memory bounds
            if (vm.pc >= virtual_memory.len) {
                std.debug.print("WARNING: PC (0x{x}) is beyond virtual memory (len=0x{x}), trying pcoffset\n", .{ vm.pc, virtual_memory.len });
                vm.pc = if (pcoffset > 0 and pcoffset < virtual_memory.len) pcoffset else 0x100;
            }
            return;
        } else {
            std.debug.print("WARNING: Could not translate code_start_addr=0x{x}, trying pcoffset\n", .{code_start_addr});
            vm.pc = if (pcoffset > 0 and pcoffset < virtual_memory.len) pcoffset else 0x100;
            return;
        }
    } else {
        // Address translation failed - try using pcoffset or default
        std.debug.print("WARNING: Could not translate fnheader_addr=0x{x}, trying pcoffset\n", .{fnheader_addr});
        if (pcoffset > 0 and pcoffset < virtual_memory.len) {
            vm.pc = pcoffset;
            std.debug.print("Using pcoffset={} as PC\n", .{pcoffset});
        } else {
            // Last resort: try to find a reasonable starting point
            // For now, start at a small offset to avoid immediate errors
            vm.pc = 0x100; // Start at offset 256 bytes as a safe default
            std.debug.print("WARNING: Using default PC=0x100\n", .{});
        }
        return;
    }
}