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

    // CRITICAL: Read current frame and initialize PC from function entry point
    // C: start_lisp() -> FastRetCALL -> reads CURRENTFX->fnheader -> gets startpc
    // C: NativeAligned2FromStackOffset(InterfacePage->currentfxp)
    // currentfxp is a DLword StackOffset (not LispPTR!)
    // C: NativeAligned2FromStackOffset(DLword StackOffset) = Stackspace + StackOffset
    // Stackspace is DLword*, so Stackspace + StackOffset adds StackOffset DLwords = StackOffset * 2 bytes
    // Per maiko/inc/adr68k.h:72-75
    //
    // Stackspace initialization: Stackspace = NativeAligned2FromLAddr(STK_OFFSET)
    // STK_OFFSET = 0x00010000 (DLword offset from Lisp_world)
    // So Stackspace byte offset = STK_OFFSET * 2 = 0x20000 bytes
    // Per maiko/src/initsout.c:222 and maiko/alternatives/zig/src/memory/layout.zig
    const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
    const stackspace_byte_offset = STK_OFFSET * 2; // Convert DLword offset to byte offset

    const currentfxp_stack_offset = ifpage.currentfxp; // DLword offset from Stackspace

    // Calculate frame byte offset: Stackspace base + currentfxp offset
    // Frame offset = stackspace_byte_offset + (currentfxp_stack_offset * 2)
    const frame_offset = stackspace_byte_offset + (@as(usize, @intCast(currentfxp_stack_offset)) * 2);
    std.debug.print("DEBUG: Reading frame at offset 0x{x}\n", .{frame_offset});
    if (frame_offset + 14 > virtual_memory.len) {
        std.debug.print("WARNING: Frame offset + 14 exceeds virtual memory\n", .{});
        vm.pc = 0;
        return;
    }

    // Read fnheader field (offset 4-7 bytes from frame start, after flags+usecount and alink)
    // Frame layout: flags+usecount (2), alink (2), lofnheader (2), hi1fnheader+hi2fnheader (2)
    // For non-BIGVM: fnheader is 24-bit (hi2fnheader << 16 | lofnheader)
    // For BIGVM: fnheader is full 32-bit LispPTR
    // C: FX_FNHEADER = (CURRENTFX->hi2fnheader << 16) | CURRENTFX->lofnheader (non-BIGVM)
    // lofnheader is at bytes 4-5, hi2fnheader is at byte 6
    const lofnheader_be: types.DLword = (@as(types.DLword, virtual_memory[frame_offset + 4]) << 8) |
        (@as(types.DLword, virtual_memory[frame_offset + 5]));
    const hi2fnheader: u8 = virtual_memory[frame_offset + 6];
    const fnheader_be = (@as(LispPTR, hi2fnheader) << 16) | lofnheader_be;

    // For non-BIGVM, mask to 24 bits (0xFFFFFF)
    // C: hi2fnheader is 8 bits, lofnheader is 16 bits = 24 bits total
    const is_bigvm = false; // TODO: Detect from build config or sysout
    const fnheader_addr = if (is_bigvm) fnheader_be else (fnheader_be & 0xFFFFFF);

    std.debug.print("DEBUG: Read fnheader_addr=0x{x} (big-endian) from frame\n", .{fnheader_addr});

    // Read pc field from frame (offset 10 bytes from frame start, 2 bytes)
    // C: CURRENTFX->pc is a DLword field in the frame structure
    // Frame layout: flags+usecount (2), alink (2), fnheader (4), nextblock (2), pc (2)
    // So pc is at offset 10 bytes from frame start
    // Per maiko/inc/stack.h:81-100 (frameex1 structure)
    const pc_be: types.DLword = (@as(types.DLword, virtual_memory[frame_offset + 10]) << 8) |
        (@as(types.DLword, virtual_memory[frame_offset + 11]));
    const frame_pc = pc_be;
    std.debug.print("DEBUG: Read frame->pc={} (big-endian) from frame\n", .{frame_pc});

    if (fnheader_addr == 0) {
        // Frame fnheader is 0 - frame may be uninitialized
        // C: FastRetCALL uses: PC = (ByteCode *)FuncObj + CURRENTFX->pc
        // But if fnheader is 0, we can't get FuncObj, so we can't use that formula
        //
        // However, CURRENTFX->pc might still be valid even if fnheader is 0
        // For now, if frame->pc is non-zero, it might be a direct offset we can use
        // This is a workaround until we understand the correct initialization
        if (frame_pc > 0 and frame_pc < virtual_memory.len) {
            vm.pc = @as(LispPTR, @intCast(frame_pc));
            std.debug.print("WARNING: fnheader=0, using frame->pc={} (0x{x}) as PC\n", .{ frame_pc, frame_pc });
            return;
        } else {
            std.debug.print("WARNING: Frame at offset 0x{x} has fnheader=0 and frame->pc={}\n", .{ frame_offset, frame_pc });
            std.debug.print("  Frame appears uninitialized - using default PC=0x100\n", .{});
            vm.pc = 0x100;
            return;
        }
    }

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
            vm.pc = if (frame_pc > 0 and frame_pc < virtual_memory.len) @as(LispPTR, @intCast(frame_pc)) else 0x100;
            return;
        }

        // Read startpc field (offset 6 bytes from function header start)
        // DLword is 2 bytes, stored BIG-ENDIAN in sysout
        // Byte-swap: read as big-endian and convert to little-endian
        const startpc_be: types.DLword = (@as(types.DLword, virtual_memory[fnheader_offset + 6]) << 8) |
            (@as(types.DLword, virtual_memory[fnheader_offset + 7]));
        const startpc = startpc_be;

        std.debug.print("DEBUG: Read startpc={} (big-endian) from function header\n", .{startpc});

        // Calculate actual PC: fnheader offset + startpc (byte offset)
        // C: PC = (ByteCode *)FuncObj + FuncObj->startpc;
        // CRITICAL: startpc is a BYTE offset from FuncObj, not a DLword offset!
        // The comment in maiko/inc/stack.h saying "DLword offset from stkmin" is INCORRECT.
        // The actual C code uses it as a byte offset: (ByteCode *)FuncObj + FuncObj->startpc
        // Per maiko/src/intcall.c:106, maiko/src/bbtsub.c:1730, maiko/src/loopsops.c:428
        const code_start_offset = fnheader_offset + @as(usize, @intCast(startpc));

        // Validate PC is within virtual memory bounds
        if (code_start_offset >= virtual_memory.len) {
            std.debug.print("WARNING: PC offset (0x{x}) is beyond virtual memory (len=0x{x}), trying frame->pc\n", .{ code_start_offset, virtual_memory.len });
            vm.pc = if (frame_pc > 0 and frame_pc < virtual_memory.len) @as(LispPTR, @intCast(frame_pc)) else 0x100;
            return;
        }

        vm.pc = @as(LispPTR, @intCast(code_start_offset));
        std.debug.print("Initialized PC from frame: currentfxp_stack_offset={}, fnheader=0x{x}, startpc={}, PC=0x{x}\n", .{
            currentfxp_stack_offset,
            fnheader_addr,
            startpc,
            vm.pc,
        });
        return;
    } else {
        // Address translation failed - try using frame->pc or default
        std.debug.print("WARNING: Could not translate fnheader_addr=0x{x}, trying frame->pc\n", .{fnheader_addr});
        if (frame_pc > 0 and frame_pc < virtual_memory.len) {
            vm.pc = frame_pc;
            std.debug.print("Using frame->pc={} as PC\n", .{frame_pc});
        } else {
            // Last resort: try to find a reasonable starting point
            // For now, start at a small offset to avoid immediate errors
            vm.pc = 0x100; // Start at offset 256 bytes as a safe default
            std.debug.print("WARNING: Using default PC=0x100\n", .{});
        }
        return;
    }
}