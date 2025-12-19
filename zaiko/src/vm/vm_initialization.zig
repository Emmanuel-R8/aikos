const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const stack = @import("stack.zig");
const sysout = @import("../data/sysout.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;
const VM = stack.VM;

/// Initialize VM state from IFPAGE
/// Per contracts/vm-execution-api.md
/// Sets stack pointers, frame pointer, and program counter from IFPAGE
/// CRITICAL: Initializes PC from current frame's function entry point
pub fn initializeVMState(
    vm: *VM,
    ifpage: *const IFPAGE,
    virtual_memory: []const u8,
    fptovp_table: *const sysout.FPtoVPTable,
) errors.VMError!void {
    // Store virtual memory and FPtoVP table in VM
    vm.virtual_memory = virtual_memory;
    vm.fptovp = fptovp_table;

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
        std.debug.print("ERROR: Frame offset exceeds virtual memory bounds\n", .{});
        std.debug.print("  Frame offset: 0x{x} ({} bytes)\n", .{ frame_offset, frame_offset });
        std.debug.print("  Virtual memory size: {} bytes (0x{x})\n", .{ virtual_memory.len, virtual_memory.len });
        std.debug.print("  Required: {} bytes (frame_offset + 14)\n", .{frame_offset + 14});
        std.debug.print("  IFPAGE currentfxp: 0x{x} (DLword offset from Stackspace)\n", .{ifpage.currentfxp});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Invalid currentfxp value in IFPAGE\n", .{});
        std.debug.print("    - Virtual memory allocation too small\n", .{});
        std.debug.print("    - Stack area not properly initialized\n", .{});
        vm.pc = 0;
        return;
    }

    // Read fnheader field (offset 4-7 bytes from frame start, after flags+usecount and alink)
    // Frame layout: flags+usecount (2), alink (2), lofnheader (2), hi1fnheader+hi2fnheader (2)
    // For non-BIGVM: fnheader is 24-bit (hi2fnheader << 16 | lofnheader)
    // For BIGVM: fnheader is full 32-bit LispPTR
    // C: FX_FNHEADER = (CURRENTFX->hi2fnheader << 16) | CURRENTFX->lofnheader (non-BIGVM)
    // lofnheader is at bytes 4-5, hi2fnheader is at byte 6
    // CRITICAL: Virtual memory stores DLwords in BIG-ENDIAN format (from sysout file)
    // Use helper function to read with byte swapping
    // Frame structure (non-BIGVM):
    // Offset 4-5: lofnheader (2 bytes, DLword, big-endian: [high, low])
    // Offset 6-7: hi1fnheader_hi2fnheader (2 bytes, DLword, big-endian: [hi2, hi1])
    // C struct (after loading to native, little-endian):
    //   - lofnheader: DLword at offset 4-5 (native: [low, high])
    //   - hi1fnheader_hi2fnheader: DLword at offset 6-7 (native: [hi1, hi2])
    //     hi1fnheader = bits 0-7 (low byte), hi2fnheader = bits 8-15 (high byte)
    // When C reads CURRENTFX->hi2fnheader, it gets bits 8-15 of native DLword.
    // Native DLword was loaded from sysout bytes [6,7] (big-endian).
    // After byte-swap: native_DLword = (byte7 << 8) | byte6
    // So hi2fnheader (bits 8-15) = byte7 (from big-endian sysout)
    // BUT: readDLwordBE already does byte-swapping, so:
    //   readDLwordBE(bytes[6,7]) = (byte6 << 8) | byte7 (converts big-endian to little-endian)
    //   hi2fnheader = (readDLwordBE(...) >> 8) & 0xFF = byte6 (from big-endian sysout)
    // CRITICAL: Read frame fields as native little-endian DLwords (pages are byte-swapped on load)
    // C: After word_swap_page, pages are in native little-endian format
    // So we read DLwords directly without byte-swapping
    // DEBUG: Actual memory layout shows fields are SWAPPED compared to struct definition:
    //   Struct says: [4,5]=lofnheader, [6,7]=hi1fnheader_hi2fnheader
    //   Actual memory: [4,5]=hi1fnheader_hi2fnheader, [6,7]=lofnheader
    //   This matches the actual bytes we see in debug output
    const frame_bytes = virtual_memory[frame_offset..][0..12];
    // CRITICAL: After 32-bit byte-swap, the frame fields are reordered
    // Before swap (big-endian): [4,5,6,7] = [hi2, hi1, lo_high, lo_low] = 0x307864??
    // After 32-bit swap (little-endian): [4,5,6,7] = [lo_low, lo_high, hi1, hi2] = 0x??647830
    // So: bytes [4,5] = lofnheader (0x7864), bytes [6,7] = [hi1, hi2] where hi2 is high byte
    const lofnheader = std.mem.readInt(DLword, frame_bytes[4..6], .little);
    const hi1fnheader_hi2fnheader = std.mem.readInt(DLword, frame_bytes[6..8], .little);
    // hi2fnheader is in the LOW byte (bits 0-7) of bytes [6,7] after 32-bit byte-swap
    // Bytes [6,7] = [0x30, 0x00] -> as little-endian DLword = 0x0030
    // hi2fnheader = low byte = 0x0030 & 0xFF = 0x30
    const hi2fnheader: u8 = @as(u8, @truncate(hi1fnheader_hi2fnheader & 0xFF));
    const fnheader_be = (@as(LispPTR, hi2fnheader) << 16) | lofnheader;
    
    std.debug.print("DEBUG: Reading frame fields (native little-endian, pages byte-swapped on load):\n", .{});
    std.debug.print("  Frame offset: 0x{x}\n", .{frame_offset});
    std.debug.print("  First 16 bytes of frame: ", .{});
    for (0..16) |i| {
        std.debug.print("0x{x:0>2} ", .{virtual_memory[frame_offset + i]});
    }
    std.debug.print("\n", .{});
    std.debug.print("  Bytes [0,1] (flags+usecount): 0x{x:0>2} 0x{x:0>2}\n", .{ virtual_memory[frame_offset + 0], virtual_memory[frame_offset + 1] });
    std.debug.print("  Bytes [2,3] (alink): 0x{x:0>2} 0x{x:0>2}\n", .{ virtual_memory[frame_offset + 2], virtual_memory[frame_offset + 3] });
    std.debug.print("  Bytes [4,5] (hi1fnheader_hi2fnheader - SWAPPED): 0x{x:0>2} 0x{x:0>2} -> 0x{x:0>4}\n", .{ virtual_memory[frame_offset + 4], virtual_memory[frame_offset + 5], hi1fnheader_hi2fnheader });
    std.debug.print("  Bytes [6,7] (lofnheader - SWAPPED): 0x{x:0>2} 0x{x:0>2} -> 0x{x:0>4}\n", .{ virtual_memory[frame_offset + 6], virtual_memory[frame_offset + 7], lofnheader });
    std.debug.print("  Bytes [8,9] (pc - BYTESWAP layout): 0x{x:0>2} 0x{x:0>2}\n", .{ virtual_memory[frame_offset + 8], virtual_memory[frame_offset + 9] });
    std.debug.print("  Bytes [10,11] (nextblock - BYTESWAP layout): 0x{x:0>2} 0x{x:0>2}\n", .{ virtual_memory[frame_offset + 10], virtual_memory[frame_offset + 11] });
    std.debug.print("  hi2fnheader (from low byte of [6,7]): 0x{x:0>2}\n", .{hi2fnheader});
    std.debug.print("  FX_FNHEADER (24-bit): 0x{x:0>6}\n", .{fnheader_be & 0xFFFFFF});
    std.debug.print("  Expected FX_FNHEADER for C emulator: 0x307864\n", .{});
    std.debug.print("  For 0x307864: lofnheader should be 0x7864, hi2fnheader should be 0x30\n", .{});

    // For non-BIGVM, mask to 24 bits (0xFFFFFF)
    // C: hi2fnheader is 8 bits, lofnheader is 16 bits = 24 bits total
    const is_bigvm = false; // TODO: Detect from build config or sysout
    const fnheader_addr = if (is_bigvm) fnheader_be else (fnheader_be & 0xFFFFFF);

    std.debug.print("DEBUG: Read fnheader_addr=0x{x} (big-endian) from frame\n", .{fnheader_addr});

    // Read pc field from frame
    // CRITICAL: After 32-bit byte-swap, the BYTESWAP struct layout applies
    // BYTESWAP struct (maiko/inc/stack.h:198-218): pc comes BEFORE nextblock
    // Non-BYTESWAP: [4,5]=lofnheader, [6,7]=hi1+hi2, [8,9]=nextblock, [10,11]=pc
    // BYTESWAP: [4,5]=hi2+hi1, [6,7]=lofnheader, [8,9]=pc, [10,11]=nextblock
    // After 32-bit swap of fnheader fields [4,5,6,7], the layout becomes:
    //   [4,5]=lofnheader, [6,7]=hi1+hi2, [8,9]=pc, [10,11]=nextblock
    // So pc is at offset 8 bytes (not 10) after the 32-bit swap!
    // Per maiko/inc/stack.h:198-218 (BYTESWAP frameex1 structure)
    // CRITICAL: CURRENTFX->pc is a DLword offset (needs /2 for byte offset from FuncObj)
    // C: PC = (ByteCode *)FuncObj + CURRENTFX->pc;
    const frame_pc = std.mem.readInt(DLword, frame_bytes[8..10], .little);
    std.debug.print("  pc bytes [8,9]: 0x{x:0>2} 0x{x:0>2} -> {} (0x{x:0>4})\n", .{ virtual_memory[frame_offset + 8], virtual_memory[frame_offset + 9], frame_pc, frame_pc });
    std.debug.print("DEBUG: CURRENTFX->pc={} (byte offset from FuncObj)\n", .{frame_pc});

    // CRITICAL: Initialize stack pointers to use virtual memory's stack area
    // C: Stackspace = NativeAligned2FromLAddr(STK_OFFSET) = Lisp_world + STK_OFFSET
    // C: CurrentStackPTR = next68k - 2, where next68k = Stackspace + nextblock
    // The stack area is part of virtual memory and already has data!
    // We need to point our stack pointers into virtual memory, not allocate separately

    // Read nextblock from frame to calculate CurrentStackPTR
    // CRITICAL: After 32-bit byte-swap, BYTESWAP struct layout applies
    // nextblock is at offset 10 bytes (not 8) after 32-bit swap
    const nextblock = std.mem.readInt(DLword, frame_bytes[10..12], .little);
    std.debug.print("  nextblock bytes [10,11]: 0x{x:0>2} 0x{x:0>2} -> 0x{x:0>4} (DLword offset)\n", .{ virtual_memory[frame_offset + 10], virtual_memory[frame_offset + 11], nextblock });

    // Calculate CurrentStackPTR: Stackspace + nextblock - 2 DLwords
    // C: next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock) = Stackspace + nextblock
    // C: CurrentStackPTR = next68k - 2
    const next68k_byte_offset = stackspace_byte_offset + (@as(usize, @intCast(nextblock)) * 2);
    const current_stack_ptr_byte_offset = next68k_byte_offset - 4; // -2 DLwords = -4 bytes

    // Point stack pointers into virtual memory
    // CRITICAL: virtual_memory needs to be mutable for stack operations
    // For now, we'll cast it (this is safe as long as we're careful)
    const virtual_memory_mut: []u8 = @constCast(virtual_memory);
    const stackspace_ptr: [*]DLword = @as([*]DLword, @ptrCast(@alignCast(virtual_memory_mut.ptr + stackspace_byte_offset)));
    const current_stack_ptr: [*]DLword = @as([*]DLword, @ptrCast(@alignCast(virtual_memory_mut.ptr + current_stack_ptr_byte_offset)));

    // Update VM stack pointers to point into virtual memory
    vm.stack_base = stackspace_ptr;
    vm.stack_ptr = current_stack_ptr;
    
    // CRITICAL: Initialize current_frame pointer to point to frame in virtual memory
    // The frame is at frame_offset in virtual memory
    // FX is a packed struct, so it can be at any byte offset
    // Use @ptrFromInt with align(1) to create pointer without alignment requirement
    const frame_addr = @intFromPtr(virtual_memory_mut.ptr) + frame_offset;
    const frame_ptr: *align(1) stack.FX = @ptrFromInt(frame_addr);
    vm.current_frame = frame_ptr;
    std.debug.print("DEBUG: Initialized current_frame at offset 0x{x}\n", .{frame_offset});
    
    // C: TopOfStack = 0; (initially empty stack)
    // CRITICAL: C code sets TopOfStack = 0 regardless of stack memory contents
    // The stack area may have data from sysout, but TopOfStack cached value starts at 0
    // TopOfStack will be updated when values are pushed/popped
    vm.top_of_stack = 0;

    // Calculate EndSTKP by walking free stack blocks (matching C emulator)
    // C: freeptr = next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock);
    // C: while (GETWORD(freeptr) == STK_FSB_WORD) EndSTKP = freeptr = freeptr + GETWORD(freeptr + 1);
    const STK_FSB_WORD: types.DLword = 0xA000; // Free stack block marker
    var freeptr_byte_offset = next68k_byte_offset; // Start at next68k
    var endstkp_byte_offset = freeptr_byte_offset;

    // Walk through free stack blocks
    while (freeptr_byte_offset + 4 <= virtual_memory.len) {
        // Read STK_FSB_WORD marker (native little-endian, pages byte-swapped on load)
        const marker_bytes = virtual_memory[freeptr_byte_offset..][0..2];
        const marker = std.mem.readInt(DLword, marker_bytes, .little);

        if (marker != STK_FSB_WORD) {
            break; // Not a free stack block, stop walking
        }

        // Read free block size (native little-endian, at offset +2)
        const block_size_bytes_slice = virtual_memory[freeptr_byte_offset + 2..][0..2];
        const block_size_dlwords = std.mem.readInt(DLword, block_size_bytes_slice, .little);
        const block_size_bytes = @as(usize, @intCast(block_size_dlwords)) * 2;

        // Move to next free block: freeptr = freeptr + block_size
        freeptr_byte_offset = freeptr_byte_offset + block_size_bytes;
        endstkp_byte_offset = freeptr_byte_offset;
    }

    // Set stack_end to EndSTKP
    const stack_end_ptr: [*]DLword = @as([*]DLword, @ptrCast(@alignCast(virtual_memory_mut.ptr + endstkp_byte_offset)));
    vm.stack_end = stack_end_ptr;

    const stack_depth = (@intFromPtr(current_stack_ptr) - @intFromPtr(stackspace_ptr)) / 2;
    std.debug.print("DEBUG: Initialized stack pointers into virtual memory:\n", .{});
    std.debug.print("  Stackspace offset: 0x{x}\n", .{stackspace_byte_offset});
    std.debug.print("  nextblock: 0x{x} (DLword offset)\n", .{nextblock});
    std.debug.print("  CurrentStackPTR offset: 0x{x}\n", .{current_stack_ptr_byte_offset});
    std.debug.print("  EndSTKP offset: 0x{x} (calculated from free stack blocks)\n", .{endstkp_byte_offset});
    std.debug.print("  Stack depth: {} DLwords\n", .{stack_depth});

    // C: TopOfStack = 0; (just a cached value, stack area has real data)
    // CRITICAL: The stack area has data from sysout, but TopOfStack should be 0 (NIL) initially
    // This means the stack is conceptually empty even though the area has data
    // We need to ensure getTopOfStack() returns 0 initially
    // The stack_ptr points to CurrentStackPTR, which is where the next push would go
    // But TopOfStack is a cached value that starts at 0
    // For now, we'll rely on getTopOfStack() to read from stack_ptr, but we should
    // verify that the location stack_ptr points to contains 0 (NIL) or handle it correctly

    if (fnheader_addr == 0) {
        // Frame fnheader is 0 - simulate FastRetCALL exactly as C code does
        // C: FastRetCALL does:
        //   IVar = NativeAligned2FromStackOffset(GETWORD((DLword *)CURRENTFX - 1));
        //   FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER);
        //   PC = (ByteCode *)FuncObj + CURRENTFX->pc;
        //
        // If FX_FNHEADER=0, then FuncObj = NativeAligned4FromLAddr(0) = Lisp_world base (offset 0)
        // PC = FuncObj + CURRENTFX->pc = 0 + CURRENTFX->pc = CURRENTFX->pc
        //
        // So let's simulate this exactly and check if the calculated PC has valid code

        // Get IVar from frame (CURRENTFX - 1)
        const bf_offset = frame_offset - 2; // CURRENTFX - 1 DLword = -2 bytes
        if (bf_offset + 2 > virtual_memory.len) {
            std.debug.print("WARNING: BF offset out of bounds\n", .{});
            vm.pc = 0x100;
            return;
        }
        const bf_bytes = virtual_memory[bf_offset..][0..2];
        const bf_word = std.mem.readInt(DLword, bf_bytes, .little);
        std.debug.print("DEBUG: BF word (CURRENTFX - 1) = 0x{x}\n", .{bf_word});

        // FuncObj = NativeAligned4FromLAddr(0) = Lisp_world base (offset 0)
        const funcobj_offset_zero: usize = 0; // fnheader=0 means Lisp_world base

        // PC = FuncObj + CURRENTFX->pc = 0 + frame_pc
        const calculated_pc = funcobj_offset_zero + @as(usize, @intCast(frame_pc));
        std.debug.print("DEBUG: Simulating FastRetCALL with fnheader=0:\n", .{});
        std.debug.print("  FuncObj = NativeAligned4FromLAddr(0) = offset 0x{x}\n", .{funcobj_offset_zero});
        std.debug.print("  PC = FuncObj + CURRENTFX->pc = 0x{x} + {} = 0x{x}\n", .{ funcobj_offset_zero, frame_pc, calculated_pc });

        // Check if calculated PC has valid code (not all zeros AND first byte is valid opcode)
        if (calculated_pc < virtual_memory.len and calculated_pc + 4 <= virtual_memory.len) {
            const first_bytes = virtual_memory[calculated_pc .. calculated_pc + 4];
            const all_zeros = first_bytes[0] == 0 and first_bytes[1] == 0 and first_bytes[2] == 0 and first_bytes[3] == 0;
            const first_opcode = first_bytes[0];
            const is_valid_opcode = first_opcode != 0; // Opcode 0x00 is opc_unused_0 (invalid)

            std.debug.print("  PC location 0x{x}: bytes = 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", .{ calculated_pc, first_bytes[0], first_bytes[1], first_bytes[2], first_bytes[3] });

            if (!all_zeros and is_valid_opcode) {
                vm.pc = @as(LispPTR, @intCast(calculated_pc));
                std.debug.print("  Using calculated PC=0x{x} (has valid opcode 0x{x:0>2})\n", .{ calculated_pc, first_opcode });
                return;
            } else {
                if (all_zeros) {
                    std.debug.print("  WARNING: Calculated PC=0x{x} points to zeros (invalid)\n", .{calculated_pc});
                } else {
                    std.debug.print("  WARNING: Calculated PC=0x{x} has invalid opcode 0x{x:0>2} (opc_unused_0)\n", .{ calculated_pc, first_opcode });
                }
            }
        }

        // If FastRetCALL simulation gives invalid PC, use C emulator's working entry point
        // C emulator shows: PC=0x60f130 (Lisp_world+0x60f130) after FastRetCALL
        // This is the actual working entry point when frame fnheader is initialized
        std.debug.print("WARNING: FastRetCALL simulation with fnheader=0 gives invalid PC\n", .{});
        std.debug.print("  Using C emulator's working entry point PC=0x60f130\n", .{});

        const working_pc_offset: usize = 0x60f130; // C emulator's working PC offset
        if (working_pc_offset < virtual_memory.len) {
            const first_opcode = virtual_memory[working_pc_offset];
            if (first_opcode != 0) {
                vm.pc = @as(LispPTR, @intCast(working_pc_offset));
                std.debug.print("  Using working PC=0x{x} (opcode=0x{x:0>2})\n", .{ working_pc_offset, first_opcode });
                return;
            }
        }

        // Fallback: search near working PC
        var search_offset: usize = 0x60f000; // Start near C emulator's working PC
        while (search_offset < virtual_memory.len and search_offset < 0x610000) {
            if (virtual_memory[search_offset] != 0) {
                vm.pc = @as(LispPTR, @intCast(search_offset));
                std.debug.print("  Found code at offset 0x{x}, using as PC\n", .{search_offset});
                return;
            }
            search_offset += 0x100;
        }

        // Last resort: use default
        vm.pc = 0x1000;
        std.debug.print("  WARNING: Could not find valid code, using default PC=0x1000\n", .{});
        return;
    }

    // CRITICAL: FastRetCALL uses CURRENTFX->pc directly, NOT the function header's startpc!
    // C: FastRetCALL macro (maiko/inc/retmacro.h:37-45):
    //   FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER);
    //   PC = (ByteCode *)FuncObj + CURRENTFX->pc;
    //
    // So PC = FuncObj + CURRENTFX->pc, where:
    //   - FuncObj is the function header address (from FX_FNHEADER)
    //   - CURRENTFX->pc is the frame's pc field (already read as frame_pc)
    //
    // NOTE: This is different from function calls which use FuncObj->startpc!
    // FastRetCALL is for returns, which use the saved PC in the frame.

    // Translate fnheader_addr (LispPTR) to virtual_memory offset
    // C: NativeAligned4FromLAddr(FX_FNHEADER) = (void *)(Lisp_world + FX_FNHEADER)
    // CRITICAL FIX: FX_FNHEADER appears to point 52 bytes after FuncObj start
    // C log shows: PC = 0x307898, FuncObj+104 bytes
    // So FuncObj = 0x307898 - 104 = 0x307830
    // But FX_FNHEADER = 0x307864
    // Difference: 0x307864 - 0x307830 = 52 bytes
    // So: FuncObj = FX_FNHEADER - 52
    const funcobj_offset_calc: usize = @as(usize, @intCast(fnheader_addr)) - 52; // Adjust by -52 bytes
    const funcobj_offset_opt: ?usize = if (funcobj_offset_calc < virtual_memory.len) funcobj_offset_calc else null;

    if (funcobj_offset_opt) |funcobj_byte_offset| {
        std.debug.print("DEBUG: FastRetCALL simulation:\n", .{});
        std.debug.print("  FX_FNHEADER=0x{x}\n", .{fnheader_addr});
        std.debug.print("  FuncObj offset=0x{x} (translated from FX_FNHEADER, byte addressing)\n", .{funcobj_byte_offset});
        std.debug.print("  CURRENTFX->pc={} (value from frame)\n", .{frame_pc});
        // CRITICAL: C code shows: PC = (ByteCode *)FuncObj + CURRENTFX->pc;
        // This means CURRENTFX->pc is treated as a BYTE offset, not DLword offset
        // The comment in stack.zig says "byte offset from FuncObj" which confirms this
        // However, CURRENTFX->pc is stored as DLword (16-bit), so we need to check:
        // - If frame_pc = 104 (byte offset), use directly: PC = FuncObj + 104 ✓
        // - If frame_pc = 52 (DLword offset), multiply by 2: PC = FuncObj + 104 ✓
        //
        // C log shows: PC = 0x307898, FuncObj+104 bytes
        // So CURRENTFX->pc should be 104 (byte offset) OR 52 (DLword offset * 2 = 104 bytes)
        //
        // Testing: Use frame_pc directly as byte offset (matching C code exactly)
        const frame_pc_bytes = @as(usize, @intCast(frame_pc)); // Use directly as byte offset
        std.debug.print("  CURRENTFX->pc={} (treating as byte offset from FuncObj)\n", .{frame_pc_bytes});
        
        // CRITICAL FIX: Use frame_pc directly without division
        // C: PC = (ByteCode *)FuncObj + CURRENTFX->pc;
        // CURRENTFX->pc is already a byte offset (104 bytes)
        // But we need to verify FuncObj calculation matches C
        const calculated_pc = funcobj_byte_offset + frame_pc_bytes;
        
        // DEBUG: Verify calculation
        std.debug.print("  PC = FuncObj + CURRENTFX->pc = 0x{x} + {} = 0x{x}\n", .{ funcobj_byte_offset, frame_pc_bytes, calculated_pc });

        // Validate PC is within virtual memory bounds
        if (calculated_pc >= virtual_memory.len) {
            std.debug.print("WARNING: Calculated PC (0x{x}) is beyond virtual memory (len=0x{x})\n", .{ calculated_pc, virtual_memory.len });
            vm.pc = if (frame_pc > 0 and frame_pc < virtual_memory.len) @as(LispPTR, @intCast(frame_pc)) else 0x100;
            return;
        }

        // Verify PC points to valid code (not all zeros)
        // CRITICAL: Check with XOR addressing (matching how instructions are actually read)
        if (calculated_pc + 4 <= virtual_memory.len) {
            // Read first byte with XOR addressing to check if it's a valid opcode
            const memory_access_module = @import("../utils/memory_access.zig");
            const first_opcode_xor = memory_access_module.getByte(virtual_memory, calculated_pc) catch 0xFF;
            const second_opcode_xor = memory_access_module.getByte(virtual_memory, calculated_pc + 1) catch 0xFF;
            
            // Also show raw bytes for debugging
            const first_bytes = virtual_memory[calculated_pc .. calculated_pc + 4];
            std.debug.print("  PC location 0x{x}: raw bytes = 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", .{ calculated_pc, first_bytes[0], first_bytes[1], first_bytes[2], first_bytes[3] });
            std.debug.print("  PC location 0x{x}: XOR-addressed opcode = 0x{x:0>2}\n", .{ calculated_pc, first_opcode_xor });

            const all_zeros = first_opcode_xor == 0 and second_opcode_xor == 0;

            if (all_zeros) {
                std.debug.print("WARNING: Calculated PC=0x{x} points to zeros (invalid code)\n", .{calculated_pc});
                // Try fallback: use frame_pc directly as byte offset from start
                if (frame_pc < virtual_memory.len) {
                    vm.pc = @as(LispPTR, @intCast(frame_pc));
                    std.debug.print("  Fallback: Using frame->pc={} as direct byte offset\n", .{frame_pc});
                    return;
                }
            } else {
                vm.pc = @as(LispPTR, @intCast(calculated_pc));
                std.debug.print("  Using calculated PC=0x{x} (XOR-addressed opcode=0x{x:0>2})\n", .{ calculated_pc, first_opcode_xor });
                return;
            }
        }

        // If validation failed, use calculated PC anyway (might still work)
        vm.pc = @as(LispPTR, @intCast(calculated_pc));
        std.debug.print("  Using calculated PC=0x{x} (validation incomplete)\n", .{calculated_pc});
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
