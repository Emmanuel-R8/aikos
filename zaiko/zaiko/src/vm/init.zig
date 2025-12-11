const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const IFPAGE = types.IFPAGE;

/// Memory layout constants (from maiko/inc/lispmap.h)
/// For non-BIGVM configuration
pub const STK_OFFSET: u32 = 0x10000; // DLword offset from Lisp_world
pub const IFPAGE_OFFSET: u32 = 0x60000; // Byte offset from Lisp_world
pub const IOPAGE_OFFSET: u32 = 0x0FF00; // Byte offset from Lisp_world
pub const FPTOVP_OFFSET: u32 = 0x40000; // Byte offset from Lisp_world (non-BIGVM)
pub const MISCSTATS_OFFSET: u32 = 0x60A00; // Byte offset from Lisp_world

/// Stack constants (from maiko/inc/stack.h)
pub const STK_FSB_WORD: u16 = 0xA000; // Free Stack Block marker
pub const FRAMESIZE: u16 = 10; // Size of frame structure in DLwords

/// Initialize system state equivalent to build_lisp_map()
/// Sets up pointers into virtual_memory for various system structures
/// C: maiko/src/initsout.c:219 - build_lisp_map()
pub fn buildLispMap(virtual_memory: []u8) void {
    _ = virtual_memory; // In Zig, we use offsets directly, not global pointers
    // The equivalent of setting global pointers is handled by passing virtual_memory
    // to functions that need it. We don't need global state in Zig.
}

/// Initialize storage equivalent to init_storage()
/// C: maiko/src/storage.c:386 - init_storage()
pub fn initStorage(ifpage: *IFPAGE) void {
    _ = ifpage; // For now, just a placeholder
    // C code sets SecondMDSPage_word, but we don't need this yet
}

/// Initialize IFPAGE equivalent to init_ifpage()
/// C: maiko/src/initsout.c:111 - init_ifpage()
pub fn initIFPAGE(ifpage: *IFPAGE, sysout_size: u16) void {
    _ = sysout_size; // For now, just a placeholder
    _ = ifpage; // IFPAGE is already loaded from sysout
    // C code sets various IFPAGE fields, but most are already set from sysout
}

/// Initialize IOPAGE equivalent to init_iopage()
/// C: maiko/src/initsout.c:195 - init_iopage()
pub fn initIOPAGE(virtual_memory: []u8) void {
    _ = virtual_memory; // For now, just a placeholder
    // C code initializes IOPAGE structure at IOPAGE_OFFSET
}

/// Initialize frame if it's uninitialized
/// Repairs frame structure similar to stack_check() repair logic
/// C: maiko/src/llstk.c:544-558 - stack_check() repair logic
pub fn initializeFrame(
    virtual_memory: []u8,
    ifpage: *const IFPAGE,
    stackbase: DLword,
    endofstack: DLword,
) errors.VMError!void {
    // virtual_memory needs to be mutable, but we receive it as const
    // This function should be called with a mutable slice
    const stackspace_byte_offset = STK_OFFSET * 2; // Convert DLword offset to byte offset
    const currentfxp_stack_offset = ifpage.currentfxp; // DLword offset from Stackspace

    // Calculate frame byte offset
    const frame_offset = stackspace_byte_offset + (@as(usize, @intCast(currentfxp_stack_offset)) * 2);

    if (frame_offset + 14 > virtual_memory.len) {
        return error.MemoryAccessFailed;
    }

    // Read fnheader to check if frame is initialized
    // Frame layout: flags+usecount (2), alink (2), lofnheader (2), hi1fnheader+hi2fnheader (2), nextblock (2), pc (2)
    // fnheader = (hi2fnheader << 16) | lofnheader
    // lofnheader is at bytes 4-5, hi2fnheader is at byte 6
    const lofnheader_be: DLword = (@as(DLword, virtual_memory[frame_offset + 4]) << 8) |
        (@as(DLword, virtual_memory[frame_offset + 5]));
    const hi2fnheader: u8 = virtual_memory[frame_offset + 6];
    const fnheader_addr = (@as(LispPTR, hi2fnheader) << 16) | lofnheader_be;

    // Read nextblock to check if frame is initialized (at bytes 8-9)
    const nextblock_be: DLword = (@as(DLword, virtual_memory[frame_offset + 8]) << 8) |
        (@as(DLword, virtual_memory[frame_offset + 9]));
    const nextblock = nextblock_be;

    // If frame is initialized (both fnheader and nextblock are non-zero), don't repair
    // A valid frame needs both: fnheader points to function, nextblock points to free stack block
    if (fnheader_addr != 0 and nextblock != 0) {
        std.debug.print("DEBUG: Frame is already initialized (fnheader=0x{x}, nextblock=0x{x})\n", .{ fnheader_addr, nextblock });
        return;
    }

    std.debug.print("DEBUG: Frame needs initialization (fnheader=0x{x}, nextblock=0x{x})\n", .{ fnheader_addr, nextblock });

    std.debug.print("DEBUG: Frame is uninitialized, initializing...\n", .{});

    // Calculate where the free stack block should be
    // C: CURRENTFX->nextblock = StackOffsetFromNative(CurrentStackPTR + 2)
    // CurrentStackPTR should be at stackbase initially
    const stackbase_byte_offset = stackspace_byte_offset + (@as(usize, @intCast(stackbase)) * 2);
    const free_block_offset = stackbase_byte_offset + 4; // +2 DLwords = +4 bytes

    if (free_block_offset + 4 > virtual_memory.len) {
        std.debug.print("WARNING: Cannot initialize frame - free block offset out of bounds\n", .{});
        return;
    }

    // Set up free stack block marker
    // C: GETWORD(CurrentStackPTR + 2) = STK_FSB_WORD;
    // C: GETWORD(CurrentStackPTR + 3) = (((UNSIGNED)EndSTKP - (UNSIGNED)(CurrentStackPTR + 2)) >> 1);
    const endofstack_byte_offset = stackspace_byte_offset + (@as(usize, @intCast(endofstack)) * 2);
    const free_block_size_dlwords = (@as(u16, @intCast((endofstack_byte_offset - free_block_offset) / 2)));

    // Write STK_FSB_WORD (big-endian)
    virtual_memory[free_block_offset] = @as(u8, @truncate(STK_FSB_WORD >> 8));
    virtual_memory[free_block_offset + 1] = @as(u8, @truncate(STK_FSB_WORD & 0xFF));

    // Write free block size (big-endian)
    virtual_memory[free_block_offset + 2] = @as(u8, @truncate(free_block_size_dlwords >> 8));
    virtual_memory[free_block_offset + 3] = @as(u8, @truncate(free_block_size_dlwords & 0xFF));

    // Set CURRENTFX->nextblock to point to the free block
    // nextblock is at frame_offset + 8 (after fnheader)
    // But wait, let me check the frame structure layout again...
    // Frame layout: flags+usecount (2), alink (2), fnheader (4), nextblock (2), pc (2)
    // So nextblock is at offset 8 bytes from frame start
    const nextblock_stack_offset = @as(DLword, @intCast((free_block_offset - stackspace_byte_offset) / 2));

    // Write nextblock (big-endian) at frame_offset + 8
    virtual_memory[frame_offset + 8] = @as(u8, @truncate(nextblock_stack_offset >> 8));
    virtual_memory[frame_offset + 9] = @as(u8, @truncate(nextblock_stack_offset & 0xFF));

    std.debug.print("DEBUG: Initialized frame: nextblock=0x{x}, free_block at offset 0x{x}, size={} DLwords\n", .{
        nextblock_stack_offset,
        free_block_offset,
        free_block_size_dlwords,
    });
}

/// Complete initialization sequence before start_lisp()
/// Equivalent to: build_lisp_map(), init_ifpage(), init_iopage(), init_miscstats(), init_storage()
pub fn initializeSystem(
    virtual_memory: []u8,
    ifpage: *IFPAGE,
    sysout_size: u16,
) errors.VMError!void {
    // 1. Build Lisp map (set up pointers - handled by passing virtual_memory)
    buildLispMap(virtual_memory);

    // 2. Initialize IFPAGE
    initIFPAGE(ifpage, sysout_size);

    // 3. Initialize IOPAGE
    initIOPAGE(virtual_memory);

    // 4. Initialize storage
    initStorage(ifpage);

    // 5. Initialize frame if needed
    // Note: virtual_memory is const in the parameter, but we need mutable access
    // This is safe because we're only initializing uninitialized frames
    const stackbase = ifpage.stackbase;
    const endofstack = ifpage.endofstack;
    if (stackbase > 0 and endofstack > stackbase) {
        // Cast to mutable for frame initialization (safe - we only write to uninitialized frames)
        try initializeFrame(@constCast(virtual_memory), ifpage, stackbase, endofstack);
    } else {
        std.debug.print("WARNING: Invalid stackbase/endofstack values, skipping frame initialization\n", .{});
    }
}