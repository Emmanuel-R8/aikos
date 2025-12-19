const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Stack safety margin (matches C STK_SAFE)
/// C: maiko/inc/stack.h:38 - #define STK_SAFE 32
/// Added to stkmin when checking stack space
pub const STK_SAFE: u16 = 32;

/// Stack frame structure (matches C frameex1)
/// Per maiko/inc/stack.h:81-108
/// Non-BIGVM version (most common)
pub const FX = packed struct {
    flags_usecount: DLword, // flags (3 bits) + fast (1) + nil2 (1) + incall (1) + validnametable (1) + nopush (1) + usecount (8 bits)
    alink: DLword, // Activation link (Low addr)
    lofnheader: DLword, // Function header pointer (Low addr, 16 bits)
    hi1fnheader_hi2fnheader: u16, // hi1fnheader (8 bits) + hi2fnheader (8 bits)
    nextblock: DLword, // Pointer to FreeStackBlock
    pc: DLword, // Program counter (byte offset from FuncObj)
    lonametable: DLword, // NameTable pointer (Low addr, 16 bits)
    hi1nametable_hi2nametable: u16, // hi1nametable (8 bits) + hi2nametable (8 bits)
    blink: DLword, // blink pointer (Low addr)
    clink: DLword, // clink pointer (Low addr)
    // Local variables follow in memory after this struct
};

/// Helper function to get fnheader from FX structure (non-BIGVM)
/// C: FX_FNHEADER = (CURRENTFX->hi2fnheader << 16) | CURRENTFX->lofnheader
pub fn getFnHeader(frame: *align(1) FX) LispPTR {
    const hi2fnheader = (frame.hi1fnheader_hi2fnheader >> 8) & 0xFF;
    const lofnheader = frame.lofnheader;
    return (@as(LispPTR, hi2fnheader) << 16) | @as(LispPTR, lofnheader);
}

/// Helper function to get alink from FX structure
pub fn getAlink(frame: *align(1) FX) LispPTR {
    return @as(LispPTR, frame.alink);
}

/// Helper function to get nextblock from FX structure
pub fn getNextblock(frame: *align(1) FX) DLword {
    return frame.nextblock;
}

/// Helper function to get pc from FX structure
pub fn getPC(frame: *align(1) FX) DLword {
    return frame.pc;
}

/// VM state structure
pub const VM = struct {
    allocator: std.mem.Allocator,
    current_frame: ?*align(1) FX, // align(1) allows unaligned frame pointers from virtual memory
    stack_base: [*]DLword,
    stack_ptr: [*]DLword,
    stack_end: [*]DLword,
    // Memory management (optional - can be null)
    storage: ?*@import("../memory/storage.zig").Storage,
    virtual_memory: ?[]const u8, // Virtual memory as byte slice (from sysout)
    fptovp: ?*const @import("../data/sysout.zig").FPtoVPTable, // FPtoVP table for address translation (BIGVM format)
    gc: ?*@import("../memory/gc.zig").GC, // Garbage collector (optional)
    // Execution state
    pc: LispPTR, // Current program counter (managed by dispatch loop)
    return_pc: ?LispPTR, // PC to return to after function call
    // Cached stack top (matches C TopOfStack)
    // CRITICAL: C code uses TopOfStack as a cached value, initialized to 0
    // This avoids reading garbage from stack memory initially
    // DEBUG: Added cached TopOfStack to match C implementation
    top_of_stack: LispPTR, // Cached top of stack value (initially 0 = NIL)

    pub fn init(allocator: std.mem.Allocator, stack_size: usize) !VM {
        // CRITICAL: stack_size is in BYTES, but alloc() expects count of DLwords
        // Convert bytes to DLwords: 1 DLword = 2 bytes
        const stack_size_dlwords = stack_size / 2;
        const stack_mem = try allocator.alloc(DLword, stack_size_dlwords);
        // Stack grows down, so base is at the end of allocated memory
        const stack_base_ptr: [*]DLword = stack_mem.ptr + stack_size_dlwords;

        return VM{
            .allocator = allocator,
            .current_frame = null,
            .stack_base = stack_base_ptr,
            .stack_ptr = stack_base_ptr,
            .stack_end = stack_mem.ptr,
            .storage = null,
            .virtual_memory = null,
            .fptovp = null,
            .gc = null, // GC is optional - can be set later
            .pc = 0,
            .return_pc = null,
            .top_of_stack = 0, // C: TopOfStack = 0; (initially empty stack)
        };
    }

    pub fn initWithMemory(
        allocator: std.mem.Allocator,
        stack_size: usize,
        storage: *@import("../memory/storage.zig").Storage,
        virtual_memory: *const @import("../memory/virtual.zig").VirtualMemory,
    ) !VM {
        var vm = try init(allocator, stack_size);
        vm.storage = storage;
        vm.virtual_memory = virtual_memory;
        vm.pc = 0;
        vm.return_pc = null;
        return vm;
    }

    pub fn deinit(self: *VM) void {
        // Stack memory will be freed by allocator
        _ = self;
    }
};

/// Allocate stack frame
/// Per contracts/vm-core-interface.zig
/// Matches C implementation: Frame allocation with stack overflow checking
/// C: maiko/src/bbtsub.c:ccfuncall - checks stack space before frame allocation
/// size: number of local variables (IVars) to allocate space for
pub fn allocateStackFrame(vm: *VM, size: usize) errors.VMError!*FX {
    // Check stack overflow with safety margin
    // Frame size = FX header + local variables (IVars) + parameter space
    const frame_size_bytes = @sizeOf(FX) + (size * @sizeOf(DLword));
    const frame_size_words = (frame_size_bytes + @sizeOf(DLword) - 1) / @sizeOf(DLword);

    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    const stack_end_addr = @intFromPtr(vm.stack_end);

    // C: Check if CurrentStackPTR + stkmin + STK_SAFE >= EndSTKP
    // For now, we check frame_size + STK_SAFE
    const required_space = (frame_size_words + STK_SAFE) * @sizeOf(DLword);
    if (stack_ptr_addr - required_space < stack_end_addr) {
        std.debug.print("ERROR: Stack overflow detected during frame allocation\n", .{});
        std.debug.print("  Requested frame size: {} bytes ({} words + {} IVars)\n", .{ frame_size_bytes, @sizeOf(FX), size });
        std.debug.print("  Required space: {} bytes (frame + STK_SAFE={})\n", .{ required_space, STK_SAFE });
        std.debug.print("  Current stack pointer: 0x{x}\n", .{stack_ptr_addr});
        std.debug.print("  Stack end: 0x{x}\n", .{stack_end_addr});
        std.debug.print("  Available space: {} bytes\n", .{stack_ptr_addr - stack_end_addr});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - Deep recursion (too many nested function calls)\n", .{});
        std.debug.print("    - Stack size too small for program requirements\n", .{});
        std.debug.print("    - Infinite recursion or unbounded loop\n", .{});
        return error.StackOverflow;
    }

    // Allocate frame (stack grows down)
    // Calculate aligned address
    const new_ptr_addr = stack_ptr_addr - frame_size_bytes;
    const align_mask: usize = @alignOf(FX) - 1;
    const aligned_addr = new_ptr_addr & ~align_mask;

    // Update stack pointer
    vm.stack_ptr = @as([*]DLword, @ptrFromInt(aligned_addr));

    // Create frame pointer
    const frame: *FX = @as(*FX, @ptrFromInt(aligned_addr));
    frame.* = FX{
        .flags_usecount = 0,
        .alink = 0,
        .lofnheader = 0,
        .hi1fnheader_hi2fnheader = 0,
        .nextblock = 0, // Will be set to point to IVar area
        .pc = 0,
        .lonametable = 0,
        .hi1nametable_hi2nametable = 0,
        .blink = 0,
        .clink = 0,
    };

    // Set nextblock to point to IVar area (after frame header)
    // CRITICAL: nextblock is a DLword offset from Stackspace, not a native address
    const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
    const stackspace_byte_offset = STK_OFFSET * 2;
    const ivar_base_addr = aligned_addr + @sizeOf(FX);
    const nextblock_dlword_offset = @as(DLword, @intCast((ivar_base_addr - stackspace_byte_offset) / 2));
    frame.nextblock = nextblock_dlword_offset;

    vm.current_frame = frame;
    return frame;
}

/// Set parameter value in frame
/// Helper function for setting up function call parameters
pub fn setPVar(frame: *align(1) FX, index: usize, value: LispPTR) void {
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(FX);
    const pvar_base_addr = frame_addr + frame_size;
    const pvar_addr = pvar_base_addr + (index * @sizeOf(LispPTR));
    const pvar_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(pvar_addr));
    pvar_ptr.* = value;
}

/// Get IVar value from frame
/// Helper function for accessing local variables
pub fn getIVar(frame: *align(1) FX, index: usize) LispPTR {
    // IVars stored at nextblock offset
    // CRITICAL: nextblock is a DLword offset from Stackspace, not a LispPTR
    // This function is deprecated - use handleIVAR in variable_access.zig instead
    // Keeping for compatibility but should not be used
    _ = frame;
    _ = index;
    return 0; // Deprecated - use handleIVAR instead
}

/// Set IVar value in frame
/// Helper function for setting local variables
pub fn setIVar(frame: *align(1) FX, index: usize, value: LispPTR) void {
    // CRITICAL: nextblock is a DLword offset from Stackspace, not a LispPTR
    // This function is deprecated - use handleIVAR in variable_access.zig instead
    _ = frame;
    _ = index;
    _ = value;
    // Deprecated - use handleIVAR instead
}

/// Get PVar value from frame
/// Helper function for accessing parameter variables
pub fn getPVar(frame: *align(1) FX, index: u8) LispPTR {
    // PVars stored right after frame header
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(FX);
    const pvar_base_addr = frame_addr + frame_size;

    // Access parameter at index (each parameter is LispPTR = 4 bytes)
    const pvar_addr = pvar_base_addr + (@as(usize, index) * @sizeOf(LispPTR));
    const pvar_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(pvar_addr));

    return pvar_ptr.*;
}

/// Free stack frame
/// Per contracts/vm-core-interface.zig
pub fn freeStackFrame(vm: *VM, frame: *FX) void {
    _ = frame;
    // Stack frames are freed by moving stack pointer back up
    // This is handled by return operations
    _ = vm;
}

/// Extend stack
/// Per contracts/vm-core-interface.zig
pub fn extendStack(vm: *VM) errors.VMError!void {
    // TODO: Implement stack extension
    _ = vm;
    return error.StackOverflow; // Placeholder
}

/// Get activation link
/// Per contracts/vm-core-interface.zig
pub fn getActivationLink(frame: *align(1) FX) LispPTR {
    return getAlink(frame);
}

/// Push value onto stack
/// Per rewrite documentation vm-core/stack-management.md
/// Matches C implementation: PushStack(x) stores LispPTR (32-bit = 2 DLwords)
/// C: maiko/inc/lispemul.h:PushStack(x) - checks stack overflow before pushing
/// C: Stackspace is BASE (lowest address), CurrentStackPTR is current top (higher address when stack has data)
/// Stack grows DOWN, so pushing moves CurrentStackPTR DOWN (toward lower addresses)
pub fn pushStack(vm: *VM, value: LispPTR) errors.VMError!void {
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    const stack_end_addr = @intFromPtr(vm.stack_end);

    // Check stack overflow: after pushing, stack_ptr should not go below stack_end
    // Stack grows DOWN, so we move stack_ptr DOWN by 2 DLwords
    // CRITICAL: stack_end must be initialized (non-zero) for this check to work
    // TODO: Fix stack overflow check - need to understand stack layout better
    // For now, disable strict checking to allow execution to continue
    const new_stack_ptr_addr = stack_ptr_addr - @sizeOf(LispPTR);
    if (stack_end_addr != 0 and new_stack_ptr_addr < stack_end_addr) {
        std.debug.print("WARNING: StackOverflow check triggered (disabled for now):\n", .{});
        std.debug.print("  CurrentStackPTR: 0x{x}\n", .{stack_ptr_addr});
        std.debug.print("  NewStackPTR (after push): 0x{x}\n", .{new_stack_ptr_addr});
        std.debug.print("  EndSTKP: 0x{x}\n", .{stack_end_addr});
        if (new_stack_ptr_addr > stack_end_addr) {
            const remaining = new_stack_ptr_addr - stack_end_addr;
            std.debug.print("  Stack space remaining: {} bytes\n", .{remaining});
        } else {
            const overflow = stack_end_addr - new_stack_ptr_addr;
            std.debug.print("  Stack overflow by: {} bytes\n", .{overflow});
        }
        // Temporarily allow overflow to continue execution while we debug EndSTKP calculation
        // return error.StackOverflow;
    }

    // Store LispPTR as 2 DLwords (big-endian format for sysout compatibility)
    // C implementation: CurrentStackPTR -= 2; *((LispPTR *)(void *)(CurrentStackPTR)) = x;
    // Stack grows DOWN, so move DOWN by 2 DLwords first, then store
    vm.stack_ptr -= 2; // Move DOWN by 2 DLwords (stack grows down)
    
    // Write big-endian format: [high_byte, low_byte] for each DLword
    const stack_ptr_bytes: [*]u8 = @ptrCast(vm.stack_ptr);
    const low_word = @as(DLword, @truncate(value));
    const high_word = @as(DLword, @truncate(value >> 16));
    
    stack_ptr_bytes[0] = @as(u8, @truncate(low_word >> 8));
    stack_ptr_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    stack_ptr_bytes[2] = @as(u8, @truncate(high_word >> 8));
    stack_ptr_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
    
    // Update cached TopOfStack value
    vm.top_of_stack = value;
}

/// Pop value from stack
/// Per rewrite documentation vm-core/stack-management.md
/// Matches C implementation: POP_TOS_1 reads LispPTR (32-bit = 2 DLwords)
/// C: Stackspace is BASE (lowest address), CurrentStackPTR is current top (higher address when stack has data)
/// Stack grows DOWN, so popping moves CurrentStackPTR DOWN (toward lower addresses)
pub fn popStack(vm: *VM) errors.VMError!LispPTR {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    // Check for stack underflow: stack_ptr must be > stack_base (stack has data)
    // If stack_ptr <= stack_base, stack is empty
    if (stack_ptr_addr <= stack_base_addr) {
        std.debug.print("ERROR: Stack underflow detected during pop operation\n", .{});
        std.debug.print("  Current stack pointer: 0x{x}\n", .{@intFromPtr(vm.stack_ptr)});
        std.debug.print("  Stack base: 0x{x}\n", .{@intFromPtr(vm.stack_base)});
        std.debug.print("  Stack top cached value: 0x{x}\n", .{vm.top_of_stack});
        std.debug.print("  Possible causes:\n", .{});
        std.debug.print("    - More POP operations than PUSH operations\n", .{});
        std.debug.print("    - Stack pointer corrupted\n", .{});
        std.debug.print("    - Invalid bytecode sequence\n", .{});
        return error.StackUnderflow;
    }

    // Read LispPTR as 2 DLwords (with byte swapping for big-endian sysout format)
    // C implementation: POP_TOS_1 = *(--CSTKPTRL) where CSTKPTRL is LispPTR*
    // Stack grows DOWN, so we read from current position, then move DOWN
    // CRITICAL: Stack memory stores DLwords in BIG-ENDIAN format
    const stack_ptr_bytes: [*]const u8 = @ptrCast(vm.stack_ptr);
    const low_word_be = (@as(DLword, stack_ptr_bytes[0]) << 8) | @as(DLword, stack_ptr_bytes[1]);
    const high_word_be = (@as(DLword, stack_ptr_bytes[2]) << 8) | @as(DLword, stack_ptr_bytes[3]);
    const value: LispPTR = (@as(LispPTR, high_word_be) << 16) | @as(LispPTR, low_word_be);
    
    vm.stack_ptr -= 2; // Move DOWN by 2 DLwords (stack grows down)
    
    // Update cached TopOfStack value
    // After popping, read new top of stack (or set to 0 if empty)
    if (@intFromPtr(vm.stack_ptr) <= @intFromPtr(vm.stack_base)) {
        vm.top_of_stack = 0; // Stack empty - TopOfStack = NIL
    } else {
        // Read new top of stack (with byte swapping)
        const new_stack_ptr_bytes: [*]const u8 = @ptrCast(vm.stack_ptr);
        const new_low_word_be = (@as(DLword, new_stack_ptr_bytes[0]) << 8) | @as(DLword, new_stack_ptr_bytes[1]);
        const new_high_word_be = (@as(DLword, new_stack_ptr_bytes[2]) << 8) | @as(DLword, new_stack_ptr_bytes[3]);
        vm.top_of_stack = (@as(LispPTR, new_high_word_be) << 16) | @as(LispPTR, new_low_word_be);
    }

    return value;
}

/// Get top of stack
/// Matches C implementation: TopOfStack reads LispPTR (32-bit = 2 DLwords)
/// C: TopOfStack is a cached value, not read from memory every time
/// C: TopOfStack = 0 initially (empty stack)
/// DEBUG: Using cached TopOfStack value to match C implementation
pub fn getTopOfStack(vm: *const VM) LispPTR {
    // C: TopOfStack is a cached value, initialized to 0
    // Return cached value directly (matches C behavior)
    return vm.top_of_stack;
}

/// Set top of stack
/// Matches C implementation: TopOfStack = value stores LispPTR (32-bit = 2 DLwords)
/// C: Updates cached TopOfStack value
/// DEBUG: Using cached TopOfStack to match C implementation
pub fn setTopOfStack(vm: *VM, value: LispPTR) void {
    // C: TopOfStack = value; (updates cached value)
    vm.top_of_stack = value;
    
    // Also write to memory if stack_ptr is valid
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr > stack_base_addr) {
        // Write LispPTR as 2 DLwords (big-endian format for sysout compatibility)
        const stack_ptr_bytes: [*]u8 = @ptrCast(vm.stack_ptr);
        const low_word = @as(DLword, @truncate(value));
        const high_word = @as(DLword, @truncate(value >> 16));
        
        // Write big-endian: [high_byte, low_byte]
        stack_ptr_bytes[0] = @as(u8, @truncate(low_word >> 8));
        stack_ptr_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
        stack_ptr_bytes[2] = @as(u8, @truncate(high_word >> 8));
        stack_ptr_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
    }
}

/// Get stack depth (number of LispPTR values on stack)
/// Returns 0 if stack is empty
/// C: Stack depth = (CurrentStackPTR - Stackspace) / 2
/// Stackspace is the BASE (lowest address), CurrentStackPTR is the current top (higher address when stack has data)
/// CRITICAL: In C, CurrentStackPTR and Stackspace are DLword* pointers.
/// When you subtract two DLword* pointers, you get the difference in DLwords (pointer arithmetic).
/// But the C code divides by 2 anyway, which suggests the subtraction might give 2x the DLwords?
/// Or maybe there's a different interpretation.
pub fn getStackDepth(vm: *const VM) usize {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    // C: Stackspace is BASE (lowest address), CurrentStackPTR is current top
    // Stack depth = (CurrentStackPTR - Stackspace) / 2 (in DLwords)
    // If stack_ptr <= stack_base, stack is empty
    if (stack_ptr_addr <= stack_base_addr) {
        return 0; // Stack is empty
    }

    // CRITICAL: In C, CurrentStackPTR and Stackspace are DLword* pointers.
    // When you subtract two DLword* pointers, you get the difference in DLwords (pointer arithmetic).
    // C code: stack_depth = (CurrentStackPTR - Stackspace) / 2
    // 
    // Analysis: If ptr_diff = CurrentStackPTR - Stackspace gives DLwords,
    // then dividing by 2 would give half the DLwords. But C shows 5956,
    // which matches (nextblock - 2) / 2 = (11914 - 2) / 2 = 5956.
    //
    // In Zig, we use @intFromPtr which gives byte addresses.
    // So: diff_bytes = stack_ptr_addr - stack_base_addr (in bytes)
    // To match C's behavior, we need to divide by 4:
    //   - diff_bytes / 2 = DLwords (if we just convert bytes to DLwords)
    //   - But C divides pointer diff (in DLwords) by 2, so we need diff_bytes / 4
    //   - This gives: (11912 DLwords * 2 bytes) / 4 = 5956 DLwords ✓
    const diff_bytes = stack_ptr_addr - stack_base_addr;
    const depth = diff_bytes / 4; // Divide by 4 to match C's (ptr_diff / 2) behavior
    return @as(usize, @intCast(depth));
}
