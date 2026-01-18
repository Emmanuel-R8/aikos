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
    // TOS-stack pointer (C: CSTKPTRL in tos1defs.h)
    // Points to where TopOfStack would be stored (cell pointer, 4-byte units).
    // IMPORTANT: This is distinct from CURRENT stack frame pointers and the traced
    // CurrentStackPTR value. Many bytecodes manipulate CSTKPTRL without immediately
    // syncing CurrentStackPTR (see maiko/inc/tos1defs.h StackPtrSave/Restore).
    cstkptrl: ?[*]align(1) LispPTR,
    // Execution tracer (persists across dispatch() calls to avoid overwriting log)
    execution_tracer: ?@import("execution_trace.zig").ExecutionTrace,
    // Stop flag used by the outer main loop.
    // When set, the main loop should stop calling dispatch().
    stop_requested: bool,

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
            .cstkptrl = null,
            .execution_tracer = null, // Will be initialized on first dispatch() call
            .stop_requested = false,
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
        // Free stack memory if it was allocated
        // Stack memory is allocated in init() and stored in stack_base/stack_end
        // stack_base points to end of allocation, stack_end points to start
        // Check if stack_end is valid (not null pointer)
        const stack_end_addr = @intFromPtr(self.stack_end);
        const stack_base_addr = @intFromPtr(self.stack_base);
        if (stack_end_addr != 0 and stack_base_addr > stack_end_addr) {
            // Calculate allocation size: stack_base - stack_end (in DLwords)
            const stack_size_dlwords = (stack_base_addr - stack_end_addr) / @sizeOf(DLword);
            if (stack_size_dlwords > 0) {
                // Convert to slice and free
                const stack_slice = self.stack_end[0..stack_size_dlwords];
                self.allocator.free(stack_slice);
            }
        }
    }
};

/// Initialize CSTKPTRL from CurrentStackPTR (DLword*).
/// C: StackPtrRestore => CSTKPTRL = (void *)(CurrentStackPTR + 2); (CurrentStackPTR in DLwords)
pub fn initCSTKPTRLFromCurrentStackPTR(vm: *VM) void {
    const addr = @intFromPtr(vm.stack_ptr) + 4; // +2 DLwords = +4 bytes = +1 LispPTR cell
    vm.cstkptrl = @as([*]align(1) LispPTR, @ptrFromInt(addr));
}

/// POP macro (tos1defs.h): `POP` => `TOPOFSTACK = *(--CSTKPTRL)`
pub fn tosPop(vm: *VM) errors.VMError!void {
    const p = vm.cstkptrl orelse return error.StackUnderflow;
    // Decrement by one LispPTR cell and load into cached TOS
    const p_prev: [*]align(1) LispPTR = p - 1;
    vm.cstkptrl = p_prev;
    vm.top_of_stack = p_prev[0];
}

/// PUSH macro (tos1defs.h): `PUSH(x)` => `HARD_PUSH(TOPOFSTACK); TOPOFSTACK = x;`
/// where `HARD_PUSH(y)` => `*(CSTKPTRL++) = y`
pub fn tosPush(vm: *VM, x: LispPTR) errors.VMError!void {
    const p = vm.cstkptrl orelse return error.StackUnderflow;
    // Store current TOS at CSTKPTRL, then advance by one cell, then set new TOS
    @as([*]align(1) LispPTR, p)[0] = vm.top_of_stack;
    vm.cstkptrl = p + 1;
    vm.top_of_stack = x;
}

/// HARD_PUSH macro (tos1defs.h): `HARD_PUSH(x)` => `*(CSTKPTRL++) = x`
/// Used by opcodes like COPY which push without changing TOPOFSTACK.
pub fn tosHardPush(vm: *VM, x: LispPTR) errors.VMError!void {
    const p = vm.cstkptrl orelse return error.StackUnderflow;
    @as([*]align(1) LispPTR, p)[0] = x;
    vm.cstkptrl = p + 1;
}

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
    // C reference (maiko/inc/lispemul.h):
    //   #define PushStack(x) do { CurrentStackPTR += 2; *((LispPTR *)(void *)(CurrentStackPTR)) = x; } while (0)
    //
    // So CurrentStackPTR grows toward higher addresses, in DLword units.
    // Also: after sysout load, pages are already word-swapped into native endianness,
    // so we read/write native little-endian values (no manual byte swapping here).

    // TODO: re-enable strict overflow checks once EndSTKP invariants are nailed down.
    vm.stack_ptr += 2;
    const stack_ptr_bytes: [*]u8 = @ptrCast(vm.stack_ptr);
    std.mem.writeInt(LispPTR, stack_ptr_bytes[0..4], value, .little);
}

/// Pop value from stack
/// Per rewrite documentation vm-core/stack-management.md
/// Matches C implementation: POP_TOS_1 reads LispPTR (32-bit = 2 DLwords)
/// C: Stackspace is BASE (lowest address), CurrentStackPTR is current top (higher address when stack has data)
/// Stack grows DOWN, so popping moves CurrentStackPTR DOWN (toward lower addresses)
pub fn popStack(vm: *VM) errors.VMError!LispPTR {
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    const stack_base_addr = @intFromPtr(vm.stack_base);

    // C reference (maiko/inc/lispemul.h):
    //   #define PopStackTo(Place) do { (Place) = *((LispPTR *)(void *)(CurrentStackPTR)); CurrentStackPTR -= 2; } while (0)
    //
    // So pop reads at CurrentStackPTR, then decrements by 2 DLwords.

    if (stack_ptr_addr < stack_base_addr) {
        return error.StackUnderflow;
    }

    const stack_ptr_bytes: [*]const u8 = @ptrCast(vm.stack_ptr);
    const value: LispPTR = std.mem.readInt(LispPTR, stack_ptr_bytes[0..4], .little);

    vm.stack_ptr -= 2;
    vm.top_of_stack = value;
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
        const stack_ptr_bytes: [*]u8 = @ptrCast(vm.stack_ptr);
        std.mem.writeInt(LispPTR, stack_ptr_bytes[0..4], value, .little);
    }
}

/// Get stack depth (number of LispPTR values on stack)
/// Returns 0 if stack is empty
/// C: Stack depth = (CurrentStackPTR - Stackspace) / 2
/// Stackspace is the BASE (lowest address), CurrentStackPTR is the current top (higher address when stack has data)
/// CRITICAL FIX: C code uses DLword* pointer arithmetic
/// C: xc.c:747: int stack_depth = (CurrentStackPTR - Stackspace) / 2;
/// When you subtract two DLword* pointers, you get the difference in DLwords (pointer arithmetic).
/// Then dividing by 2 gives half the DLwords (which is the number of LispPTR values, since each LispPTR = 2 DLwords).
/// In Zig, we use @intFromPtr which gives byte addresses, so:
///   diff_bytes = stack_ptr_addr - stack_base_addr (in bytes)
///   diff_dlwords = diff_bytes / 2 (convert bytes to DLwords)
///   depth = diff_dlwords / 2 (divide by 2 to match C's division)
///   This simplifies to: depth = diff_bytes / 4
/// But wait - C shows stack_depth = 5956 when stack_ptr_offset = 11912
///   If stack_ptr_offset is in DLwords, then depth = 11912 / 2 = 5956 ✓
/// So C's formula is: depth = (stack_ptr_offset_in_dlwords) / 2
/// We calculate stack_ptr_offset in DLwords, then divide by 2
pub fn getStackDepth(vm: *const VM) usize {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    // C: Stackspace is BASE (lowest address), CurrentStackPTR is current top
    // Stack depth = (CurrentStackPTR - Stackspace) / 2 (in DLwords)
    // If stack_ptr <= stack_base, stack is empty
    if (stack_ptr_addr <= stack_base_addr) {
        return 0; // Stack is empty
    }

    // CRITICAL FIX: Match C's calculation exactly
    // C: xc.c:745: LispPTR stack_ptr_offset = (LispPTR)((char *)CurrentStackPTR - (char *)Stackspace) / 2;
    // C: xc.c:747: int stack_depth = (CurrentStackPTR - Stackspace) / 2;
    // In C, CurrentStackPTR and Stackspace are DLword* pointers.
    // Pointer subtraction gives difference in DLwords (not bytes).
    // Then dividing by 2 gives number of LispPTR values (each LispPTR = 2 DLwords).
    // In Zig, we have byte addresses, so:
    //   diff_bytes = stack_ptr_addr - stack_base_addr
    //   diff_dlwords = diff_bytes / 2
    //   depth = diff_dlwords / 2 = diff_bytes / 4
    // But C log shows: stack_ptr_offset = 11912, stack_depth = 5956
    //   This matches: depth = offset / 2 = 11912 / 2 = 5956 ✓
    const diff_bytes = stack_ptr_addr - stack_base_addr;
    const diff_dlwords = diff_bytes / 2; // Convert bytes to DLwords
    const depth = diff_dlwords / 2; // Divide by 2 to get number of LispPTR values
    return @as(usize, @intCast(depth));
}
