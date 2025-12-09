const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Stack safety margin (matches C STK_SAFE)
/// C: maiko/inc/stack.h:38 - #define STK_SAFE 32
/// Added to stkmin when checking stack space
pub const STK_SAFE: u16 = 32;

/// Stack frame structure (matches C FX)
/// Per data-model.md
pub const FX = packed struct {
    nextblock: LispPTR, // Next stack block pointer
    link: LispPTR, // Activation link (previous frame)
    fnheader: LispPTR, // Function header pointer
    pcoffset: DLword, // PC offset
    // Local variables follow in memory after this struct
};

/// VM state structure
pub const VM = struct {
    allocator: std.mem.Allocator,
    current_frame: ?*FX,
    stack_base: [*]DLword,
    stack_ptr: [*]DLword,
    stack_end: [*]DLword,
    // Memory management (optional - can be null)
    storage: ?*@import("../memory/storage.zig").Storage,
    virtual_memory: ?[]const u8, // Virtual memory as byte slice (from sysout)
    fptovp: ?[]const u16, // FPtoVP table for address translation
    // Execution state
    pc: LispPTR, // Current program counter (managed by dispatch loop)
    return_pc: ?LispPTR, // PC to return to after function call

    pub fn init(allocator: std.mem.Allocator, stack_size: usize) !VM {
        const stack_mem = try allocator.alloc(DLword, stack_size);
        const stack_base_ptr: [*]DLword = stack_mem.ptr + stack_size;

        return VM{
            .allocator = allocator,
            .current_frame = null,
            .stack_base = stack_base_ptr,
            .stack_ptr = stack_base_ptr,
            .stack_end = stack_mem.ptr,
            .storage = null,
            .virtual_memory = null,
            .fptovp = null,
            .pc = 0,
            .return_pc = null,
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
        .nextblock = 0, // Will be set to point to IVar area
        .link = if (vm.current_frame) |cf| @as(LispPTR, @truncate(@intFromPtr(cf))) else 0,
        .fnheader = 0,
        .pcoffset = 0,
    };

    // Set nextblock to point to IVar area (after frame header)
    const ivar_base_addr = aligned_addr + @sizeOf(FX);
    frame.nextblock = @as(LispPTR, @intCast(ivar_base_addr));

    vm.current_frame = frame;
    return frame;
}

/// Set parameter value in frame
/// Helper function for setting up function call parameters
pub fn setPVar(frame: *FX, index: usize, value: LispPTR) void {
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(FX);
    const pvar_base_addr = frame_addr + frame_size;
    const pvar_addr = pvar_base_addr + (index * @sizeOf(LispPTR));
    const pvar_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(pvar_addr));
    pvar_ptr.* = value;
}

/// Get IVar value from frame
/// Helper function for accessing local variables
pub fn getIVar(frame: *FX, index: usize) LispPTR {
    // IVars stored at nextblock offset
    const ivar_base_addr = frame.nextblock;
    if (ivar_base_addr == 0) {
        return 0; // No IVar area allocated
    }

    // Access IVar at index (each IVar is LispPTR = 4 bytes)
    const ivar_addr = ivar_base_addr + (@as(LispPTR, @intCast(index)) * @sizeOf(LispPTR));

    // Convert to native pointer and read
    const ivar_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(@as(usize, ivar_addr)));
    return ivar_ptr.*;
}

/// Set IVar value in frame
/// Helper function for setting local variables
pub fn setIVar(frame: *FX, index: usize, value: LispPTR) void {
    const ivar_base_addr = frame.nextblock;
    if (ivar_base_addr == 0) {
        return; // No IVar area allocated
    }

    const ivar_addr = ivar_base_addr + (@as(LispPTR, @intCast(index)) * @sizeOf(LispPTR));
    const ivar_ptr: *LispPTR = @as(*LispPTR, @ptrFromInt(@as(usize, ivar_addr)));
    ivar_ptr.* = value;
}

/// Get PVar value from frame
/// Helper function for accessing parameter variables
pub fn getPVar(frame: *FX, index: u8) LispPTR {
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
pub fn getActivationLink(frame: *FX) LispPTR {
    return frame.link;
}

/// Push value onto stack
/// Per rewrite documentation vm-core/stack-management.md
/// Matches C implementation: PushStack(x) stores LispPTR (32-bit = 2 DLwords)
/// C: maiko/inc/lispemul.h:PushStack(x) - checks stack overflow before pushing
pub fn pushStack(vm: *VM, value: LispPTR) errors.VMError!void {
    // Stack grows down, so move pointer down by 4 bytes (2 DLwords for LispPTR)
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    const stack_end_addr = @intFromPtr(vm.stack_end);

    // Check stack overflow with safety margin
    // C: Checks if CurrentStackPTR - required_space < EndSTKP
    // For push, required space is 2 DLwords (1 LispPTR)
    const required_space = @sizeOf(LispPTR);
    if (stack_ptr_addr - required_space < stack_end_addr) {
        return error.StackOverflow;
    }

    // Store LispPTR as 2 DLwords (low word first, then high word)
    // C implementation: CurrentStackPTR += 2; *((LispPTR *)(void *)(CurrentStackPTR)) = x;
    vm.stack_ptr -= 2; // Move down by 2 DLwords
    vm.stack_ptr[0] = @as(DLword, @truncate(value)); // Low 16 bits
    vm.stack_ptr[1] = @as(DLword, @truncate(value >> 16)); // High 16 bits
}

/// Pop value from stack
/// Per rewrite documentation vm-core/stack-management.md
/// Matches C implementation: POP_TOS_1 reads LispPTR (32-bit = 2 DLwords)
pub fn popStack(vm: *VM) errors.VMError!LispPTR {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    // Check for stack underflow (stack_ptr must be below stack_base)
    if (stack_ptr_addr + @sizeOf(LispPTR) > stack_base_addr) {
        return error.StackUnderflow;
    }

    // Read LispPTR as 2 DLwords (low word first, then high word)
    // C implementation: POP_TOS_1 = *(--CSTKPTRL) where CSTKPTRL is LispPTR*
    const low_word = vm.stack_ptr[0];
    const high_word = vm.stack_ptr[1];
    const value: LispPTR = (@as(LispPTR, high_word) << 16) | @as(LispPTR, low_word);
    vm.stack_ptr += 2; // Move up by 2 DLwords

    return value;
}

/// Get top of stack
/// Matches C implementation: TopOfStack reads LispPTR (32-bit = 2 DLwords)
pub fn getTopOfStack(vm: *const VM) LispPTR {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr + @sizeOf(LispPTR) > stack_base_addr) {
        return 0; // Stack empty
    }

    // Read LispPTR as 2 DLwords (low word first, then high word)
    const low_word = vm.stack_ptr[0];
    const high_word = vm.stack_ptr[1];
    return (@as(LispPTR, high_word) << 16) | @as(LispPTR, low_word);
}

/// Set top of stack
/// Matches C implementation: TopOfStack = value stores LispPTR (32-bit = 2 DLwords)
pub fn setTopOfStack(vm: *VM, value: LispPTR) void {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr + @sizeOf(LispPTR) <= stack_base_addr) {
        // Write LispPTR as 2 DLwords (low word first, then high word)
        vm.stack_ptr[0] = @as(DLword, @truncate(value)); // Low 16 bits
        vm.stack_ptr[1] = @as(DLword, @truncate(value >> 16)); // High 16 bits
    }
}