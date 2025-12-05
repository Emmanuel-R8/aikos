const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;

/// Stack frame structure (matches C FX)
/// Per data-model.md
pub const FX = packed struct {
    nextblock: LispPTR,      // Next stack block pointer
    link: LispPTR,           // Activation link (previous frame)
    fnheader: LispPTR,       // Function header pointer
    pcoffset: DLword,        // PC offset
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
    storage: ?*const @import("../memory/storage.zig").Storage,
    virtual_memory: ?*const @import("../memory/virtual.zig").VirtualMemory,
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
            .pc = 0,
            .return_pc = null,
        };
    }

    pub fn initWithMemory(
        allocator: std.mem.Allocator,
        stack_size: usize,
        storage: *const @import("../memory/storage.zig").Storage,
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
/// size: number of local variables (IVars) to allocate space for
pub fn allocateStackFrame(vm: *VM, size: usize) errors.VMError!*FX {
    // Check stack overflow
    // Frame size = FX header + local variables (IVars) + parameter space
    const frame_size_bytes = @sizeOf(FX) + (size * @sizeOf(DLword));
    const frame_size_words = (frame_size_bytes + @sizeOf(DLword) - 1) / @sizeOf(DLword);

    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    const stack_end_addr = @intFromPtr(vm.stack_end);

    if (stack_ptr_addr - stack_end_addr < frame_size_words * @sizeOf(DLword)) {
        return error.StackOverflow;
    }

    // Allocate frame (stack grows down)
    // Calculate aligned address
    const new_ptr_addr = stack_ptr_addr - frame_size_bytes;
    const align_mask = @alignOf(FX) - 1;
    const aligned_addr = new_ptr_addr & ~align_mask;

    // Update stack pointer
    vm.stack_ptr = @as([*]DLword, @ptrFromInt(aligned_addr));

    // Create frame pointer
    const frame: *FX = @as(*FX, @ptrFromInt(aligned_addr));
    frame.* = FX{
        .nextblock = 0, // Will be set to point to IVar area
        .link = if (vm.current_frame) |cf| @intFromPtr(cf) else 0,
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
pub fn pushStack(vm: *VM, value: LispPTR) errors.VMError!void {
    // Stack grows down, so move pointer down by 2 bytes (1 DLword)
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
    const stack_end_addr = @intFromPtr(vm.stack_end);

    if (stack_ptr_addr - @sizeOf(DLword) < stack_end_addr) {
        return error.StackOverflow;
    }

    vm.stack_ptr -= 1; // Move down by 1 DLword
    vm.stack_ptr[0] = @as(DLword, @truncate(value)); // Store low 16 bits
    // TODO: Handle 32-bit values properly (may need 2 DLwords)
}

/// Pop value from stack
/// Per rewrite documentation vm-core/stack-management.md
pub fn popStack(vm: *VM) errors.VMError!LispPTR {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr >= stack_base_addr) {
        return error.StackOverflow; // Stack underflow
    }

    const value = @as(LispPTR, vm.stack_ptr[0]);
    vm.stack_ptr += 1; // Move up by 1 DLword

    return value;
}

/// Get top of stack
pub fn getTopOfStack(vm: *const VM) LispPTR {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr >= stack_base_addr) {
        return 0; // Stack empty
    }

    return @as(LispPTR, vm.stack_ptr[0]);
}

/// Set top of stack
pub fn setTopOfStack(vm: *VM, value: LispPTR) void {
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr < stack_base_addr) {
        vm.stack_ptr[0] = @as(DLword, @truncate(value));
    }
}