//! VM instrumentation and tracing for understanding Zaiko internals.
//! Enable with --trace or ZAIKO_TRACE=1. Use dumpVMState() on error paths for full state dumps.

const std = @import("std");
const stack = @import("stack.zig");
const VM = stack.VM;
const FX = stack.FX;
const LispPTR = @import("../utils/types.zig").LispPTR;
const DLword = @import("../utils/types.zig").DLword;

/// When true, step trace and stack ops are logged (controlled by --trace or ZAIKO_TRACE).
var trace_enabled: bool = false;

pub fn setTraceEnabled(enabled: bool) void {
    trace_enabled = enabled;
}

pub fn isTraceEnabled() bool {
    return trace_enabled;
}

/// Ring buffer of last N steps for post-error context (PC, opcode, SP offset).
const RingLen = 24;
var ring_index: usize = 0;
var ring_pc: [RingLen]u32 = undefined;
var ring_opcode: [RingLen]u8 = undefined;
var ring_sp_off: [RingLen]u16 = undefined;
var ring_filled: bool = false;

/// Record one step for the ring buffer (call before execute).
pub fn recordStep(vm: *const VM, opcode_byte: u8) void {
    const sp_off: u16 = @intCast(getStackPtrOffset(vm) & 0xFFFF);
    ring_pc[ring_index] = @intCast(vm.pc & 0xFFFFFFFF);
    ring_opcode[ring_index] = opcode_byte;
    ring_sp_off[ring_index] = sp_off;
    ring_index += 1;
    if (ring_index >= RingLen) {
        ring_index = 0;
        ring_filled = true;
    }
}

fn getStackPtrOffset(vm: *const VM) usize {
    const base = @intFromPtr(vm.stack_base);
    const end = @intFromPtr(vm.stack_end);
    const ptr = @intFromPtr(vm.stack_ptr);
    if (ptr <= base) return 0;
    if (ptr < end) return 0; // avoid underflow when ptr < end (e.g. corrupt state)
    return (ptr - end) / @sizeOf(DLword);
}

/// One-line step trace when trace is enabled. Logs first 100 steps and every 500th.
pub fn traceStep(vm: *const VM, instruction_count: u64, opcode_byte: u8, opcode_name: []const u8, inst_len: u8) void {
    if (!trace_enabled) return;
    const log_this = instruction_count <= 100 or instruction_count % 500 == 0;
    if (!log_this) return;
    const sp_off = getStackPtrOffset(vm);
    const depth = stack.getStackDepth(vm);
    std.debug.print("[TRACE] step={} pc=0x{x} op=0x{x:0>2} {s:<12} len={} sp_off={} depth={} tos=0x{x}\n", .{
        instruction_count,
        vm.pc,
        opcode_byte,
        if (opcode_name.len > 12) opcode_name[0..12] else opcode_name,
        inst_len,
        sp_off,
        depth,
        vm.top_of_stack,
    });
}

/// Full VM state dump for debugging (use on error paths or when trace enabled and condition met).
pub fn dumpVMState(vm: *const VM, label: []const u8) void {
    std.debug.print("\n=== VM STATE DUMP [{s}] ===\n", .{label});
    std.debug.print("PC=0x{x}  TOS(cached)=0x{x}\n", .{ vm.pc, vm.top_of_stack });
    const sp_off = getStackPtrOffset(vm);
    const depth = stack.getStackDepth(vm);
    std.debug.print("SP: ptr=0x{x}  offset(DLword)={}  stack_depth(LispPTRs)={}\n", .{
        @intFromPtr(vm.stack_ptr),
        sp_off,
        depth,
    });
    std.debug.print("Stack: base=0x{x} end=0x{x}\n", .{ @intFromPtr(vm.stack_base), @intFromPtr(vm.stack_end) });
    if (vm.cstkptrl) |cstk| {
        std.debug.print("CSTKPTRL=0x{x}\n", .{@intFromPtr(cstk)});
    } else {
        std.debug.print("CSTKPTRL=NULL\n", .{});
    }
    if (vm.current_frame) |frame| {
        std.debug.print("CurrentFrame=0x{x} alink=0x{x} pc=0x{x} lofn=0x{x} nextblock=0x{x}\n", .{
            @intFromPtr(frame),
            frame.alink,
            frame.pc,
            frame.lofnheader,
            frame.nextblock,
        });
    } else {
        std.debug.print("CurrentFrame=NULL\n", .{});
    }
    std.debug.print("total_instruction_count={}  stop_requested={}\n", .{ vm.total_instruction_count, vm.stop_requested });

    // Recent step ring (last 24 steps)
    std.debug.print("--- Last {} steps ---\n", .{if (ring_filled) RingLen else ring_index});
    const start: usize = if (ring_filled) ring_index else 0;
    const count: usize = if (ring_filled) RingLen else ring_index;
    var i: usize = 0;
    while (i < count) : (i += 1) {
        const j = (start + i) % RingLen;
        std.debug.print("  [{:>2}] pc=0x{x:0>6} op=0x{x:0>2} sp_off={}\n", .{ i, ring_pc[j], ring_opcode[j], ring_sp_off[j] });
    }
    std.debug.print("=== END DUMP ===\n\n", .{});
}

/// Log stack push (when trace enabled and depth is small or early steps).
pub fn logStackPush(vm: *const VM, value: LispPTR, instruction_count: u64) void {
    if (!trace_enabled) return;
    const depth = stack.getStackDepth(vm);
    if (instruction_count <= 50 or depth <= 8)
        std.debug.print("[TRACE STACK] PUSH depth={} value=0x{x}\n", .{ depth, value });
}

/// Log stack pop (when trace enabled).
pub fn logStackPop(vm: *const VM, value: LispPTR, instruction_count: u64) void {
    if (!trace_enabled) return;
    const depth = stack.getStackDepth(vm);
    if (instruction_count <= 50 or depth <= 8)
        std.debug.print("[TRACE STACK] POP  depth={} value=0x{x}\n", .{ depth, value });
}
