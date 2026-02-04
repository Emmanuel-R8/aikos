const std = @import("std");
const types = @import("../utils/types.zig");
const stack = @import("stack.zig");
const instruction = @import("dispatch/instruction.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;
const Instruction = instruction.Instruction;
const Opcode = instruction.Opcode;

/// Execution trace logger matching C emulator format (per emulator_debug.log)
pub const ExecutionTrace = struct {
    log_file: ?std.fs.File,
    instruction_count: u64,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, log_path: []const u8) !ExecutionTrace {
        std.debug.print("DEBUG execution_trace: Attempting to create log file: {s}\n", .{log_path});
        const log_file = std.fs.cwd().createFile(log_path, .{}) catch |err| {
            std.debug.print("ERROR: Failed to create execution log file '{s}': {}\n", .{ log_path, err });
            return ExecutionTrace{
                .log_file = null,
                .instruction_count = 0,
                .allocator = allocator,
            };
        };

        std.debug.print("DEBUG execution_trace: Successfully created log file: {s}\n", .{log_path});

        return ExecutionTrace{
            .log_file = log_file,
            .instruction_count = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ExecutionTrace) void {
        if (self.log_file) |file| {
            file.close();
        }
    }

    /// Generate unified single-line trace format for rapid C/Zig comparison.
    /// Format matches C emulator (execution_trace.c) for byte-for-byte diff.
    /// When logging after execution, pass pc_executed so the line shows that instruction and state after it.
    pub fn unifiedTraceLog(self: *ExecutionTrace, vm: *VM, inst: Instruction, pc_override: ?LispPTR) !void {
        if (self.log_file == null) return;

        const pc_byte_offset = if (pc_override) |pc| pc else vm.pc;
        var buffer: [2048]u8 = undefined;
        var pos: usize = 0;

        // Helper to append formatted string
        const append = struct {
            fn append(buf: []u8, p: *usize, comptime fmt: []const u8, args: anytype) !void {
                const written = try std.fmt.bufPrint(buf[p.*..], fmt, args);
                p.* += written.len;
            }
        }.append;

        // 1. LINE#
        try append(&buffer, &pos, "{d:>6}|", .{self.instruction_count});

        // 2. PC
        try append(&buffer, &pos, "0x{x:0>6}|", .{pc_byte_offset});

        // 3. INSTRUCTION
        const opcode_name = switch (inst.opcode) {
            .CONST_0 => "0",
            .CONST_1 => "1",
            else => @tagName(inst.opcode),
        };
        var instr_padded: [17]u8 = undefined;
        @memset(instr_padded[0..], ' ');
        const name_len = @min(opcode_name.len, 16);
        @memcpy(instr_padded[0..name_len], opcode_name[0..name_len]);
        try append(&buffer, &pos, "{s}|", .{instr_padded[0..16]});

        // 4. OPCODE
        const opcode_byte = @intFromEnum(inst.opcode);
        try append(&buffer, &pos, "0x{x:0>2}|", .{opcode_byte});

        // 5. OPERANDS - empty 20 chars to match C
        const operands_blank = "                    ";
        try append(&buffer, &pos, "{s}|", .{operands_blank[0..20]});

        // 6. REGISTERS - full CPU state: r1=PC_lo, r2=TOS_lo, r3=TOS_hi (30 chars to match C)
        const tos_val = stack.getTopOfStack(vm);
        const r1 = @as(u16, @truncate(pc_byte_offset));
        const r2 = @as(u16, @truncate(tos_val));
        const r3 = @as(u16, @truncate(tos_val >> 16));
        try append(&buffer, &pos, "r1:0x{x:0>4} r2:0x{x:0>4} r3:0x{x:0>2}  |", .{ r1, r2, r3 });

        // 7. FLAGS - Z=(TOS==0), N=(TOS negative), C=0 (10 chars to match C field width)
        const z: u1 = if (tos_val == 0) 1 else 0;
        const n: u1 = if (tos_val & 0x80000000 != 0) 1 else 0;
        try append(&buffer, &pos, "Z:{} N:{} C:0|", .{ z, n });

        // 8. SP_FP
        const currentfx_offset = self.getCurrentFXOffset(vm);
        try append(&buffer, &pos, "SP:0x{x:0>6} FP:0x{x:0>6}|", .{ self.getStackPtrOffset(vm), currentfx_offset });

        // 9. STACK_SUMMARY - match C: TOS only, N1/N2 fixed 0x00000000 (no D:)
        const tos = stack.getTopOfStack(vm);
        try append(&buffer, &pos, "TOS:0x{x:0>8} N1:0x00000000 N2:0x00000000|", .{tos});

        // 10. MEMORY_CONTEXT - match C: @mem:?
        const BYTESPER_PAGE: usize = 512;
        const pc_vpage = pc_byte_offset / BYTESPER_PAGE;
        const pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
        try append(&buffer, &pos, "@mem:? [vpage:{} off:0x{x:0>3}]|", .{ pc_vpage, pc_offset_in_page });

        // 11. FP_VP_FO_VA
        const virtual_address = pc_vpage * BYTESPER_PAGE;
        try append(&buffer, &pos, "FP:0 VP:{} FO:0x0 VA:0x{x:0>6}|", .{ pc_vpage, virtual_address });

        // 12. BS_MEM - match C: ????????????
        try append(&buffer, &pos, "BS:RAW MEM:????????|", .{});

        // 13. NOTES - empty 30 chars to match C
        const notes_blank = "                              ";
        try append(&buffer, &pos, "{s}", .{notes_blank[0..30]});

        // Newline
        try append(&buffer, &pos, "\n", .{});

        // Write to file
        try self.log_file.?.writeAll(buffer[0..pos]);
        self.instruction_count += 1;
    }

    /// Log instruction execution (unified format only for C/Zig parity comparison).
    /// pc_override: when logging after execution, pass the PC of the instruction just executed.
    pub fn logInstruction(self: *ExecutionTrace, vm: *VM, inst: Instruction, pc_override: ?LispPTR) !void {
        if (self.log_file == null) {
            if (self.instruction_count == 0) {
                std.debug.print("DEBUG execution_trace: log_file is null, skipping log\n", .{});
            }
            return;
        }

        // Write only unified trace format for parity comparison (one line per instruction).
        try self.unifiedTraceLog(vm, inst, pc_override);
    }


    fn getCurrentFXOffset(self: *ExecutionTrace, vm: *VM) usize {
        _ = self;
        // C: xc.c:945 fp_offset = (unsigned long)((DLword *)CURRENTFX - (DLword *)Lisp_world);
        // So FP is DLword offset from Lisp_world (virtual_memory start).
        if (vm.current_frame == null or vm.virtual_memory == null) return 0;
        const frame_addr = @intFromPtr(vm.current_frame.?);
        const lisp_world = @intFromPtr(vm.virtual_memory.?.ptr);
        if (frame_addr < lisp_world) return 0;
        return @as(usize, @intCast((frame_addr - lisp_world) / 2));
    }

    fn getStackPtrOffset(self: *ExecutionTrace, vm: *VM) usize {
        _ = self;
        // C: xc.c:944 sp_offset = (unsigned long)((DLword *)CSTKPTRL - (DLword *)Lisp_world);
        // Log CSTKPTRL offset (DLwords from Lisp_world). Use cstkptrl when set (updated by POP/PUSH/etc).
        if (vm.virtual_memory == null) return 0;
        const lisp_world = @intFromPtr(vm.virtual_memory.?.ptr);
        if (vm.cstkptrl) |cstkptrl| {
            const addr = @intFromPtr(cstkptrl);
            if (addr >= lisp_world) return @as(usize, @intCast((addr - lisp_world) / 2));
        }
        // Fallback before cstkptrl is set: CSTKPTRL = CurrentStackPTR + 2 DLwords
        const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
        if (stack_ptr_addr < lisp_world) return 0;
        return @as(usize, @intCast((stack_ptr_addr - lisp_world) / 2 + 2));
    }

    fn getNextStackValues(self: *ExecutionTrace, vm: *VM, count: usize) [4]LispPTR {
        _ = self;
        var values: [4]LispPTR = [_]LispPTR{0} ** 4;
        // Match C trace (maiko/src/xc.c):
        //   for i: stack_slot = CurrentStackPTR - i*2 (DLwords); next_values[i] = *((LispPTR*)stack_slot)
        // i.e. read 32-bit values at CurrentStackPTR, then 4 bytes below, etc.
        var i: usize = 0;
        while (i < count and i < 4) : (i += 1) {
            const dlword_off: usize = i * 2; // 2 DLwords = 1 LispPTR cell
            const stack_slot: [*]DLword = vm.stack_ptr - dlword_off;
            if (@intFromPtr(stack_slot) <= @intFromPtr(vm.stack_base)) break;
            const cell_ptr: [*]align(1) LispPTR = @ptrCast(@as([*]u8, @ptrCast(stack_slot)));
            values[i] = cell_ptr[0];
        }
        return values;
    }

    /// Format FuncObj offset to match C's %5d format exactly
    /// C's %5d right-aligns to 5 characters and includes the sign in the width
    /// Examples: 104 -> "  104", -104 -> " -104", -69432 -> "-69432" (overflow)
    /// Returns a 5-character string (right-aligned, with sign included in width)
    fn formatFuncObjOffset(self: *ExecutionTrace, offset: i32) [5]u8 {
        _ = self;
        var result: [5]u8 = undefined;
        @memset(result[0..], ' ');

        // Convert to string first
        var num_buf: [16]u8 = undefined;
        const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{offset}) catch {
            // If formatting fails, return spaces
            return result;
        };
        const num_len = num_str.len;

        if (num_len <= 5) {
            // Number fits in 5 chars - right-align it
            // Copy from right to left
            var src_idx: usize = num_len;
            var dst_idx: usize = 5;
            while (src_idx > 0) {
                src_idx -= 1;
                dst_idx -= 1;
                result[dst_idx] = num_str[src_idx];
            }
            // Left side already filled with spaces from memset
        } else {
            // Number too large - C %5d will overflow, just copy leftmost 5 chars
            const copy_len = 5;
            @memcpy(result[0..copy_len], num_str[0..copy_len]);
        }
        return result;
    }
};
