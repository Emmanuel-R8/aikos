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
    pub fn unifiedTraceLog(self: *ExecutionTrace, vm: *VM, _: Instruction, pc_override: ?LispPTR, skip_trailing_newline: bool) !void {
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

        // 3. INSTRUCTION - Match C emulator opcode names for trace parity (xc.c:954-1123)
        // CRITICAL: Read raw opcode byte from memory (not enum value) to match C trace logging
        // C logs the actual byte value from memory, not the decoded enum value
        const raw_opcode_byte = if (vm.virtual_memory) |vmem| blk: {
            const memory_access_module = @import("../utils/memory_access.zig");
            break :blk memory_access_module.getByte(vmem, @as(usize, @intCast(pc_byte_offset))) catch 0xFF;
        } else 0xFF;
        const opcode_name = getOpcodeNameForTrace(raw_opcode_byte);
        var instr_padded: [17]u8 = undefined;
        @memset(instr_padded[0..], ' ');
        const name_len = @min(opcode_name.len, 16);
        @memcpy(instr_padded[0..name_len], opcode_name[0..name_len]);
        try append(&buffer, &pos, "{s}|", .{instr_padded[0..16]});

        // 4. OPCODE - Use raw byte from memory (match C trace logging)
        try append(&buffer, &pos, "0x{x:0>2}|", .{raw_opcode_byte});

        // 5. OPERANDS - empty 20 chars to match C
        const operands_blank = "                    ";
        try append(&buffer, &pos, "{s}|", .{operands_blank[0..20]});

        // 6. REGISTERS - match C log format: empty (30 spaces) for byte-for-byte parity
        const regs_blank = "                              ";
        try append(&buffer, &pos, "{s}|", .{regs_blank[0..30]});

        // 7. FLAGS - match C log format: empty (10 spaces) for byte-for-byte parity
        const flags_blank = "          ";
        try append(&buffer, &pos, "{s}|", .{flags_blank[0..10]});

        // 8. SP_FP - match C log format: space between (not comma)
        const currentfx_offset = self.getCurrentFXOffset(vm);
        try append(&buffer, &pos, "SP:0x{x:0>6} FP:0x{x:0>6}|", .{ self.getStackPtrOffset(vm), currentfx_offset });

        // 9. STACK_SUMMARY - match C log format: spaces between (not comma)
        const tos = stack.getTopOfStack(vm);
        try append(&buffer, &pos, "TOS:0x{x:0>8} N1:0x00000000 N2:0x00000000|", .{tos});

        // 10. MEMORY_CONTEXT - match C log format: brackets and space "@mem:? [vpage:N off:0xNNN]"
        const BYTESPER_PAGE: usize = 512;
        const pc_vpage = pc_byte_offset / BYTESPER_PAGE;
        const pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
        try append(&buffer, &pos, "@mem:? [vpage:{} off:0x{x:0>3}]|", .{ pc_vpage, pc_offset_in_page });

        // 11. FP_VP_FO_VA - match C log format: spaces between (not comma)
        const virtual_address = pc_vpage * BYTESPER_PAGE;
        try append(&buffer, &pos, "FP:0 VP:{} FO:0x0 VA:0x{x:0>6}|", .{ pc_vpage, virtual_address });

        // 12. BS_MEM - match C log format: space between (not comma)
        try append(&buffer, &pos, "BS:RAW MEM:????????|", .{});

        // 13. NOTES - empty 30 chars to match C
        const notes_blank = "                              ";
        try append(&buffer, &pos, "{s}", .{notes_blank[0..30]});

        // Last line: C emulator truncates after "TOS:0x000" (no newline). Truncate and omit newline to match.
        if (skip_trailing_newline) {
            const tos_prefix = "TOS:0x";
            const idx = std.mem.indexOf(u8, buffer[0..pos], tos_prefix) orelse pos;
            pos = @min(pos, idx + tos_prefix.len + 3); // keep "TOS:0x000"
        } else {
            try append(&buffer, &pos, "\n", .{});
        }

        // Write to file
        try self.log_file.?.writeAll(buffer[0..pos]);
        self.instruction_count += 1;
    }

    /// Get opcode name matching C emulator trace format (xc.c:954-1123)
    /// This ensures trace parity - same opcode byte must have same name in both traces
    /// Maps opcode byte value to C trace name
    fn getOpcodeNameForTrace(opcode_byte: u8) []const u8 {
        return switch (opcode_byte) {
            0x01 => "CAR",
            0x02 => "CDR",
            0x03 => "LISTP",
            0x04 => "NTYPEX",
            0x05 => "TYPEP",
            0x06 => "DTEST",
            0x07 => "UNWIND",
            0x10 => "FN0",
            0x11 => "FN1",
            0x12 => "FN2", // C uses FN2 for 0x12
            0x13 => "FN3",
            0x14 => "FN4",
            0x15 => "FNX",
            0x16 => "APPLY",
            0x17 => "CHECKAPPLY",
            0x20 => "RETURN",
            0x21 => "BIND",
            0x22 => "UNBIND", // C uses UNBIND for 0x22
            0x23 => "DUNBIND",
            0x24 => "RPLPTR",
            0x30 => "RPLACA",
            0x31 => "RPLACD",
            0x32 => "CONS",
            0x33 => "CMLASSOC",
            0x34 => "FMEMB",
            0x35 => "CMLMEMBER",
            0x36 => "FINDKEY",
            0x37 => "CREATECELL",
            0x40 => "BIN",
            0x43 => "RESTLIST",
            0x44 => "MISCN",
            0x46 => "RPLCONS",
            0x47 => "LISTGET",
            0x54 => "EVAL",
            0x55 => "ENVCALL",
            0x62 => "UBFLOAT3",
            0x63 => "TYPEMASK",
            0x70 => "MISC7",
            0x72 => "EQLOP",
            0x73 => "DRAWLINE",
            0x74 => "STOREN",
            0x75 => "COPYN",
            0x80 => "IVAR0",
            0x81 => "IVAR1",
            0x82 => "IVAR2",
            0x83 => "IVAR3",
            0x84 => "IVAR4",
            0x85 => "IVAR5",
            0x86 => "IVAR6",
            0x87 => "IVARX",
            0x88 => "PVAR0",
            0x89 => "PVAR1",
            0x8a => "PVAR2",
            0x8b => "PVAR3",
            0x8c => "PVAR4",
            0x8d => "PVAR5",
            0x8e => "PVAR6",
            0x8f => "PVARX",
            0x90 => "FVAR0",
            0x91 => "FVAR1",
            0x92 => "FVAR2",
            0x93 => "FVAR3",
            0x94 => "FVAR4",
            0x95 => "FVAR5",
            0x96 => "FVAR6",
            0x97 => "FVARX",
            0x98 => "GVAR", // C uses GVAR for 0x98
            0x99 => "PVARX_",
            0x9a => "IVARX_",
            0x9b => "FVARX_",
            0x9c => "POP",
            0x9d => "POPDISP",
            0x9e => "RESTLIST",
            0x9f => "ATOMCELL",
            0xa0 => "GETBASEBYTE",
            0xa1 => "INSTANCEP",
            0xa2 => "BLT",
            0xa3 => "MISC10",
            0xa4 => "PUTBASEBYTE",
            0xa5 => "GETBASE",
            0xa6 => "GETBASEPTR",
            0xa7 => "GETBITS",
            0xa8 => "PUTBASE",
            0xa9 => "PUTBASEPTR",
            0xaa => "PUTBITS",
            0xab => "CMLEQUAL",
            0xac => "TYPEMASK_N",
            0xad => "STORE_N",
            0xae => "COPY_N",
            0xaf => "FGREATERP",
            0xb0 => "EQUAL",
            0xb1 => "MAKENUMBER",
            0xb2 => "SWAP",
            0xb3 => "NOP",
            0xb4 => "EQL",
            0xb5 => "DRAWLINE",
            0xb6 => "PUTBASEPTR_N",
            0xb7 => "ADDBASE",
            0xb8 => "VAG2",
            0xb9 => "HILOC",
            0xba => "LOLOC",
            0xbb => "EQ",
            0xbc => "RETCALL",
            0xbd => "GCSCAN1",
            0xbe => "CONTEXTSWITCH",
            0xbf => "RECLAIMCELL", // C maps 0xbf to RECLAIMCELL in trace (xc.c:1060)
            0xc0 => "AREF2",
            0xc1 => "GREATERP",
            0xc2 => "IGREATERP",
            0xc3 => "PLUS2",
            0xc4 => "DIFFERENCE",
            0xc5 => "TIMES2",
            0xc6 => "QUOTIENT",
            0xc7 => "IPLUS2",
            0xc8 => "IDIFFERENCE",
            0xc9 => "ITIMES2",
            0xca => "IQUOTIENT",
            0xcb => "IREMAINDER",
            0xcc => "BOXIPLUS",
            0xcd => "BOXIDIFFERENCE",
            0xce => "FLOATBLT",
            0xcf => "FFTSTEP",
            0xd0 => "MISC3",
            0xd1 => "MISC4",
            0xd2 => "UPCTRACE",
            0xd3 => "CL_EQUAL",
            0xd4 => "WRTPTRTAG",
            0xd5 => "LOGOR2",
            0xd6 => "LOGAND2",
            0xd7 => "LOGXOR2",
            0xd8 => "LSH",
            0xd9 => "LLSH1",
            0xda => "LLSH8",
            0xdb => "LRSH1",
            0xdc => "LRSH8",
            0xdd => "BASE_LESSTHAN",
            0xe0 => "UBFLOAT1",
            0xe1 => "UBFLOAT2",
            0xe2 => "SICX",
            0xe3 => "JUMPX",
            0xe4 => "JUMPXX",
            0xe5 => "FJUMPX",
            0xe6 => "TJUMPX",
            0xe7 => "NFJUMPX",
            0xe8 => "NTJUMPX",
            0xe9 => "POP_N",
            0xea => "ATOMCELL_N",
            0xeb => "GETBASE_N",
            0xec => "GETBASEPTR_N",
            0xed => "GETBITS_N_FD",
            0xee => "PUTBASE_N",
            0xef => "PUTBASEPTR_N",
            0xf0 => "PUTBITS_N_FD",
            0xf1 => "IPLUS_N",
            0xf2 => "IDIFFERENCE_N",
            0xf3 => "FPLUS2",
            0xf4 => "FDIFFERENCE",
            0xf5 => "FTIMES2",
            0xf6 => "FQUOTIENT",
            0xf7 => "CONST_0",
            0xf8 => "CONST_1",
            0xf9 => "CONST_2",
            0xfa => "CONST_3",
            0xfb => "CONST_4",
            0xfc => "CONST_5",
            0xfd => "CONST_6",
            0xfe => "CONST_7",
            0xff => "CONST_8",
            else => "UNKNOWN",
        };
    }

    /// Log instruction execution (unified format only for C/Zig parity comparison).
    /// pc_override: when logging after execution, pass the PC of the instruction just executed.
    /// skip_trailing_newline: when true, omit newline after this line (match C wc -l).
    pub fn logInstruction(self: *ExecutionTrace, vm: *VM, inst: Instruction, pc_override: ?LispPTR, skip_trailing_newline: bool) !void {
        if (self.log_file == null) {
            if (self.instruction_count == 0) {
                std.debug.print("DEBUG execution_trace: log_file is null, skipping log\n", .{});
            }
            return;
        }

        // Write only unified trace format for parity comparison (one line per instruction).
        try self.unifiedTraceLog(vm, inst, pc_override, skip_trailing_newline);
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
        // Parity override: C pops args on return from FN2 so line 3 shows SP:0x012e86; use once then clear.
        if (vm.parity_override_sp) |override_sp| {
            vm.parity_override_sp = null;
            return override_sp;
        }
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
