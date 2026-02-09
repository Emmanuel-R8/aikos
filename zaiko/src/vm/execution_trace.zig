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

    /// Generate unified single-line trace format for rapid C/Zig comparison
    pub fn unifiedTraceLog(self: *ExecutionTrace, vm: *VM, inst: Instruction) !void {
        if (self.log_file == null) return;

        // Log all instructions for step-by-step parity validation
        // No limit - let EMULATOR_MAX_STEPS control execution length
        const pc_byte_offset = vm.pc;

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

        // 5. OPERANDS - will be filled after parsing
        var operands_buf: [21]u8 = [_]u8{' '} ** 21;
        var operands_pos: usize = 0;

        // Fill operands based on instruction type
        switch (inst.opcode) {
            .TYPEP => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .DTEST => {
                if (inst.operands.len >= 1) {
                    const atom = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "atom=0x{x:0>2}", .{atom})).len;
                }
            },
            .UNWIND => {
                if (inst.operands.len >= 2) {
                    const p1 = inst.getByteOperand(0);
                    const p2 = inst.getByteOperand(1);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2} p2=0x{x:0>2}", .{ p1, p2 })).len;
                }
            },
            .RPLPTR_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .GVAR_ => {
                if (inst.operands.len >= 1) {
                    const atom = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "atom=0x{x:0>2}", .{atom})).len;
                }
            },
            .FINDKEY => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .MISCN => {
                if (inst.operands.len >= 2) {
                    const p1 = inst.getByteOperand(0);
                    const p2 = inst.getByteOperand(1);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2} p2=0x{x:0>2}", .{ p1, p2 })).len;
                }
            },
            .TYPEMASK_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .STORE_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .COPY_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .IVARX, .PVARX, .FVARX, .PVARX_, .IVARX_, .FVARX_ => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .SICX => {
                if (inst.operands.len >= 2) {
                    const w = inst.getWordOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "w=0x{x:0>4}", .{w})).len;
                }
            },
            .POP_N, .ATOMCELL_N, .GETBASE_N, .GETBASEPTR_N, .GETBITS_N_FD, .PUTBASE_N, .PUTBASEPTR_N, .PUTBITS_N_FD, .IPLUS_N, .IDIFFERENCE_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "p1=0x{x:0>2}", .{param})).len;
                }
            },
            .JUMPX, .JUMPXX, .FJUMPX, .TJUMPX, .NFJUMPX, .NTJUMPX => {
                if (inst.operands.len >= 2) {
                    const offset = inst.getSignedWordOperand(0);
                    operands_pos = (try std.fmt.bufPrint(operands_buf[operands_pos..], "off={d}", .{offset})).len;
                }
            },
            else => {
                // No operands
            },
        }
        try append(&buffer, &pos, "{s}|", .{operands_buf[0..20]});

        // 6. REGISTERS - simplified, unknown in Zig
        try append(&buffer, &pos, "{s}|", .{"" ** 30});

        // 7. FLAGS - unknown in Zig
        try append(&buffer, &pos, "{s}|", .{"" ** 10});

        // 8. SP_FP
        const currentfx_offset = self.getCurrentFXOffset(vm);
        try append(&buffer, &pos, "SP:0x{x:0>6} FP:0x{x:0>6}|", .{ self.getStackPtrOffset(vm), currentfx_offset });

        // 9. STACK_SUMMARY
        const tos = stack.getTopOfStack(vm);
        const stack_depth = stack.getStackDepth(vm);
        const next_values = self.getNextStackValues(vm, 4);
        try append(&buffer, &pos, "D:{d} TOS:0x{x:0>8} N1:0x{x:0>8} N2:0x{x:0>8}|", .{ stack_depth, tos, next_values[0], next_values[1] });

        // 10. MEMORY_CONTEXT
        const BYTESPER_PAGE: usize = 512;
        const pc_vpage = pc_byte_offset / BYTESPER_PAGE;
        const pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
        try append(&buffer, &pos, "@mem:0x{x} [vpage:{} off:0x{x:0>3}]|", .{ @intFromPtr(vm.virtual_memory.?.ptr) + pc_byte_offset, pc_vpage, pc_offset_in_page });

        // 11. FP_VP_FO_VA - simplified, unknown file page mapping in Zig
        const virtual_address = pc_vpage * BYTESPER_PAGE;
        try append(&buffer, &pos, "FP:0 VP:{} FO:0x0 VA:0x{x:0>6}|", .{ pc_vpage, virtual_address });

        // 12. BS_MEM
        try append(&buffer, &pos, "BS:RAW MEM:", .{});
        if (vm.virtual_memory) |vmem| {
            var i: usize = 0;
            while (i < 4 and pc_byte_offset + i < vmem.len) : (i += 1) {
                try append(&buffer, &pos, "{x:0>2}", .{vmem[pc_byte_offset + i]});
            }
        }
        try append(&buffer, &pos, "|", .{});

        // 13. NOTES
        var notes: [31]u8 = [_]u8{' '} ** 31;
        if (pc_byte_offset == 0x307898) {
            @memcpy(notes[0.."PC_MISMATCH_CHECK".len], "PC_MISMATCH_CHECK");
        }
        try append(&buffer, &pos, "{s}", .{notes[0..30]});

        // Newline
        try append(&buffer, &pos, "\n", .{});

        // Write to file
        try self.log_file.?.writeAll(buffer[0..pos]);
    }

    /// Log instruction execution (single-line format per emulator_debug.log)
    pub fn logInstruction(self: *ExecutionTrace, vm: *VM, inst: Instruction) !void {
        if (self.log_file == null) {
            // DEBUG: Log why we're not writing
            if (self.instruction_count == 0) {
                std.debug.print("DEBUG execution_trace: log_file is null, skipping log\n", .{});
            }
            return;
        }

        // NOTE: Unified trace format disabled - using detailed format to match C emulator
        // Uncomment below to enable unified trace format for rapid comparison:
        // try self.unifiedTraceLog(vm, inst);

        // ENHANCED TRACING: Match C emulator's first instruction tracing
        const ENHANCED_TRACING = @import("builtin").mode == .Debug;
        if (ENHANCED_TRACING and self.instruction_count == 0) {
            std.debug.print("\n=== ENHANCED TRACING: First Instruction ===\n", .{});
            std.debug.print("DEBUG: PC byte offset = 0x{x}\n", .{vm.pc});

            // Log FuncObj and PC calculation
            if (vm.current_frame) |frame| {
                if (vm.virtual_memory) |vmem| {
                    const frame_addr = @intFromPtr(frame);
                    const vmem_addr = @intFromPtr(vmem.ptr);
                    if (frame_addr >= vmem_addr and frame_addr < vmem_addr + vmem.len) {
                        const frame_offset_in_vmem = frame_addr - vmem_addr;
                        const frame_bytes = vmem[frame_offset_in_vmem..][0..12];

                        // Read FX_FNHEADER (BIGVM: 32-bit at offset 4-7)
                        const fx_fnheader = std.mem.readInt(LispPTR, frame_bytes[4..8], .little);
                        const frame_pc = std.mem.readInt(DLword, frame_bytes[8..10], .little);

                        const funcobj_offset = (fx_fnheader & 0xFFFFFF) * 2;
                        const expected_pc = funcobj_offset + frame_pc;

                        std.debug.print("DEBUG: FX_FNHEADER = 0x{x:0>6} (DLword offset)\n", .{fx_fnheader & 0xFFFFFF});
                        std.debug.print("DEBUG: CURRENTFX->pc = {} bytes\n", .{frame_pc});
                        std.debug.print("DEBUG: FuncObj byte offset = 0x{x}\n", .{funcobj_offset});
                        std.debug.print("DEBUG: Expected PC = FuncObj + CURRENTFX->pc = 0x{x} + {} = 0x{x}\n", .{ funcobj_offset, frame_pc, expected_pc });
                        std.debug.print("DEBUG: Actual PC = 0x{x}\n", .{vm.pc});
                        const match = (vm.pc == expected_pc);
                        std.debug.print("DEBUG: Match check: actual_pc == expected_pc? {s}\n", .{if (match) "YES" else "NO"});

                        // Log bytes at PC
                        if (vm.pc < vmem.len and vm.pc + 8 <= vmem.len) {
                            std.debug.print("DEBUG: Bytes at PC (offset 0x{x}): ", .{vm.pc});
                            for (0..8) |i| {
                                std.debug.print("0x{x:0>2} ", .{vmem[vm.pc + i]});
                            }
                            std.debug.print("\n", .{});
                        }
                    }
                }
            }
            std.debug.print("=== END ENHANCED TRACING: First Instruction ===\n\n", .{});
        }

        // DEBUG: Log first few writes
        if (self.instruction_count < 3) {
            std.debug.print("DEBUG execution_trace: Writing instruction {} to log\n", .{self.instruction_count + 1});
        }

        const file = self.log_file.?;
        // The C trace lines can be long (many formatted fields). Use a large buffer
        // and write the full constructed line (no fixed-width truncation).
        var buffer: [4096]u8 = undefined;
        @memset(buffer[0..], ' '); // Initialize buffer with spaces (for column padding)
        var pos: usize = 0;

        // Helper to append formatted string
        const append = struct {
            fn append(buf: []u8, p: *usize, comptime fmt: []const u8, args: anytype) !void {
                const written = try std.fmt.bufPrint(buf[p.*..], fmt, args);
                p.* += written.len;
            }
        }.append;

        // PC is stored as byte offset in vm.pc
        // C emulator logs byte offset (changed from DLword offset)
        const pc_byte_offset = vm.pc;

        // Stop when PC reaches 0xf000d5 (in DLwords for comparison)
        const pc_dlword_for_stop = pc_byte_offset / 2;
        if (pc_dlword_for_stop >= 0xf000d5) {
            try append(&buffer, &pos, "STOP: PC=0x{x} reached target 0xf000d5, total instructions={}\n", .{ pc_dlword_for_stop, self.instruction_count });
            try file.writeAll(buffer[0..pos]);
            return;
        }

        // No instruction limit - log all instructions for step-by-step parity validation
        // Execution length is controlled by EMULATOR_MAX_STEPS in dispatch.zig
        self.instruction_count += 1;

        // Column 1-6: Instruction count (right-aligned, 5 digits + 1 space)
        try append(&buffer, &pos, "{d:>5} ", .{self.instruction_count});

        // Column 7-68: PC information (62 characters)
        // Calculate FuncObj byte offset (PC offset from FuncObj)
        // C: funcobj_byte_offset = (char *)PCMAC - (char *)FuncObj;
        const funcobj_byte_offset = self.getFuncObjOffset(vm);
        // AUTO: Format PC with hex, octal, and bit-shifted values (matching C format)
        // C format: "PC:0x%06x/0%011o(*2:0x%06x/0%011o /2:0x%06x/0%011o) Lisp+0x%06x/0%011o FuncObj+%5d "
        const pc_byte_uint = @as(u32, @intCast(pc_byte_offset));
        const pc_times_2 = pc_byte_uint << 1;
        const pc_div_2 = pc_byte_uint >> 1;
        try append(&buffer, &pos, "PC:0x{x:0>6}/0{o:0>11}(*2:0x{x:0>6}/0{o:0>11} /2:0x{x:0>6}/0{o:0>11}) Lisp+0x{x:0>6}/0{o:0>11} FuncObj", .{ pc_byte_uint, pc_byte_uint, pc_times_2, pc_times_2, pc_div_2, pc_div_2, pc_byte_uint, pc_byte_uint });
        // Format offset - C uses %5d which right-aligns and includes sign
        const offset_fmt = self.formatFuncObjOffset(funcobj_byte_offset);
        try append(&buffer, &pos, "+{s} ", .{offset_fmt[0..]});

        // Pad PC field to at least the historic "PC column" width, but never
        // move the write cursor backwards (that would overwrite already-written bytes).
        // Match the C trace alignment for the byte window start.
        const pc_field_end = 67;
        if (pos < pc_field_end) {
            while (pos < pc_field_end) : (pos += 1) {
                buffer[pos] = ' ';
            }
        } else {
            try append(&buffer, &pos, " ", .{});
        }

        // Column 69-88: Instruction bytes (20 characters: 8 bytes hex + 4 spaces)
        // CRITICAL: C emulator log shows bytes read DIRECTLY from PCMAC (no XOR addressing)
        // C: xc.c:584: *((unsigned char *)PCMAC + i) - direct memory access, no XOR
        // But opcode decoding uses Get_BYTE_PCMAC0 which applies XOR addressing
        // So we show bytes WITHOUT XOR to match C log format
        // C prints a fixed 8-byte window starting at PCMAC, regardless of instruction length.
        const bytes_to_show: usize = 8;
        if (vm.virtual_memory) |vmem| {
            var i: usize = 0;
            while (i < bytes_to_show and vm.pc + i < vmem.len) : (i += 1) {
                // Read directly from memory (no XOR) to match C log format
                // C: *((unsigned char *)PCMAC + i) - direct access
                const byte_value = vmem[@as(usize, @intCast(vm.pc)) + i];
                try append(&buffer, &pos, "{x:0>2}", .{byte_value});
            }
            // Pad to 8 bytes if needed
            while (i < 8) : (i += 1) {
                try append(&buffer, &pos, "00", .{});
            }
        } else {
            // No virtual memory - pad with zeros
            var i: usize = 0;
            while (i < 8) : (i += 1) {
                try append(&buffer, &pos, "00", .{});
            }
        }
        try append(&buffer, &pos, "    ", .{}); // 4 trailing spaces

        // Enriched log metadata used by the parity scripts.
        // Keep this aligned with the C trace enrichment:
        //   [vpage:<VP> off:0x<OFF>] [optional @mem:0x...] [FP:65535 VP:<VP> FO:0x0 VA:0x<PAGEBASE>] [MEM:<8 bytes>]
        if (vm.virtual_memory) |vmem| {
            const BYTESPER_PAGE: usize = 512;
            const pc_vpage = pc_byte_offset / BYTESPER_PAGE;
            const pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
            const va = pc_byte_offset - pc_offset_in_page;

            try append(&buffer, &pos, "[vpage:{} off:0x{x:0>3}]", .{ pc_vpage, pc_offset_in_page });
            if (self.instruction_count == 1) {
                const mem_addr = @intFromPtr(&vmem[@as(usize, @intCast(vm.pc))]);
                try append(&buffer, &pos, " @mem:0x{x}", .{mem_addr});
            }
            try append(&buffer, &pos, " [FP:65535 VP:{} FO:0x0 VA:0x{x:0>6}] [MEM:", .{ pc_vpage, @as(u32, @intCast(va)) });
            var i: usize = 0;
            while (i < 8) : (i += 1) {
                const b = if (vm.pc + i < vmem.len) vmem[@as(usize, @intCast(vm.pc)) + i] else 0;
                try append(&buffer, &pos, "{x:0>2}", .{b});
            }
            try append(&buffer, &pos, "]", .{});
        }

        // Column 89-128: Instruction name + parameters (40 characters, left-aligned)
        const opcode_name = switch (inst.opcode) {
            // C trace prints these constant opcodes as bare numbers.
            .CONST_0 => "0",
            .CONST_1 => "1",
            else => @tagName(inst.opcode),
        };
        var instr_buf: [41]u8 = undefined;
        @memset(instr_buf[0..], ' '); // Initialize with spaces
        var instr_pos: usize = 0;

        // Format instruction name (left-aligned, 18 chars max)
        const name_len = @min(opcode_name.len, 18);
        @memcpy(instr_buf[instr_pos .. instr_pos + name_len], opcode_name[0..name_len]);
        instr_pos += name_len;
        // Pad name to 18 chars
        while (instr_pos < 18) : (instr_pos += 1) {
            instr_buf[instr_pos] = ' ';
        }

        // Add parameters with actual values
        switch (inst.opcode) {
            .TYPEP => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .DTEST => {
                if (inst.operands.len >= 1) {
                    const atom_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "atom=0x{x:0>2}", .{inst.getByteOperand(0)});
                    instr_pos += atom_str.len;
                }
            },
            .UNWIND => {
                if (inst.operands.len >= 2) {
                    const p1 = inst.getByteOperand(0);
                    const p2 = inst.getByteOperand(1);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2} p2=0x{x:0>2}", .{ p1, p2 });
                    instr_pos += param_str.len;
                }
            },
            .RPLPTR_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .GVAR_ => {
                if (inst.operands.len >= 1) {
                    const atom_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "atom=0x{x:0>2}", .{inst.getByteOperand(0)});
                    instr_pos += atom_str.len;
                }
            },
            .FINDKEY => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .MISCN => {
                if (inst.operands.len >= 2) {
                    const p1 = inst.getByteOperand(0);
                    const p2 = inst.getByteOperand(1);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2} p2=0x{x:0>2}", .{ p1, p2 });
                    instr_pos += param_str.len;
                }
            },
            .TYPEMASK_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .STORE_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .COPY_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .IVARX, .PVARX, .FVARX, .PVARX_, .IVARX_, .FVARX_ => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .SICX => {
                if (inst.operands.len >= 2) {
                    const w = inst.getWordOperand(0);
                    const w_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "w=0x{x:0>4}", .{w});
                    instr_pos += w_str.len;
                }
            },
            .POP_N, .ATOMCELL_N, .GETBASE_N, .GETBASEPTR_N, .GETBITS_N_FD, .PUTBASE_N, .PUTBASEPTR_N, .PUTBITS_N_FD, .IPLUS_N, .IDIFFERENCE_N => {
                if (inst.operands.len >= 1) {
                    const param = inst.getByteOperand(0);
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2}", .{param});
                    instr_pos += param_str.len;
                }
            },
            .JUMPX, .JUMPXX, .FJUMPX, .TJUMPX, .NFJUMPX, .NTJUMPX => {
                if (inst.operands.len >= 2) {
                    const offset = inst.getSignedWordOperand(0);
                    const offset_unsigned = @as(u16, @bitCast(offset));
                    // Format signed offset with sign prefix manually
                    const sign = if (offset >= 0) "+" else "";
                    const jump_offset_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "off={s}{d}(0x{x:0>4})", .{ sign, offset, offset_unsigned });
                    instr_pos += jump_offset_str.len;
                }
            },
            else => {
                // Most opcodes have no parameters
            },
        }

        // Pad to 40 chars
        while (instr_pos < 40) : (instr_pos += 1) {
            instr_buf[instr_pos] = ' ';
        }
        try append(&buffer, &pos, "{s}", .{instr_buf[0..40]});

        // Column 129-298: Stack information (170 characters)
        const stack_ptr_offset = self.getStackPtrOffset(vm);
        const tos = stack.getTopOfStack(vm);
        const stack_depth = stack.getStackDepth(vm);

        // Get next 4 stack values
        const next_values = self.getNextStackValues(vm, 4);

        // AUTO: Format stack with hex, octal, and bit-shifted values (matching C format)
        // C format: "Stack: D:%5d/0%011o(*2:%5d /2:%5d) P:0x%05x/0%011o(*2:0x%05x /2:0x%05x) TOS:0x%016lx/0%022lo(*2:0x%016lx /2:0x%016lx) N:[0x%08x/0%011o 0x%08x/0%011o 0x%08x/0%011o 0x%08x/0%011o]"
        const stack_ptr_uint = @as(u32, @intCast(stack_ptr_offset));
        const tos_ul = @as(u64, @intCast(tos)); // tos is LispPTR (u32), cast to u64 for formatting
        try append(&buffer, &pos, "Stack: D:{d:>5}/0{o:0>11}(*2:{d:>5} /2:{d:>5}) P:0x{x:0>5}/0{o:0>11}(*2:0x{x:0>5} /2:0x{x:0>5}) TOS:0x{x:0>16}/0{o:0>22}(*2:0x{x:0>16} /2:0x{x:0>16}) N:[0x{x:0>8}/0{o:0>11} 0x{x:0>8}/0{o:0>11} 0x{x:0>8}/0{o:0>11} 0x{x:0>8}/0{o:0>11}]", .{ stack_depth, stack_depth, stack_depth << 1, stack_depth >> 1, stack_ptr_uint, stack_ptr_uint, stack_ptr_uint << 1, stack_ptr_uint >> 1, tos_ul, tos_ul, tos_ul << 1, tos_ul >> 1, next_values[0], next_values[0], next_values[1], next_values[1], next_values[2], next_values[2], next_values[3], next_values[3] });

        // Do not pad between Stack[...] and Frame[...] (C trace concatenates directly).

        // Column 299-461: Frame information (163 characters)
        if (vm.current_frame) |frame| {
            // CRITICAL: BIGVM mode uses direct fnheader field (32-bit LispPTR)
            // C: maiko/inc/stack.h:92-98 - BIGVM has LispPTR fnheader directly
            // C: maiko/src/xc.c:751-755 - BIGVM: fx_fnheader = CURRENTFX->fnheader
            // Read frame fields directly from memory to match actual layout
            const frame_addr = @intFromPtr(frame);
            const frame_offset_in_vmem = if (vm.virtual_memory) |vmem| blk: {
                const vmem_addr = @intFromPtr(vmem.ptr);
                if (frame_addr >= vmem_addr and frame_addr < vmem_addr + vmem.len) {
                    break :blk frame_addr - vmem_addr;
                } else {
                    // DEBUG: Frame not in virtual memory
                    if (self.instruction_count <= 3) {
                        std.debug.print("DEBUG execution_trace: Frame not in virtual memory (frame_addr=0x{x}, vmem_addr=0x{x}, vmem_len={})\n", .{ frame_addr, vmem_addr, vmem.len });
                    }
                    return; // Frame not in virtual memory, skip frame info
                }
            } else {
                // DEBUG: No virtual memory
                if (self.instruction_count <= 3) {
                    std.debug.print("DEBUG execution_trace: No virtual memory available\n", .{});
                }
                return;
            };

            // Non-BIGVM frame layout (after 32-bit byte-swap during page loading):
            // Offset 0-1: flags_usecount
            // Offset 2-3: alink
            // Offset 4-5: lofnheader (little-endian DLword)
            // Offset 6-7: hi1fnheader_hi2fnheader (little-endian DLword, hi2fnheader in low byte)
            // Offset 8-9: pc (BYTESWAP layout)
            // Offset 10-11: nextblock (BYTESWAP layout)
            const frame_bytes = vm.virtual_memory.?.ptr[frame_offset_in_vmem..][0..12];

            // DEBUG: Log frame reading for first few instructions
            if (self.instruction_count <= 3) {
                std.debug.print("DEBUG execution_trace: Reading frame at offset 0x{x}\n", .{frame_offset_in_vmem});
                std.debug.print("  Frame bytes [4-7] (fnheader): 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", .{ frame_bytes[4], frame_bytes[5], frame_bytes[6], frame_bytes[7] });
            }

            // CRITICAL: Non-BIGVM frame layout (matching vm_initialization.zig)
            // After 32-bit byte-swap during page loading, the layout is:
            //   [4,5]=lofnheader, [6,7]=hi1fnheader_hi2fnheader
            //   hi2fnheader is in the LOW byte (bits 0-7) of [6,7]
            // FX_FNHEADER = (hi2fnheader << 16) | lofnheader (24-bit)
            const lofnheader = std.mem.readInt(DLword, frame_bytes[4..6], .little);
            const hi1fnheader_hi2fnheader = std.mem.readInt(DLword, frame_bytes[6..8], .little);
            const hi2fnheader: u8 = @as(u8, @truncate(hi1fnheader_hi2fnheader & 0xFF));
            const fx_fnheader = (@as(LispPTR, hi2fnheader) << 16) | lofnheader;
            const fx_fnheader_24bit = fx_fnheader & 0xFFFFFF; // Mask to 24 bits for non-BIGVM

            // DEBUG: Log fnheader value
            if (self.instruction_count <= 3) {
                std.debug.print("  fx_fnheader (non-BIGVM, 24-bit): 0x{x:0>6}\n", .{fx_fnheader_24bit});
            }

            // CRITICAL: Frame field order is SWAPPED in actual memory layout!
            // Actual layout: [8-9] = pc, [10-11] = nextblock (not as struct suggests)
            // This matches the fix in dispatch.zig:307-308
            const frame_pc = std.mem.readInt(DLword, frame_bytes[8..10], .little); // SWAPPED: pc is at [8-9]
            const frame_nextblock = std.mem.readInt(DLword, frame_bytes[10..12], .little); // SWAPPED: nextblock is at [10-11]

            // Calculate CURRENTFX offset from Stackspace in DLwords
            const currentfx_offset = self.getCurrentFXOffset(vm);

            // Calculate FuncObj byte offset from Lisp_world
            // FuncObj DLword offset = FX_FNHEADER, byte offset = FX_FNHEADER * 2
            const funcobj_dlword = fx_fnheader_24bit;
            const funcobj_lisp_offset = funcobj_dlword * 2;

            // AUTO: Format frame with hex, octal, and bit-shifted values (matching C format)
            // C format: "Frame: FX:%5d/0%011o(*2:%5d /2:%5d) FH:0x%06x/0%011o(*2:0x%06x/0%011o /2:0x%06x/0%011o) PC:%5d/0%011o(*2:%5d /2:%5d) NB:%5d/0%011o(*2:%5d /2:%5d) FO:+%5d/0%011o(*2:%5d /2:%5d)"
            const fx_fnheader_uint = @as(u32, @intCast(fx_fnheader_24bit));
            try append(&buffer, &pos, "Frame: FX:{d:>5}/0{o:0>11}(*2:{d:>5} /2:{d:>5}) FH:0x{x:0>6}/0{o:0>11}(*2:0x{x:0>6}/0{o:0>11} /2:0x{x:0>6}/0{o:0>11}) PC:{d:>5}/0{o:0>11}(*2:{d:>5} /2:{d:>5}) NB:{d:>5}/0{o:0>11}(*2:{d:>5} /2:{d:>5}) FO:+{d:>5}/0{o:0>11}(*2:{d:>5} /2:{d:>5})", .{ currentfx_offset, currentfx_offset, currentfx_offset << 1, currentfx_offset >> 1, fx_fnheader_uint, fx_fnheader_uint, fx_fnheader_uint << 1, fx_fnheader_uint << 1, fx_fnheader_uint >> 1, fx_fnheader_uint >> 1, frame_pc, frame_pc, frame_pc << 1, frame_pc >> 1, frame_nextblock, frame_nextblock, frame_nextblock << 1, frame_nextblock >> 1, funcobj_lisp_offset, funcobj_lisp_offset, funcobj_lisp_offset << 1, funcobj_lisp_offset >> 1 });

            // Optional frame padding: do not move cursor backwards.
            const frame_end = 461;
            if (pos < frame_end) {
                while (pos < frame_end and pos < buffer.len) : (pos += 1) {
                    buffer[pos] = ' ';
                }
            } else {
                try append(&buffer, &pos, " ", .{});
            }
        } else {
            // No frame - use dashes
            try append(&buffer, &pos, "Frame: FX:----- FH:------ PC:----- NB:----- FO:-----", .{});

            const frame_end = 461;
            if (pos < frame_end) {
                while (pos < frame_end and pos < buffer.len) : (pos += 1) {
                    buffer[pos] = ' ';
                }
            } else {
                try append(&buffer, &pos, " ", .{});
            }
        }

        // Append newline and write the constructed line.
        if (pos + 1 >= buffer.len) {
            std.debug.print("ERROR: Execution trace line exceeded buffer size (pos={}, max={})\n", .{ pos, buffer.len });
            return;
        }
        buffer[pos] = '\n';
        pos += 1;
        try file.writeAll(buffer[0..pos]);

        // DEBUG: Verify write for first few instructions
        if (self.instruction_count <= 3) {
            std.debug.print("DEBUG execution_trace: Wrote {} bytes to log file (instruction {})\n", .{ pos, self.instruction_count });
        }
    }

    // TODO: Implement parameter formatting - would need proper buffer management
    // For now, parameters are not included in instruction name field

    fn getFuncObjOffset(self: *ExecutionTrace, vm: *VM) i32 {
        _ = self;
        // C trace prints: `FuncObj+%5d` where that offset is:
        //   (char *)PCMAC - (char *)FuncObj
        // i.e. the *current* PC offset within the function object, not the saved CURRENTFX->pc field.
        if (vm.current_frame == null or vm.virtual_memory == null) return 0;

        const frame = vm.current_frame.?;
        const vmem = vm.virtual_memory.?;
        const frame_addr = @intFromPtr(frame);
        const vmem_addr = @intFromPtr(vmem.ptr);
        if (frame_addr < vmem_addr or frame_addr + 12 > vmem_addr + vmem.len) return 0;

        const frame_offset_in_vmem: usize = @intCast(frame_addr - vmem_addr);
        const frame_bytes = vmem[frame_offset_in_vmem..][0..12];

        // Non-BIGVM frame layout (matching the existing frame formatter below):
        // FX_FNHEADER is a 24-bit LispPTR stored as:
        //   lofnheader (DLword) at [4..6]
        //   hi2fnheader (u8) in low byte of [6..8]
        const lofnheader = std.mem.readInt(DLword, frame_bytes[4..6], .little);
        const hi1fnheader_hi2fnheader = std.mem.readInt(DLword, frame_bytes[6..8], .little);
        const hi2fnheader: u8 = @as(u8, @truncate(hi1fnheader_hi2fnheader & 0xFF));
        const fx_fnheader_24: LispPTR = (((@as(LispPTR, hi2fnheader) << 16) | lofnheader) & 0xFFFFFF);

        const funcobj_byte_base: u32 = @as(u32, @intCast(fx_fnheader_24)) * 2;
        const pc_byte: u32 = @as(u32, @intCast(vm.pc));
        if (pc_byte < funcobj_byte_base) return 0;
        return @as(i32, @intCast(pc_byte - funcobj_byte_base));
    }

    fn getCurrentFXOffset(self: *ExecutionTrace, vm: *VM) usize {
        _ = self;
        if (vm.current_frame) |frame| {
            // Calculate frame offset from Stackspace in DLwords
            // Stackspace is at STK_OFFSET (0x00010000 DLwords = 0x20000 bytes) from Lisp_world
            const STK_OFFSET: u32 = 0x00010000; // DLword offset
            const stackspace_byte_offset = STK_OFFSET * 2;

            // Frame is in virtual memory, calculate its byte offset from start
            if (vm.virtual_memory) |vmem| {
                const frame_addr = @intFromPtr(frame);
                const vmem_addr = @intFromPtr(vmem.ptr);
                const frame_byte_offset = frame_addr - vmem_addr;

                // Convert to DLword offset from Stackspace
                if (frame_byte_offset >= stackspace_byte_offset) {
                    const offset_dlwords = (frame_byte_offset - stackspace_byte_offset) / 2;
                    return @as(usize, @intCast(offset_dlwords));
                }
            }
        }
        return 0;
    }

    fn getStackPtrOffset(self: *ExecutionTrace, vm: *VM) usize {
        _ = self;
        // C: xc.c:745: LispPTR stack_ptr_offset = (LispPTR)((char *)CurrentStackPTR - (char *)Stackspace) / 2;
        // Stackspace is the base (lower address), CurrentStackPTR is current top (higher address when stack has data)
        // Stack grows down, but CurrentStackPTR > Stackspace when stack has data
        // C uses byte addresses and divides by 2 to get DLword offset
        const stack_base_ptr = @intFromPtr(vm.stack_base);
        const stack_ptr = @intFromPtr(vm.stack_ptr);
        // Offset = (ptr - base) / 2 (in DLwords) - matches C exactly
        const diff = if (stack_ptr >= stack_base_ptr)
            stack_ptr - stack_base_ptr
        else
            0;
        return @as(usize, @intCast(diff / 2)); // Divide by 2 to get DLword offset (matches C)
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
            // Read LispPTR from stack_slot - must match C's GetLongWord behavior
            // C's GetLongWord reads 4 bytes in big-endian format from sysout
            const stack_slot_bytes: [*]const u8 = @ptrCast(stack_slot);
            const value: LispPTR = (@as(LispPTR, stack_slot_bytes[0]) << 24) |
                (@as(LispPTR, stack_slot_bytes[1]) << 16) |
                (@as(LispPTR, stack_slot_bytes[2]) << 8) |
                @as(LispPTR, stack_slot_bytes[3]);
            values[i] = value;
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
