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

    /// Log instruction execution (single-line format per emulator_debug.log)
    pub fn logInstruction(self: *ExecutionTrace, vm: *VM, inst: Instruction) !void {
        if (self.log_file == null) {
            // DEBUG: Log why we're not writing
            if (self.instruction_count == 0) {
                std.debug.print("DEBUG execution_trace: log_file is null, skipping log\n", .{});
            }
            return;
        }
        
        // DEBUG: Log first few writes
        if (self.instruction_count < 3) {
            std.debug.print("DEBUG execution_trace: Writing instruction {} to log\n", .{self.instruction_count + 1});
        }

        const file = self.log_file.?;
        var buffer: [512]u8 = undefined;
        @memset(buffer[0..], ' '); // Initialize buffer with spaces
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

        self.instruction_count += 1;

        // Column 1-6: Instruction count (right-aligned, 5 digits + 1 space)
        try append(&buffer, &pos, "{d:>5} ", .{self.instruction_count});

        // Column 7-68: PC information (62 characters)
        // Calculate FuncObj byte offset (PC offset from FuncObj)
        // C: funcobj_byte_offset = (char *)PCMAC - (char *)FuncObj;
        const funcobj_byte_offset = self.getFuncObjOffset(vm);
        // Format PC field - use byte offset (matching C emulator which was changed to print pc_byte_offset)
        // C format: "PC: 0x%06x (Lisp_world+0x%06x, FuncObj+%5d bytes) "
        // Note: C uses %5d (right-aligned, 5 digits) for the offset, which can be negative
        try append(&buffer, &pos, "PC: 0x{x:0>6} (Lisp_world+0x{x:0>6}, FuncObj", .{ 
            pc_byte_offset, pc_byte_offset 
        });
        // Format offset - C uses %5d which right-aligns and includes sign
        // C format: "FuncObj+%5d bytes)" where %5d can be negative
        // Helper function to format exactly like C's %5d (returns 5-char string)
        const offset_fmt = self.formatFuncObjOffset(funcobj_byte_offset);
        try append(&buffer, &pos, "+{s} bytes) ", .{offset_fmt[0..]});
        
        // Pad PC field to exactly 62 characters (column 7-68)
        const pc_field_end = 68;   // Column 68 (0-indexed: 67, but we want to be at 68)
        while (pos < pc_field_end) : (pos += 1) {
            buffer[pos] = ' ';
        }
        pos = pc_field_end; // Ensure we're at column 69

        // Column 69-88: Instruction bytes (20 characters: 8 bytes hex + 4 spaces)
        // DEBUG: Log actual memory read to verify we're reading from correct location
        const bytes_to_show = @min(8, inst.length);
        if (vm.virtual_memory) |vmem| {
            var i: usize = 0;
            while (i < bytes_to_show and vm.pc + i < vmem.len) : (i += 1) {
                const byte_value = vmem[vm.pc + i];
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
        
        // DEBUG: Add memory verification info (only for first few instructions to avoid log bloat)
        // This matches the C emulator's debug output format
        if (self.instruction_count <= 5) {
            if (vm.virtual_memory) |vmem| {
                // Log virtual page and offset for debugging
                // BYTESPER_PAGE = 512
                const BYTESPER_PAGE: usize = 512;
                const pc_vpage = pc_byte_offset / BYTESPER_PAGE;
                const pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
                try append(&buffer, &pos, "[vpage:{} off:0x{x:0>3}]", .{ pc_vpage, pc_offset_in_page });
                
                // Also log the actual memory address being read from (for first instruction only)
                if (self.instruction_count == 1) {
                    const mem_addr = @intFromPtr(&vmem[vm.pc]);
                    try append(&buffer, &pos, " @mem:0x{x}", .{mem_addr});
                }
            }
        }

        // Column 89-128: Instruction name + parameters (40 characters, left-aligned)
        const opcode_name = @tagName(inst.opcode);
        var instr_buf: [41]u8 = undefined;
        @memset(instr_buf[0..], ' '); // Initialize with spaces
        var instr_pos: usize = 0;
        
        // Format instruction name (left-aligned, 18 chars max)
        const name_len = @min(opcode_name.len, 18);
        @memcpy(instr_buf[instr_pos..instr_pos + name_len], opcode_name[0..name_len]);
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
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2} p2=0x{x:0>2}", .{p1, p2});
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
                    const param_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "p1=0x{x:0>2} p2=0x{x:0>2}", .{p1, p2});
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
            .POP_N, .ATOMCELL_N, .GETBASE_N, .GETBASEPTR_N, .GETBITS_N_FD, 
            .PUTBASE_N, .PUTBASEPTR_N, .PUTBITS_N_FD, .IPLUS_N, .IDIFFERENCE_N => {
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
                    const jump_offset_str = try std.fmt.bufPrint(instr_buf[instr_pos..], "off={s}{d}(0x{x:0>4})", .{sign, offset, offset_unsigned});
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
        
        // Column 129-298: Stack information (170 characters)
        try append(&buffer, &pos, 
            "Stack: D:{d:>5} P:{d:>5} TOS:0x{x:0>16} N:[0x{x:0>8} 0x{x:0>8} 0x{x:0>8} 0x{x:0>8}]",
            .{ stack_depth, stack_ptr_offset, tos, 
               next_values[0], next_values[1], next_values[2], next_values[3] });
        
        // Pad stack field to 170 characters (column 129-298)
        const stack_end = 298;   // Column 298 (0-indexed: 297, but we want to be at 298)
        while (pos < stack_end) : (pos += 1) {
            buffer[pos] = ' ';
        }
        pos = stack_end; // Ensure we're at column 299

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
                        std.debug.print("DEBUG execution_trace: Frame not in virtual memory (frame_addr=0x{x}, vmem_addr=0x{x}, vmem_len={})\n", 
                            .{ frame_addr, vmem_addr, vmem.len });
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
            
            // BIGVM frame layout (maiko/inc/stack.h:81-110):
            // Offset 0-1: flags_usecount
            // Offset 2-3: alink
            // Offset 4-7: fnheader (LispPTR, 32-bit) - BIGVM ONLY
            // Offset 8-9: nextblock
            // Offset 10-11: pc
            const frame_bytes = vm.virtual_memory.?.ptr[frame_offset_in_vmem..][0..12];
            
            // DEBUG: Log frame reading for first few instructions
            if (self.instruction_count <= 3) {
                std.debug.print("DEBUG execution_trace: Reading frame at offset 0x{x}\n", .{frame_offset_in_vmem});
                std.debug.print("  Frame bytes [4-7] (fnheader): 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", 
                    .{ frame_bytes[4], frame_bytes[5], frame_bytes[6], frame_bytes[7] });
            }
            
            // BIGVM: Read fnheader as 32-bit LispPTR directly (offset 4-7)
            const fx_fnheader = std.mem.readInt(LispPTR, frame_bytes[4..8], .little);
            
            // DEBUG: Log fnheader value
            if (self.instruction_count <= 3) {
                std.debug.print("  fx_fnheader (BIGVM, 32-bit): 0x{x:0>8}\n", .{fx_fnheader});
            }
            
            // CRITICAL: Frame field order is SWAPPED in actual memory layout!
            // Actual layout: [8-9] = pc, [10-11] = nextblock (not as struct suggests)
            // This matches the fix in dispatch.zig:307-308
            const frame_pc = std.mem.readInt(DLword, frame_bytes[8..10], .little);  // SWAPPED: pc is at [8-9]
            const frame_nextblock = std.mem.readInt(DLword, frame_bytes[10..12], .little); // SWAPPED: nextblock is at [10-11]
            
            // Calculate CURRENTFX offset from Stackspace in DLwords
            const currentfx_offset = self.getCurrentFXOffset(vm);
            
            // Calculate FuncObj byte offset from Lisp_world
            // FuncObj DLword offset = FX_FNHEADER, byte offset = FX_FNHEADER * 2
            const funcobj_dlword = fx_fnheader & 0xFFFFFF;
            const funcobj_lisp_offset = funcobj_dlword * 2;
            
            // DEBUG: Log calculated values
            if (self.instruction_count <= 3) {
                std.debug.print("  Calculated: FX={d} FH=0x{x:0>6} PC={d} NB={d} FO=+{d}\n", 
                    .{ currentfx_offset, fx_fnheader & 0xFFFFFF, frame_pc, frame_nextblock, funcobj_lisp_offset });
            }
            
            try append(&buffer, &pos,
                "Frame: FX:{d:>5} FH:0x{x:0>6} PC:{d:>5} NB:{d:>5} FO:+{d:>5}",
                .{ currentfx_offset, fx_fnheader & 0xFFFFFF, frame_pc, 
                   frame_nextblock, funcobj_lisp_offset });
            
            // Pad frame field to 163 characters (column 299-461)
            // Frame field should end at column 461 (index 461)
            const frame_start_pos = 298; // Column 299 (0-indexed: 298)
            const frame_end = 461;       // Column 461 (0-indexed: 461)
            // Calculate how many spaces we need to pad
            const frame_content_len = pos - frame_start_pos;
            const padding_needed = 163 - frame_content_len;
            // Add padding spaces
            var i: usize = 0;
            while (i < padding_needed and pos < buffer.len and pos < frame_end) : (i += 1) {
                buffer[pos] = ' ';
                pos += 1;
            }
            // Ensure we're at the frame end position
            if (pos < frame_end) {
                pos = frame_end;
            }
        } else {
            // No frame - use dashes
            try append(&buffer, &pos, "Frame: FX:----- FH:------ PC:----- NB:----- FO:-----", .{});
            
            // Pad to 163 characters
            const frame_start_pos = 298;
            const frame_end = 461;
            const no_frame_content_len = pos - frame_start_pos; // "Frame: FX:----- ..." length
            const padding_needed = 163 - no_frame_content_len;
            // Add padding spaces
            var i: usize = 0;
            while (i < padding_needed and pos < buffer.len and pos < frame_end) : (i += 1) {
                buffer[pos] = ' ';
                pos += 1;
            }
            // Ensure we're at the frame end position
            if (pos < frame_end) {
                pos = frame_end;
            }
        }
        
        // Final padding: ensure exactly 461 characters before newline
        // This handles any remaining padding needed after frame field
        while (pos < 461 and pos < buffer.len) : (pos += 1) {
            buffer[pos] = ' ';
        }
        
        // Set pos to 461 to ensure we're at the right position
        pos = 461;
        
        // Add newline at position 461
        if (461 < buffer.len) {
            buffer[461] = '\n';
            pos = 462;
        } else {
            std.debug.print("ERROR: Buffer too small for newline\n", .{});
            return;
        }
        
        // Write exactly 462 characters (461 + newline) to file
        try file.writeAll(buffer[0..462]);
        
        // DEBUG: Verify write for first few instructions
        if (self.instruction_count <= 3) {
            std.debug.print("DEBUG execution_trace: Wrote {} bytes to log file (instruction {})\n", 
                .{ 462, self.instruction_count });
        }
    }

    // TODO: Implement parameter formatting - would need proper buffer management
    // For now, parameters are not included in instruction name field

    fn getFuncObjOffset(self: *ExecutionTrace, vm: *VM) i32 {
        _ = self;
        // Calculate PC offset from FuncObj
        // C: funcobj_byte_offset = (char *)PCMAC - (char *)FuncObj;
        // FuncObj is calculated from current frame's FX_FNHEADER
        // C: FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER);
        // NativeAligned4FromLAddr treats FX_FNHEADER as DLword offset, so:
        // FuncObj byte offset from Lisp_world = FX_FNHEADER * 2
        if (vm.current_frame) |frame| {
            // CRITICAL: BIGVM mode - read fnheader directly from memory as 32-bit LispPTR
            // C: maiko/inc/stack.h:92-98 - BIGVM has LispPTR fnheader at offset 4-7
            const frame_addr = @intFromPtr(frame);
            const frame_offset_in_vmem = if (vm.virtual_memory) |vmem| blk: {
                const vmem_addr = @intFromPtr(vmem.ptr);
                if (frame_addr >= vmem_addr and frame_addr < vmem_addr + vmem.len) {
                    break :blk frame_addr - vmem_addr;
                } else {
                    return 0; // Frame not in virtual memory
                }
            } else return 0;
            
            // BIGVM: Read fnheader as 32-bit LispPTR directly (offset 4-7)
            const frame_bytes = vm.virtual_memory.?.ptr[frame_offset_in_vmem..][0..8];
            const fx_fnheader = std.mem.readInt(LispPTR, frame_bytes[4..8], .little);
            
            // FX_FNHEADER is a DLword offset (32-bit in BIGVM, but we use 24-bit mask)
            // FuncObj byte offset from Lisp_world = FX_FNHEADER * 2
            const funcobj_dlword_offset = fx_fnheader & 0xFFFFFF;
            const funcobj_byte_offset_from_lisp_world = funcobj_dlword_offset * 2;
            
            // PC byte offset from Lisp_world
            const pc_byte_offset = vm.pc;
            
            // PC offset from FuncObj = PC_byte_offset - FuncObj_byte_offset_from_lisp_world
            // This matches C: (char *)PCMAC - (char *)FuncObj
            // Note: PCMAC = PC (current instruction pointer)
            const pc_offset_from_funcobj = @as(i64, @intCast(pc_byte_offset)) - @as(i64, @intCast(funcobj_byte_offset_from_lisp_world));
            // Use @truncate to handle overflow gracefully (C would also truncate)
            return @truncate(pc_offset_from_funcobj);
        }
        return 0;
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
        // C: stack_ptr_offset = (CurrentStackPTR - Stackspace) / 2
        // Stackspace is the base (lower address), CurrentStackPTR is current top (higher address when stack has data)
        // Stack grows down, but CurrentStackPTR > Stackspace when stack has data
        const stack_base_ptr = @intFromPtr(vm.stack_base);
        const stack_ptr = @intFromPtr(vm.stack_ptr);
        // Offset = (ptr - base) / 2 (in DLwords)
        const diff = if (stack_ptr >= stack_base_ptr)
            stack_ptr - stack_base_ptr
        else
            0;
        return @as(usize, @intCast(diff / 2)); // Divide by 2 to get DLwords
    }

    fn getNextStackValues(self: *ExecutionTrace, vm: *VM, count: usize) [4]LispPTR {
        _ = self;
        var values: [4]LispPTR = [_]LispPTR{0} ** 4;
        const stack_ptr_addr = @intFromPtr(vm.stack_ptr);
        const stack_base_addr = @intFromPtr(vm.stack_base);
        
        // Stack grows down, so we read from stack_ptr (which points to next push location)
        // Top of stack is at stack_ptr - 2 DLwords (one LispPTR)
        // Next values are at stack_ptr - 4, stack_ptr - 6, etc.
        var i: usize = 0;
        while (i < count and i < 4) : (i += 1) {
            // Each LispPTR is 2 DLwords = 4 bytes
            // Read from stack_ptr - (i+1)*4 bytes (going down the stack)
            const offset_bytes = (i + 1) * 4;
            const stack_slot_addr = stack_ptr_addr - offset_bytes;
            if (stack_slot_addr >= stack_base_addr) {
                const stack_slot: [*]DLword = @ptrFromInt(stack_slot_addr);
                const low = stack_slot[0];
                const high = stack_slot[1];
                values[i] = (@as(LispPTR, high) << 16) | @as(LispPTR, low);
            }
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