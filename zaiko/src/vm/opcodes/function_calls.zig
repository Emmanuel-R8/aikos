const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");
const function_module = @import("../function.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;
const FunctionHeader = function_module.FunctionHeader;

/// FN0-FN4: Function call opcodes with fixed argument counts
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/function-calls.md
/// Matches C implementation: maiko/inc/tosfns.h:OPFN
///
/// FN0 (0x08): Call function with 0 arguments
/// FN1 (0x09): Call function with 1 argument
/// FN2 (0x0A): Call function with 2 arguments
/// FN3 (0x0B): Call function with 3 arguments
/// FN4 (0x0C): Call function with 4 arguments
///
/// Instruction format: [opcode][atom_index_byte]
/// Stack: [arg_N, ..., arg_0, function_obj] -> []
pub fn handleFN0(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 0);
}

pub fn handleFN1(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 1);
}

pub fn handleFN2(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 2);
}

pub fn handleFN3(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 3);
}

pub fn handleFN4(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction) errors.VMError!void {
    try handleFN(vm, instruction, 4);
}

/// Common FN handler implementation
/// Matches C implementation: maiko/inc/tosfns.h:OPFN
fn handleFN(vm: *VM, instruction: *const @import("../dispatch.zig").Instruction, arg_count: u8) errors.VMError!void {

    // Get atom index from instruction operand (2 bytes after opcode for non-BIGATOMS)
    // C: Get_AtomNo_PCMAC1 = Get_DLword_PCMAC1 = Get_DLword(PCMAC + 1)
    // C: Get_AtomNo(ptr) = Get_DLword(ptr) for non-BIGATOMS (no subtraction)
    // Read DLword operand (little-endian, pages already byte-swapped on load)
    const atom_index_raw: u16 = instruction.getWordOperand(0); // DLword (2 bytes)
    const atom_index: LispPTR = @as(LispPTR, atom_index_raw);
    
    std.debug.print("DEBUG handleFN: atom_index_raw=0x{x:0>4} ({d}), atom_index=0x{x} ({d})\n", 
        .{ atom_index_raw, atom_index_raw, atom_index, atom_index });
    std.debug.print("  atom_index / 2: {d} (0x{x})\n", .{ atom_index / 2, atom_index / 2 });
    std.debug.print("  atom_index * 2: {d} (0x{x})\n", .{ atom_index * 2, atom_index * 2 });

    // Lookup function definition from atom table
    // C: defcell = GetDEFCELL68k(atom_index)
    const defcell_module = @import("../../data/defcell.zig");
    std.debug.print("DEBUG handleFN: Calling readDefCell with atom_index=0x{x}\n", .{atom_index});
    const defcell = defcell_module.readDefCell(vm, atom_index) catch |err| {
        std.debug.print("ERROR handleFN: readDefCell failed with error: {}\n", .{err});
        std.debug.print("  atom_index=0x{x} ({d})\n", .{ atom_index, atom_index });
        return err;
    };
    
    // Check if C code (ccodep flag)
    // C: if (!(fn_defcell->ccodep)) { /* it's not a CCODEP */ }
    if (defcell_module.isCCode(defcell)) {
        // C code function - TODO: Handle C code functions
        // For now, return error (C functions not yet supported)
        return errors.VMError.InvalidOpcode;
    }
    
    // Lisp function - get function header from defpointer
    // C: LOCFNCELL = (struct fnhead *)NativeAligned4FromLAddr((defcell_word &= POINTERMASK))
    const fnheader_ptr = defcell_module.getFunctionHeader(defcell);
    
    // CRITICAL: Check if defpointer is valid (not 0/NIL)
    // C: op_fn_common checks GetTypeNumber(defcell->defpointer) == TYPE_COMPILED_CLOSURE
    // If not a compiled closure, uses ATOM_INTERPRETER
    // For now, if defpointer is 0, treat as undefined function (should trigger UFN)
    if (fnheader_ptr == 0) {
        std.debug.print("WARNING handleFN: defpointer is 0 (no function definition) for atom_index=0x{x}\n", .{atom_index});
        std.debug.print("  This should trigger UFN (Undefined Function Name) lookup\n", .{});
        // TODO: Implement UFN lookup (op_ufn in C code)
        // For now, return error to prevent crash
        return errors.VMError.InvalidOpcode;
    }
    
    std.debug.print("DEBUG handleFN: fnheader_ptr=0x{x}\n", .{fnheader_ptr});
    
    // Read function header from memory
    var fnheader = readFunctionHeader(vm, fnheader_ptr) catch |err| {
        std.debug.print("ERROR handleFN: readFunctionHeader failed with error: {}\n", .{err});
        std.debug.print("  fnheader_ptr=0x{x}\n", .{fnheader_ptr});
        return err;
    };
    
    // Call function with argument count
    // C: OPFN sets up frame, pushes TOS, handles spread args, sets up BF/FX markers
    try function_module.callFunction(vm, &fnheader, arg_count);
}

/// RETURN: Return from function
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/function-calls.md
/// Matches C implementation: maiko/inc/tosret.h:OPRETURN
pub fn handleRETURN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Get return value and restore frame
    // C: OPRETURN gets return value from TOPOFSTACK, restores previous frame via alink
    const return_value = try function_module.returnFromFunction(vm);

    // Set return value on stack (TOS)
    // C: TOPOFSTACK is preserved through frame restoration
    stack_module.setTopOfStack(vm, return_value);
}

/// Read function header from memory
/// C: (struct fnhead *)NativeAligned4FromLAddr(fnheader_ptr)
fn readFunctionHeader(vm: *VM, fnheader_ptr: LispPTR) errors.VMError!FunctionHeader {
    const errors_module = @import("../../utils/errors.zig");
    const virtual_memory = vm.virtual_memory orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    const fptovp_table = vm.fptovp orelse {
        return errors_module.VMError.MemoryAccessFailed;
    };
    
    // Translate address to native pointer
    const native_ptr = virtual_memory_module.translateAddress(fnheader_ptr, fptovp_table, 4) catch {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Read function header (big-endian from sysout)
    const byte_offset = @intFromPtr(native_ptr) - @intFromPtr(virtual_memory.ptr);
    if (byte_offset + @sizeOf(FunctionHeader) > virtual_memory.len) {
        return errors_module.VMError.InvalidAddress;
    }
    
    const header_bytes = virtual_memory[byte_offset..][0..@sizeOf(FunctionHeader)];
    
    // Function header structure (matches C fnhead):
    // stkmin: DLword (2 bytes, big-endian)
    // na: short (2 bytes, signed, big-endian)
    // pv: short (2 bytes, signed, big-endian)
    // startpc: DLword (2 bytes, big-endian)
    // flags: 1 byte (nil4:1, byteswapped:1, argtype:2, framename:28/24 bits)
    // framename: 28 bits (BIGVM) or 24 bits (non-BIGVM) - part of flags byte
    // ntsize: DLword (2 bytes)
    // nlocals: u8 (1 byte)
    // fvaroffset: u8 (1 byte)
    
    // Read fields with byte swapping
    const stkmin: DLword = (@as(DLword, header_bytes[0]) << 8) | @as(DLword, header_bytes[1]);
    const na_signed: i16 = (@as(i16, @bitCast(@as(u16, header_bytes[2]) << 8 | header_bytes[3])));
    const pv_signed: i16 = (@as(i16, @bitCast(@as(u16, header_bytes[4]) << 8 | header_bytes[5])));
    const startpc: DLword = (@as(DLword, header_bytes[6]) << 8) | @as(DLword, header_bytes[7]);
    
    // Read flags byte (byte 8)
    const flags_byte = header_bytes[8];
    _ = (flags_byte >> 7) & 1; // nil4 (unused for now)
    _ = (flags_byte >> 6) & 1; // byteswapped (unused for now)
    _ = @as(u2, @truncate((flags_byte >> 4) & 3)); // argtype (unused for now)
    const framename_low = @as(u4, @truncate(flags_byte & 0xF));
    
    // Read framename high bits (bytes 9-11 for BIGVM, bytes 9-10 for non-BIGVM)
    // For BIGVM: framename is 28 bits total (4 bits from flags + 24 bits from next 3 bytes)
    // For non-BIGVM: framename is 24 bits total (4 bits from flags + 20 bits from next 2.5 bytes)
    // Assuming BIGVM for now
    const framename_mid = @as(u24, header_bytes[9]) << 16 | @as(u24, header_bytes[10]) << 8 | @as(u24, header_bytes[11]);
    const framename: LispPTR = (@as(LispPTR, framename_low) << 24) | framename_mid;
    
    const ntsize: DLword = (@as(DLword, header_bytes[12]) << 8) | @as(DLword, header_bytes[13]);
    const nlocals = header_bytes[14];
    const fvaroffset = header_bytes[15];
    
    // Convert signed na and pv to unsigned (stored as DLword in FunctionHeader)
    const na: DLword = @as(DLword, @bitCast(na_signed));
    const pv: DLword = @as(DLword, @bitCast(pv_signed));
    
    return FunctionHeader{
        .stkmin = stkmin,
        .na = na,
        .pv = pv,
        .startpc = startpc,
        .framename = framename,
        .ntsize = ntsize,
        .nlocals = @as(DLword, nlocals),
        .fvaroffset = @as(DLword, fvaroffset),
    };
}
