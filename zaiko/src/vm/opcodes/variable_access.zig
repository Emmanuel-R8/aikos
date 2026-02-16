const std = @import("std");
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const function_header_module = @import("../../data/function_header.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;

// ============================================================================
// Variable Access Opcodes
// ============================================================================

/// IVAR0-IVAR6: Local variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handleIVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // IVar access: variables stored at nextblock offset
    // CRITICAL: frame.nextblock is a DLword offset from Stackspace, not a LispPTR
    // C: IVar = NativeAligned2FromStackOffset(CURRENTFX->nextblock) = Stackspace + nextblock
    // C: IVAR = ((LispPTR *)IVar), so IVAR[x] reads LispPTR at offset x from IVar
    // CRITICAL: IVar points to STACK MEMORY, not virtual memory!
    const nextblock = stack_module.getNextblock(frame);
    if (nextblock == 0) {
        try stack_module.pushStack(vm, 0); // No IVar area - return NIL
        return;
    }

    // Calculate IVar pointer: Stackspace + nextblock (in DLwords)
    // C: IVar = NativeAligned2FromStackOffset(nextblock) = Stackspace + nextblock
    // vm.stack_base is the equivalent of Stackspace in C
    const ivar_ptr: [*]DLword = vm.stack_base + @as(usize, @intCast(nextblock));

    // Cast to LispPTR* for access (C: IVAR = ((LispPTR *)IVar))
    const ivar_lispptr: [*]align(1) LispPTR = @ptrCast(ivar_ptr);

    // Access IVAR[x] - read LispPTR at index x
    // C: IVAR[x] = ((LispPTR *)IVar)[x]
    const ivar_value: types.LispPTR = ivar_lispptr[index];

    try stack_module.pushStack(vm, ivar_value);
}

/// PVAR0-PVAR6: Parameter variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
pub fn handlePVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // PVar access: parameters stored right after frame header (FRAMESIZE offset)
    // Use helper function to get parameter value
    const pvar_value = stack_module.getPVar(frame, index);
    try stack_module.pushStack(vm, pvar_value);
}

/// FVAR0-FVAR6: Free variable access
/// Per rewrite documentation instruction-set/opcodes.md and vm-core/stack-management.md
/// Free variables are stored in PVar area after regular parameters
/// Each free variable occupies 2 words (low word and high word)
pub fn handleFVAR(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // Get function header from frame
    const fnheader_ptr = stack_module.getFnHeader(frame);
    std.debug.print("DEBUG FVAR: frame.fnheader=0x{x}\n", .{fnheader_ptr});
    if (fnheader_ptr == 0) {
        return errors_module.VMError.InvalidAddress; // No function header
    }

    // Translate function header address to byte offset in virtual memory
    if (vm.virtual_memory == null or vm.fptovp == null) {
        return errors_module.VMError.InvalidAddress;
    }

    const fptovp_table = vm.fptovp.?;
    const virtual_memory = vm.virtual_memory.?;
    std.debug.print("DEBUG FVAR: Translating fnheader_ptr=0x{x}\n", .{fnheader_ptr});

    // Get page number and offset from LispPTR
    // LispPTR format: [page_num:15 bits][offset_in_page:9 bits][unused:8 bits]
    const page_num = (fnheader_ptr >> 9) & 0x7FFF; // Page number (15 bits)
    const page_offset_dlwords = fnheader_ptr & 0x1FF; // Page offset (9 bits, in DLwords)
    const page_offset_bytes = page_offset_dlwords * 2; // Convert DLwords to bytes

    // Get virtual page from FPtoVP table
    if (page_num >= fptovp_table.entries.len) {
        std.debug.print("DEBUG FVAR: Invalid page number {}\n", .{page_num});
        return errors_module.VMError.InvalidAddress;
    }

    const virtual_page = fptovp_table.getFPtoVP(page_num);
    if (virtual_page == 0) {
        std.debug.print("DEBUG FVAR: Page {} is sparse\n", .{page_num});
        return errors_module.VMError.InvalidAddress;
    }

    // Calculate byte offset in virtual memory: virtual_page * 512 + page_offset_bytes
    const BYTESPER_PAGE: usize = 512;
    const fnheader_byte_offset = (@as(usize, virtual_page) * BYTESPER_PAGE) + page_offset_bytes;

    if (fnheader_byte_offset + 8 > virtual_memory.len) {
        std.debug.print("DEBUG FVAR: Function header offset exceeds virtual memory bounds\n", .{});
        return errors_module.VMError.InvalidAddress;
    }

    // Read function header fields manually (big-endian from sysout)
    // Function header layout: stkmin (2), na (2), pv (2), startpc (2), framename (4), ...
    // Per maiko/inc/stack.h:58-66
    if (fnheader_byte_offset + 6 > virtual_memory.len) {
        std.debug.print("DEBUG FVAR: Function header offset exceeds virtual memory bounds\n", .{});
        return errors_module.VMError.InvalidAddress;
    }

    const fnheader_bytes = virtual_memory[fnheader_byte_offset..];
    // Read pv field (bytes 4-5, big-endian)
    const pv_be = (@as(types_module.DLword, fnheader_bytes[4]) << 8) | @as(types_module.DLword, fnheader_bytes[5]);

    // Free variables are stored in PVar area after regular parameters
    // PVars are stored as LispPTR (4 bytes each)
    // Free variables are stored as 2 DLwords (2 bytes each) = 4 bytes total
    // Free variable offset calculation:
    //   PVar area size = (pv + 1) * sizeof(LispPTR) = (pv + 1) * 4 bytes
    //   Free variable i starts at: PVar area size + (i * 4 bytes)
    const pvar_count = pv_be + 1; // PVar count includes return value slot
    const pvar_area_size = @as(usize, pvar_count) * @sizeOf(LispPTR);
    const fvar_offset_bytes = pvar_area_size + (@as(usize, index) * 4); // Each FVAR is 4 bytes (2 DLwords)

    // Access free variable slot (2 words)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base_addr = frame_addr + frame_size;

    // Get low word and high word (each is 2 bytes)
    const low_word_addr = pvar_base_addr + fvar_offset_bytes;
    const high_word_addr = low_word_addr + @sizeOf(types_module.DLword);

    const low_word_ptr: *types_module.DLword = @as(*types_module.DLword, @ptrFromInt(low_word_addr));
    const high_word_ptr: *types_module.DLword = @as(*types_module.DLword, @ptrFromInt(high_word_addr));

    const low_word = low_word_ptr.*;
    const high_word = high_word_ptr.*;

    // Check if unbound (LSB of low word indicates unbound)
    // For now, we'll skip the lookup and just return the value
    // TODO: Implement nfvlookup for unbound variables

    // Construct LispPTR from two words: (high_word << 16) | low_word
    const fvar_value: LispPTR = (@as(LispPTR, high_word) << 16) | @as(LispPTR, low_word);

    // Mask to get pointer (clear tag bits)
    const masked_value = fvar_value & 0xFFFFFFFE; // Clear LSB

    // Push value onto stack
    try stack_module.pushStack(vm, masked_value);
}

/// FVARX: Free variable access with byte index
/// Per C implementation: FVARX(index) - gets free variable at index
/// Same as FVAR but index is provided as a byte operand instead of being embedded in opcode
pub fn handleFVARX(vm: *VM, index: u8) errors.VMError!void {
    // FVARX is identical to FVAR, just index comes from byte operand
    // Delegate to handleFVAR which already handles the logic
    try handleFVAR(vm, index);
}

/// GVAR: Global variable access
/// Per rewrite documentation instruction-set/opcodes.md
/// Accesses global variable via atom index
/// C: GVAR macro in maiko/inc/inlineC.h
/// BIGATOMS+BIGVM mode: atom_index is a 4-byte LispPTR pointer
/// Non-BIGATOMS mode: atom_index is a 2-byte DLword
pub fn handleGVAR(vm: *VM, atom_index: types.LispPTR) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // DEBUG: Log atom_index being read
    std.debug.print("DEBUG handleGVAR: atom_index=0x{x:0>8} ({d})\n", .{ atom_index, atom_index });

    // GVAR: Read value from atom's value cell and push on stack
    // C: GVAR(x) does PUSH(GetLongWord(Valspace + ((x) << 1))) for non-BIGATOMS
    // C: For BIGATOMS+BIGVM: PUSH(GetLongWord((LispPTR *)AtomSpace + (tx * 5) + NEWATOM_VALUE_PTROFF))
    const value = try atom_module.readAtomValue(vm, atom_index);
    try stack_module.tosPush(vm, value);
}

/// ACONST: Atom constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes atom constant by atom index
/// C: ACONST macro in maiko/inc/inlineC.h
pub fn handleACONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const atom_module = @import("../../data/atom.zig");

    // ACONST: Push atom object (atom_index as LispPTR)
    const atom_index_lisp: types.LispPTR = @as(types.LispPTR, atom_index);
    const atom_ptr = atom_module.getAtomPointer(atom_index_lisp);
    try stack_module.pushStack(vm, atom_ptr);
}

/// PVARSETPOP0-PVARSETPOP6: Set parameter variable and pop value
/// C: inlineC.h PVARSETPOPMACRO(x)
/// Stack: [value] -> [] (value stored in PVar[x], then popped)
pub fn handlePVARSETPOP(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // C: PVARSETPOPMACRO(x):
    //   PVAR[x] = TOPOFSTACK;  // Use cached TopOfStack value
    //   POP;                   // Then pop (updates TopOfStack)
    //   nextop1;

    // CRITICAL: Use TopOfStack cached value first (even if it's 0)
    const value = stack_module.getTopOfStack(vm);

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress; // No current frame
    };

    // Store value in PVar[index]
    // PVar access: parameters stored right after frame header (FRAMESIZE offset)
    stack_module.setPVar(frame, index, value);

    // Now pop (updates TopOfStack cached value)
    // CRITICAL: C code does POP unconditionally, but POP checks for underflow
    // If stack is empty (stack_ptr <= stack_base), TopOfStack should already be 0
    // We need to simulate POP: move stack_ptr UP and update TopOfStack
    const stack_base_addr = @intFromPtr(vm.stack_base);
    const stack_ptr_addr = @intFromPtr(vm.stack_ptr);

    if (stack_ptr_addr > stack_base_addr) {
        // Stack has data - pop normally (this updates TopOfStack)
        _ = try stack_module.popStack(vm);
    } else {
        // Stack is empty - TopOfStack should already be 0
        // C: POP on empty stack would underflow, but TopOfStack is already 0
        // Just ensure TopOfStack is 0 (it should be already)
        vm.top_of_stack = 0;
        // Don't call popStack as it would return StackUnderflow error
    }
}

/// APPLYFN: Apply function
/// Per rewrite documentation instruction-set/opcodes.md
/// Applies a function to arguments on the stack
pub fn handleAPPLYFN(vm: *VM) errors.VMError!void {
    // APPLYFN requires:
    // 1. Function object on stack
    // 2. Arguments on stack
    // 3. Function application mechanism

    // TODO: Proper implementation needs:
    // 1. Get function object from stack
    // 2. Get argument count
    // 3. Apply function with arguments
    // 4. Handle spread arguments if needed

    // Placeholder: for now, this is similar to CALL but handles apply semantics
    // Will be properly implemented with function application system
    _ = vm;
}

/// CHECKAPPLY: Check function application
/// Per rewrite documentation instruction-set/opcodes.md
/// Validates function application arguments
pub fn handleCHECKAPPLY(vm: *VM) errors.VMError!void {
    // CHECKAPPLY requires:
    // 1. Function object on stack
    // 2. Argument count validation
    // 3. Type checking

    // TODO: Proper implementation needs:
    // 1. Get function object
    // 2. Check argument count matches function signature
    // 3. Validate argument types
    // 4. Signal error if validation fails

    // Placeholder: will be properly implemented with function validation
    _ = vm;
}

/// STKSCAN: Scan stack for variable
/// Per rewrite documentation instruction-set/opcodes.md and src/fvar.c
/// Scans stack frames for variable by atom index
pub fn handleSTKSCAN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // STKSCAN requires:
    // 1. Atom index on TOS
    // 2. Scan current and previous frames
    // 3. Look up variable in name tables
    // 4. Return address of found value

    // TODO: Proper implementation needs:
    // 1. Get atom index from TOS
    // 2. Traverse stack frames (current and previous)
    // 3. Look up variable in each frame's name table
    // 4. Return address of variable value
    // 5. Handle unbound variables

    // Placeholder: for now, return NIL
    // Will be properly implemented with name table lookup
    const atom_index = stack_module.getTopOfStack(vm);
    _ = atom_index;
    stack_module.setTopOfStack(vm, 0); // Return NIL for now
}
/// PVARX_: Set PVAR X (word offset)
/// C: PVARX_(x) macro in maiko/inc/inlineC.h
/// Sets parameter variable using DLword offset
/// Stack: [value] -> []
/// Operand: x (1B, DLword offset)
pub fn handlePVAR_SET(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: PVARX_(x): *((LispPTR *)((DLword *)PVAR + (x))) = TOPOFSTACK;
    const value = try stack_module.popStack(vm);

    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // PVAR area starts after frame header (FRAMESIZE bytes)
    const frame_addr = @intFromPtr(frame);
    const pvar_base_addr = frame_addr + @sizeOf(stack.FX);

    // Access PVAR at word_offset (DLword units)
    const pvar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const pvar_addr = pvar_base_addr + pvar_offset_bytes;

    // Write LispPTR (2 DLwords, big-endian)
    const pvar_bytes: [*]u8 = @ptrFromInt(pvar_addr);
    const low_word = @as(types_module.DLword, @truncate(value));
    const high_word = @as(types_module.DLword, @truncate(value >> 16));
    pvar_bytes[0] = @as(u8, @truncate(low_word >> 8));
    pvar_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    pvar_bytes[2] = @as(u8, @truncate(high_word >> 8));
    pvar_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
}

/// ARG0: Argument 0
/// Per rewrite documentation instruction-set/opcodes.md
/// Gets first argument
pub fn handleARG0(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // ARG0 requires:
    // 1. Get argument 0 from current frame

    // TODO: Proper implementation needs:
    // 1. Access argument 0 from current frame

    // Placeholder: return NIL
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// PVARX: Get PVAR X (word offset)
/// C: PVARX(x) macro in maiko/inc/inlineC.h
/// Gets parameter variable using DLword offset (not LispPTR offset)
/// Stack: [] -> [value]
/// Operand: x (1B, DLword offset)
pub fn handlePVARX(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: PVARX(x): PUSH(GetLongWord((DLword *)PVAR + (x)));
    // x is a DLword offset, not a LispPTR offset
    // GetLongWord reads 2 DLwords (4 bytes) as a LispPTR

    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // PVAR area starts after frame header (FRAMESIZE bytes)
    const frame_addr = @intFromPtr(frame);
    const pvar_base_addr = frame_addr + @sizeOf(stack.FX);

    // Access PVAR at word_offset (DLword units)
    // Each LispPTR is 2 DLwords, so offset in bytes = word_offset * 2
    const pvar_offset_bytes = @as(usize, word_offset) * @sizeOf(types_module.DLword);
    const pvar_addr = pvar_base_addr + pvar_offset_bytes;

    // Read LispPTR (2 DLwords, big-endian)
    const pvar_bytes: [*]const u8 = @ptrFromInt(pvar_addr);
    const low_word = (@as(types_module.DLword, pvar_bytes[0]) << 8) | @as(types_module.DLword, pvar_bytes[1]);
    const high_word = (@as(types_module.DLword, pvar_bytes[2]) << 8) | @as(types_module.DLword, pvar_bytes[3]);
    const value: types.LispPTR = (@as(types.LispPTR, high_word) << 16) | @as(types.LispPTR, low_word);

    try stack_module.pushStack(vm, value);
}

/// IVARX: Get IVAR X (word offset)
/// C: IVARX(x) macro in maiko/inc/inlineC.h
/// Gets instance variable using DLword offset (not LispPTR offset)
/// Stack: [] -> [value]
/// Operand: x (1B, DLword offset)
pub fn handleIVARX(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: IVARX(x): PUSH(GetLongWord((DLword *)IVAR + (x)));
    // IVAR = ((LispPTR *)IVar) where IVar = Stackspace + nextblock
    // x is a DLword offset, not a LispPTR offset
    // GetLongWord reads 2 DLwords (4 bytes) as a LispPTR
    // CRITICAL: IVar points to STACK MEMORY, not virtual memory!

    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // IVAR area is at frame.nextblock (DLword offset from Stackspace)
    const nextblock = stack_module.getNextblock(frame);
    if (nextblock == 0) {
        try stack_module.pushStack(vm, 0);
        return;
    }

    // Calculate IVar pointer: Stackspace + nextblock (in DLwords)
    // C: IVar = NativeAligned2FromStackOffset(nextblock) = Stackspace + nextblock
    const ivar_ptr: [*]DLword = vm.stack_base + @as(usize, @intCast(nextblock));

    // Access IVAR at word_offset (DLword units)
    // C: (DLword *)IVAR + x = (DLword *)((LispPTR *)IVar) + x
    // Since IVAR is cast to LispPTR*, we need to access as DLword* first, then offset
    const ivar_at_offset: [*]DLword = ivar_ptr + @as(usize, @intCast(word_offset));

    // Read 2 DLwords (4 bytes) as LispPTR using GetLongWord logic
    // C: GetLongWord reads big-endian from memory
    // But stack memory is already in native byte order (little-endian on x86_64)
    // So we read directly as LispPTR
    const ivar_lispptr: [*]align(1) LispPTR = @ptrCast(ivar_at_offset);
    const value: types.LispPTR = ivar_lispptr[0];

    try stack_module.pushStack(vm, value);
}

/// IVARX_: Set IVAR X (word offset)
/// C: IVARX_(x) macro in maiko/inc/inlineC.h
/// Sets instance variable using DLword offset
/// Stack: [value] -> []
/// Operand: x (1B, DLword offset)
pub fn handleIVARX_(vm: *VM, word_offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: IVARX_(x): *((LispPTR *)((DLword *)IVAR + (x))) = TOPOFSTACK;
    // CRITICAL: IVar points to STACK MEMORY, not virtual memory!
    const value = try stack_module.popStack(vm);

    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // IVAR area is at frame.nextblock (DLword offset from Stackspace)
    const nextblock = stack_module.getNextblock(frame);
    if (nextblock == 0) {
        return; // No IVAR area - ignore
    }

    // Calculate IVar pointer: Stackspace + nextblock (in DLwords)
    // C: IVar = NativeAligned2FromStackOffset(nextblock) = Stackspace + nextblock
    const ivar_ptr: [*]DLword = vm.stack_base + @as(usize, @intCast(nextblock));

    // Access IVAR at word_offset (DLword units)
    // C: (DLword *)IVAR + x = (DLword *)((LispPTR *)IVar) + x
    const ivar_at_offset: [*]DLword = ivar_ptr + @as(usize, @intCast(word_offset));

    // Cast to LispPTR* and write value
    // C: *((LispPTR *)((DLword *)IVAR + (x))) = TOPOFSTACK;
    const ivar_lispptr: [*]align(1) LispPTR = @ptrCast(ivar_at_offset);
    ivar_lispptr[0] = value;

    const virtual_memory_mut: []u8 = @constCast(vm.virtual_memory.?);
    const ivar_bytes = virtual_memory_mut[ivar_addr .. ivar_addr + 4];
    const low_word = @as(types_module.DLword, @truncate(value));
    const high_word = @as(types_module.DLword, @truncate(value >> 16));
    ivar_bytes[0] = @as(u8, @truncate(low_word >> 8));
    ivar_bytes[1] = @as(u8, @truncate(low_word & 0xFF));
    ivar_bytes[2] = @as(u8, @truncate(high_word >> 8));
    ivar_bytes[3] = @as(u8, @truncate(high_word & 0xFF));
}

/// FVARX_: Set FVAR X
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets free variable value
pub fn handleFVARX_(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // FVARX_ requires:
    // 1. Value on stack
    // 2. Set FVAR at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// COPY: Copy value
/// Per rewrite documentation instruction-set/opcodes.md
/// Copies value on stack
pub fn handleCOPY(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // C (inlineC.h): COPY => HARD_PUSH(TOPOFSTACK); nextop1;
    // This pushes the cached TOS onto the value stack without changing TOPOFSTACK,
    // and it does NOT mutate the traced CurrentStackPTR.
    const tos = stack_module.getTopOfStack(vm);
    try stack_module.tosHardPush(vm, tos);
}

/// MYARGCOUNT: My argument count
/// C: MYARGCOUNT macro in maiko/inc/inlineC.h:641-650
/// Gets argument count for current function
/// C: PUSH((DLword)((arg_num - (UNSIGNED)IVar) >> 2) | S_POSITIVE);
pub fn handleMYARGCOUNT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors.VMError.InvalidAddress; // No current frame
    };

    // C: MYARGCOUNT calculation:
    //   UNSIGNED arg_num;
    //   if ((CURRENTFX->alink & 1) == 0)
    //     arg_num = (UNSIGNED)((LispPTR *)(CURRENTFX) - 1);
    //   else
    //     arg_num = (UNSIGNED)(Stackspace + CURRENTFX->blink);
    //   PUSH((DLword)((arg_num - (UNSIGNED)IVar) >> 2) | S_POSITIVE);

    const alink = stack_module.getAlink(frame);
    const arg_num: usize = if ((alink & 1) == 0) blk: {
        // Normal frame: arguments start at (CURRENTFX) - 1
        // C: arg_num = (UNSIGNED)((LispPTR *)(CURRENTFX) - 1);
        const frame_addr = @intFromPtr(frame);
        const arg_num_addr = frame_addr - @sizeOf(types.LispPTR); // CURRENTFX - 1 LispPTR
        break :blk arg_num_addr;
    } else blk: {
        // Extended frame: arguments start at Stackspace + blink
        // C: arg_num = (UNSIGNED)(Stackspace + CURRENTFX->blink);
        const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
        const stackspace_byte_offset = STK_OFFSET * 2; // Convert DLword offset to byte offset
        const blink = frame.blink;
        const arg_num_addr = stackspace_byte_offset + (@as(usize, @intCast(blink)) * 2); // blink is DLword offset
        break :blk arg_num_addr;
    };

    // Get IVar address: IVar = Stackspace + nextblock
    // C: IVAR = NativeAligned2FromStackOffset(CURRENTFX->nextblock) = Stackspace + nextblock
    const nextblock = stack_module.getNextblock(frame);
    const STK_OFFSET: u32 = 0x00010000; // DLword offset from Lisp_world
    const stackspace_byte_offset = STK_OFFSET * 2; // Convert DLword offset to byte offset
    const ivar_addr = stackspace_byte_offset + (@as(usize, @intCast(nextblock)) * 2); // nextblock is DLword offset

    // Calculate argument count: (arg_num - IVar) >> 2
    // C: (arg_num - (UNSIGNED)IVar) >> 2
    // This gives the number of LispPTR words between IVar and arg_num
    const arg_count_dlwords = if (arg_num >= ivar_addr) (arg_num - ivar_addr) / @sizeOf(types.LispPTR) else 0;
    const arg_count: types.DLword = @as(types.DLword, @intCast(arg_count_dlwords));

    // Push result with S_POSITIVE tag
    // C: PUSH((DLword)((arg_num - (UNSIGNED)IVar) >> 2) | S_POSITIVE);
    const result = types_module.S_POSITIVE | @as(types.LispPTR, arg_count);
    try stack_module.pushStack(vm, result);
}

/// MYALINK: My activation link
/// C: MYALINK macro in maiko/inc/inlineC.h
/// Pushes activation link address (previous frame pointer)
/// Stack: [] -> [alink_address]
pub fn handleMYALINK(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");

    // C: MYALINK: PUSH((((CURRENTFX->alink) & 0xfffe) - FRAMESIZE) | S_POSITIVE);
    // FRAMESIZE = 10 DLwords = 20 bytes (frame size in virtual memory)
    // C's FRAMESIZE is defined in lispemul.h: #define FRAMESIZE 10
    const FRAMESIZE: u32 = 10; // DLwords

    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // Get alink (activation link to previous frame)
    const alink = stack_module.getAlink(frame);

    // CRITICAL: Handle invalid alink values (should not happen in valid execution)
    // Maiko's native_newframe explicitly checks: if (alink == 0) error("alink is 0 in native_newframe");
    // Stack boundaries should be marked with ENDSTACKMARK (0xb), not 0.
    // This workaround allows continued execution for debugging purposes.
    if (alink == 0) {
        std.debug.print("DEBUG MYALINK: alink=0 encountered - should be ENDSTACKMARK (0xb) at stack boundary\n", .{});
        // Temporary fix: treat as stack boundary to prevent crash
        // TODO: Fix frame initialization to use proper ENDSTACKMARK values
        const alink_addr = 0; // Stack boundary placeholder
        const result = types_module.S_POSITIVE | alink_addr;
        try stack_module.pushStack(vm, result);
        return;
    }

    // C: (alink & 0xfffe) - FRAMESIZE
    // Clear LSB and subtract FRAMESIZE (in DLwords, so multiply by 2 for bytes)
    const alink_cleared = alink & 0xFFFFFFFE; // Clear LSB

    // C equivalent: ((((CURRENTFX->alink) & 0xfffe) - FRAMESIZE) | S_POSITIVE)
    // Note: Maiko doesn't check for underflow here because valid execution should not reach this point
    // with problematic alink values. The underflow we're seeing indicates a deeper frame management issue.
    const alink_addr = alink_cleared - (FRAMESIZE * 2); // FRAMESIZE in bytes

    // C: | S_POSITIVE
    const result = types_module.S_POSITIVE | alink_addr;

    try stack_module.pushStack(vm, result);
}
