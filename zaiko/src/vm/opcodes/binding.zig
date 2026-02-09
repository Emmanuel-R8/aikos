const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const std = @import("std");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// Binding Operations
/// BIND: Bind variables from stack
/// Per rewrite documentation instruction-set/opcodes.md
/// C: BIND macro in maiko/inc/inlineC.h
/// Binds variables in PVAR area using encoded parameters
/// Stack: [values..., TOS] -> [marker]
/// Operands: byte1 (n1:4, n2:4), byte2 (offset)
pub fn handleBIND(vm: *VM, byte1: u8, byte2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");

    // Save current TOPOFSTACK for restoration during UNBIND
    const saved_topofstack = stack_module.getTopOfStack(vm);

    // Parse byte1: n1 (high 4 bits), n2 (low 4 bits)
    const n1 = byte1 >> 4;
    const n2 = byte1 & 0xF;

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // Calculate ppvar: (LispPTR *)PVAR + 1 + offset
    // PVAR is the parameter variable area in the frame
    // offset is byte2 (second operand)
    const offset = byte2;

    // Get PVAR base address (after frame header)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base = frame_addr + frame_size;
    const pvar_ptr: [*]LispPTR = @ptrFromInt(pvar_base);

    // Calculate ppvar: PVAR + 1 + offset (in LispPTR units, so +1 means +4 bytes)
    const ppvar: [*]LispPTR = pvar_ptr + 1 + offset;

    // Push n1 NIL values (going backwards from ppvar)
    // Calculate pointer address for negative indexing
    var i: u8 = 0;
    while (i < n1) : (i += 1) {
        const target_ptr: [*]LispPTR = @ptrFromInt(@intFromPtr(ppvar) - (@as(usize, i + 1) * @sizeOf(LispPTR)));
        target_ptr[0] = 0; // NIL_PTR
    }

    // Get TOS value
    const tos_value = stack_module.getTopOfStack(vm);

    if (n2 == 0) {
        // Push TOS onto stack (C: *CSTKPTRL++ = TOPOFSTACK)
        try stack_module.pushStack(vm, tos_value);
    } else {
        // Push TOS to ppvar, then push n2-1 more values from stack
        const tos_ptr: [*]LispPTR = @ptrFromInt(@intFromPtr(ppvar) - (@as(usize, n1 + 1) * @sizeOf(LispPTR)));
        tos_ptr[0] = tos_value;

        // Pop n2-1 values from stack and push to ppvar (going backwards)
        i = 1;
        while (i < n2) : (i += 1) {
            const value = try stack_module.popStack(vm);
            const value_ptr: [*]LispPTR = @ptrFromInt(@intFromPtr(ppvar) - (@as(usize, n1 + i + 1) * @sizeOf(LispPTR)));
            value_ptr[0] = value;
        }
    }

    // Push saved TOPOFSTACK for restoration during UNBIND
    try stack_module.pushStack(vm, saved_topofstack);

    // Set TOS to marker: ((~(n1 + n2)) << 16) | (offset << 1)
    const total = n1 + n2;
    const marker: LispPTR = (@as(LispPTR, ~total) << 16) | (@as(LispPTR, offset) << 1);
    stack_module.setTopOfStack(vm, marker);
}

/// UNBIND: Unbind variables from previous BIND operation
/// Per rewrite documentation instruction-set/opcodes.md
/// C: UNBIND macro in maiko/inc/inlineC.h lines 545-559
///
/// Algorithm:
/// 1. Walk backwards through stack to find the BIND marker (negative value)
/// 2. Extract binding information from the marker
/// 3. Clear the bound variables by setting them to unbound state (0xffffffff)
/// 4. Restore the environment (TOPOFSTACK) to pre-binding state
///
/// Stack: [marker, bound_values...] -> [marker] (marker removed by subsequent operations)
///
/// CRITICAL: The loop uses SIGNED comparison ((int)*--CSTKPTRL >= 0), NOT unsigned.
/// This is because LispPTR markers like 0xfffe0002 have the sign bit set but represent
/// negative values when interpreted as signed integers.
///
/// The marker encodes: ((~(num_vars)) << 16) | (offset << 1)
/// - num_vars: number of variables that were bound
/// - offset: offset into PVAR area where variables are stored
///
/// Example from C trace:
/// - Marker: 0xfffe0002
/// - num = (~0xfffe0002) >> 16 = (~0xfffe0002) >> 16 = 0x0001fffd >> 16 = 1
/// - offset = GetLoWord(0xfffe0002) = 0x0002, then offset << 1 = 4
///
/// Variables are cleared by setting *--ppvar = 0xffffffff (unbound marker)
///
/// NOTE: TOPOFSTACK restoration now properly implemented by saving/restoring from stack during BIND/UNBIND.
pub fn handleUNBIND(vm_obj: *VM) errors.VMError!void {
    const errors_module = @import("../../utils/errors.zig");

    // std.debug.print("DEBUG UNBIND: starting, CSTKPTRL=0x{x}\n", .{@intFromPtr(vm_obj.cstkptrl.?)});

    // PHASE 1: Find the BIND marker by walking backwards through stack
    // C: for (; (((int)*--CSTKPTRL) >= 0););
    // Walks backwards until finding a negative (signed) value = BIND marker
    var marker: LispPTR = 0;
    var iterations: u32 = 0;
    while (iterations < 100) : (iterations += 1) {
        if (vm_obj.cstkptrl == null) {
            std.debug.print("DEBUG UNBIND: CSTKPTRL is null, returning error\n", .{});
            return errors_module.VMError.StackUnderflow;
        }

        // CRITICAL: Decrement CSTKPTRL BEFORE reading (*--CSTKPTRL in C)
        vm_obj.cstkptrl = vm_obj.cstkptrl.? - 1;
        const v = vm_obj.cstkptrl.?[0];
        // std.debug.print("DEBUG UNBIND: iter {}, CSTKPTRL=0x{x}, value=0x{x}\n", .{ iterations, @intFromPtr(vm_obj.cstkptrl.?), v });

        // CRITICAL: Use SIGNED comparison, NOT unsigned!
        // C: ((int)*--CSTKPTRL) >= 0
        // Values like 0xfffe0002 are negative when cast to int, stopping the loop
        // Unsigned comparison would continue past these markers
        if (@as(i32, @bitCast(v)) >= 0) {
            // Continue searching - this is a bound value, not the marker
        } else {
            // Found the BIND marker (negative signed value)
            marker = v;
            // std.debug.print("DEBUG UNBIND: found marker=0x{x}\n", .{marker});
            break;
        }
    }

    if (iterations >= 100) {
        std.debug.print("DEBUG UNBIND: too many iterations, no marker found\n", .{});
        return errors_module.VMError.StackUnderflow;
    }

    // PHASE 2: Extract binding information from marker
    // C: value = *CSTKPTR; num = (~value) >> 16;
    const value = vm_obj.cstkptrl.?[0]; // Redundant with marker, but matches C
    // std.debug.print("DEBUG UNBIND: marker value=0x{x} from CSTKPTRL=0x{x}\n", .{ value, @intFromPtr(vm_obj.cstkptrl.?) });

    // Extract number of variables bound: num = (~value) >> 16
    // BIND marker: ((~(num_vars)) << 16) | (offset << 1)
    const num = (~@as(i32, @bitCast(value))) >> 16;
    // std.debug.print("DEBUG UNBIND: num = (~0x{x}) >> 16 = {}\n", .{ value, num });

    // PHASE 3: Calculate PVAR pointer for variable clearing
    // C: ppvar = (LispPTR *)((DLword *)PVAR + 2 + GetLoWord(value));
    const frame = vm_obj.current_frame orelse {
        std.debug.print("DEBUG UNBIND: no current frame, returning error\n", .{});
        return errors_module.VMError.InvalidAddress;
    };

    // PVAR is the parameter variable area in the frame (after frame header)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base = frame_addr + frame_size;

    // Extract offset from marker: GetLoWord(value) << 1
    const loword = types.getLoWord(value);
    const offset = loword << 1; // C: GetLoWord gives DLword offset, then << 1 for byte offset
    const ppvar_offset = 2 + offset; // C: +2 for base offset in PVAR
    const ppvar_addr = pvar_base + ppvar_offset;
    var ppvar: [*]align(1) LispPTR = @ptrFromInt(ppvar_addr);

    // std.debug.print("DEBUG UNBIND: PVAR base=0x{x}, loword=0x{x}, offset=0x{x}, ppvar_offset=0x{x}, ppvar_addr=0x{x}\n", .{ pvar_base, loword, offset, ppvar_offset, ppvar_addr });

    // PHASE 4: Clear bound variables
    // C: for (i = num; --i >= 0;) { *--ppvar = 0xffffffff; }
    // Sets variables to unbound state (0xffffffff = -1 signed)
    var i: u32 = if (num >= 0) @as(u32, @intCast(num)) else 0;
    while (i > 0) {
        i -= 1;
        ppvar -= 1; // *--ppvar in C (decrement pointer before assignment)
        ppvar[0] = 0xffffffff; // Unbound marker
        // std.debug.print("DEBUG UNBIND: cleared variable at 0x{x} to 0xffffffff\n", .{@intFromPtr(ppvar)});
    }

    // PHASE 5: Restore environment (TOPOFSTACK)
    // CRITICAL FIX: Use proper TOPOFSTACK synchronization after UNBIND stack manipulation
    // The UNBIND macro modifies CSTKPTRL (walking stack backwards), so we need
    // to re-read TOPOFSTACK from memory to get the correct new stack top.
    // This matches the pattern that should be used in the C UNBIND macro.
    const stack_module = @import("../stack.zig");
    stack_module.readTopOfStackFromMemory(vm_obj);
    // std.debug.print("DEBUG UNBIND: synchronized TOPOFSTACK to 0x{x} from memory\n", .{vm_obj.top_of_stack});

    if (marker == 0) return errors_module.VMError.StackUnderflow;
}

/// DUNBIND: Dynamic unbind
/// Per rewrite documentation instruction-set/opcodes.md
/// C: DUNBIND macro in maiko/inc/inlineC.h
/// Unbind with dynamic scope handling (checks TOS first)
/// Stack: [marker, ...] or [TOS] -> []
pub fn handleDUNBIND(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types_module = @import("../../utils/types.zig");
    const errors_module = @import("../../utils/errors.zig");
    const DLword = types_module.DLword;

    const tos = stack_module.getTopOfStack(vm);

    // Check if TOS is negative (marker)
    // C: if ((int)TOPOFSTACK < 0)
    if (@as(i32, @bitCast(tos)) < 0) {
        // TOS is the marker - extract num and offset
        const num = (~tos) >> 16;
        const offset = types_module.getLoWord(tos); // C uses GetLoWord directly, no shift

        if (num != 0) {
            // Get current frame
            const frame = vm.current_frame orelse {
                return errors_module.VMError.InvalidAddress;
            };

            // Calculate ppvar: (LispPTR *)((DLword *)PVAR + 2 + offset)
            const frame_addr = @intFromPtr(frame);
            const frame_size = @sizeOf(stack.FX);
            const pvar_base = frame_addr + frame_size;
            const pvar_dlword_ptr: [*]DLword = @ptrFromInt(pvar_base);
            const ppvar_dlword: [*]DLword = pvar_dlword_ptr + 2 + offset;
            const ppvar: [*]align(1) LispPTR = @ptrCast(ppvar_dlword);

            // Restore num values to unbound
            // C: for (i = num; --i >= 0;) { *--ppvar = 0xffffffff; }
            var i: u32 = num;
            var current_ppvar: [*]align(1) LispPTR = ppvar;
            while (i > 0) : (i -= 1) {
                current_ppvar -= 1; // Decrement before assignment
                current_ppvar[0] = 0xFFFFFFFF;
            }
        }

        // C: POP after unbinding (tos1defs.h)
        try stack_module.tosPop(vm);
    } else {
        // TOS is not marker - scan CSTKPTRL backwards to find marker (same as UNBIND).
        var p = vm.cstkptrl orelse return errors_module.VMError.StackUnderflow;
        var marker: LispPTR = 0;
        while (true) {
            p -= 1; // C: *--CSTKPTRL
            const v = p[0];
            if (@as(i32, @bitCast(v)) < 0) {
                marker = v;
                vm.cstkptrl = p;
                break;
            }
        }

        const num = (~marker) >> 16;
        const offset = types_module.getLoWord(marker); // C uses GetLoWord directly, no shift

        if (num != 0) {
            const frame = vm.current_frame orelse {
                return errors_module.VMError.InvalidAddress;
            };

            const frame_addr = @intFromPtr(frame);
            const frame_size = @sizeOf(stack.FX);
            const pvar_base = frame_addr + frame_size;
            const pvar_dlword_ptr: [*]DLword = @ptrFromInt(pvar_base);
            const ppvar_dlword: [*]DLword = pvar_dlword_ptr + 2 + offset;
            const ppvar: [*]align(1) LispPTR = @ptrCast(ppvar_dlword);

            var i: u32 = num;
            var current_ppvar: [*]align(1) LispPTR = ppvar;
            while (i > 0) : (i -= 1) {
                current_ppvar -= 1; // Decrement before assignment
                current_ppvar[0] = 0xFFFFFFFF;
            }
        }

        // C: POP after unbinding
        try stack_module.tosPop(vm);
    }
}
