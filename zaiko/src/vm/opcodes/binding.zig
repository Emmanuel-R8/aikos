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

    // Set TOS to marker: ((~(n1 + n2)) << 16) | (offset << 1)
    const total = n1 + n2;
    const marker: LispPTR = (@as(LispPTR, ~total) << 16) | (@as(LispPTR, offset) << 1);
    stack_module.setTopOfStack(vm, marker);
}

/// UNBIND: Unbind variables
/// Per rewrite documentation instruction-set/opcodes.md
/// C: UNBIND macro in maiko/inc/inlineC.h
/// Unbinds variables by walking backwards through stack to find marker
/// Stack: [marker, ...] -> []
pub fn handleUNBIND(vm_obj: *VM) errors.VMError!void {
    const errors_module = @import("../../utils/errors.zig");
    const types_module = @import("../../utils/types.zig");
    const DLword = types_module.DLword;
    // CRITICAL: CSTKPTRL is a pointer to LispPTR (4-byte) values
    // C: for (; (((int)*--CSTKPTRL) >= 0););
    // CSTKPTRL is declared as LispPTR* (see maiko/src/xc.c:133)
    // So *--CSTKPTRL decrements by 1 LispPTR (4 bytes), NOT 1 DLword (2 bytes)
    var marker: LispPTR = 0;

    // DEBUG: Verify CSTKPTRL is in virtual memory
    const cstkptrl_addr_debug = @intFromPtr(vm_obj.cstkptrl.?);
    const vmem_base = if (vm_obj.virtual_memory) |vm| @intFromPtr(vm.ptr) else 0;
    const vmem_len = if (vm_obj.virtual_memory) |vm| vm.len else 0;
    std.debug.print("DEBUG UNBIND: CSTKPTRL=0x{x}, vmem_base=0x{x}, vmem_len=0x{x}\n", .{ cstkptrl_addr_debug, vmem_base, vmem_len });
    std.debug.print("DEBUG UNBIND: CSTKPTRL in vmem range: {}\n", .{cstkptrl_addr_debug >= vmem_base and cstkptrl_addr_debug < vmem_base + vmem_len});

    while (true) {
        // C: *--CSTKPTRL - decrement by 1 LispPTR (4 bytes) BEFORE reading
        vm_obj.cstkptrl = vm_obj.cstkptrl.?;
        vm_obj.cstkptrl = vm_obj.cstkptrl.? - 1;
        const v = vm_obj.cstkptrl.?[0];
        std.debug.print("DEBUG UNBIND: CSTKPTRL=0x{x}, v=0x{x}\n", .{ @intFromPtr(vm_obj.cstkptrl), v });
        if (@as(i32, @bitCast(v)) < 0) {
            marker = v;
            // C: After loop exits, CSTKPTRL points TO the negative marker
            // The loop decrements CSTKPTRL before checking, so we're pointing at the marker
            break;
        }
    }
    // CSTKPTRL now correctly points to the negative marker (matching C behavior)

    if (marker == 0) return errors_module.VMError.StackUnderflow;

    // CRITICAL: After UNBIND, CSTKPTRL points to the marker
    // C: UNBIND modifies CSTKPTRL directly but does NOT update CurrentStackPTR
    // This means CurrentStackPTR becomes stale after UNBIND
    // However, the C dispatch loop doesn't call StackPtrRestore at nextopcode:
    // So CSTKPTRL stays at the marker position, and TOPOFSTACK is read from memory
    //
    // In Zig, the dispatch loop calls initCSTKPTRLFromCurrentStackPTR before each opcode
    // This would restore CSTKPTRL from the stale stack_ptr, undoing UNBIND's work
    // So we need to update stack_ptr to match CSTKPTRL's new position
    // C equivalent: CurrentStackPTR = (void *)(CSTKPTR - 1) (StackPtrSave pattern)
    if (vm_obj.cstkptrl) |cstkptrl| {
        // Convert CSTKPTRL (LispPTR*) to stack_ptr (DLword*)
        // CSTKPTRL points to LispPTR values, stack_ptr points to DLword values
        // CSTKPTRL - 1 gives us the position before the marker
        // Convert to DLword*: (LispPTR* - 1) = (LispPTR* - 4 bytes) = (DLword* - 2 DLwords)
        const cstkptrl_addr = @intFromPtr(cstkptrl);
        const stack_ptr_addr = cstkptrl_addr - 4; // -1 LispPTR = -4 bytes = -2 DLwords
        vm_obj.stack_ptr = @as([*]DLword, @ptrFromInt(stack_ptr_addr));
    }

    // TOPOFSTACK will be read from memory by the dispatch loop's initCSTKPTRLFromCurrentStackPTR
    // This matches C behavior where TopOfStack (MachineState.tosvalue) is read from memory
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
