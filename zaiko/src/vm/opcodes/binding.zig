const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

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
pub fn handleUNBIND(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const errors_module = @import("../../utils/errors.zig");
    // C (tos1defs.h): UNBIND walks *CSTKPTRL (LispPTR cell pointer), NOT CurrentStackPTR.
    // Also, it does NOT modify TOPOFSTACK.
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

    // TODO: Implement PVAR/ppvar unbinding correctly.
    // The C macro mutates PVAR cells based on the marker. Our current VM does not yet
    // model PVAR precisely, and incorrect pointer math here can corrupt VM state.
    _ = stack_module;
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
