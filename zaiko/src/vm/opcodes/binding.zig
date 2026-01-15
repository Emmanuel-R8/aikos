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
    const types_module = @import("../../utils/types.zig");
    const errors_module = @import("../../utils/errors.zig");
    const DLword = types_module.DLword;

    // Walk backwards through stack until we find a negative value (marker)
    // C: for (; (((int)*--CSTKPTRL) >= 0););
    // This means: decrement stack pointer FIRST, then read value
    // Keep doing this until we find a value with high bit set (negative when cast to int)
    // CRITICAL: C code decrements FIRST, then reads, so we need to read directly from stack memory

    while (true) {
        // Check if we can decrement (stack has data)
        // Stack grows DOWN, so stack_ptr must be > stack_base to have data
        const stack_base_addr = @intFromPtr(vm.stack_base);
        var stack_ptr_addr = @intFromPtr(vm.stack_ptr);

        // Check if we can decrement (need at least 2 DLwords = 4 bytes below current position)
        if (stack_ptr_addr <= stack_base_addr + 4) {
            // Stack doesn't have enough data - no marker found
            return errors_module.VMError.StackUnderflow;
        }

        // Decrement stack pointer FIRST (C: --CSTKPTRL)
        // Stack grows DOWN, so decrementing means moving DOWN (subtracting)
        // Move DOWN by 2 DLwords (1 LispPTR = 4 bytes)
        stack_ptr_addr -= 4; // Move DOWN by 4 bytes (2 DLwords)
        vm.stack_ptr = @ptrFromInt(stack_ptr_addr);

        // Read value directly from stack memory (C: *--CSTKPTRL)
        // CRITICAL: Read with byte swapping for big-endian format
        const stack_ptr_bytes: [*]const u8 = @ptrCast(vm.stack_ptr);
        const low_word_be = (@as(DLword, stack_ptr_bytes[0]) << 8) | @as(DLword, stack_ptr_bytes[1]);
        const high_word_be = (@as(DLword, stack_ptr_bytes[2]) << 8) | @as(DLword, stack_ptr_bytes[3]);
        const value: LispPTR = (@as(LispPTR, high_word_be) << 16) | @as(LispPTR, low_word_be);

        // Check if value is negative (high bit set) - this is the marker
        if (@as(i32, @bitCast(value)) < 0) {
            // Found marker - update cached TopOfStack and break
            vm.top_of_stack = value;
            break;
        }
        // Continue searching (loop will decrement again)
    }

    // Get marker value (already in vm.top_of_stack, but read it explicitly)
    const marker = vm.top_of_stack;

    // Extract num and offset from marker: num = (~value) >> 16, offset = GetLoWord(value)
    // C: num = (~value) >> 16; offset = GetLoWord(value) (used directly, not shifted)
    const num = (~marker) >> 16;
    const offset = types_module.getLoWord(marker); // C uses GetLoWord directly, no shift

    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };

    // Calculate ppvar: (LispPTR *)((DLword *)PVAR + 2 + offset)
    // C: ppvar = (LispPTR *)((DLword *)PVAR + 2 + GetLoWord(value))
    // PVAR is a DLword pointer, so arithmetic is in DLword units
    // Then cast to LispPTR* (which treats as LispPTR array)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base = frame_addr + frame_size;

    // PVAR is a DLword pointer, so do DLword arithmetic first
    // C: ppvar = (LispPTR *)((DLword *)PVAR + 2 + GetLoWord(value))
    // PVAR is a DLword pointer, so arithmetic is in DLword units
    const pvar_dlword_ptr: [*]DLword = @ptrFromInt(pvar_base);
    const ppvar_dlword: [*]DLword = pvar_dlword_ptr + 2 + offset;
    // Then cast to LispPTR* (C does this cast after arithmetic)
    // CRITICAL: C code does this cast directly, which may result in unaligned access
    // In Zig, we need to handle potential misalignment. Since DLword=2 and LispPTR=4,
    // the address might not be 4-byte aligned. Use @ptrCast with align(1) to allow unaligned access
    const ppvar: [*]align(1) LispPTR = @ptrCast(ppvar_dlword);

    // Restore num values to 0xffffffff (unbound marker)
    // C: for (i = num; --i >= 0;) { *--ppvar = 0xffffffff; }
    // This decrements ppvar before each assignment, starting from ppvar and going backwards
    // For num=1: i=1, --i=0 (>=0), --ppvar, then assign
    var i: u32 = num;
    var current_ppvar: [*]align(1) LispPTR = ppvar;
    while (i > 0) : (i -= 1) {
        current_ppvar -= 1; // Decrement before assignment (C: *--ppvar)
        current_ppvar[0] = 0xFFFFFFFF; // Unbound marker
    }

    // Pop marker from stack
    _ = try stack_module.popStack(vm);
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

        // Pop marker
        _ = try stack_module.popStack(vm);
    } else {
        // TOS is not marker - walk backwards to find marker (same as UNBIND)
        while (true) {
            if (stack_module.getStackDepth(vm) == 0) {
                return errors_module.VMError.StackUnderflow;
            }

            const value = stack_module.getTopOfStack(vm);
            if (@as(i32, @bitCast(value)) < 0) {
                break; // Found marker
            }
            _ = try stack_module.popStack(vm);
        }

        const marker = stack_module.getTopOfStack(vm);
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
            const ppvar: [*]LispPTR = @ptrCast(@alignCast(ppvar_dlword));

            var i: u32 = num;
            var current_ppvar: [*]LispPTR = ppvar;
            while (i > 0) : (i -= 1) {
                current_ppvar -= 1; // Decrement before assignment
                current_ppvar[0] = 0xFFFFFFFF;
            }
        }

        // Pop marker
        _ = try stack_module.popStack(vm);
    }
}
