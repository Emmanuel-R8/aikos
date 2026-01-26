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
    
    // Walk backwards through stack until we find a negative value (marker)
    // C: for (; (((int)*--CSTKPTRL) >= 0););
    // This means: keep popping until we find a value with high bit set (negative when cast to int)
    while (true) {
        if (stack_module.getStackDepth(vm) == 0) {
            // Stack is empty - no marker found
            return errors_module.VMError.StackUnderflow;
        }
        
        const value = stack_module.getTopOfStack(vm);
        // Check if value is negative (high bit set) - this is the marker
        if (@as(i32, @bitCast(value)) < 0) {
            break; // Found marker
        }
        
        // Pop and continue searching
        _ = try stack_module.popStack(vm);
    }
    
    // Get marker value
    const marker = stack_module.getTopOfStack(vm);
    
    // Extract num and offset from marker: num = (~value) >> 16, offset = GetLoWord(value)
    const num = (~marker) >> 16;
    const offset = types_module.getLoWord(marker) >> 1; // offset was stored as << 1, so >> 1 to get back
    
    // Get current frame
    const frame = vm.current_frame orelse {
        return errors_module.VMError.InvalidAddress;
    };
    
    // Calculate ppvar: (LispPTR *)((DLword *)PVAR + 2 + offset)
    const frame_addr = @intFromPtr(frame);
    const frame_size = @sizeOf(stack.FX);
    const pvar_base = frame_addr + frame_size;
    const pvar_lisp_ptr: [*]LispPTR = @ptrFromInt(pvar_base);
    
    // ppvar = (LispPTR *)((DLword *)PVAR + 2 + offset)
    // PVAR + 2 DLwords = PVAR + 4 bytes = PVAR + 1 LispPTR
    const ppvar: [*]LispPTR = pvar_lisp_ptr + 2 + offset;
    
    // Restore num values to 0xffffffff (unbound marker)
    // C: for (i = num; --i >= 0;) { *--ppvar = 0xffffffff; }
    var i: u32 = num;
    while (i > 0) : (i -= 1) {
        const target_ptr: [*]LispPTR = @ptrFromInt(@intFromPtr(ppvar) - (@as(usize, i) * @sizeOf(LispPTR)));
        target_ptr[0] = 0xFFFFFFFF; // Unbound marker
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
    
    const tos = stack_module.getTopOfStack(vm);
    
    // Check if TOS is negative (marker)
    // C: if ((int)TOPOFSTACK < 0)
    if (@as(i32, @bitCast(tos)) < 0) {
        // TOS is the marker - extract num and offset
        const num = (~tos) >> 16;
        const offset = types_module.getLoWord(tos) >> 1;
        
        if (num != 0) {
            // Get current frame
            const frame = vm.current_frame orelse {
                return errors_module.VMError.InvalidAddress;
            };
            
            // Calculate ppvar
            const frame_addr = @intFromPtr(frame);
            const frame_size = @sizeOf(stack.FX);
            const pvar_base = frame_addr + frame_size;
            const pvar_lisp_ptr: [*]LispPTR = @ptrFromInt(pvar_base);
            const ppvar: [*]LispPTR = pvar_lisp_ptr + 2 + offset;
            
            // Restore num values to unbound
            var i: u32 = num;
            while (i > 0) : (i -= 1) {
                const target_ptr: [*]LispPTR = @ptrFromInt(@intFromPtr(ppvar) - (@as(usize, i) * @sizeOf(LispPTR)));
                target_ptr[0] = 0xFFFFFFFF;
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
        const offset = types_module.getLoWord(marker) >> 1;
        
        if (num != 0) {
            const frame = vm.current_frame orelse {
                return errors_module.VMError.InvalidAddress;
            };
            
            const frame_addr = @intFromPtr(frame);
            const frame_size = @sizeOf(stack.FX);
            const pvar_base = frame_addr + frame_size;
            const pvar_lisp_ptr: [*]LispPTR = @ptrFromInt(pvar_base);
            const ppvar: [*]LispPTR = pvar_lisp_ptr + 2 + offset;
            
            var i: u32 = num;
            while (i > 0) : (i -= 1) {
                const target_ptr: [*]LispPTR = @ptrFromInt(@intFromPtr(ppvar) - (@as(usize, i) * @sizeOf(LispPTR)));
                target_ptr[0] = 0xFFFFFFFF;
            }
        }
        
        // Pop marker
        _ = try stack_module.popStack(vm);
    }
}