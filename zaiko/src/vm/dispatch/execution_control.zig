const errors = @import("../../utils/errors.zig");
const opcodes = @import("../opcodes.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;

// ============================================================================
// Execution - Control Flow Opcodes
// ============================================================================

/// Helper function for FJUMP handlers
/// C: FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
pub fn handleFJUMPWithOffset(vm: *VM, offset: i8) errors.VMError!?i64 {
    const stack_module = @import("../stack.zig");
    const tos = stack_module.getTopOfStack(vm);
    _ = try stack_module.popStack(vm); // Always pop (C: POP in both branches)
    if (tos == 0) {
        // NIL - jump
        try opcodes.handleFJUMP(vm, offset);
        return @as(i64, offset);
    }
    // Not NIL - continue
    return null;
}

/// Helper function for TJUMP handlers
/// C: TJUMPMACRO(x): if (TOPOFSTACK == 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
pub fn handleTJUMPWithOffset(vm: *VM, offset: i8) errors.VMError!?i64 {
    const std = @import("std");
    const stack_module = @import("../stack.zig");

    // Check stack depth before popping
    const stack_depth = stack_module.getStackDepth(vm);
    std.debug.print("DEBUG: TJUMP: stack_depth={}, offset={}\n", .{ stack_depth, offset });

    if (stack_depth == 0) {
        std.debug.print("ERROR: TJUMP: Stack is empty, cannot pop!\n", .{});
        return error.StackUnderflow;
    }

    const tos = stack_module.getTopOfStack(vm);
    std.debug.print("DEBUG: TJUMP: TOS=0x{x}, offset={}\n", .{ tos, offset });
    _ = try stack_module.popStack(vm); // Always pop (C: POP in both branches)

    const new_stack_depth = stack_module.getStackDepth(vm);
    std.debug.print("DEBUG: TJUMP: After pop, stack_depth={}\n", .{new_stack_depth});

    if (tos != 0) {
        // Not NIL - jump
        std.debug.print("DEBUG: TJUMP: TOS is non-NIL, jumping by {}\n", .{offset});
        try opcodes.handleTJUMP(vm, offset);
        return @as(i64, offset);
    }
    // NIL - continue
    std.debug.print("DEBUG: TJUMP: TOS is NIL, continuing\n", .{});
    return null;
}

/// Handle control flow opcodes
/// Returns jump offset if instruction is a jump, null if handled with no jump
/// Returns error.NotHandled if opcode doesn't match this category
pub fn handleControlFlow(vm: *VM, opcode: Opcode, instruction: Instruction) errors.VMError!?i64 {
    switch (opcode) {
        // Control flow - Function calls
        .FN0 => {
            try opcodes.handleFN0(vm, &instruction);
            return null;
        },
        .FN1 => {
            try opcodes.handleFN1(vm, &instruction);
            return null;
        },
        .FN2 => {
            try opcodes.handleFN2(vm, &instruction);
            return null;
        },
        .FN3 => {
            try opcodes.handleFN3(vm, &instruction);
            return null;
        },
        .FN4 => {
            try opcodes.handleFN4(vm, &instruction);
            return null;
        },
        .FNX => {
            // FNX has variable argument count - will implement later
            // TODO: Implement handleFNX similar to handleFN but with variable arg count
            return error.InvalidOpcode; // Not yet implemented
        },
        .APPLYFN => {
            try opcodes.handleAPPLYFN(vm);
            return null;
        },
        .CHECKAPPLY => {
            try opcodes.handleCHECKAPPLY(vm);
            return null;
        },
        .RETURN => {
            try opcodes.handleRETURN(vm);
            return null;
        },
        .BIND => {
            // BIND takes 2 byte operands: byte1 (n1:4, n2:4), byte2 (offset)
            try opcodes.handleBIND(vm, instruction.getByteOperand(0), instruction.getByteOperand(1));
            return null;
        },
        .UNBIND => {
            try opcodes.handleUNBIND(vm);
            return null;
        },
        .DUNBIND => {
            try opcodes.handleDUNBIND(vm);
            return null;
        },
        .RPLPTR_N => {
            try opcodes.handleRPLPTR_N(vm, instruction.getByteOperand(0));
            return null;
        },
        .GCREF => {
            try opcodes.handleGCREF(vm, instruction.getByteOperand(0));
            return null;
        },
        .ASSOC => try opcodes.handleASSOC(vm),
        .GVAR_ => try opcodes.handleGVAR_(vm, instruction.getWordOperand(0)),
        // Note: No generic JUMP opcode - use JUMPX, JUMPXX, or JUMP0-JUMP15
        // Optimized jump variants (offset encoded in opcode)
        .JUMP0 => {
            try opcodes.handleJUMP(vm);
            return 0;
        },
        .JUMP1 => {
            try opcodes.handleJUMP(vm);
            return 1;
        },
        .JUMP2 => {
            try opcodes.handleJUMP(vm);
            return 2;
        },
        .JUMP3 => {
            try opcodes.handleJUMP(vm);
            return 3;
        },
        .JUMP4 => {
            try opcodes.handleJUMP(vm);
            return 4;
        },
        .JUMP5 => {
            try opcodes.handleJUMP(vm);
            return 5;
        },
        .JUMP6 => {
            try opcodes.handleJUMP(vm);
            return 6;
        },
        .JUMP7 => {
            try opcodes.handleJUMP(vm);
            return 7;
        },
        .JUMP8 => {
            try opcodes.handleJUMP(vm);
            return 8;
        },
        .JUMP9 => {
            try opcodes.handleJUMP(vm);
            return 9;
        },
        .JUMP10 => {
            try opcodes.handleJUMP(vm);
            return 10;
        },
        .JUMP11 => {
            try opcodes.handleJUMP(vm);
            return 11;
        },
        .JUMP12 => {
            try opcodes.handleJUMP(vm);
            return 12;
        },
        .JUMP13 => {
            try opcodes.handleJUMP(vm);
            return 13;
        },
        .JUMP14 => {
            try opcodes.handleJUMP(vm);
            return 14;
        },
        .JUMP15 => {
            try opcodes.handleJUMP(vm);
            return 15;
        },
        // Note: No generic FJUMP opcode - use FJUMPX or FJUMP0-FJUMP15
        // Optimized false jump variants
        // C: FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
        .FJUMP0 => return handleFJUMPWithOffset(vm, 0),
        .FJUMP1 => return handleFJUMPWithOffset(vm, 1),
        .FJUMP2 => return handleFJUMPWithOffset(vm, 2),
        .FJUMP3 => return handleFJUMPWithOffset(vm, 3),
        .FJUMP4 => return handleFJUMPWithOffset(vm, 4),
        .FJUMP5 => return handleFJUMPWithOffset(vm, 5),
        .FJUMP6 => return handleFJUMPWithOffset(vm, 6),
        .FJUMP7 => return handleFJUMPWithOffset(vm, 7),
        .FJUMP8 => return handleFJUMPWithOffset(vm, 8),
        .FJUMP9 => return handleFJUMPWithOffset(vm, 9),
        .FJUMP10 => return handleFJUMPWithOffset(vm, 10),
        .FJUMP11 => return handleFJUMPWithOffset(vm, 11),
        .FJUMP12 => return handleFJUMPWithOffset(vm, 12),
        .FJUMP13 => return handleFJUMPWithOffset(vm, 13),
        .FJUMP14 => return handleFJUMPWithOffset(vm, 14),
        .FJUMP15 => return handleFJUMPWithOffset(vm, 15),
        // Note: No generic TJUMP opcode - use TJUMPX or TJUMP0-TJUMP15
        // Optimized true jump variants
        // C: TJUMPMACRO(x): if (TOPOFSTACK == 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }
        .TJUMP0 => return handleTJUMPWithOffset(vm, 0),
        .TJUMP1 => return handleTJUMPWithOffset(vm, 1),
        .TJUMP2 => return handleTJUMPWithOffset(vm, 2),
        .TJUMP3 => return handleTJUMPWithOffset(vm, 3),
        .TJUMP4 => return handleTJUMPWithOffset(vm, 4),
        .TJUMP5 => return handleTJUMPWithOffset(vm, 5),
        .TJUMP6 => return handleTJUMPWithOffset(vm, 6),
        .TJUMP7 => return handleTJUMPWithOffset(vm, 7),
        .TJUMP8 => return handleTJUMPWithOffset(vm, 8),
        .TJUMP9 => return handleTJUMPWithOffset(vm, 9),
        .TJUMP10 => return handleTJUMPWithOffset(vm, 10),
        .TJUMP11 => return handleTJUMPWithOffset(vm, 11),
        .TJUMP12 => return handleTJUMPWithOffset(vm, 12),
        .TJUMP13 => return handleTJUMPWithOffset(vm, 13),
        .TJUMP14 => return handleTJUMPWithOffset(vm, 14),
        .TJUMP15 => return handleTJUMPWithOffset(vm, 15),
        .JUMPX => {
            try opcodes.handleJUMPX(vm);
            return @as(i64, instruction.getSignedWordOperand(0));
        },
        .JUMPXX => {
            try opcodes.handleJUMPXX(vm);
            return @as(i64, instruction.getSignedWordOperand(0));
        },
        .FJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleFJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .TJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleTJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos != 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .NFJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleNFJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        .NTJUMPX => {
            const offset = instruction.getSignedWordOperand(0);
            try opcodes.handleNTJUMPX(vm, offset);
            const stack_module = @import("../stack.zig");
            const tos = stack_module.getTopOfStack(vm);
            if (tos == 0) {
                return @as(i64, offset);
            }
            return null;
        },
        else => return error.NotHandled, // Not a control flow opcode
    }
    return error.NotHandled; // Should never reach here, but satisfy compiler
}
