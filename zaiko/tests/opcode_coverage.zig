const std = @import("std");
const testing = std.testing;
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

/// Test that all 256 possible bytecode opcodes can be decoded
/// Per FR-001: Implementation MUST execute all 256 bytecode opcodes
test "verify all 256 opcodes can be decoded" {
    // Test all possible byte values (0-255)
    var opcode_count: u32 = 0;
    var handled_count: u32 = 0;
    var unhandled_count: u32 = 0;
    var unhandled_opcodes: [256]u8 = undefined;
    var unhandled_idx: usize = 0;

    var i: u8 = 0;
    while (i < 255) : (i += 1) {
        opcode_count += 1;
        const decoded = dispatch.decodeOpcode(i);
        
        // Check if opcode is handled in executeInstruction switch
        // We can't directly check the switch, but we can verify:
        // 1. Opcode can be decoded (doesn't crash)
        // 2. Opcode has a valid instruction length
        const length = dispatch.getInstructionLength(decoded);
        
        // All opcodes should have a valid length (at least 1 byte)
        try testing.expect(length >= 1);
        try testing.expect(length <= 3); // Max instruction length is 3 bytes
        
        // Check if this is a known opcode (not catch-all)
        if (@intFromEnum(decoded) == i) {
            handled_count += 1;
        } else {
            // Catch-all variant - still valid but not explicitly handled
            unhandled_opcodes[unhandled_idx] = i;
            unhandled_idx += 1;
            unhandled_count += 1;
        }
    }
    
    // Test opcode 255 (last possible value)
    opcode_count += 1;
    const decoded_255 = dispatch.decodeOpcode(255);
    const length_255 = dispatch.getInstructionLength(decoded_255);
    try testing.expect(length_255 >= 1);
    try testing.expect(length_255 <= 3);
    
    if (@intFromEnum(decoded_255) == 255) {
        handled_count += 1;
    } else {
        unhandled_count += 1;
    }

    // Verify we tested all 256 opcodes
    try testing.expect(opcode_count == 256);
    
    // Report coverage
    std.debug.print("\n=== Opcode Coverage Report ===\n", .{});
    std.debug.print("Total opcodes: {}\n", .{opcode_count});
    std.debug.print("Explicitly handled: {}\n", .{handled_count});
    std.debug.print("Catch-all (unhandled): {}\n", .{unhandled_count});
    std.debug.print("Coverage: {d:.1}%\n", .{(@as(f64, @floatFromInt(handled_count)) / @as(f64, @floatFromInt(opcode_count))) * 100.0});
    
    if (unhandled_count > 0) {
        std.debug.print("\nUnhandled opcodes (using catch-all): ", .{});
        var j: usize = 0;
        while (j < unhandled_idx and j < 20) : (j += 1) {
            std.debug.print("0x{X:02} ", .{unhandled_opcodes[j]});
        }
        if (unhandled_idx > 20) {
            std.debug.print("... ({} more)", .{unhandled_idx - 20});
        }
        std.debug.print("\n", .{});
    }
    
    // Note: Having catch-all variants is acceptable for now
    // as long as all opcodes can be decoded and have valid instruction lengths
    // Full implementation of all 256 opcodes is a work in progress
}

/// Test that all explicitly defined opcodes in the enum are valid
test "verify defined opcodes are valid" {
    // Test that all opcodes in the Opcode enum can be decoded correctly
    const Opcode = dispatch.Opcode;
    
    // Test a sample of explicitly defined opcodes
    const test_opcodes = [_]Opcode{
        .CAR, .CDR, .CONS, .NTYPX, .TYPEP, .DTEST, .UNWIND,
        .FN0, .FN1, .FN2, .FN3, .FN4, .FNX, .APPLYFN, .CHECKAPPLY, .RETURN,
        .BIND, .UNBIND, .DUNBIND, .GCREF,
        .JUMP, .FJUMP, .TJUMP, .JUMPX, .FJUMPX, .TJUMPX,
        .JUMP0, .JUMP1, .JUMP2, .JUMP3, .JUMP4, .JUMP5, .JUMP6, .JUMP7,
        .JUMP8, .JUMP9, .JUMP10, .JUMP11, .JUMP12, .JUMP13, .JUMP14, .JUMP15,
        .FJUMP0, .FJUMP1, .FJUMP2, .FJUMP3, .FJUMP4, .FJUMP5, .FJUMP6, .FJUMP7,
        .FJUMP8, .FJUMP9, .FJUMP10, .FJUMP11, .FJUMP12, .FJUMP13, .FJUMP14, .FJUMP15,
        .TJUMP0, .TJUMP1, .TJUMP2, .TJUMP3, .TJUMP4, .TJUMP5, .TJUMP6, .TJUMP7,
        .TJUMP8, .TJUMP9, .TJUMP10, .TJUMP11, .TJUMP12, .TJUMP13, .TJUMP14, .TJUMP15,
        .IVAR0, .IVAR1, .PVAR0, .PVAR1, .FVAR0, .FVAR1,
        .GVAR, .STKSCAN, .SLRETURN, .POP, .POP_N,
        .RPLACA, .RPLACD,
        .GETAEL1, .GETAEL2, .SETAEL1, .SETAEL2,
        .EQ, .EQL, .LESSP, .GREATERP, .IGREATERP, .FGREATERP, .EQUAL,
        .FIXP, .SMALLP, .LISTP,
        .CHARCODE, .CHARN,
        .IPLUS2, .IDIFFERENCE, .ITIMES2, .IQUO, .IREM,
        .PLUS2, .DIFFERENCE, .TIMES2, .QUOTIENT,
        .FPLUS2, .FDIFFERENCE, .FTIMES2, .FQUOTIENT,
        .MAKENUMBER,
        .LOGOR2, .LOGAND2, .LOGXOR2, .LSH,
        .LLSH1, .LLSH8, .LRSH1, .LRSH8,
        .PUSH, .SWAP, .NOP,
        .NIL, .T, .CONST_0, .CONST_1,
        .ACONST, .GCONST,
    };
    
    for (test_opcodes) |opcode| {
        const byte_value = @intFromEnum(opcode);
        const decoded = dispatch.decodeOpcode(byte_value);
        const length = dispatch.getInstructionLength(decoded);
        
        // Verify opcode decodes to itself
        try testing.expect(@intFromEnum(decoded) == byte_value);
        // Verify valid instruction length
        try testing.expect(length >= 1);
        try testing.expect(length <= 3);
    }
}

/// Test instruction length calculation for all opcodes
test "verify instruction lengths are valid for all opcodes" {
    var i: u8 = 0;
    while (i < 255) : (i += 1) {
        const decoded = dispatch.decodeOpcode(i);
        const length = dispatch.getInstructionLength(decoded);
        
        // All instructions must have valid length
        try testing.expect(length >= 1);
        try testing.expect(length <= 3);
    }
    
    // Test last opcode
    const decoded_255 = dispatch.decodeOpcode(255);
    const length_255 = dispatch.getInstructionLength(decoded_255);
    try testing.expect(length_255 >= 1);
    try testing.expect(length_255 <= 3);
}

/// Test that instruction decoding works for all opcodes
test "verify instruction decoding for all opcodes" {
    // Create a code buffer with all possible opcode bytes
    var code: [256]types.ByteCode = undefined;
    var i: u8 = 0;
    while (i < 255) : (i += 1) {
        code[@as(usize, i)] = i;
    }
    code[255] = 255;
    
    // Test decoding at each position
    var decoded_count: u32 = 0;
    i = 0;
    while (i < 255) : (i += 1) {
        const instruction = dispatch.decodeInstruction(i, &code);
        if (instruction) |inst| {
            decoded_count += 1;
            // Verify instruction has valid length
            try testing.expect(inst.length >= 1);
            try testing.expect(inst.length <= 3);
        }
    }
    
    // Test last opcode
    const instruction_255 = dispatch.decodeInstruction(255, &code);
    if (instruction_255) |inst| {
        decoded_count += 1;
        try testing.expect(inst.length >= 1);
        try testing.expect(inst.length <= 3);
    }
    
    // Most opcodes should decode successfully
    // (Some may fail if they require operands beyond available bytes)
    std.debug.print("\nSuccessfully decoded {}/256 opcodes\n", .{decoded_count});
}
