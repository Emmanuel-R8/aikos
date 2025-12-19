const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "JUMP0-JUMP15 instruction decoding" {
    // Test that all JUMP variants can be decoded
    const jump_opcodes = [_]types.ByteCode{ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F };

    for (jump_opcodes, 0..) |opcode, i| {
        const code: []const types.ByteCode = &[_]types.ByteCode{opcode};
        const instruction = dispatch.decodeInstruction(0, code);
        try testing.expect(instruction != null);
        if (instruction) |inst| {
            try testing.expect(inst.length == 1); // 1-byte opcodes
            // Verify it's a JUMP variant
            const opcode_enum = inst.opcode;
            const expected_offset = @as(i64, @intCast(i));
            // Check that executing returns the correct offset
            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            const allocator = gpa.allocator();
            var vm = try stack.VM.init(allocator, 1024);
            defer vm.deinit();

            const jump_offset = try dispatch.executeInstruction(&vm, inst);
            try testing.expect(jump_offset != null);
            if (jump_offset) |offset| {
                try testing.expect(offset == expected_offset);
            }
        }
    }
}

test "FJUMP0-FJUMP15 instruction decoding" {
    const fjump_opcodes = [_]types.ByteCode{ 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F };

    for (fjump_opcodes, 0..) |opcode, i| {
        const code: []const types.ByteCode = &[_]types.ByteCode{opcode};
        const instruction = dispatch.decodeInstruction(0, code);
        try testing.expect(instruction != null);
        if (instruction) |inst| {
            try testing.expect(inst.length == 1);

            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            const allocator = gpa.allocator();
            var vm = try stack.VM.init(allocator, 1024);
            defer vm.deinit();

            // Push false (0) on stack - should jump
            try stack.pushStack(&vm, 0);
            const jump_offset = try dispatch.executeInstruction(&vm, inst);
            try testing.expect(jump_offset != null);
            if (jump_offset) |offset| {
                try testing.expect(offset == @as(i64, @intCast(i)));
            }

            // Push true (non-zero) on stack - should not jump
            try stack.pushStack(&vm, 1);
            const no_jump = try dispatch.executeInstruction(&vm, inst);
            try testing.expect(no_jump == null);
        }
    }
}

test "TJUMP0-TJUMP15 instruction decoding" {
    const tjump_opcodes = [_]types.ByteCode{ 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF };

    for (tjump_opcodes, 0..) |opcode, i| {
        const code: []const types.ByteCode = &[_]types.ByteCode{opcode};
        const instruction = dispatch.decodeInstruction(0, code);
        try testing.expect(instruction != null);
        if (instruction) |inst| {
            try testing.expect(inst.length == 1);

            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            const allocator = gpa.allocator();
            var vm = try stack.VM.init(allocator, 1024);
            defer vm.deinit();

            // Push true (non-zero) on stack - should jump
            try stack.pushStack(&vm, 1);
            const jump_offset = try dispatch.executeInstruction(&vm, inst);
            try testing.expect(jump_offset != null);
            if (jump_offset) |offset| {
                try testing.expect(offset == @as(i64, @intCast(i)));
            }

            // Push false (0) on stack - should not jump
            try stack.pushStack(&vm, 0);
            const no_jump = try dispatch.executeInstruction(&vm, inst);
            try testing.expect(no_jump == null);
        }
    }
}

/// T055: Test jump opcode variants
/// Per tasks.md T055: Add test case for jump opcode variants
test "T055: JUMPX - unconditional jump with 16-bit offset" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // JUMPX opcode (0xB0) with 2-byte signed offset
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0xB0, 0x10, 0x00 }; // Jump +16
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.length == 3);

        const jump_offset = try dispatch.executeInstruction(&vm, inst);
        try testing.expect(jump_offset != null);
        if (jump_offset) |offset| {
            try testing.expect(offset == 16);
        }
    }
}

test "T055: FJUMPX - conditional jump if false with 16-bit offset" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // FJUMPX opcode (0xB2) with 2-byte signed offset
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0xB2, 0x20, 0x00 }; // Jump +32 if false
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.length == 3);

        // Push false (0) on stack - should jump
        try stack.pushStack(&vm, 0);
        const jump_offset = try dispatch.executeInstruction(&vm, inst);
        try testing.expect(jump_offset != null);
        if (jump_offset) |offset| {
            try testing.expect(offset == 32);
        }

        // Push true (non-zero) on stack - should not jump
        try stack.pushStack(&vm, 1);
        const no_jump = try dispatch.executeInstruction(&vm, inst);
        try testing.expect(no_jump == null);
    }
}

test "T055: TJUMPX - conditional jump if true with 16-bit offset" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // TJUMPX opcode (0xB3) with 2-byte signed offset
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0xB3, 0x30, 0x00 }; // Jump +48 if true
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.length == 3);

        // Push true (non-zero) on stack - should jump
        try stack.pushStack(&vm, 1);
        const jump_offset = try dispatch.executeInstruction(&vm, inst);
        try testing.expect(jump_offset != null);
        if (jump_offset) |offset| {
            try testing.expect(offset == 48);
        }

        // Push false (0) on stack - should not jump
        try stack.pushStack(&vm, 0);
        const no_jump = try dispatch.executeInstruction(&vm, inst);
        try testing.expect(no_jump == null);
    }
}

test "T055: Negative jump offsets" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // JUMPX with negative offset (backward jump)
    // -16 in two's complement: 0xFFF0
    const code: []const types.ByteCode = &[_]types.ByteCode{ 0xB0, 0xF0, 0xFF }; // Jump -16
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        const jump_offset = try dispatch.executeInstruction(&vm, inst);
        try testing.expect(jump_offset != null);
        if (jump_offset) |offset| {
            try testing.expect(offset == -16);
        }
    }
}
