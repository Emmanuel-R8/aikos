const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const dispatch = @import("../src/vm/dispatch.zig");
const types = @import("../src/utils/types.zig");

test "LSH - left shift" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and positive shift amount
    try stack.pushStack(&vm, 0b1010); // Value: 10
    try stack.pushStack(&vm, 2); // Shift left by 2
    
    // Perform LSH: 0b1010 << 2 = 0b101000 (40)
    try opcodes.handleLSH(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b101000); // 40
}

test "LSH - right shift" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and negative shift amount
    try stack.pushStack(&vm, 0b101000); // Value: 40
    const shift_amount: types.LispPTR = @bitCast(@as(i32, -2)); // Shift right by 2
    try stack.pushStack(&vm, shift_amount);
    
    // Perform LSH: 0b101000 >> 2 = 0b1010 (10)
    try opcodes.handleLSH(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1010); // 10
}

test "LSH - zero shift" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and zero shift amount
    try stack.pushStack(&vm, 0b1010); // Value: 10
    try stack.pushStack(&vm, 0); // Shift by 0
    
    // Perform LSH: value should remain unchanged
    try opcodes.handleLSH(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0b1010); // 10
}

test "LSH - large left shift" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and large shift amount
    try stack.pushStack(&vm, 1);
    try stack.pushStack(&vm, 31); // Shift left by 31
    
    // Perform LSH: 1 << 31 = 0x80000000
    try opcodes.handleLSH(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0x80000000);
}

test "LSH - large right shift" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and large negative shift amount
    try stack.pushStack(&vm, 0x80000000);
    const shift_amount: types.LispPTR = @bitCast(@as(i32, -31)); // Shift right by 31
    
    try stack.pushStack(&vm, shift_amount);
    
    // Perform LSH: 0x80000000 >> 31 = 1 (logical shift, zero-fill)
    try opcodes.handleLSH(&vm);
    
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1);
}

test "LSH - shift beyond 31 bits (clamped)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value and shift amount > 31
    try stack.pushStack(&vm, 1);
    try stack.pushStack(&vm, 100); // Shift left by 100 (should clamp to 31)
    
    // Perform LSH: Should clamp to 31-bit shift
    try opcodes.handleLSH(&vm);
    
    const result = stack.getTopOfStack(&vm);
    // Result should be 1 << 31 = 0x80000000 (clamped)
    try testing.expect(result == 0x80000000);
}

test "LSH instruction decoding" {
    // Test that LSH instruction is decoded correctly
    const code: []const types.ByteCode = &[_]types.ByteCode{
        0xE7, // LSH opcode
    };
    
    const instruction = dispatch.decodeInstruction(0, code);
    try testing.expect(instruction != null);
    if (instruction) |inst| {
        try testing.expect(inst.opcode == .LSH);
        try testing.expect(inst.length == 1);
    }
}

test "LSH - multiple shifts" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();
    
    // Push value
    try stack.pushStack(&vm, 0b1010); // 10
    
    // Shift left by 2
    try stack.pushStack(&vm, 0b1010);
    try stack.pushStack(&vm, 2);
    try opcodes.handleLSH(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0b101000); // 40
    
    // Shift right by 1
    try stack.pushStack(&vm, stack.getTopOfStack(&vm));
    const shift_right: types.LispPTR = @bitCast(@as(i32, -1));
    try stack.pushStack(&vm, shift_right);
    try opcodes.handleLSH(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0b10100); // 20
}
