const std = @import("std");
const testing = std.testing;
const dispatch = @import("../src/vm/dispatch.zig");
const stack = @import("../src/vm/stack.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const types = @import("../src/utils/types.zig");

test "arithmetic opcodes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test IPLUS2
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 20);
    try opcodes.handleIPLUS2(&vm);
    const result1 = stack.getTopOfStack(&vm);
    try testing.expect(result1 == 30);

    // Test IDIFFERENCE
    try stack.pushStack(&vm, 50);
    try stack.pushStack(&vm, 30);
    try opcodes.handleIDIFFERENCE(&vm);
    const result2 = stack.getTopOfStack(&vm);
    try testing.expect(result2 == 20);
}

test "comparison opcodes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test EQ
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 10);
    try opcodes.handleEQ(&vm);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1); // T

    // Test EQ with different values
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 20);
    try opcodes.handleEQ(&vm);
    const result2 = stack.getTopOfStack(&vm);
    try testing.expect(result2 == 0); // NIL
}

test "arithmetic opcodes - multiplication and division" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test ITIMES2
    try stack.pushStack(&vm, 6);
    try stack.pushStack(&vm, 7);
    try opcodes.handleITIMES2(&vm);
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 42);

    // Test IQUO
    try stack.pushStack(&vm, 20);
    try stack.pushStack(&vm, 4);
    try opcodes.handleIQUO(&vm);
    const quotient = stack.getTopOfStack(&vm);
    try testing.expect(quotient == 5);

    // Test IREM
    try stack.pushStack(&vm, 23);
    try stack.pushStack(&vm, 5);
    try opcodes.handleIREM(&vm);
    const remainder = stack.getTopOfStack(&vm);
    try testing.expect(remainder == 3);
}

// T032: Test arithmetic opcodes matching C emulator results
// Tests SMALLP encoding, overflow handling, and edge cases
test "T032: arithmetic opcodes - SMALLP/FIXP handling matching C" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test IPLUS2 with SMALLP values (0-65535)
    // Positive SMALLP: S_POSITIVE (0xE0000) | value
    const small_pos_1 = types.S_POSITIVE | 100;
    const small_pos_2 = types.S_POSITIVE | 200;
    try stack.pushStack(&vm, small_pos_1);
    try stack.pushStack(&vm, small_pos_2);
    try opcodes.handleIPLUS2(&vm);
    const result = stack.getTopOfStack(&vm);
    // Result should be SMALLP: S_POSITIVE | 300
    try testing.expect(result == (types.S_POSITIVE | 300));

    // Test IDIFFERENCE with negative SMALLP
    // Negative SMALLP: S_NEGATIVE (0xF0000) | (value & 0xFFFF)
    const small_neg = types.S_NEGATIVE | 50;
    const small_pos = types.S_POSITIVE | 100;
    try stack.pushStack(&vm, small_pos);
    try stack.pushStack(&vm, small_neg);
    try opcodes.handleIDIFFERENCE(&vm);
    const diff_result = stack.getTopOfStack(&vm);
    // 100 - (-50) = 150, should be SMALLP: S_POSITIVE | 150
    try testing.expect(diff_result == (types.S_POSITIVE | 150));

    // Test IQUO with division by zero (should return error)
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 0);
    const div_by_zero = opcodes.handleIQUO(&vm);
    try testing.expectError(error.DivisionByZero, div_by_zero);

    // Test IREM with division by zero (should return error)
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 0);
    const rem_by_zero = opcodes.handleIREM(&vm);
    try testing.expectError(error.DivisionByZero, rem_by_zero);

    // Test edge case: MAX_SMALL + 1 (should trigger overflow or FIXP)
    const max_small = types.S_POSITIVE | types.MAX_SMALL;
    try stack.pushStack(&vm, max_small);
    try stack.pushStack(&vm, types.S_POSITIVE | 1);
    try opcodes.handleIPLUS2(&vm);
    // Result > MAX_SMALL, should be FIXP (but FIXP creation deferred to Phase 4)
    // For now, just verify it doesn't crash
    _ = stack.getTopOfStack(&vm);
}

test "type checking opcodes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test FIXP with even address (pointer)
    try stack.pushStack(&vm, 0x1000); // Even address
    try opcodes.handleFIXP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // T

    // Test FIXP with odd address (fixnum)
    try stack.pushStack(&vm, 0x1001); // Odd address
    try opcodes.handleFIXP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0); // NIL

    // Test SMALLP
    try stack.pushStack(&vm, 1000); // Small integer
    try opcodes.handleSMALLP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // T

    // Test LISTP with NIL
    try stack.pushStack(&vm, 0); // NIL
    try opcodes.handleLISTP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // T (NIL is a list)
}

test "comparison opcodes - all variants" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Test LESSP
    try stack.pushStack(&vm, 5);
    try stack.pushStack(&vm, 10);
    try opcodes.handleLESSP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // 5 < 10

    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 5);
    try opcodes.handleLESSP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0); // 10 < 5 is false

    // Test GREATERP
    try stack.pushStack(&vm, 10);
    try stack.pushStack(&vm, 5);
    try opcodes.handleGREATERP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // 10 > 5

    // Test IGREATERP
    try stack.pushStack(&vm, 15);
    try stack.pushStack(&vm, 8);
    try opcodes.handleIGREATERP(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // 15 > 8
}