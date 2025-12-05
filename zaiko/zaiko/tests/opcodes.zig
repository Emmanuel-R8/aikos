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