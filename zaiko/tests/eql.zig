const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const storage = @import("../src/memory/storage.zig");
const virtual_memory = @import("../src/memory/virtual.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const cons = @import("../src/data/cons.zig");
const types = @import("../src/utils/types.zig");

test "EQL - pointer equality (same as EQ)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Same pointer values
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 100);
    try opcodes.handleEQL(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // T

    // Different pointer values
    try stack.pushStack(&vm, 100);
    try stack.pushStack(&vm, 200);
    try opcodes.handleEQL(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0); // NIL
}

test "EQL - NIL comparison" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = try stack.VM.init(allocator, 1024);
    defer vm.deinit();

    // Both NIL
    try stack.pushStack(&vm, 0);
    try stack.pushStack(&vm, 0);
    try opcodes.handleEQL(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 1); // T

    // One NIL, one not
    try stack.pushStack(&vm, 0);
    try stack.pushStack(&vm, 100);
    try opcodes.handleEQL(&vm);
    try testing.expect(stack.getTopOfStack(&vm) == 0); // NIL
}

test "EQL - deep comparison of cons cells" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create two identical cons cells
    const cell1_addr = try storage.allocateConsCell(&mem_storage);
    const cell2_addr = try storage.allocateConsCell(&mem_storage);

    const native_ptr1 = try virtual_memory.translateAddress(cell1_addr, vmem.fptovp, 4);
    const cell1: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr1)));
    cell1.car_field = 42;
    cell1.cdr_code = cons.CDR_NIL;

    const native_ptr2 = try virtual_memory.translateAddress(cell2_addr, vmem.fptovp, 4);
    const cell2: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr2)));
    cell2.car_field = 42;
    cell2.cdr_code = cons.CDR_NIL;

    // Compare with EQL (should be equal by value)
    try stack.pushStack(&vm, cell1_addr);
    try stack.pushStack(&vm, cell2_addr);
    try opcodes.handleEQL(&vm);

    // EQL should return T (equal by value, even though different pointers)
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 1); // T
}

test "EQL - different cons cells" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create two different cons cells
    const cell1_addr = try storage.allocateConsCell(&mem_storage);
    const cell2_addr = try storage.allocateConsCell(&mem_storage);

    const native_ptr1 = try virtual_memory.translateAddress(cell1_addr, vmem.fptovp, 4);
    const cell1: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr1)));
    cell1.car_field = 42;
    cell1.cdr_code = cons.CDR_NIL;

    const native_ptr2 = try virtual_memory.translateAddress(cell2_addr, vmem.fptovp, 4);
    const cell2: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr2)));
    cell2.car_field = 99; // Different CAR
    cell2.cdr_code = cons.CDR_NIL;

    // Compare with EQL
    try stack.pushStack(&vm, cell1_addr);
    try stack.pushStack(&vm, cell2_addr);
    try opcodes.handleEQL(&vm);

    // Should return NIL (different values)
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0); // NIL
}

test "EQL vs EQ - pointer vs deep equality" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create two identical cons cells (different pointers, same values)
    const cell1_addr = try storage.allocateConsCell(&mem_storage);
    const cell2_addr = try storage.allocateConsCell(&mem_storage);

    const native_ptr1 = try virtual_memory.translateAddress(cell1_addr, vmem.fptovp, 4);
    const cell1: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr1)));
    cell1.car_field = 50;
    cell1.cdr_code = cons.CDR_NIL;

    const native_ptr2 = try virtual_memory.translateAddress(cell2_addr, vmem.fptovp, 4);
    const cell2: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr2)));
    cell2.car_field = 50;
    cell2.cdr_code = cons.CDR_NIL;

    // EQ should return NIL (different pointers)
    try stack.pushStack(&vm, cell1_addr);
    try stack.pushStack(&vm, cell2_addr);
    try opcodes.handleEQ(&vm);
    const eq_result = stack.getTopOfStack(&vm);
    try testing.expect(eq_result == 0); // NIL (different pointers)

    // EQL should return T (same values)
    try stack.pushStack(&vm, cell1_addr);
    try stack.pushStack(&vm, cell2_addr);
    try opcodes.handleEQL(&vm);
    const eql_result = stack.getTopOfStack(&vm);
    try testing.expect(eql_result == 1); // T (equal by value)
}
