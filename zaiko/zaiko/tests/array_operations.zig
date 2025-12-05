const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const storage = @import("../memory/storage.zig");
const virtual_memory = @import("../memory/virtual.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const array = @import("../src/data/array.zig");
const types = @import("../src/utils/types.zig");

test "GETAEL1 - get array element with 1-byte index" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Allocate array with 5 elements
    const array_addr = try storage.allocateArray(&mem_storage, 5, .TYP_ARRAY);
    const native_ptr = try virtual_memory.translateAddress(array_addr, vmem.fptovp, 4);
    const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

    // Initialize array elements
    array.setArrayElement(header, 0, 10);
    array.setArrayElement(header, 1, 20);
    array.setArrayElement(header, 2, 30);
    array.setArrayElement(header, 3, 40);
    array.setArrayElement(header, 4, 50);

    // Push array pointer and execute GETAEL1
    try stack.pushStack(&vm, array_addr);
    try opcodes.handleGETAEL1(&vm, 2); // Get element at index 2

    // Check element value
    const element = stack.getTopOfStack(&vm);
    try testing.expect(element == 30);
}

test "GETAEL2 - get array element with 2-byte index" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Allocate array with 10 elements
    const array_addr = try storage.allocateArray(&mem_storage, 10, .TYP_ARRAY);
    const native_ptr = try virtual_memory.translateAddress(array_addr, vmem.fptovp, 4);
    const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

    // Initialize array elements
    array.setArrayElement(header, 5, 100);

    // Push array pointer and execute GETAEL2
    try stack.pushStack(&vm, array_addr);
    try opcodes.handleGETAEL2(&vm, 5); // Get element at index 5

    // Check element value
    const element = stack.getTopOfStack(&vm);
    try testing.expect(element == 100);
}

test "SETAEL1 - set array element with 1-byte index" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Allocate array
    const array_addr = try storage.allocateArray(&mem_storage, 5, .TYP_ARRAY);
    const native_ptr = try virtual_memory.translateAddress(array_addr, vmem.fptovp, 4);
    const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

    // Push array pointer, value, and execute SETAEL1
    try stack.pushStack(&vm, array_addr);
    try stack.pushStack(&vm, 99); // Value to set
    try opcodes.handleSETAEL1(&vm, 3); // Set element at index 3

    // Check array pointer is returned
    const returned_ptr = stack.getTopOfStack(&vm);
    try testing.expect(returned_ptr == array_addr);

    // Check element was set
    const element = array.getArrayElement(header, 3);
    try testing.expect(element == 99);
}

test "SETAEL2 - set array element with 2-byte index" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Allocate array
    const array_addr = try storage.allocateArray(&mem_storage, 10, .TYP_ARRAY);
    const native_ptr = try virtual_memory.translateAddress(array_addr, vmem.fptovp, 4);
    const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

    // Push array pointer, value, and execute SETAEL2
    try stack.pushStack(&vm, array_addr);
    try stack.pushStack(&vm, 200); // Value to set
    try opcodes.handleSETAEL2(&vm, 7); // Set element at index 7

    // Check array pointer is returned
    const returned_ptr = stack.getTopOfStack(&vm);
    try testing.expect(returned_ptr == array_addr);

    // Check element was set
    const element = array.getArrayElement(header, 7);
    try testing.expect(element == 200);
}

test "GETAEL1 - out of bounds returns NIL" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Allocate array with 3 elements
    const array_addr = try storage.allocateArray(&mem_storage, 3, .TYP_ARRAY);

    // Push array pointer and execute GETAEL1 with out-of-bounds index
    try stack.pushStack(&vm, array_addr);
    try opcodes.handleGETAEL1(&vm, 10); // Index 10 is out of bounds

    // Check NIL is returned
    const element = stack.getTopOfStack(&vm);
    try testing.expect(element == 0); // NIL
}

test "SETAEL1 - error on NIL array" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Try SETAEL1 on NIL
    try stack.pushStack(&vm, 0); // NIL
    try stack.pushStack(&vm, 100); // Value

    const result = opcodes.handleSETAEL1(&vm, 0);
    try testing.expectError(error.InvalidAddress, result);
}

test "array element access - multiple operations" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Allocate array
    const array_addr = try storage.allocateArray(&mem_storage, 5, .TYP_ARRAY);
    const native_ptr = try virtual_memory.translateAddress(array_addr, vmem.fptovp, 4);
    const header: *array.ArrayHeader = @as(*array.ArrayHeader, @ptrCast(@alignCast(native_ptr)));

    // Set multiple elements
    try stack.pushStack(&vm, array_addr);
    try stack.pushStack(&vm, 11);
    try opcodes.handleSETAEL1(&vm, 0);

    try stack.pushStack(&vm, array_addr);
    try stack.pushStack(&vm, 22);
    try opcodes.handleSETAEL1(&vm, 1);

    try stack.pushStack(&vm, array_addr);
    try stack.pushStack(&vm, 33);
    try opcodes.handleSETAEL1(&vm, 2);

    // Get elements back
    try stack.pushStack(&vm, array_addr);
    try opcodes.handleGETAEL1(&vm, 0);
    try testing.expect(stack.getTopOfStack(&vm) == 11);

    try stack.pushStack(&vm, array_addr);
    try opcodes.handleGETAEL1(&vm, 1);
    try testing.expect(stack.getTopOfStack(&vm) == 22);

    try stack.pushStack(&vm, array_addr);
    try opcodes.handleGETAEL1(&vm, 2);
    try testing.expect(stack.getTopOfStack(&vm) == 33);
}