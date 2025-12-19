const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const storage = @import("../src/memory/storage.zig");
const virtual_memory = @import("../src/memory/virtual.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const cons = @import("../src/data/cons.zig");
const types = @import("../src/utils/types.zig");

/// T053: Test cons cell operations (CAR, CDR, CONS)
/// Per tasks.md T053: Add test case for cons cell operations (CAR, CDR, CONS)
test "T053: CAR operation - get CAR of cons cell" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create a cons cell
    const cell_addr = try storage.allocateConsCell(&mem_storage);
    const native_ptr = try virtual_memory.translateAddress(cell_addr, vmem.fptovp, 4);
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
    cell.car_field = 100;
    cell.cdr_code = cons.CDR_NIL;

    // Push cons cell pointer on stack
    try stack.pushStack(&vm, cell_addr);

    // Execute CAR
    try opcodes.handleCAR(&vm);

    // Check CAR value is on stack
    const car_value = stack.getTopOfStack(&vm);
    try testing.expect(car_value == 100);
}

test "T053: CAR operation - NIL returns NIL" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Push NIL on stack
    try stack.pushStack(&vm, 0);

    // Execute CAR on NIL
    try opcodes.handleCAR(&vm);

    // CAR of NIL is NIL
    const result = stack.getTopOfStack(&vm);
    try testing.expect(result == 0);
}

test "T053: CAR operation - with indirect CDR encoding" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create cons cell with indirect CDR encoding
    const cell_addr = try storage.allocateConsCell(&mem_storage);
    const indirect_addr = try storage.allocateConsCell(&mem_storage);

    const native_ptr = try virtual_memory.translateAddress(cell_addr, vmem.fptovp, 4);
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));

    const indirect_native = try virtual_memory.translateAddress(indirect_addr, vmem.fptovp, 4);
    const indirect_cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(indirect_native)));

    // Set up indirect encoding: CAR points to indirect cell, CDR is INDIRECT
    cell.car_field = indirect_addr;
    cell.cdr_code = cons.CDR_INDIRECT;
    indirect_cell.car_field = 200; // Actual CAR value
    indirect_cell.cdr_code = cons.CDR_NIL;

    // Push cons cell pointer on stack
    try stack.pushStack(&vm, cell_addr);

    // Execute CAR
    try opcodes.handleCAR(&vm);

    // Check CAR value from indirect cell
    const car_value = stack.getTopOfStack(&vm);
    try testing.expect(car_value == 200);
}

test "T053: CDR operation - get CDR of cons cell" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create a cons cell with CDR = NIL
    const cell_addr = try storage.allocateConsCell(&mem_storage);
    const native_ptr = try virtual_memory.translateAddress(cell_addr, vmem.fptovp, 4);
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
    cell.car_field = 100;
    cell.cdr_code = cons.CDR_NIL;

    // Push cons cell pointer on stack
    try stack.pushStack(&vm, cell_addr);

    // Execute CDR
    try opcodes.handleCDR(&vm);

    // Check CDR value is NIL
    const cdr_value = stack.getTopOfStack(&vm);
    try testing.expect(cdr_value == 0);
}

test "T053: CDR operation - with same-page encoding" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create two cons cells on same page
    const cell1_addr = try storage.allocateConsCell(&mem_storage);
    const cell2_addr = try storage.allocateConsCell(&mem_storage);

    const native_ptr1 = try virtual_memory.translateAddress(cell1_addr, vmem.fptovp, 4);
    const cell1: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr1)));
    cell1.car_field = 100;

    // Set CDR to point to cell2 using same-page encoding
    const cell1_page = cell1_addr & 0xFFFF00;
    const cell2_page = cell2_addr & 0xFFFF00;

    if (cell1_page == cell2_page and cell2_addr > cell1_addr and cell2_addr <= cell1_addr + 14) {
        // Same page - can use same-page encoding
        cons.setCDR(cell1, cell1_addr, cell2_addr);

        const native_ptr2 = try virtual_memory.translateAddress(cell2_addr, vmem.fptovp, 4);
        const cell2: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr2)));
        cell2.car_field = 200;
        cell2.cdr_code = cons.CDR_NIL;

        // Push cell1 pointer on stack
        try stack.pushStack(&vm, cell1_addr);

        // Execute CDR
        try opcodes.handleCDR(&vm);

        // Check CDR value points to cell2
        const cdr_value = stack.getTopOfStack(&vm);
        try testing.expect(cdr_value == cell2_addr);
    }
}

test "T053: CONS operation - create new cons cell" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Push CDR (NIL) and CAR (100) on stack
    try stack.pushStack(&vm, 0); // CDR = NIL
    try stack.pushStack(&vm, 100); // CAR = 100

    // Execute CONS
    try opcodes.handleCONS(&vm);

    // Check new cons cell address is on stack
    const cell_addr = stack.getTopOfStack(&vm);
    try testing.expect(cell_addr != 0);

    // Verify cons cell contents
    const native_ptr = try virtual_memory.translateAddress(cell_addr, vmem.fptovp, 4);
    const cell: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr)));
    try testing.expect(cell.car_field == 100);
    try testing.expect(cell.cdr_code == cons.CDR_NIL);
}

test "T053: CONS operation - create list with multiple elements" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();

    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();

    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();

    // Create first cons cell: (100 . NIL)
    try stack.pushStack(&vm, 0); // CDR = NIL
    try stack.pushStack(&vm, 100); // CAR = 100
    try opcodes.handleCONS(&vm);
    const cell1_addr = stack.getTopOfStack(&vm);

    // Create second cons cell: (200 . (100 . NIL))
    try stack.pushStack(&vm, cell1_addr); // CDR = first cell
    try stack.pushStack(&vm, 200); // CAR = 200
    try opcodes.handleCONS(&vm);
    const cell2_addr = stack.getTopOfStack(&vm);

    // Verify list structure
    const native_ptr2 = try virtual_memory.translateAddress(cell2_addr, vmem.fptovp, 4);
    const cell2: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr2)));
    try testing.expect(cell2.car_field == 200);

    // Get CDR of cell2 (should point to cell1)
    const cdr2 = cons.getCDR(cell2, cell2_addr);
    try testing.expect(cdr2 == cell1_addr);

    // Verify cell1
    const native_ptr1 = try virtual_memory.translateAddress(cell1_addr, vmem.fptovp, 4);
    const cell1: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr1)));
    try testing.expect(cell1.car_field == 100);
    try testing.expect(cell1.cdr_code == cons.CDR_NIL);
}
