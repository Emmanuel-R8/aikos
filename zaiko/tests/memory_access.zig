const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const storage = @import("../src/memory/storage.zig");
const virtual_memory = @import("../src/memory/virtual.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const types = @import("../src/utils/types.zig");
const dispatch = @import("../src/vm/dispatch.zig");

test "CONS with memory" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Initialize storage and virtual memory
    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();
    
    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();
    
    // Initialize VM with memory
    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();
    
    // Push two values
    try stack.pushStack(&vm, 42); // CAR
    try stack.pushStack(&vm, 0);  // CDR (NIL)
    
    // Execute CONS
    try opcodes.handleCONS(&vm);
    
    // Check result
    const cell_addr = stack.getTopOfStack(&vm);
    try testing.expect(cell_addr != 0); // Should have allocated a cell
}

test "CAR operation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();
    
    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();
    
    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();
    
    // Create a cons cell manually
    const cell_addr = try storage.allocateConsCell(&mem_storage);
    const native_ptr = try virtual_memory.translateAddress(cell_addr, vmem.fptovp, 4);
    const cell: *@import("../src/data/cons.zig").ConsCell = @as(*@import("../src/data/cons.zig").ConsCell, @ptrCast(@alignCast(native_ptr)));
    cell.car_field = 123;
    cell.cdr_code = @import("../src/data/cons.zig").CDR_NIL;
    
    // Push cell address and execute CAR
    try stack.pushStack(&vm, cell_addr);
    try opcodes.handleCAR(&vm);
    
    // Check CAR value
    const car_value = stack.getTopOfStack(&vm);
    try testing.expect(car_value == 123);
}

test "CDR operation with NIL" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();
    
    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();
    
    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();
    
    // Create a cons cell with NIL CDR
    const cell_addr = try storage.allocateConsCell(&mem_storage);
    const native_ptr = try virtual_memory.translateAddress(cell_addr, vmem.fptovp, 4);
    const cell: *@import("../src/data/cons.zig").ConsCell = @as(*@import("../src/data/cons.zig").ConsCell, @ptrCast(@alignCast(native_ptr)));
    cell.car_field = 456;
    cell.cdr_code = @import("../src/data/cons.zig").CDR_NIL;
    
    // Push cell address and execute CDR
    try stack.pushStack(&vm, cell_addr);
    try opcodes.handleCDR(&vm);
    
    // Check CDR value (should be NIL)
    const cdr_value = stack.getTopOfStack(&vm);
    try testing.expect(cdr_value == 0);
}
