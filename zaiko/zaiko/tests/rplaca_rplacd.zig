const std = @import("std");
const testing = std.testing;
const stack = @import("../src/vm/stack.zig");
const storage = @import("../src/memory/storage.zig");
const virtual_memory = @import("../src/memory/virtual.zig");
const opcodes = @import("../src/vm/opcodes.zig");
const cons = @import("../src/data/cons.zig");
const types = @import("../src/utils/types.zig");

test "RPLACA - replace CAR of cons cell" {
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
    
    // Push cons cell pointer and new CAR value
    try stack.pushStack(&vm, cell_addr);
    try stack.pushStack(&vm, 200); // New CAR value
    
    // Execute RPLACA
    try opcodes.handleRPLACA(&vm);
    
    // Check cons cell pointer is returned
    const returned_ptr = stack.getTopOfStack(&vm);
    try testing.expect(returned_ptr == cell_addr);
    
    // Check CAR was updated
    try testing.expect(cell.car_field == 200);
}

test "RPLACD - replace CDR of cons cell" {
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
    cell.cdr_code = cons.CDR_NIL; // CDR is NIL
    
    // Push cons cell pointer and new CDR value (NIL)
    try stack.pushStack(&vm, cell_addr);
    try stack.pushStack(&vm, 0); // New CDR value (NIL)
    
    // Execute RPLACD
    try opcodes.handleRPLACD(&vm);
    
    // Check cons cell pointer is returned
    const returned_ptr = stack.getTopOfStack(&vm);
    try testing.expect(returned_ptr == cell_addr);
    
    // Check CDR was updated (should still be NIL)
    try testing.expect(cell.cdr_code == cons.CDR_NIL);
}

test "RPLACD - set CDR to non-NIL value" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();
    
    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();
    
    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();
    
    // Create two cons cells
    const cell1_addr = try storage.allocateConsCell(&mem_storage);
    const cell2_addr = try storage.allocateConsCell(&mem_storage);
    
    const native_ptr1 = try virtual_memory.translateAddress(cell1_addr, vmem.fptovp, 4);
    const cell1: *cons.ConsCell = @as(*cons.ConsCell, @ptrCast(@alignCast(native_ptr1)));
    cell1.car_field = 100;
    cell1.cdr_code = cons.CDR_NIL;
    
    // Set cell1's CDR to point to cell2 (if on same page)
    const cell1_page = cell1_addr & 0xFFFF00;
    const cell2_page = cell2_addr & 0xFFFF00;
    
    if (cell1_page == cell2_page and cell2_addr > cell1_addr and cell2_addr <= cell1_addr + 14) {
        // Same page - can use same-page encoding
        try stack.pushStack(&vm, cell1_addr);
        try stack.pushStack(&vm, cell2_addr);
        
        try opcodes.handleRPLACD(&vm);
        
        // Check CDR was encoded
        try testing.expect(cell1.cdr_code != cons.CDR_NIL);
        try testing.expect(cell1.cdr_code >= cons.CDR_ONPAGE_MIN);
        
        // Verify CDR decoding
        const decoded_cdr = cons.getCDR(cell1, cell1_addr);
        try testing.expect(decoded_cdr == cell2_addr);
    }
}

test "RPLACA - with indirect CDR encoding" {
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
    
    // Set up indirect encoding
    cell.car_field = indirect_addr; // CAR points to indirect cell
    cell.cdr_code = cons.CDR_INDIRECT;
    indirect_cell.car_field = 50; // Actual CAR value
    
    // Replace CAR via RPLACA
    try stack.pushStack(&vm, cell_addr);
    try stack.pushStack(&vm, 75); // New CAR value
    
    try opcodes.handleRPLACA(&vm);
    
    // Check indirect cell's CAR was updated
    try testing.expect(indirect_cell.car_field == 75);
}

test "RPLACA - error on NIL" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var mem_storage = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer mem_storage.deinit();
    
    var vmem = try virtual_memory.VirtualMemory.init(allocator, 100);
    defer vmem.deinit();
    
    var vm = try stack.VM.initWithMemory(allocator, 1024, &mem_storage, &vmem);
    defer vm.deinit();
    
    // Try RPLACA on NIL
    try stack.pushStack(&vm, 0); // NIL
    try stack.pushStack(&vm, 100); // New CAR
    
    const result = opcodes.handleRPLACA(&vm);
    try testing.expectError(error.InvalidAddress, result);
}