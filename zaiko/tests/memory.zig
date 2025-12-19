const std = @import("std");
const testing = std.testing;
const storage = @import("../src/memory/storage.zig");
const cons = @import("../src/data/cons.zig");

test "cons cell allocation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var st = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer st.deinit();
    
    const cell_addr = try storage.allocateConsCell(&st);
    try testing.expect(cell_addr != 0);
}

test "array allocation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var st = try storage.Storage.init(allocator, 1024 * 1024, 100);
    defer st.deinit();
    
    const array_addr = try storage.allocateArray(&st, 10, .TYP_ARRAY);
    try testing.expect(array_addr != 0);
}
