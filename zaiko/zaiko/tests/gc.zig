const std = @import("std");
const testing = std.testing;
const gc_module = @import("../src/memory/gc.zig");

test "GC initialization" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();
    
    try testing.expect(gc.htmain.len == 1024);
    try testing.expect(gc.htcoll.len == 1024);
}

test "reference counting" {
    // TODO: Implement test cases for reference counting
    // Test addReference, deleteReference, markStackReference
    try testing.expect(true);
}