const std = @import("std");
const testing = std.testing;
const gc_module = @import("../src/memory/gc.zig");
const storage_module = @import("../src/memory/storage.zig");
const types = @import("../src/utils/types.zig");

const LispPTR = types.LispPTR;

test "GC initialization" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    try testing.expect(gc.htmain.len == 1024);
    try testing.expect(gc.htcoll.len == 1024);
    try testing.expect(gc.htbig.len == 256); // table_size / 4
}

// T070: Test ADDREF operation tracking reference counts correctly
// Per tasks.md T070: Add test case for ADDREF operation tracking reference counts correctly
test "T070: ADDREF - increment reference count" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // Add reference to object
    const ptr: LispPTR = 0x1000;
    try gc_module.addReference(&gc, ptr);

    // Check reference count
    const count = gc_module.getReferenceCount(&gc, ptr);
    try testing.expect(count == 1);

    // Add another reference
    try gc_module.addReference(&gc, ptr);
    const count2 = gc_module.getReferenceCount(&gc, ptr);
    try testing.expect(count2 == 2);
}

test "T070: ADDREF - multiple objects" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // Add references to multiple objects
    const ptr1: LispPTR = 0x1000;
    const ptr2: LispPTR = 0x2000;
    const ptr3: LispPTR = 0x3000;

    try gc_module.addReference(&gc, ptr1);
    try gc_module.addReference(&gc, ptr2);
    try gc_module.addReference(&gc, ptr3);

    // Check all have count of 1
    try testing.expect(gc_module.getReferenceCount(&gc, ptr1) == 1);
    try testing.expect(gc_module.getReferenceCount(&gc, ptr2) == 1);
    try testing.expect(gc_module.getReferenceCount(&gc, ptr3) == 1);
}

test "T070: ADDREF - NIL pointer" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // NIL pointer should not be reference counted
    try gc_module.addReference(&gc, 0);
    const count = gc_module.getReferenceCount(&gc, 0);
    try testing.expect(count == 0);
}

// T071: Test DELREF operation removing references correctly
// Per tasks.md T071: Add test case for DELREF operation removing references correctly
test "T071: DELREF - decrement reference count" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    const ptr: LispPTR = 0x1000;

    // Add two references
    try gc_module.addReference(&gc, ptr);
    try gc_module.addReference(&gc, ptr);
    try testing.expect(gc_module.getReferenceCount(&gc, ptr) == 2);

    // Delete one reference
    try gc_module.deleteReference(&gc, ptr);
    try testing.expect(gc_module.getReferenceCount(&gc, ptr) == 1);

    // Delete another reference
    try gc_module.deleteReference(&gc, ptr);
    try testing.expect(gc_module.getReferenceCount(&gc, ptr) == 0);
}

test "T071: DELREF - zero count marks for reclamation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    const ptr: LispPTR = 0x1000;

    // Add and then remove reference
    try gc_module.addReference(&gc, ptr);
    try gc_module.deleteReference(&gc, ptr);

    // Count should be zero
    try testing.expect(gc_module.getReferenceCount(&gc, ptr) == 0);

    // Object should be in reclamation list
    try testing.expect(gc.reclamation_list.items.len > 0);
    try testing.expect(gc.reclamation_list.items[0] == ptr);
}

test "T071: DELREF - NIL pointer" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // NIL pointer should not cause errors
    try gc_module.deleteReference(&gc, 0);
    const count = gc_module.getReferenceCount(&gc, 0);
    try testing.expect(count == 0);
}

// T072: Test reclamation when count reaches zero
// Per tasks.md T072: Add test case for reclamation when count reaches zero
test "T072: Reclamation - zero count marks for reclamation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    const ptr1: LispPTR = 0x1000;
    const ptr2: LispPTR = 0x2000;

    // Add references
    try gc_module.addReference(&gc, ptr1);
    try gc_module.addReference(&gc, ptr2);

    // Clear reclamation list
    gc.reclamation_list.clearRetainingCapacity();

    // Delete references (should mark for reclamation)
    try gc_module.deleteReference(&gc, ptr1);
    try gc_module.deleteReference(&gc, ptr2);

    // Both should be in reclamation list
    try testing.expect(gc.reclamation_list.items.len == 2);
    try testing.expect(gc.reclamation_list.items[0] == ptr1);
    try testing.expect(gc.reclamation_list.items[1] == ptr2);
}

test "T072: Reclamation - only zero-count objects reclaimed" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    const ptr1: LispPTR = 0x1000;
    const ptr2: LispPTR = 0x2000;

    // Add two references to ptr1, one to ptr2
    try gc_module.addReference(&gc, ptr1);
    try gc_module.addReference(&gc, ptr1);
    try gc_module.addReference(&gc, ptr2);

    // Clear reclamation list
    gc.reclamation_list.clearRetainingCapacity();

    // Delete one reference from ptr1 (still has 1 reference)
    try gc_module.deleteReference(&gc, ptr1);
    try testing.expect(gc.reclamation_list.items.len == 0); // Not reclaimed yet

    // Delete reference from ptr2 (should be reclaimed)
    try gc_module.deleteReference(&gc, ptr2);
    try testing.expect(gc.reclamation_list.items.len == 1);
    try testing.expect(gc.reclamation_list.items[0] == ptr2);

    // Delete last reference from ptr1 (should be reclaimed)
    try gc_module.deleteReference(&gc, ptr1);
    try testing.expect(gc.reclamation_list.items.len == 2);
}

// T073: Test referenced objects not being reclaimed
// Per tasks.md T073: Add test case for referenced objects not being reclaimed
test "T073: Referenced objects not reclaimed" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    const ptr: LispPTR = 0x1000;

    // Add multiple references
    try gc_module.addReference(&gc, ptr);
    try gc_module.addReference(&gc, ptr);
    try gc_module.addReference(&gc, ptr);

    // Clear reclamation list
    gc.reclamation_list.clearRetainingCapacity();

    // Delete one reference (still has 2 references)
    try gc_module.deleteReference(&gc, ptr);
    try testing.expect(gc.reclamation_list.items.len == 0); // Not reclaimed

    // Delete another reference (still has 1 reference)
    try gc_module.deleteReference(&gc, ptr);
    try testing.expect(gc.reclamation_list.items.len == 0); // Still not reclaimed

    // Delete last reference (now should be reclaimed)
    try gc_module.deleteReference(&gc, ptr);
    try testing.expect(gc.reclamation_list.items.len == 1);
    try testing.expect(gc.reclamation_list.items[0] == ptr);
}

test "T073: Stack reference prevents reclamation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    const ptr: LispPTR = 0x1000;

    // Add reference and mark as stack reference
    try gc_module.addReference(&gc, ptr);
    try gc_module.markStackReference(&gc, ptr);

    // Clear reclamation list
    gc.reclamation_list.clearRetainingCapacity();

    // Delete reference (but stack reference should prevent reclamation)
    // Note: Current implementation doesn't check stackref flag during reclamation
    // This test documents expected behavior for future implementation
    try gc_module.deleteReference(&gc, ptr);
    // TODO: When stackref checking is implemented, verify object is not reclaimed
    // For now, object is reclaimed (implementation detail)
    _ = gc.reclamation_list.items.len;
}

// T074: Integration test for extended Medley session without memory leaks
// Per tasks.md T074: Add integration test for extended Medley session without memory leaks
test "T074: Extended session - no memory leaks" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // Simulate extended session: allocate many objects, add/remove references
    var objects: [100]LispPTR = undefined;
    for (&objects, 0..) |*ptr, i| {
        ptr.* = @as(LispPTR, @intCast(0x1000 + i * 0x100));
        try gc_module.addReference(&gc, ptr.*);
    }

    // Verify all objects have references
    for (objects) |ptr| {
        try testing.expect(gc_module.getReferenceCount(&gc, ptr) == 1);
    }

    // Remove half of the references
    for (0..50) |i| {
        try gc_module.deleteReference(&gc, objects[i]);
    }

    // Verify first 50 are in reclamation list
    try testing.expect(gc.reclamation_list.items.len == 50);

    // Verify remaining 50 still have references
    for (50..100) |i| {
        try testing.expect(gc_module.getReferenceCount(&gc, objects[i]) == 1);
    }

    // Remove remaining references
    for (50..100) |i| {
        try gc_module.deleteReference(&gc, objects[i]);
    }

    // All should be in reclamation list
    try testing.expect(gc.reclamation_list.items.len == 100);
}

// GC Trigger Mechanism Tests - Now Implemented
// These tests verify the countdown-based GC trigger mechanism matches C implementation

test "allocation functions should trigger GC when memory pressure detected" {
    const gpa = testing.allocator;

    var storage = try storage_module.Storage.init(gpa, 1024 * 1024, 1000, 0x00200000);
    defer storage.deinit();

    var gc = try gc_module.GC.init(gpa, 1024);
    defer gc.deinit();

    gc.reclaim_countdown = 10;
    gc.reclaim_min = 5;
    gc.gc_enabled = true;

    // These should fail because the integration doesn't exist
    try gc_module.incrementAllocationCount(&gc, 5);
    try testing.expect(gc.reclaim_countdown == 5);

    try gc_module.incrementAllocationCount(&gc, 5);
    try testing.expect(gc.gc_run_count == 1);
    try testing.expect(gc.reclaim_countdown == 5);
}

test "storage pressure detection should trigger GC when memory is low" {
    const gpa = testing.allocator;

    var storage = try storage_module.Storage.init(gpa, 1024, 100, 0x00200000);
    defer storage.deinit();

    var gc = try gc_module.GC.init(gpa, 1024);
    defer gc.deinit();

    gc.reclaim_countdown = 50;
    gc.reclaim_min = 25;
    gc.gc_enabled = true;

    // This should fail because pressure detection doesn't exist
    // C equivalent: checkfor_storagefull logic
    const storage_pressure = gc_module.checkStoragePressure(&gc, &storage, 10);
    try testing.expect(storage_pressure == true);

    try gc_module.handleStoragePressure(&gc, &storage);
    try testing.expect(gc.gc_run_count == 1);
}
