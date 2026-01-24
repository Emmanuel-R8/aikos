const std = @import("std");
const testing = std.testing;
const gc_module = @import("../src/memory/gc.zig");
const storage_module = @import("../src/memory/storage.zig");
const types_module = @import("../src/utils/types.zig");

const GC = gc_module.GC;
const Storage = storage_module.Storage;
const LispPTR = types_module.LispPTR;

test "GC countdown mechanism triggers garbage collection when counter reaches zero" {
    // Setup: Create GC with countdown mechanism
    const gpa = testing.allocator;
    var gc_instance = try GC.init(gpa, 1024);
    defer gc_instance.deinit();

    // Initialize GC countdown state (C equivalent: Reclaim_cnt)
    gc_instance.reclaim_countdown = 100;
    gc_instance.reclaim_min = 50;
    gc_instance.gc_enabled = true;

    // Test: Increment allocation count should decrement countdown
    try gc_instance.incrementAllocationCount(30);
    try testing.expect(gc_instance.reclaim_countdown == 70);

    // Test: When countdown reaches zero, GC should be triggered
    try gc_instance.incrementAllocationCount(70);
    // After this, reclaim_countdown should be 0 and GC should have run
    try testing.expect(gc_instance.reclaim_countdown == 0);
    try testing.expect(gc_instance.gc_run_count == 1);

    // Test: After GC trigger, countdown should be reset to reclaim_min
    try testing.expect(gc_instance.reclaim_countdown == 50);
}

test "allocation functions should trigger GC when memory pressure detected" {
    // Setup: Create storage and GC
    const gpa = testing.allocator;
    var storage_instance = try Storage.init(gpa, 1024 * 1024, 1000, 0x00200000);
    defer storage_instance.deinit();

    var gc_instance = try GC.init(gpa, 1024);
    defer gc_instance.deinit();

    // Link GC to storage (integration setup)
    gc_instance.reclaim_countdown = 10;
    gc_instance.reclaim_min = 5;
    gc_instance.gc_enabled = true;

    // Test: Multiple allocations should decrement countdown
    // Simulate allocation triggering incrementAllocationCount
    try gc_instance.incrementAllocationCount(5);
    try testing.expect(gc_instance.reclaim_countdown == 5);

    // Next allocation that crosses threshold should trigger GC
    try gc_instance.incrementAllocationCount(5);
    try testing.expect(gc_instance.gc_run_count == 1);
    try testing.expect(gc_instance.reclaim_countdown == 5); // Reset to reclaim_min
}

test "GC trigger should be disabled when GC is disabled" {
    const gpa = testing.allocator;
    var gc_instance = try GC.init(gpa, 1024);
    defer gc_instance.deinit();

    // Setup: GC disabled
    gc_instance.reclaim_countdown = 10;
    gc_instance.reclaim_min = 5;
    gc_instance.gc_enabled = false;

    // Test: Even when countdown reaches zero, GC should not run
    try gc_instance.incrementAllocationCount(10);
    try testing.expect(gc_instance.reclaim_countdown == 0);
    try testing.expect(gc_instance.gc_run_count == 0); // GC should not run
}

test "storage pressure detection should trigger GC when memory is low" {
    const gpa = testing.allocator;
    var storage_instance = try Storage.init(gpa, 1024, 100, 0x00200000);
    defer storage_instance.deinit();

    var gc_instance = try GC.init(gpa, 1024);
    defer gc_instance.deinit();

    // Setup: Simulate storage pressure
    gc_instance.reclaim_countdown = 50;
    gc_instance.reclaim_min = 25;
    gc_instance.gc_enabled = true;

    // Simulate storage reaching pressure threshold
    // (Equivalent to C checkfor_storagefull logic)
    const storage_pressure = gc_instance.checkStoragePressure(&storage_instance, 10);
    try testing.expect(storage_pressure == true);

    // When storage pressure is detected, it should trigger GC
    try gc_instance.handleStoragePressure(&storage_instance);
    try testing.expect(gc_instance.gc_run_count == 1);
}
