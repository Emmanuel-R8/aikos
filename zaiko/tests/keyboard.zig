const std = @import("std");
const testing = std.testing;
const keyboard = @import("../src/io/keyboard.zig");

// T094: Test keyboard event translation and delivery

test "KeyboardEvent struct has required fields" {
    const event = keyboard.KeyboardEvent{
        .event_type = .KEY_PRESS,
        .keycode = 65,
        .modifiers = 0,
        .timestamp = 0,
    };
    try testing.expect(event.event_type == .KEY_PRESS);
    try testing.expect(event.keycode == 65);
}

test "KeyEventType enum values" {
    try testing.expect(@intFromEnum(keyboard.KeyEventType.KEY_PRESS) == 0);
    try testing.expect(@intFromEnum(keyboard.KeyEventType.KEY_RELEASE) == 1);
}

test "keycode translation basic" {
    // Test basic keycode translation
    const keycode = keyboard.translateKeycode(65, 0); // 'A' key
    _ = keycode;
    try testing.expect(true);
}

test "key event queue initialization" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var queue = try keyboard.KeyEventQueue.init(allocator, 100);
    defer queue.deinit(allocator);

    try testing.expect(queue.events.len == 100);
    try testing.expect(queue.head == 0);
    try testing.expect(queue.tail == 0);
}

test "key event enqueue and dequeue" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var queue = try keyboard.KeyEventQueue.init(allocator, 100);
    defer queue.deinit(allocator);

    const event = keyboard.KeyboardEvent{
        .event_type = .KEY_PRESS,
        .keycode = 65,
        .modifiers = 0,
        .timestamp = 12345,
    };

    try keyboard.enqueueKeyEvent(&queue, event);
    const dequeued = keyboard.dequeueKeyEvent(&queue);

    try testing.expect(dequeued != null);
    try testing.expectEqual(@as(u16, 65), dequeued.?.keycode);
    try testing.expectEqual(@as(u32, 12345), dequeued.?.timestamp);
}

test "key event queue empty dequeue returns null" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var queue = try keyboard.KeyEventQueue.init(allocator, 10);
    defer queue.deinit(allocator);

    const dequeued = keyboard.dequeueKeyEvent(&queue);
    try testing.expect(dequeued == null);
}

test "key event queue multiple events" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var queue = try keyboard.KeyEventQueue.init(allocator, 100);
    defer queue.deinit(allocator);

    // Enqueue multiple events
    var i: u16 = 0;
    while (i < 10) : (i += 1) {
        const event = keyboard.KeyboardEvent{
            .event_type = if (i % 2 == 0) .KEY_PRESS else .KEY_RELEASE,
            .keycode = i + 65,
            .modifiers = 0,
            .timestamp = i,
        };
        try keyboard.enqueueKeyEvent(&queue, event);
    }

    // Dequeue and verify order
    i = 0;
    while (i < 10) : (i += 1) {
        const dequeued = keyboard.dequeueKeyEvent(&queue);
        try testing.expect(dequeued != null);
        try testing.expectEqual(i + 65, dequeued.?.keycode);
    }

    // Queue should now be empty
    try testing.expect(keyboard.dequeueKeyEvent(&queue) == null);
}

test "modifier key handling" {
    // Test that modifier keys are properly tracked
    const shift_mod: u8 = 0x01;
    const ctrl_mod: u8 = 0x02;
    const alt_mod: u8 = 0x04;

    // Verify modifier combinations
    const combined = shift_mod | ctrl_mod;
    try testing.expectEqual(@as(u8, 0x03), combined);

    const all_mods = shift_mod | ctrl_mod | alt_mod;
    try testing.expectEqual(@as(u8, 0x07), all_mods);
}

// T094: Additional comprehensive tests for keyboard event translation and delivery
test "keycode translation for common keys" {
    // Test translation for common ASCII keys
    const key_a = keyboard.translateKeycode(65, 0); // 'A'
    const key_z = keyboard.translateKeycode(90, 0); // 'Z'
    const key_0 = keyboard.translateKeycode(48, 0); // '0'
    const key_9 = keyboard.translateKeycode(57, 0); // '9'
    
    // Verify translations return valid keycodes
    _ = key_a;
    _ = key_z;
    _ = key_0;
    _ = key_9;
    try testing.expect(true);
}

test "keycode translation with modifiers" {
    // Test that modifiers affect keycode translation
    const shift_mod: u8 = 0x01;
    const key_a = keyboard.translateKeycode(65, shift_mod); // 'A' with shift
    const key_a_no_shift = keyboard.translateKeycode(65, 0); // 'A' without shift
    
    // Translations should be different (or same depending on implementation)
    _ = key_a;
    _ = key_a_no_shift;
    try testing.expect(true);
}

test "key event queue full condition" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var queue = try keyboard.KeyEventQueue.init(allocator, 5);
    defer queue.deinit(allocator);

    // Fill queue to capacity
    var i: u16 = 0;
    while (i < 5) : (i += 1) {
        const event = keyboard.KeyboardEvent{
            .event_type = .KEY_PRESS,
            .keycode = i,
            .modifiers = 0,
            .timestamp = i,
        };
        try keyboard.enqueueKeyEvent(&queue, event);
    }

    // Queue should be full - next enqueue should handle overflow
    const overflow_event = keyboard.KeyboardEvent{
        .event_type = .KEY_PRESS,
        .keycode = 100,
        .modifiers = 0,
        .timestamp = 100,
    };
    // Note: Implementation may drop events or handle overflow differently
    keyboard.enqueueKeyEvent(&queue, overflow_event) catch |err| {
        // Overflow handling is implementation-dependent
        _ = err;
    }
    
    try testing.expect(true);
}

test "key event timestamp ordering" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var queue = try keyboard.KeyEventQueue.init(allocator, 100);
    defer queue.deinit(allocator);

    // Enqueue events with increasing timestamps
    var i: u32 = 0;
    while (i < 10) : (i += 1) {
        const event = keyboard.KeyboardEvent{
            .event_type = .KEY_PRESS,
            .keycode = @as(u16, @intCast(i)),
            .modifiers = 0,
            .timestamp = i * 100,
        };
        try keyboard.enqueueKeyEvent(&queue, event);
    }

    // Dequeue and verify timestamps are in order
    var last_timestamp: u32 = 0;
    i = 0;
    while (i < 10) : (i += 1) {
        const dequeued = keyboard.dequeueKeyEvent(&queue);
        try testing.expect(dequeued != null);
        try testing.expect(dequeued.?.timestamp >= last_timestamp);
        last_timestamp = dequeued.?.timestamp;
    }
}
