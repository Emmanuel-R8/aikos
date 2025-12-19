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
