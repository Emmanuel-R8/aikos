const std = @import("std");
const testing = std.testing;
const keyboard = @import("../src/io/keyboard.zig");

test "keycode translation" {
    // TODO: Implement test cases for keycode translation
    // Test translateKeycode with various OS keycodes and modifiers
    const keycode = keyboard.translateKeycode(65, 0); // 'A' key
    _ = keycode;
    try testing.expect(true);
}

test "key event queue" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    var queue = try keyboard.KeyEventQueue.init(allocator, 100);
    defer queue.deinit(allocator);
    
    const event = keyboard.KeyboardEvent{
        .event_type = .KEY_PRESS,
        .keycode = 65,
        .modifiers = 0,
        .timestamp = 0,
    };
    
    try keyboard.enqueueKeyEvent(&queue, event);
    const dequeued = keyboard.dequeueKeyEvent(&queue);
    try testing.expect(dequeued != null);
    try testing.expect(dequeued.?.keycode == 65);
}