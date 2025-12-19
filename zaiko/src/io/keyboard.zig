const std = @import("std");
const errors = @import("../utils/errors.zig");

/// Keyboard event (translated to Lisp format)
/// Per data-model.md
pub const KeyboardEvent = struct {
    event_type: EventType, // KEY_PRESS or KEY_RELEASE
    keycode: u16, // Lisp keycode
    modifiers: u16, // Modifier flags
    timestamp: u32, // Event timestamp
};

/// Event type
pub const EventType = enum {
    KEY_PRESS,
    KEY_RELEASE,
};

/// Modifier flags
pub const Modifiers = struct {
    pub const SHIFT: u16 = 1 << 0;
    pub const CONTROL: u16 = 1 << 1;
    pub const META: u16 = 1 << 2;
    pub const ALT: u16 = 1 << 3;
};

/// Key event queue
pub const KeyEventQueue = struct {
    events: []KeyboardEvent,
    head: usize,
    tail: usize,
    size: usize,
    buffering: bool,

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !KeyEventQueue {
        const events_buffer = try allocator.alloc(KeyboardEvent, capacity);
        return KeyEventQueue{
            .events = events_buffer,
            .head = 0,
            .tail = 0,
            .size = 0,
            .buffering = false,
        };
    }

    pub fn deinit(self: *KeyEventQueue, allocator: std.mem.Allocator) void {
        allocator.free(self.events);
    }
};

/// Translate OS keycode to Lisp keycode
/// Per contracts/io-interface.zig
pub fn translateKeycode(os_keycode: u32, modifiers: u16) u16 {
    _ = os_keycode;
    _ = modifiers;
    // TODO: Implement keycode translation
    // 1. Look up base keycode in map
    // 2. Apply modifier transformations
    return 0;
}

/// Enqueue key event
/// Per contracts/io-interface.zig
pub fn enqueueKeyEvent(queue: *KeyEventQueue, event: KeyboardEvent) errors.IOError!void {
    if (queue.size >= queue.events.len) {
        return error.KeycodeTranslationFailed; // Queue full
    }

    queue.events[queue.tail] = event;
    queue.tail = (queue.tail + 1) % queue.events.len;
    queue.size += 1;
}

/// Dequeue key event
/// Per contracts/io-interface.zig
pub fn dequeueKeyEvent(queue: *KeyEventQueue) ?KeyboardEvent {
    if (queue.size == 0) {
        return null;
    }

    const event = queue.events[queue.head];
    queue.head = (queue.head + 1) % queue.events.len;
    queue.size -= 1;

    return event;
}
