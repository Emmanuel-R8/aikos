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
    // Basic mapping for common keys
    // Map SDL keycodes to Lisp keycodes (ASCII for printable, special for others)

    var lisp_keycode: u16 = 0;

    // Handle printable ASCII characters
    if (os_keycode >= 'a' and os_keycode <= 'z') {
        lisp_keycode = @intCast(os_keycode);
        if (modifiers & Modifiers.SHIFT != 0) {
            lisp_keycode -= 32; // Convert to uppercase
        }
    } else if (os_keycode >= 'A' and os_keycode <= 'Z') {
        lisp_keycode = @intCast(os_keycode);
        if (modifiers & Modifiers.SHIFT == 0) {
            lisp_keycode += 32; // Convert to lowercase
        }
    } else if (os_keycode >= '0' and os_keycode <= '9') {
        lisp_keycode = @intCast(os_keycode);
    } else {
        // Special keys mapping (basic)
        switch (os_keycode) {
            13 => lisp_keycode = 13, // Enter
            32 => lisp_keycode = 32, // Space
            8 => lisp_keycode = 8,   // Backspace
            9 => lisp_keycode = 9,   // Tab
            27 => lisp_keycode = 27, // Escape
            // Add more special keys as needed
            else => lisp_keycode = 0, // Unknown
        }
    }

    // Apply control modifier
    if (modifiers & Modifiers.CONTROL != 0) {
        if (lisp_keycode >= 'a' and lisp_keycode <= 'z') {
            lisp_keycode -= 96; // Control-a = 1, etc.
        } else if (lisp_keycode >= 'A' and lisp_keycode <= 'Z') {
            lisp_keycode -= 64; // Control-A = 1, etc.
        }
    }

    // Meta modifier (basic - just set high bit for now)
    if (modifiers & Modifiers.META != 0) {
        lisp_keycode |= 0x80;
    }

    return lisp_keycode;
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
