const std = @import("std");
const errors = @import("../utils/errors.zig");
const sdl_backend = @import("sdl_backend.zig");
const keyboard = @import("../io/keyboard.zig");
const mouse = @import("../io/mouse.zig");

/// Event type
pub const EventType = enum {
    KEY_PRESS,
    KEY_RELEASE,
    BUTTON_PRESS,
    BUTTON_RELEASE,
    MOTION,
    QUIT,
};

/// Poll events
/// Per contracts/display-interface.zig
pub fn pollEvents(display: *sdl_backend.DisplayInterface, allocator: std.mem.Allocator) errors.DisplayError![]Event {
    _ = display;
    _ = allocator;
    // TODO: Poll SDL events
    // TODO: Translate to Lisp events
    return &[_]Event{};
}

/// Process mouse event
/// Per contracts/display-interface.zig
pub fn processMouseEvent(display: *sdl_backend.DisplayInterface, sdl_event: *const anyopaque) mouse.MouseEvent {
    _ = display;
    _ = sdl_event;
    // TODO: Translate SDL mouse event to Lisp mouse event
    return mouse.MouseEvent{
        .event_type = .MOTION,
        .button = 0,
        .x = 0,
        .y = 0,
        .modifiers = 0,
        .timestamp = 0,
    };
}

/// Event structure
pub const Event = union(EventType) {
    KEY_PRESS: keyboard.KeyboardEvent,
    KEY_RELEASE: keyboard.KeyboardEvent,
    BUTTON_PRESS: mouse.MouseEvent,
    BUTTON_RELEASE: mouse.MouseEvent,
    MOTION: mouse.MouseEvent,
    QUIT: void,
};