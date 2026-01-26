const std = @import("std");
const errors = @import("../utils/errors.zig");

/// Mouse event (translated to Lisp format)
/// Per data-model.md
pub const MouseEvent = struct {
    event_type: EventType, // BUTTON_PRESS, BUTTON_RELEASE, MOTION
    button: u8, // Button number (1-3) or 0 for motion
    x: i32, // X coordinate
    y: i32, // Y coordinate
    modifiers: u16, // Modifier flags
    timestamp: u32, // Event timestamp
};

/// Event type
pub const EventType = enum {
    BUTTON_PRESS,
    BUTTON_RELEASE,
    MOTION,
};

/// Mouse state
pub const MouseState = struct {
    x: i32,
    y: i32,
    buttons: u8, // Bitmask of pressed buttons
};

/// Translate mouse event
/// Per contracts/io-interface.zig
pub fn translateMouseEvent(os_event: *const anyopaque) MouseEvent {
    _ = os_event;
    // TODO: Translate OS mouse event to Lisp mouse event format
    return MouseEvent{
        .event_type = .MOTION,
        .button = 0,
        .x = 0,
        .y = 0,
        .modifiers = 0,
        .timestamp = 0,
    };
}

/// Update mouse position
/// Per contracts/io-interface.zig
pub fn updateMousePosition(state: *MouseState, x: i32, y: i32) void {
    state.x = x;
    state.y = y;
}

/// Get mouse position
/// Per contracts/io-interface.zig
pub fn getMousePosition(state: *const MouseState) struct { x: i32, y: i32 } {
    return .{ .x = state.x, .y = state.y };
}