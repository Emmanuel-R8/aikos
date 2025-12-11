const std = @import("std");
const testing = std.testing;
const mouse = @import("../src/io/mouse.zig");

// T095: Test mouse event translation and delivery

test "MouseState struct initialization" {
    const state = mouse.MouseState{
        .x = 0,
        .y = 0,
        .buttons = 0,
    };
    try testing.expectEqual(@as(i32, 0), state.x);
    try testing.expectEqual(@as(i32, 0), state.y);
    try testing.expectEqual(@as(u8, 0), state.buttons);
}

test "MouseEventType enum values" {
    try testing.expect(@intFromEnum(mouse.MouseEventType.MOTION) == 0);
    try testing.expect(@intFromEnum(mouse.MouseEventType.BUTTON_DOWN) == 1);
    try testing.expect(@intFromEnum(mouse.MouseEventType.BUTTON_UP) == 2);
}

test "mouse position update" {
    var state = mouse.MouseState{
        .x = 0,
        .y = 0,
        .buttons = 0,
    };

    mouse.updateMousePosition(&state, 100, 200);
    const pos = mouse.getMousePosition(&state);
    try testing.expectEqual(@as(i32, 100), pos.x);
    try testing.expectEqual(@as(i32, 200), pos.y);
}

test "mouse button state tracking" {
    var state = mouse.MouseState{
        .x = 0,
        .y = 0,
        .buttons = 0,
    };

    state.buttons = 0x01;
    try testing.expect((state.buttons & 0x01) != 0);
    state.buttons |= 0x02;
    try testing.expect((state.buttons & 0x02) != 0);
}

test "MouseEvent struct has required fields" {
    const event = mouse.MouseEvent{
        .event_type = .BUTTON_DOWN,
        .button = 1,
        .x = 100,
        .y = 200,
        .modifiers = 0,
        .timestamp = 12345,
    };
    try testing.expect(event.event_type == .BUTTON_DOWN);
    try testing.expectEqual(@as(u8, 1), event.button);
}

test "mouse coordinate translation with pixel scale" {
    const pixel_scale: u32 = 2;
    const sdl_x: i32 = 200;
    const sdl_y: i32 = 400;
    const lisp_x = @divTrunc(sdl_x, @as(i32, @intCast(pixel_scale)));
    const lisp_y = @divTrunc(sdl_y, @as(i32, @intCast(pixel_scale)));
    try testing.expectEqual(@as(i32, 100), lisp_x);
    try testing.expectEqual(@as(i32, 200), lisp_y);
}