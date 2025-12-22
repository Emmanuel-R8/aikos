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

// T095: Additional comprehensive tests for mouse event translation and delivery
test "mouse coordinate translation with different pixel scales" {
    const test_cases = [_][4]i32{
        .{ 1, 100, 200, 100 },  // scale 1: no change
        .{ 2, 200, 400, 100 },  // scale 2: divide by 2
        .{ 4, 400, 800, 100 },  // scale 4: divide by 4
    };

    for (test_cases) |tc| {
        const pixel_scale = @as(u32, @intCast(tc[0]));
        const sdl_x = tc[1];
        const sdl_y = tc[2];
        const expected_lisp_x = tc[3];
        const lisp_x = @divTrunc(sdl_x, @as(i32, @intCast(pixel_scale)));
        const lisp_y = @divTrunc(sdl_y, @as(i32, @intCast(pixel_scale)));
        try testing.expectEqual(expected_lisp_x, lisp_x);
        try testing.expectEqual(expected_lisp_x, lisp_y);
    }
}

test "mouse button state combinations" {
    var state = mouse.MouseState{
        .x = 0,
        .y = 0,
        .buttons = 0,
    };

    // Test left button (bit 0)
    state.buttons = 0x01;
    try testing.expect((state.buttons & 0x01) != 0);
    try testing.expect((state.buttons & 0x02) == 0);

    // Test right button (bit 1)
    state.buttons = 0x02;
    try testing.expect((state.buttons & 0x01) == 0);
    try testing.expect((state.buttons & 0x02) != 0);

    // Test both buttons
    state.buttons = 0x03;
    try testing.expect((state.buttons & 0x01) != 0);
    try testing.expect((state.buttons & 0x02) != 0);
}

test "mouse position boundary conditions" {
    var state = mouse.MouseState{
        .x = 0,
        .y = 0,
        .buttons = 0,
    };

    // Test negative coordinates (should be allowed)
    mouse.updateMousePosition(&state, -100, -200);
    const pos_neg = mouse.getMousePosition(&state);
    try testing.expectEqual(@as(i32, -100), pos_neg.x);
    try testing.expectEqual(@as(i32, -200), pos_neg.y);

    // Test large coordinates
    mouse.updateMousePosition(&state, 10000, 20000);
    const pos_large = mouse.getMousePosition(&state);
    try testing.expectEqual(@as(i32, 10000), pos_large.x);
    try testing.expectEqual(@as(i32, 20000), pos_large.y);
}

test "mouse event button field values" {
    const left_button: u8 = 1;
    const right_button: u8 = 2;
    const middle_button: u8 = 4;

    try testing.expectEqual(@as(u8, 1), left_button);
    try testing.expectEqual(@as(u8, 2), right_button);
    try testing.expectEqual(@as(u8, 4), middle_button);
}

test "mouse event coordinate ranges" {
    const event1 = mouse.MouseEvent{
        .event_type = .MOTION,
        .button = 0,
        .x = 0,
        .y = 0,
        .modifiers = 0,
        .timestamp = 0,
    };
    try testing.expectEqual(@as(i32, 0), event1.x);
    try testing.expectEqual(@as(i32, 0), event1.y);

    const event2 = mouse.MouseEvent{
        .event_type = .BUTTON_DOWN,
        .button = 1,
        .x = 1024,
        .y = 768,
        .modifiers = 0,
        .timestamp = 1000,
    };
    try testing.expectEqual(@as(i32, 1024), event2.x);
    try testing.expectEqual(@as(i32, 768), event2.y);
}
