const std = @import("std");
const testing = std.testing;
const mouse = @import("../src/io/mouse.zig");

test "mouse position" {
    var state = mouse.MouseState{
        .x = 0,
        .y = 0,
        .buttons = 0,
    };
    
    mouse.updateMousePosition(&state, 100, 200);
    const pos = mouse.getMousePosition(&state);
    try testing.expect(pos.x == 100);
    try testing.expect(pos.y == 200);
}

test "mouse event translation" {
    // TODO: Implement test cases for mouse event translation
    // This requires SDL event structure
    try testing.expect(true);
}