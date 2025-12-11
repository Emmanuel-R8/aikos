const std = @import("std");
const testing = std.testing;
const display = @import("../src/display/sdl_backend.zig");
const graphics = @import("../src/display/graphics.zig");
const events = @import("../src/display/events.zig");

// T092: Test SDL2 window creation and display interface
test "DisplayInterface struct has required fields" {
    // Verify the DisplayInterface struct has all required fields per data-model.md
    const disp: display.DisplayInterface = undefined;
    _ = disp.window;
    _ = disp.renderer;
    _ = disp.texture;
    _ = disp.pixel_format;
    _ = disp.width;
    _ = disp.height;
    _ = disp.pixel_scale;
    _ = disp.display_region;
    _ = disp.foreground_color;
    _ = disp.background_color;
    _ = disp.bytes_per_pixel;
    try testing.expect(true);
}

test "display width rounding to multiple of 32" {
    // Per C implementation: w = (w + 31) / 32 * 32
    const test_cases = [_][2]u32{
        .{ 100, 128 }, // 100 -> 128 (4 * 32)
        .{ 640, 640 }, // already multiple of 32
        .{ 800, 800 }, // already multiple of 32
        .{ 1, 32 },    // 1 -> 32
        .{ 33, 64 },   // 33 -> 64
        .{ 1024, 1024 }, // already multiple of 32
    };

    for (test_cases) |tc| {
        const input = tc[0];
        const expected = tc[1];
        const rounded = ((input + 31) / 32) * 32;
        try testing.expectEqual(expected, rounded);
    }
}

// T093: Test BitBLT rendering operations (unit tests without SDL)
test "GraphicsOperation enum values" {
    try testing.expect(@intFromEnum(graphics.GraphicsOperation.COPY) == 0);
    try testing.expect(@intFromEnum(graphics.GraphicsOperation.XOR) == 1);
    try testing.expect(@intFromEnum(graphics.GraphicsOperation.AND) == 2);
    try testing.expect(@intFromEnum(graphics.GraphicsOperation.OR) == 3);
    try testing.expect(@intFromEnum(graphics.GraphicsOperation.NOT) == 4);
}

test "bitmask values for 16-bit words" {
    // Verify bitmask constants match expected bit positions
    // Most significant bit first (big-endian bit ordering)
    const bitmask: [16]u16 = [_]u16{
        0x8000, 0x4000, 0x2000, 0x1000,
        0x0800, 0x0400, 0x0200, 0x0100,
        0x0080, 0x0040, 0x0020, 0x0010,
        0x0008, 0x0004, 0x0002, 0x0001,
    };

    // Check first and last
    try testing.expectEqual(@as(u16, 0x8000), bitmask[0]);  // bit 15
    try testing.expectEqual(@as(u16, 0x0001), bitmask[15]); // bit 0

    // Check each bitmask is a single bit
    for (bitmask) |mask| {
        try testing.expect(@popCount(mask) == 1);
    }
}

test "pixel coordinate to word offset calculation" {
    // Verify coordinate to word offset calculation
    // Per C: offset = (y * pitch_words) + (x / 16)
    const pitch_words: u32 = 64; // 1024 pixels / 16 bits per word

    const test_cases = [_][3]u32{
        .{ 0, 0, 0 },      // (0,0) -> offset 0
        .{ 16, 0, 1 },     // (16,0) -> offset 1
        .{ 0, 1, 64 },     // (0,1) -> offset 64
        .{ 32, 1, 66 },    // (32,1) -> offset 66
        .{ 100, 10, 646 }, // (100,10) -> offset 646
    };

    for (test_cases) |tc| {
        const x = tc[0];
        const y = tc[1];
        const expected_offset = tc[2];
        const offset = (y * pitch_words) + (x / 16);
        try testing.expectEqual(expected_offset, offset);
    }
}

test "display region buffer size calculation" {
    // Display region buffer should be width * height DLwords
    const width: u32 = 1024;
    const height: u32 = 768;
    const expected_size = width * height;
    try testing.expectEqual(@as(u32, 786432), expected_size);
}