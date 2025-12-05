const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const DLword = types.DLword;

/// Display interface (abstracts SDL backend)
/// Per data-model.md
pub const DisplayInterface = struct {
    // SDL2 types would be here if linked
    // window: *sdl.SDL_Window,
    // renderer: *sdl.SDL_Renderer,
    // texture: *sdl.SDL_Texture,
    width: u32,
    height: u32,
    display_region: []DLword, // Display region buffer

    pub fn init(allocator: std.mem.Allocator, width: u32, height: u32) errors.DisplayError!DisplayInterface {
        // TODO: Initialize SDL2
        // TODO: Create window and renderer
        // TODO: Allocate display region buffer

        const buffer_size = width * height;
        const display_buffer = try allocator.alloc(DLword, buffer_size);
        @memset(display_buffer, 0);

        return DisplayInterface{
            .width = width,
            .height = height,
            .display_region = display_buffer,
        };
    }

    pub fn deinit(self: *DisplayInterface, allocator: std.mem.Allocator) void {
        allocator.free(self.display_region);
        // TODO: Destroy SDL2 window and renderer
        _ = self;
    }
};

/// Initialize SDL display
/// Per contracts/display-interface.zig
pub fn initDisplay(width: u32, height: u32, depth: u32, allocator: std.mem.Allocator) errors.DisplayError!*DisplayInterface {
    _ = depth; // TODO: Use depth for pixel format
    return try DisplayInterface.init(allocator, width, height);
}

/// Destroy display
/// Per contracts/display-interface.zig
pub fn destroyDisplay(display: *DisplayInterface, allocator: std.mem.Allocator) void {
    display.deinit(allocator);
}

/// Get display region buffer
/// Per contracts/display-interface.zig
pub fn getDisplayRegion(display: *DisplayInterface) []DLword {
    return display.display_region;
}