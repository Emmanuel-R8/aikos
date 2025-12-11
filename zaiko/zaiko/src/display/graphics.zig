// Graphics operations (BitBLT)
// Per maiko/src/sdl.c:sdl_bitblt_to_texture

const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const sdl_backend = @import("sdl_backend.zig");
const sdl2 = @import("sdl2.zig");

const DLword = types.DLword;
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

/// Graphics operation types
pub const GraphicsOperation = enum {
    COPY,
    XOR,
    AND,
    OR,
    NOT,
};

/// Bitmask for extracting bits from DLword
const bitmask: [16]u16 = [_]u16{
    0x8000, 0x4000, 0x2000, 0x1000,
    0x0800, 0x0400, 0x0200, 0x0100,
    0x0080, 0x0040, 0x0020, 0x0010,
    0x0008, 0x0004, 0x0002, 0x0001,
};

/// Copy display region buffer to SDL texture (T079)
/// Per maiko/src/sdl.c:sdl_bitblt_to_texture
pub fn copyDisplayToTexture(
    display: *sdl_backend.DisplayInterface,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
) errors.DisplayError!void {
    if (display.texture == null or display.renderer == null) {
        return error.InvalidRegion;
    }

    const src = display.display_region;
    const bits_per_word = 8 * @sizeOf(DLword);
    const source_pitch_words = display.width / bits_per_word;
    const xstart = x / bits_per_word;
    const xlimit = (x + width + bits_per_word - 1) / bits_per_word;
    const ystart = y * source_pitch_words;
    const ylimit = (y + height) * source_pitch_words;

    // Create destination rectangle (stretch to word boundaries)
    var dstrect: sdl2.SDL_Rect = undefined;
    dstrect.x = @as(c_int, @intCast(xstart * bits_per_word));
    dstrect.w = @as(c_int, @intCast((xlimit * bits_per_word) - dstrect.x));
    dstrect.y = @as(c_int, @intCast(y));
    dstrect.h = @as(c_int, @intCast(height));

    // Lock texture
    var dst: ?*anyopaque = null;
    var dst_pitch_bytes: c_int = undefined;
    if (sdl2.SDL_LockTexture(display.texture, &dstrect, &dst, &dst_pitch_bytes) != 0) {
        return error.InvalidRegion;
    }
    defer sdl2.SDL_UnlockTexture(display.texture);

    if (dst == null) {
        return error.InvalidRegion;
    }

    const dst_pitch_pixels = @as(usize, @intCast(dst_pitch_bytes)) / display.bytes_per_pixel;
    const dst_pixels: [*]u32 = @as([*]u32, @ptrCast(dst.?));

    var dy: usize = 0;
    // For each line in the source image
    var sy = ystart;
    while (sy < ylimit) : ({
        sy += source_pitch_words;
        dy += dst_pitch_pixels;
    }) {
        // For each word in the line
        var dx: usize = 0;
        var sx = xstart;
        while (sx < xlimit) : ({
            sx += 1;
            dx += bits_per_word;
        }) {
            const srcw = src[sy + sx];
            // For each bit in the word
            var b: usize = 0;
            while (b < bits_per_word) : (b += 1) {
                const pixel_index = dy + dx + b;
                if (pixel_index < dst_pitch_pixels * height) {
                    dst_pixels[pixel_index] = if ((srcw & bitmask[b]) != 0)
                        display.foreground_color
                    else
                        display.background_color;
                }
            }
        }
    }
}

/// Render texture to screen (T080)
/// Per maiko/src/sdl.c:SDL_RenderCopy
pub fn renderTextureToScreen(display: *sdl_backend.DisplayInterface) errors.DisplayError!void {
    if (display.texture == null or display.renderer == null) {
        return error.InvalidRegion;
    }

    // Render texture to screen (full screen)
    if (sdl2.SDL_RenderCopy(display.renderer, display.texture, null, null) != 0) {
        return error.InvalidRegion;
    }
    sdl2.SDL_RenderPresent(display.renderer);
}

/// BitBLT COPY mode operation (T081)
/// Per maiko/src/sdl.c and display.h
pub fn bitbltCopy(
    display: *sdl_backend.DisplayInterface,
    source_x: u32,
    source_y: u32,
    width: u32,
    height: u32,
    dest_x: u32,
    dest_y: u32,
) errors.DisplayError!void {
    _ = source_x;
    _ = source_y;
    _ = dest_x;
    _ = dest_y;

    // For now, COPY mode just copies the entire region
    // TODO: Implement proper source/dest coordinate handling
    try copyDisplayToTexture(display, source_x, source_y, width, height);
    try renderTextureToScreen(display);
}

/// BitBLT XOR mode operation (T082)
/// Per maiko/src/sdl.c and display.h
pub fn bitbltXOR(
    display: *sdl_backend.DisplayInterface,
    source_x: u32,
    source_y: u32,
    width: u32,
    height: u32,
    dest_x: u32,
    dest_y: u32,
) errors.DisplayError!void {
    // XOR mode: destination = source XOR destination
    // For now, implement as COPY (will enhance later)
    _ = source_x;
    _ = source_y;
    _ = dest_x;
    _ = dest_y;

    try copyDisplayToTexture(display, source_x, source_y, width, height);
    try renderTextureToScreen(display);
}

/// Connect BitBLT operations to SDL2 rendering pipeline (T083)
/// Per maiko/src/sdl.c:sdl_bitblt_to_screen
pub fn bitbltToScreen(
    display: *sdl_backend.DisplayInterface,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
) errors.DisplayError!void {
    // Copy display region to texture
    try copyDisplayToTexture(display, x, y, width, height);
    // Render texture to screen
    try renderTextureToScreen(display);
}

/// Render region (BitBLT) - unified interface
/// Per contracts/display-interface.zig
pub fn renderRegion(
    display: *sdl_backend.DisplayInterface,
    source_x: u32,
    source_y: u32,
    width: u32,
    height: u32,
    dest_x: u32,
    dest_y: u32,
    operation: GraphicsOperation,
) errors.DisplayError!void {
    switch (operation) {
        .COPY => try bitbltCopy(display, source_x, source_y, width, height, dest_x, dest_y),
        .XOR => try bitbltXOR(display, source_x, source_y, width, height, dest_x, dest_y),
        .AND, .OR, .NOT => {
            // TODO: Implement other operations
            return error.InvalidRegion;
        },
    }
}

/// Flush display region
/// Per contracts/display-interface.zig
pub fn flushDisplayRegion(
    display: *sdl_backend.DisplayInterface,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
) errors.DisplayError!void {
    try bitbltToScreen(display, x, y, width, height);
}