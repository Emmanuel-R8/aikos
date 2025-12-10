const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const sdl2 = @import("sdl2.zig");

const DLword = types.DLword;

/// Display interface (abstracts SDL backend)
/// Per data-model.md
pub const DisplayInterface = struct {
    // SDL2 objects
    window: ?*sdl2.SDL_Window,
    renderer: ?*sdl2.SDL_Renderer,
    texture: ?*sdl2.SDL_Texture,
    pixel_format: ?*sdl2.SDL_PixelFormat,

    // Display properties
    width: u32,
    height: u32,
    pixel_scale: u32, // Pixel scaling factor
    display_region: []DLword, // Display region buffer

    // Color information
    foreground_color: u32,
    background_color: u32,
    bytes_per_pixel: u32,

    /// Initialize SDL2 display (T075-T078)
    /// Per maiko/src/sdl.c:init_SDL
    pub fn init(
        allocator: std.mem.Allocator,
        window_title: []const u8,
        width: u32,
        height: u32,
        pixel_scale: u32,
    ) errors.DisplayError!DisplayInterface {
        // T075: Initialize SDL2
        if (sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO) < 0) {
            const error_msg = std.mem.span(sdl2.SDL_GetError());
            std.debug.print("SDL could not be initialized. SDL_Error: {s}\n", .{error_msg});
            return error.SDLInitFailed;
        }
        std.debug.print("SDL initialized\n", .{});

        // Round width to multiple of 32 (C: w = (w + 31) / 32 * 32)
        const display_width = ((width + 31) / 32) * 32;
        const display_height = height;
        const window_width = display_width * pixel_scale;
        const window_height = display_height * pixel_scale;

        std.debug.print("requested width: {}, height: {}\n", .{ display_width, display_height });

        // T076: Create SDL_Window
        const title_cstr = allocator.dupeZ(u8, window_title) catch {
            return error.SDLInitFailed; // Map OutOfMemory to DisplayError
        };
        defer allocator.free(title_cstr);

        const window = sdl2.SDL_CreateWindow(
            title_cstr.ptr,
            sdl2.SDL_WINDOWPOS_UNDEFINED,
            sdl2.SDL_WINDOWPOS_UNDEFINED,
            @as(c_int, @intCast(window_width)),
            @as(c_int, @intCast(window_height)),
            0,
        );

        if (window == null) {
            const error_msg = std.mem.span(sdl2.SDL_GetError());
            std.debug.print("Window could not be created. SDL_Error: {s}\n", .{error_msg});
            sdl2.SDL_Quit();
            return error.SDLWindowCreationFailed;
        }
        std.debug.print("Window created\n", .{});

        // T077: Create SDL_Renderer
        std.debug.print("Creating renderer...\n", .{});
        const renderer = sdl2.SDL_CreateRenderer(window.?, -1, sdl2.SDL_RENDERER_ACCELERATED);

        if (renderer == null) {
            const error_msg = std.mem.span(sdl2.SDL_GetError());
            std.debug.print("SDL Error: {s}\n", .{error_msg});
            sdl2.SDL_DestroyWindow(window.?);
            sdl2.SDL_Quit();
            return error.SDLRendererCreationFailed;
        }

        var renderer_info: sdl2.SDL_RendererInfo = undefined;
        _ = sdl2.SDL_GetRendererInfo(renderer, &renderer_info);
        _ = sdl2.SDL_SetRenderDrawColor(renderer, 127, 127, 127, 255);
        _ = sdl2.SDL_RenderClear(renderer);
        sdl2.SDL_RenderPresent(renderer);
        _ = sdl2.SDL_RenderSetScale(renderer, 1.0, 1.0);

        // Get pixel format from renderer
        const pixel_format = sdl2.SDL_AllocFormat(renderer_info.texture_formats[0]);
        if (pixel_format == null) {
            sdl2.SDL_DestroyRenderer(renderer.?);
            sdl2.SDL_DestroyWindow(window.?);
            sdl2.SDL_Quit();
            return error.SDLPixelFormatFailed;
        }

        // T078: Create SDL_Texture
        std.debug.print("Creating texture...\n", .{});
        const texture = sdl2.SDL_CreateTexture(
            renderer,
            pixel_format.?.format,
            sdl2.SDL_TEXTUREACCESS_STREAMING,
            @as(c_int, @intCast(display_width)),
            @as(c_int, @intCast(display_height)),
        );

        if (texture == null) {
            const error_msg = std.mem.span(sdl2.SDL_GetError());
            std.debug.print("Texture could not be created. SDL_Error: {s}\n", .{error_msg});
            sdl2.SDL_DestroyRenderer(renderer.?);
            sdl2.SDL_DestroyWindow(window.?);
            sdl2.SDL_Quit();
            return error.SDLTextureCreationFailed;
        }

        // Set up colors (default: black foreground, white background)
        const foreground_color = sdl2.SDL_MapColorName(pixel_format.?, "black");
        const background_color = sdl2.SDL_MapColorName(pixel_format.?, "white");
        const bytes_per_pixel = pixel_format.?.BytesPerPixel;

        // Allocate display region buffer
        const buffer_size = display_width * display_height;
        const display_buffer = try allocator.alloc(DLword, buffer_size);
        @memset(display_buffer, 0);

        return DisplayInterface{
            .window = window,
            .renderer = renderer,
            .texture = texture,
            .pixel_format = pixel_format,
            .width = display_width,
            .height = display_height,
            .pixel_scale = pixel_scale,
            .display_region = display_buffer,
            .foreground_color = foreground_color,
            .background_color = background_color,
            .bytes_per_pixel = bytes_per_pixel,
        };
    }

    pub fn deinit(self: *DisplayInterface, allocator: std.mem.Allocator) void {
        allocator.free(self.display_region);

        // Destroy SDL2 objects
        if (self.texture) |tex| {
            sdl2.SDL_DestroyTexture(tex.?);
        }
        if (self.renderer) |ren| {
            sdl2.SDL_DestroyRenderer(ren);
        }
        if (self.window) |win| {
            sdl2.SDL_DestroyWindow(win);
        }
        if (self.pixel_format) |fmt| {
            // SDL_FreeFormat is not exposed, but format is managed by SDL
            _ = fmt;
        }

        sdl2.SDL_Quit();
    }
};

/// Initialize SDL display (T075-T078)
/// Per contracts/display-interface.zig and maiko/src/sdl.c:init_SDL
pub fn initDisplay(
    window_title: []const u8,
    width: u32,
    height: u32,
    pixel_scale: u32,
    allocator: std.mem.Allocator,
) errors.DisplayError!*DisplayInterface {
    // Allocate DisplayInterface on heap (caller must free)
    const display = allocator.create(DisplayInterface) catch {
        return error.SDLInitFailed; // Map OutOfMemory to DisplayError
    };
    display.* = try DisplayInterface.init(allocator, window_title, width, height, pixel_scale);
    return display;
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