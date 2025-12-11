// SDL2 C interop bindings for Zig
// Per maiko/src/sdl.c and SDL2 API

const std = @import("std");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

pub const SDL_INIT_VIDEO = c.SDL_INIT_VIDEO;
pub const SDL_WINDOWPOS_UNDEFINED = c.SDL_WINDOWPOS_UNDEFINED;
pub const SDL_RENDERER_ACCELERATED = c.SDL_RENDERER_ACCELERATED;
pub const SDL_TEXTUREACCESS_STREAMING = c.SDL_TEXTUREACCESS_STREAMING;

pub const SDL_Window = c.SDL_Window;
pub const SDL_Renderer = c.SDL_Renderer;
pub const SDL_Texture = c.SDL_Texture;
pub const SDL_PixelFormat = c.SDL_PixelFormat;
pub const SDL_RendererInfo = c.SDL_RendererInfo;

pub const SDL_Event = c.SDL_Event;
pub const SDL_KeyboardEvent = c.SDL_KeyboardEvent;
pub const SDL_MouseMotionEvent = c.SDL_MouseMotionEvent;
pub const SDL_MouseButtonEvent = c.SDL_MouseButtonEvent;

pub const SDL_QUIT = c.SDL_QUIT;
pub const SDL_KEYDOWN = c.SDL_KEYDOWN;
pub const SDL_KEYUP = c.SDL_KEYUP;
pub const SDL_MOUSEMOTION = c.SDL_MOUSEMOTION;
pub const SDL_MOUSEBUTTONDOWN = c.SDL_MOUSEBUTTONDOWN;
pub const SDL_MOUSEBUTTONUP = c.SDL_MOUSEBUTTONUP;

// SDL keycodes
pub const SDLK_5 = c.SDLK_5;
pub const SDLK_4 = c.SDLK_4;
pub const SDLK_6 = c.SDLK_6;
pub const SDLK_e = c.SDLK_e;
pub const SDLK_7 = c.SDLK_7;
pub const SDLK_d = c.SDLK_d;
pub const SDLK_u = c.SDLK_u;
pub const SDLK_v = c.SDLK_v;
pub const SDLK_RIGHTPAREN = c.SDLK_RIGHTPAREN;
pub const SDLK_0 = c.SDLK_0;
pub const SDLK_k = c.SDLK_k;
pub const SDLK_MINUS = c.SDLK_MINUS;
pub const SDLK_p = c.SDLK_p;
pub const SDLK_SLASH = c.SDLK_SLASH;
pub const SDLK_KP_PERIOD = c.SDLK_KP_PERIOD;
pub const SDLK_SCROLLLOCK = c.SDLK_SCROLLLOCK;
pub const SDLK_BACKSPACE = c.SDLK_BACKSPACE;
pub const SDLK_3 = c.SDLK_3;
pub const SDLK_2 = c.SDLK_2;
pub const SDLK_w = c.SDLK_w;
pub const SDLK_q = c.SDLK_q;
pub const SDLK_s = c.SDLK_s;
pub const SDLK_a = c.SDLK_a;
pub const SDLK_LEFTPAREN = c.SDLK_LEFTPAREN;
pub const SDLK_9 = c.SDLK_9;
pub const SDLK_i = c.SDLK_i;
pub const SDLK_x = c.SDLK_x;
pub const SDLK_o = c.SDLK_o;
pub const SDLK_l = c.SDLK_l;
pub const SDLK_COMMA = c.SDLK_COMMA;
pub const SDLK_QUOTE = c.SDLK_QUOTE;
pub const SDLK_RIGHTBRACKET = c.SDLK_RIGHTBRACKET;
pub const SDLK_LALT = c.SDLK_LALT;
pub const SDLK_1 = c.SDLK_1;
pub const SDLK_ESCAPE = c.SDLK_ESCAPE;
pub const SDLK_TAB = c.SDLK_TAB;
pub const SDLK_f = c.SDLK_f;
pub const SDLK_LCTRL = c.SDLK_LCTRL;
pub const SDLK_c = c.SDLK_c;
pub const SDLK_j = c.SDLK_j;
pub const SDLK_b = c.SDLK_b;
pub const SDLK_z = c.SDLK_z;
pub const SDLK_LSHIFT = c.SDLK_LSHIFT;
pub const SDLK_PERIOD = c.SDLK_PERIOD;
pub const SDLK_SEMICOLON = c.SDLK_SEMICOLON;
pub const SDLK_COLON = c.SDLK_COLON;
pub const SDLK_RETURN = c.SDLK_RETURN;
pub const SDLK_BACKQUOTE = c.SDLK_BACKQUOTE;
pub const SDLK_RCTRL = c.SDLK_RCTRL;
pub const SDLK_r = c.SDLK_r;
pub const SDLK_t = c.SDLK_t;
pub const SDLK_g = c.SDLK_g;
pub const SDLK_y = c.SDLK_y;
pub const SDLK_h = c.SDLK_h;
pub const SDLK_8 = c.SDLK_8;
pub const SDLK_n = c.SDLK_n;
pub const SDLK_m = c.SDLK_m;
pub const SDLK_CAPSLOCK = c.SDLK_CAPSLOCK;
pub const SDLK_SPACE = c.SDLK_SPACE;
pub const SDLK_LEFTBRACKET = c.SDLK_LEFTBRACKET;
pub const SDLK_EQUALS = c.SDLK_EQUALS;
pub const SDLK_RSHIFT = c.SDLK_RSHIFT;
pub const SDLK_F11 = c.SDLK_F11;
pub const SDLK_PAUSE = c.SDLK_PAUSE;
pub const SDLK_HOME = c.SDLK_HOME;
pub const SDLK_PAGEUP = c.SDLK_PAGEUP;
pub const SDLK_KP_EQUALS = c.SDLK_KP_EQUALS;
pub const SDLK_KP_DIVIDE = c.SDLK_KP_DIVIDE;
pub const SDLK_F7 = c.SDLK_F7;
pub const SDLK_F4 = c.SDLK_F4;
pub const SDLK_F5 = c.SDLK_F5;
pub const SDLK_KP_2 = c.SDLK_KP_2;
pub const SDLK_KP_3 = c.SDLK_KP_3;
pub const SDLK_KP_ENTER = c.SDLK_KP_ENTER;

// SDL mouse buttons
pub const SDL_BUTTON_LEFT = c.SDL_BUTTON_LEFT;
pub const SDL_BUTTON_MIDDLE = c.SDL_BUTTON_MIDDLE;
pub const SDL_BUTTON_RIGHT = c.SDL_BUTTON_RIGHT;

// SDL key modifiers
pub const KMOD_SHIFT = c.KMOD_SHIFT;
pub const KMOD_CTRL = c.KMOD_CTRL;
pub const KMOD_ALT = c.KMOD_ALT;
pub const KMOD_GUI = c.KMOD_GUI;

pub fn SDL_Init(flags: u32) c_int {
    return c.SDL_Init(flags);
}

pub fn SDL_GetError() [*c]const u8 {
    return c.SDL_GetError();
}

pub fn SDL_CreateWindow(
    title: [*c]const u8,
    x: c_int,
    y: c_int,
    w: c_int,
    h: c_int,
    flags: u32,
) ?*SDL_Window {
    return c.SDL_CreateWindow(title, x, y, w, h, flags);
}

pub fn SDL_CreateRenderer(
    window: *SDL_Window,
    index: c_int,
    flags: u32,
) ?*SDL_Renderer {
    return c.SDL_CreateRenderer(window, index, flags);
}

pub fn SDL_GetRendererInfo(renderer: *SDL_Renderer, info: *SDL_RendererInfo) c_int {
    return c.SDL_GetRendererInfo(renderer, info);
}

pub fn SDL_SetRenderDrawColor(
    renderer: *SDL_Renderer,
    r: u8,
    g: u8,
    b: u8,
    a: u8,
) c_int {
    return c.SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

pub fn SDL_RenderClear(renderer: *SDL_Renderer) c_int {
    return c.SDL_RenderClear(renderer);
}

pub fn SDL_RenderPresent(renderer: *SDL_Renderer) void {
    c.SDL_RenderPresent(renderer);
}

pub fn SDL_RenderSetScale(renderer: *SDL_Renderer, scaleX: f32, scaleY: f32) c_int {
    return c.SDL_RenderSetScale(renderer, scaleX, scaleY);
}

pub fn SDL_AllocFormat(format: u32) ?*SDL_PixelFormat {
    return c.SDL_AllocFormat(format);
}

pub fn SDL_CreateTexture(
    renderer: *SDL_Renderer,
    format: u32,
    access: c_int,
    w: c_int,
    h: c_int,
) ?*SDL_Texture {
    return c.SDL_CreateTexture(renderer, format, access, w, h);
}

// Helper to map color name to RGB (simplified - matches C sdl_MapColorName)
pub fn SDL_MapColorName(format: *SDL_PixelFormat, name: [*c]const u8) u32 {
    // C implementation: sdl_MapColorName approximates X11 color parsing
    // For now, use simple color name mapping
    const name_slice = std.mem.span(name);

    // Common color names (matching C implementation logic)
    if (std.mem.eql(u8, name_slice, "black")) {
        return c.SDL_MapRGB(format, 0, 0, 0);
    } else if (std.mem.eql(u8, name_slice, "white")) {
        return c.SDL_MapRGB(format, 255, 255, 255);
    } else if (std.mem.eql(u8, name_slice, "red")) {
        return c.SDL_MapRGB(format, 255, 0, 0);
    } else if (std.mem.eql(u8, name_slice, "green")) {
        return c.SDL_MapRGB(format, 0, 255, 0);
    } else if (std.mem.eql(u8, name_slice, "blue")) {
        return c.SDL_MapRGB(format, 0, 0, 255);
    }

    // Default to black if name not recognized
    return c.SDL_MapRGB(format, 0, 0, 0);
}

pub fn SDL_UpdateTexture(
    texture: *SDL_Texture,
    rect: ?*const c.SDL_Rect,
    pixels: *const anyopaque,
    pitch: c_int,
) c_int {
    return c.SDL_UpdateTexture(texture, rect, pixels, pitch);
}

pub fn SDL_LockTexture(
    texture: *SDL_Texture,
    rect: ?*const c.SDL_Rect,
    pixels: *?*anyopaque,
    pitch: *c_int,
) c_int {
    return c.SDL_LockTexture(texture, rect, pixels, pitch);
}

pub fn SDL_UnlockTexture(texture: *SDL_Texture) void {
    c.SDL_UnlockTexture(texture);
}

pub fn SDL_RenderCopy(
    renderer: *SDL_Renderer,
    texture: *SDL_Texture,
    srcrect: ?*const c.SDL_Rect,
    dstrect: ?*const c.SDL_Rect,
) c_int {
    return c.SDL_RenderCopy(renderer, texture, srcrect, dstrect);
}

pub fn SDL_PollEvent(event: *SDL_Event) c_int {
    return c.SDL_PollEvent(event);
}

pub fn SDL_DestroyTexture(texture: *SDL_Texture) void {
    c.SDL_DestroyTexture(texture);
}

pub fn SDL_DestroyRenderer(renderer: *SDL_Renderer) void {
    c.SDL_DestroyRenderer(renderer);
}

pub fn SDL_DestroyWindow(window: *SDL_Window) void {
    c.SDL_DestroyWindow(window);
}

pub fn SDL_Quit() void {
    c.SDL_Quit();
}