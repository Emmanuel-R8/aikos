// SDL2 event handling (T084-T090)
// Per maiko/src/sdl.c:process_SDLevents

const std = @import("std");
const errors = @import("../utils/errors.zig");
const sdl_backend = @import("sdl_backend.zig");
const sdl2 = @import("sdl2.zig");
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

/// Event structure
pub const Event = union(EventType) {
    KEY_PRESS: keyboard.KeyboardEvent,
    KEY_RELEASE: keyboard.KeyboardEvent,
    BUTTON_PRESS: mouse.MouseEvent,
    BUTTON_RELEASE: mouse.MouseEvent,
    MOTION: mouse.MouseEvent,
    QUIT: void,
};

/// SDL keycode to Lisp keycode mapping
/// Per maiko/src/sdl.c:keymap[]
const KeyMapEntry = struct {
    lisp_keycode: u16,
    sdl_keycode: i32,
};

const KEYMAP: []const KeyMapEntry = &[_]KeyMapEntry{
    .{ .lisp_keycode = 0, .sdl_keycode = sdl2.SDLK_5 },
    .{ .lisp_keycode = 1, .sdl_keycode = sdl2.SDLK_4 },
    .{ .lisp_keycode = 2, .sdl_keycode = sdl2.SDLK_6 },
    .{ .lisp_keycode = 3, .sdl_keycode = sdl2.SDLK_e },
    .{ .lisp_keycode = 4, .sdl_keycode = sdl2.SDLK_7 },
    .{ .lisp_keycode = 5, .sdl_keycode = sdl2.SDLK_d },
    .{ .lisp_keycode = 6, .sdl_keycode = sdl2.SDLK_u },
    .{ .lisp_keycode = 7, .sdl_keycode = sdl2.SDLK_v },
    .{ .lisp_keycode = 8, .sdl_keycode = sdl2.SDLK_RIGHTPAREN },
    .{ .lisp_keycode = 8, .sdl_keycode = sdl2.SDLK_0 },
    .{ .lisp_keycode = 9, .sdl_keycode = sdl2.SDLK_k },
    .{ .lisp_keycode = 10, .sdl_keycode = sdl2.SDLK_MINUS },
    .{ .lisp_keycode = 11, .sdl_keycode = sdl2.SDLK_p },
    .{ .lisp_keycode = 12, .sdl_keycode = sdl2.SDLK_SLASH },
    .{ .lisp_keycode = 13, .sdl_keycode = sdl2.SDLK_KP_PERIOD },
    .{ .lisp_keycode = 14, .sdl_keycode = sdl2.SDLK_SCROLLLOCK },
    .{ .lisp_keycode = 15, .sdl_keycode = sdl2.SDLK_BACKSPACE },
    .{ .lisp_keycode = 16, .sdl_keycode = sdl2.SDLK_3 },
    .{ .lisp_keycode = 17, .sdl_keycode = sdl2.SDLK_2 },
    .{ .lisp_keycode = 18, .sdl_keycode = sdl2.SDLK_w },
    .{ .lisp_keycode = 19, .sdl_keycode = sdl2.SDLK_q },
    .{ .lisp_keycode = 20, .sdl_keycode = sdl2.SDLK_s },
    .{ .lisp_keycode = 21, .sdl_keycode = sdl2.SDLK_a },
    .{ .lisp_keycode = 22, .sdl_keycode = sdl2.SDLK_LEFTPAREN },
    .{ .lisp_keycode = 22, .sdl_keycode = sdl2.SDLK_9 },
    .{ .lisp_keycode = 23, .sdl_keycode = sdl2.SDLK_i },
    .{ .lisp_keycode = 24, .sdl_keycode = sdl2.SDLK_x },
    .{ .lisp_keycode = 25, .sdl_keycode = sdl2.SDLK_o },
    .{ .lisp_keycode = 26, .sdl_keycode = sdl2.SDLK_l },
    .{ .lisp_keycode = 27, .sdl_keycode = sdl2.SDLK_COMMA },
    .{ .lisp_keycode = 28, .sdl_keycode = sdl2.SDLK_QUOTE },
    .{ .lisp_keycode = 29, .sdl_keycode = sdl2.SDLK_RIGHTBRACKET },
    .{ .lisp_keycode = 31, .sdl_keycode = sdl2.SDLK_LALT },
    .{ .lisp_keycode = 32, .sdl_keycode = sdl2.SDLK_1 },
    .{ .lisp_keycode = 33, .sdl_keycode = sdl2.SDLK_ESCAPE },
    .{ .lisp_keycode = 34, .sdl_keycode = sdl2.SDLK_TAB },
    .{ .lisp_keycode = 35, .sdl_keycode = sdl2.SDLK_f },
    .{ .lisp_keycode = 36, .sdl_keycode = sdl2.SDLK_LCTRL },
    .{ .lisp_keycode = 37, .sdl_keycode = sdl2.SDLK_c },
    .{ .lisp_keycode = 38, .sdl_keycode = sdl2.SDLK_j },
    .{ .lisp_keycode = 39, .sdl_keycode = sdl2.SDLK_b },
    .{ .lisp_keycode = 40, .sdl_keycode = sdl2.SDLK_z },
    .{ .lisp_keycode = 41, .sdl_keycode = sdl2.SDLK_LSHIFT },
    .{ .lisp_keycode = 42, .sdl_keycode = sdl2.SDLK_PERIOD },
    .{ .lisp_keycode = 43, .sdl_keycode = sdl2.SDLK_SEMICOLON },
    .{ .lisp_keycode = 43, .sdl_keycode = sdl2.SDLK_COLON },
    .{ .lisp_keycode = 44, .sdl_keycode = sdl2.SDLK_RETURN },
    .{ .lisp_keycode = 45, .sdl_keycode = sdl2.SDLK_BACKQUOTE },
    .{ .lisp_keycode = 47, .sdl_keycode = sdl2.SDLK_RCTRL },
    .{ .lisp_keycode = 48, .sdl_keycode = sdl2.SDLK_r },
    .{ .lisp_keycode = 49, .sdl_keycode = sdl2.SDLK_t },
    .{ .lisp_keycode = 50, .sdl_keycode = sdl2.SDLK_g },
    .{ .lisp_keycode = 51, .sdl_keycode = sdl2.SDLK_y },
    .{ .lisp_keycode = 52, .sdl_keycode = sdl2.SDLK_h },
    .{ .lisp_keycode = 53, .sdl_keycode = sdl2.SDLK_8 },
    .{ .lisp_keycode = 54, .sdl_keycode = sdl2.SDLK_n },
    .{ .lisp_keycode = 55, .sdl_keycode = sdl2.SDLK_m },
    .{ .lisp_keycode = 56, .sdl_keycode = sdl2.SDLK_CAPSLOCK },
    .{ .lisp_keycode = 57, .sdl_keycode = sdl2.SDLK_SPACE },
    .{ .lisp_keycode = 58, .sdl_keycode = sdl2.SDLK_LEFTBRACKET },
    .{ .lisp_keycode = 59, .sdl_keycode = sdl2.SDLK_EQUALS },
    .{ .lisp_keycode = 60, .sdl_keycode = sdl2.SDLK_RSHIFT },
    .{ .lisp_keycode = 61, .sdl_keycode = sdl2.SDLK_F11 },
    .{ .lisp_keycode = 61, .sdl_keycode = sdl2.SDLK_PAUSE },
    .{ .lisp_keycode = 62, .sdl_keycode = sdl2.SDLK_HOME },
    .{ .lisp_keycode = 63, .sdl_keycode = sdl2.SDLK_PAGEUP },
    .{ .lisp_keycode = 64, .sdl_keycode = sdl2.SDLK_KP_EQUALS },
    .{ .lisp_keycode = 65, .sdl_keycode = sdl2.SDLK_KP_DIVIDE },
    .{ .lisp_keycode = 66, .sdl_keycode = sdl2.SDLK_F7 },
    .{ .lisp_keycode = 67, .sdl_keycode = sdl2.SDLK_F4 },
    .{ .lisp_keycode = 68, .sdl_keycode = sdl2.SDLK_F5 },
    .{ .lisp_keycode = 69, .sdl_keycode = sdl2.SDLK_KP_2 },
    .{ .lisp_keycode = 70, .sdl_keycode = sdl2.SDLK_KP_3 },
    .{ .lisp_keycode = 76, .sdl_keycode = sdl2.SDLK_KP_ENTER },
};

const KEYCODE_OFFSET: u16 = 0;

/// Map SDL keycode to Lisp keycode (T086)
/// Per maiko/src/sdl.c:map_key()
fn mapKey(sdl_keycode: i32) ?u16 {
    for (KEYMAP) |entry| {
        if (entry.sdl_keycode == sdl_keycode) {
            return entry.lisp_keycode;
        }
    }
    return null;
}

/// Extract modifier flags from SDL keymod
fn extractModifiers(sdl_mod: u16) u16 {
    var modifiers: u16 = 0;
    if (sdl_mod & sdl2.KMOD_SHIFT != 0) modifiers |= keyboard.Modifiers.SHIFT;
    if (sdl_mod & sdl2.KMOD_CTRL != 0) modifiers |= keyboard.Modifiers.CONTROL;
    if (sdl_mod & sdl2.KMOD_ALT != 0) modifiers |= keyboard.Modifiers.ALT;
    if (sdl_mod & sdl2.KMOD_GUI != 0) modifiers |= keyboard.Modifiers.META;
    return modifiers;
}

/// Poll SDL2 events (T084)
/// Per maiko/src/sdl.c:process_SDLevents()
pub fn pollEvents(
    display: *sdl_backend.DisplayInterface,
    key_queue: *keyboard.KeyEventQueue,
    mouse_state: *mouse.MouseState,
    allocator: std.mem.Allocator,
) errors.DisplayError!bool {
    _ = allocator;

    var sdl_event: sdl2.SDL_Event = undefined;

    // T084: Poll events in loop
    while (sdl2.SDL_PollEvent(&sdl_event) != 0) {
        switch (sdl_event.type) {
            sdl2.SDL_QUIT => {
                // Quit event - return true to signal quit
                return true;
            },

            // T085: Handle keyboard events
            sdl2.SDL_KEYDOWN => {
                const key_event = sdl_event.key;
                if (key_event.repeat != 0) {
                    // For key repeat, send UP before DOWN (C: "Lisp needs to see the UP transition before the DOWN transition")
                    if (mapKey(key_event.keysym.sym)) |lisp_keycode| {
                        const up_event = keyboard.KeyboardEvent{
                            .event_type = .KEY_RELEASE,
                            .keycode = lisp_keycode - KEYCODE_OFFSET,
                            .modifiers = extractModifiers(key_event.keysym.mod),
                            .timestamp = key_event.timestamp,
                        };
                        keyboard.enqueueKeyEvent(key_queue, up_event) catch {};
                    }
                }

                // T086: Translate keycode and T087: Deliver to queue
                if (mapKey(key_event.keysym.sym)) |lisp_keycode| {
                    const kb_event = keyboard.KeyboardEvent{
                        .event_type = .KEY_PRESS,
                        .keycode = lisp_keycode - KEYCODE_OFFSET,
                        .modifiers = extractModifiers(key_event.keysym.mod),
                        .timestamp = key_event.timestamp,
                    };
                    keyboard.enqueueKeyEvent(key_queue, kb_event) catch {};
                } else {
                    // No mapping found (C: printf("No mapping for key %s\n", SDL_GetKeyName(k)))
                    std.debug.print("No mapping for key {}\n", .{key_event.keysym.sym});
                }
            },

            sdl2.SDL_KEYUP => {
                const key_event = sdl_event.key;
                // T086: Translate keycode and T087: Deliver to queue
                if (mapKey(key_event.keysym.sym)) |lisp_keycode| {
                    const kb_event = keyboard.KeyboardEvent{
                        .event_type = .KEY_RELEASE,
                        .keycode = lisp_keycode - KEYCODE_OFFSET,
                        .modifiers = extractModifiers(key_event.keysym.mod),
                        .timestamp = key_event.timestamp,
                    };
                    keyboard.enqueueKeyEvent(key_queue, kb_event) catch {};
                }
            },

            // T088: Handle mouse events
            sdl2.SDL_MOUSEMOTION => {
                const motion_event = sdl_event.motion;
                // T089: Translate coordinates (divide by pixel_scale)
                const x = @divTrunc(@as(i32, @intCast(motion_event.x)), @as(i32, @intCast(display.pixel_scale)));
                const y = @divTrunc(@as(i32, @intCast(motion_event.y)), @as(i32, @intCast(display.pixel_scale)));

                // T090: Update mouse state
                mouse.updateMousePosition(mouse_state, x, y);
            },

            sdl2.SDL_MOUSEBUTTONDOWN => {
                const button_event = sdl_event.button;
                // T089: Translate coordinates
                const x = @divTrunc(@as(i32, @intCast(button_event.x)), @as(i32, @intCast(display.pixel_scale)));
                const y = @divTrunc(@as(i32, @intCast(button_event.y)), @as(i32, @intCast(display.pixel_scale)));

                // T090: Create mouse event
                // TODO: Use button_num for event delivery to Lisp
                _ = switch (button_event.button) {
                    sdl2.SDL_BUTTON_LEFT => @as(u8, 1),
                    sdl2.SDL_BUTTON_MIDDLE => @as(u8, 2),
                    sdl2.SDL_BUTTON_RIGHT => @as(u8, 3),
                    else => @as(u8, 0),
                };

                mouse.updateMousePosition(mouse_state, x, y);
                // Note: Button state is tracked in mouse_state, but event delivery to Lisp
                // would happen via a separate mechanism (similar to kb_trans)
            },

            sdl2.SDL_MOUSEBUTTONUP => {
                const button_event = sdl_event.button;
                // T089: Translate coordinates
                const x = @divTrunc(@as(i32, @intCast(button_event.x)), @as(i32, @intCast(display.pixel_scale)));
                const y = @divTrunc(@as(i32, @intCast(button_event.y)), @as(i32, @intCast(display.pixel_scale)));

                mouse.updateMousePosition(mouse_state, x, y);
                // Note: Button release handling similar to button down
            },

            else => {
                // Other events (window events, etc.) - ignore for now
            },
        }
    }
    return false;
}

/// Process mouse event (legacy interface)
/// Per contracts/display-interface.zig
pub fn processMouseEvent(display: *sdl_backend.DisplayInterface, sdl_event: *const anyopaque) mouse.MouseEvent {
    _ = display;
    const event: *const sdl2.SDL_Event = @ptrCast(@alignCast(sdl_event));

    switch (event.type) {
        sdl2.SDL_MOUSEBUTTONDOWN => {
            return mouse.MouseEvent{
                .event_type = .BUTTON_PRESS,
                .button = @intCast(event.button.button),
                .x = event.button.x,
                .y = event.button.y,
                .modifiers = 0, // TODO: Get modifier state
                .timestamp = event.button.timestamp,
            };
        },
        sdl2.SDL_MOUSEBUTTONUP => {
            return mouse.MouseEvent{
                .event_type = .BUTTON_RELEASE,
                .button = @intCast(event.button.button),
                .x = event.button.x,
                .y = event.button.y,
                .modifiers = 0, // TODO: Get modifier state
                .timestamp = event.button.timestamp,
            };
        },
        sdl2.SDL_MOUSEMOTION => {
            return mouse.MouseEvent{
                .event_type = .MOTION,
                .button = 0,
                .x = event.motion.x,
                .y = event.motion.y,
                .modifiers = 0, // TODO: Get modifier state
                .timestamp = event.motion.timestamp,
            };
        },
        else => {
            // Unknown mouse event type, return motion with zeros
            return mouse.MouseEvent{
                .event_type = .MOTION,
                .button = 0,
                .x = 0,
                .y = 0,
                .modifiers = 0,
                .timestamp = 0,
            };
        },
    }
}
