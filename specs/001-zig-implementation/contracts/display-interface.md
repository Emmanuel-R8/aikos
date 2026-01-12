# Display Interface Contract (SDL)

**Version**: 1.0  
**Date**: 2025-12-04  
**Type**: Interface Specification  
**Language**: Zig

## Overview

This contract specifies the SDL display interface for the Zig implementation. SDL3 is used as the display backend.

## Display Initialization Interface

```zig
// Initialize SDL display
pub fn initDisplay(width: u32, height: u32, depth: u32) !*DisplayInterface;

// Destroy display
pub fn destroyDisplay(display: *DisplayInterface) void;

// Get display region buffer
pub fn getDisplayRegion(display: *DisplayInterface) []DLword;
```

**Preconditions**: SDL3 available, valid dimensions

**Postconditions**: Display initialized, window created, buffer allocated

**Semantics**: Initialize SDL display subsystem and create window.

## Graphics Operations Interface

```zig
// Render region (BitBLT)
pub fn renderRegion(display: *DisplayInterface, 
                   source_x: u32, source_y: u32,
                   width: u32, height: u32,
                   dest_x: u32, dest_y: u32,
                   operation: GraphicsOperation) !void;

// Flush display region
pub fn flushDisplayRegion(display: *DisplayInterface,
                         x: u32, y: u32, width: u32, height: u32) void;

// Update display
pub fn updateDisplay(display: *DisplayInterface) void;
```

**Preconditions**: Display initialized, regions within bounds

**Postconditions**: Region rendered, display updated

**Semantics**: Copy rectangular region from display memory to SDL window.

## Event Handling Interface

```zig
// Poll events
pub fn pollEvents(display: *DisplayInterface) ![]Event;

// Translate keycode
pub fn translateKeycode(os_keycode: u32, modifiers: u16) u16;

// Process mouse event
pub fn processMouseEvent(display: *DisplayInterface, sdl_event: *sdl.SDL_Event) MouseEvent;
```

**Preconditions**: Display initialized

**Postconditions**: Events polled, keycodes translated

**Semantics**: Retrieve and translate SDL events to Lisp event format.

## Window Management Interface

```zig
// Set window title
pub fn setWindowTitle(display: *DisplayInterface, title: []const u8) void;

// Resize window
pub fn resizeWindow(display: *DisplayInterface, width: u32, height: u32) !void;

// Set cursor
pub fn setCursor(display: *DisplayInterface, bitmap: []const u8, hotspot_x: u32, hotspot_y: u32) !void;
```

**Preconditions**: Display initialized

**Postconditions**: Window title set, resized, cursor set

**Semantics**: Standard window management operations.

## Error Types

```zig
pub const DisplayError = error{
    SDLInitFailed,
    WindowCreationFailed,
    RendererCreationFailed,
    TextureCreationFailed,
    InvalidRegion,
};
```

## Related Documentation

- `documentation/rewrite-spec/display/interface-abstraction.md` - Display interface specification
- `documentation/rewrite-spec/display/graphics-operations.md` - Graphics operations
