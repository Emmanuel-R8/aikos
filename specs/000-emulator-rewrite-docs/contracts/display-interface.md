# Display Interface Contract

**Version**: 1.0
**Date**: 2025-12-04
**Type**: Interface Specification

## Overview

This contract specifies the interface between the VM core and display subsystems. Any display implementation must fulfill this contract to maintain compatibility.

## Required Operations

### Display Initialization

**Operation**: `initialize_display(width, height, options)`

**Preconditions**:

- `width > 0` and `height > 0`
- Display subsystem available on platform

**Postconditions**:

- Display window created and visible
- Display region memory allocated
- Event handling enabled

**Semantics**: Initialize display subsystem and create main window with specified dimensions.

### Graphics Rendering

**Operation**: `render_region(source_x, source_y, width, height, dest_x, dest_y, operation)`

**Preconditions**:

- Source and destination regions within display bounds
- `operation` is valid graphics operation (copy, XOR, etc.)

**Postconditions**:

- Specified region rendered to display
- Display updated (may be buffered)

**Semantics**: Copy rectangular region from display memory to screen using specified operation.

### Event Handling

**Operation**: `poll_events() -> Event[]`

**Preconditions**: Display initialized

**Postconditions**: Returns array of pending events (may be empty)

**Semantics**: Retrieve pending input events (keyboard, mouse, window events).

### Window Management

**Operation**: `set_window_title(title)`

**Operation**: `resize_window(width, height)`

**Operation**: `set_cursor(cursor_bitmap, hotspot_x, hotspot_y)`

**Semantics**: Standard window management operations.

## Event Protocol

### Keyboard Event Format

```
KeyboardEvent {
    type: KEY_PRESS | KEY_RELEASE
    keycode: integer  // Lisp keycode (not OS keycode)
    modifiers: bitmask  // Shift, Control, Meta flags
    timestamp: integer
}
```

### Mouse Event Format

```
MouseEvent {
    type: BUTTON_PRESS | BUTTON_RELEASE | MOTION
    button: integer  // Button number (1, 2, 3) or 0 for motion
    x: integer  // X coordinate
    y: integer  // Y coordinate
    modifiers: bitmask
    timestamp: integer
}
```

## Display Region Protocol

The display region is a memory-mapped area representing screen contents:

- **Format**: Pixel data in specified format (monochrome, indexed color, true color)
- **Layout**: Row-major order, specified bytes per pixel
- **Update**: VM writes to display region, calls render operations to update screen

## Platform Abstraction

### Required Behaviors (Must Match)

- Event keycode translation to Lisp keycodes
- Display region memory mapping
- Graphics operation semantics (copy, XOR, etc.)

### Implementation Choices (May Differ)

- Specific graphics library (X11, SDL, DirectX, etc.)
- Window decoration style
- Cursor appearance (as long as hotspot correct)
- Event delivery mechanism (polling vs callbacks)

## Compatibility Requirements

- Must support at least 1024x768 resolution
- Must support monochrome and at least 8-bit color modes
- Must translate OS keycodes to Lisp keycodes correctly
- Must handle window resize events

## Error Handling

- Invalid parameters: Return error, do not crash
- Display unavailable: Return error during initialization
- Rendering failure: Log error, continue operation if possible
