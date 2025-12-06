# Display Interface Abstraction Specification

**Navigation**: [README](README.md) | [Graphics Operations](graphics-operations.md) | [Event Protocols](event-protocols.md)

Complete specification of the display interface abstraction, including required operations, event protocols, and platform abstraction requirements.

## Overview

The display interface abstraction provides a platform-independent interface for graphics output and input events. Any display backend implementation must fulfill this contract to maintain compatibility.

## Interface Contract

### Display Initialization

**Operation**: `initialize_display(width, height, depth, options)`

**Preconditions**:

- `width > 0` and `height > 0`
- `depth` is valid (1, 8, 16, 24, 32 bits per pixel)
- Display subsystem available on platform

**Postconditions**:

- Display window created and visible
- Display region memory allocated and mapped
- Event handling enabled
- Color map initialized (if applicable)

**Semantics**:

```pseudocode
function InitializeDisplay(width, height, depth, options):
    // Allocate display region memory
    display_region = AllocateDisplayRegion(width, height, depth)

    // Create display window
    window = CreateWindow(width, height, depth)

    // Initialize color map
    if depth <= 8:
        InitializeColorMap()

    // Set up event handling
    EnableEventHandling(window)

    // Store display interface
    dsp_interface.display_id = window
    dsp_interface.Display.width = width
    dsp_interface.Display.height = height
    dsp_interface.DisplayRegion68k = display_region

    return dsp_interface
```

### Graphics Rendering

**Operation**: `render_region(source_x, source_y, width, height, dest_x, dest_y, operation)`

**Preconditions**:

- Display initialized
- Source and destination regions within display bounds
- `operation` is valid (COPY, XOR, AND, OR, etc.)

**Postconditions**:

- Specified region rendered to display
- Display updated (may be buffered)

**Semantics**:

```pseudocode
function RenderRegion(source_x, source_y, width, height, dest_x, dest_y, operation):
    // Get source and destination addresses
    source_addr = DisplayRegion68k + CalculateOffset(source_x, source_y)
    dest_addr = DisplayRegion68k + CalculateOffset(dest_x, dest_y)

    // Perform BitBLT operation
    BitBLT(source_addr, dest_addr, width, height, operation)

    // Flush to screen
    FlushDisplayRegion(dest_x, dest_y, width, height)
```

### Window Management

**Operation**: `set_window_title(title)`

**Semantics**: Set window title text

**Operation**: `resize_window(width, height)`

**Semantics**: Resize display window (may require display region reallocation)

**Operation**: `set_cursor(cursor_bitmap, hotspot_x, hotspot_y)`

**Semantics**: Set cursor bitmap and hotspot position

## Display Region Protocol

### Memory Layout

The display region is a memory-mapped area representing screen contents:

```pseudocode
struct DisplayRegion:
    base_address: LispPTR      // Base address in Lisp memory
    width: uint                // Width in pixels
    height: uint               // Height in pixels
    depth: uint                // Bits per pixel
    bytes_per_line: uint       // Bytes per scanline
    format: PixelFormat        // Pixel format (monochrome, indexed, RGB)
```

### Pixel Formats

**Monochrome (1 bpp)**:

- 1 bit per pixel
- 0 = background, 1 = foreground
- Packed into words (16 pixels per DLword)

**Indexed Color (8 bpp)**:

- 8 bits per pixel
- Index into color map
- 1 byte per pixel

**True Color (16/24/32 bpp)**:

- Direct RGB values
- Format: RGB565, RGB888, ARGB8888

### Update Mechanism

```pseudocode
function UpdateDisplay():
    // VM writes to DisplayRegion68k
    // BitBLT operations copy regions
    // Flush operations update screen
    FlushDisplayRegion(x, y, width, height)
```

## Event Protocol

### Event Types

**Keyboard Events**:

- KEY_PRESS: Key pressed
- KEY_RELEASE: Key released

**Mouse Events**:

- BUTTON_PRESS: Mouse button pressed
- BUTTON_RELEASE: Mouse button released
- MOTION: Mouse moved

**Window Events**:

- EXPOSE: Window exposed (needs redraw)
- RESIZE: Window resized
- FOCUS_IN: Window gained focus
- FOCUS_OUT: Window lost focus

### Event Format

```pseudocode
struct DisplayEvent:
    type: EventType           // Event type
    timestamp: uint           // Event timestamp
    data: EventData           // Event-specific data

struct KeyboardEvent:
    type: KEY_PRESS | KEY_RELEASE
    keycode: uint             // Lisp keycode (translated)
    modifiers: bitmask        // Shift, Control, Meta flags
    timestamp: uint

struct MouseEvent:
    type: BUTTON_PRESS | BUTTON_RELEASE | MOTION
    button: uint              // Button number (1-3) or 0 for motion
    x: int                     // X coordinate
    y: int                     // Y coordinate
    modifiers: bitmask        // Modifier keys
    timestamp: uint
```

### Event Polling

```pseudocode
function PollEvents() -> Event[]:
    events = []
    while HasPendingEvents():
        event = GetNextEvent()
        if event.type == KEYBOARD_EVENT:
            event = TranslateKeyEvent(event)  // Translate OS keycode to Lisp keycode
        events.append(event)
    return events
```

## Required Operations

### Display Operations

- **Initialize**: Create display window
- **Destroy**: Cleanup display resources
- **Flush**: Update screen from display region
- **Lock/Unlock**: Prevent concurrent access

### Graphics Operations

- **BitBLT**: Bit-block transfer
- **Line Drawing**: Draw lines
- **Fill**: Fill regions
- **Copy**: Copy regions

### Event Operations

- **Poll Events**: Retrieve pending events
- **Translate Keycodes**: Convert OS keycodes to Lisp keycodes
- **Enable/Disable Events**: Control event delivery

## Platform Abstraction

### Required Behaviors (Must Match)

- **Keycode Translation**: OS keycodes → Lisp keycodes
- **Display Region Mapping**: Memory-mapped display buffer
- **Graphics Operation Semantics**: BitBLT operations must match exactly
- **Event Coordinate System**: Origin at top-left, Y increases downward

### Implementation Choices (May Differ)

- **Graphics Library**: X11, SDL, DirectX, etc.
- **Window Decoration**: Title bar, borders (as long as content area correct)
- **Cursor Appearance**: Visual appearance (hotspot must be correct)
- **Event Delivery**: Polling vs callbacks (as long as events available)
- **Color Map Management**: Platform-specific color handling

## Error Handling

### Invalid Parameters

```pseudocode
function ValidateParameters(...):
    if width <= 0 or height <= 0:
        return Error("Invalid dimensions")
    if operation not in valid_operations:
        return Error("Invalid operation")
    return OK
```

### Display Unavailable

```pseudocode
function InitializeDisplay(...):
    if not DisplayAvailable():
        return Error("Display subsystem unavailable")
    // ... continue initialization
```

## Related Documentation

- [Graphics Operations](graphics-operations.md) - BitBLT and rendering
- [Event Protocols](event-protocols.md) - Event handling details
- [Platform Abstraction](../platform-abstraction/) - Required vs optional behaviors
