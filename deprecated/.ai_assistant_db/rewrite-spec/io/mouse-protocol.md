---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Mouse Protocol Specification

**Navigation**: [README](README.md) | [Keyboard Protocol](keyboard-protocol.md) | [File System](file-system.md)

Complete specification of mouse event handling protocol.

## Overview

The mouse protocol handles mouse button presses, releases, and motion events, translating platform-specific mouse events to Lisp mouse events.

## Mouse Event Structure

### Event Format

```pseudocode
struct MouseEvent:
    type: BUTTON_PRESS | BUTTON_RELEASE | MOTION
    button: uint            // Button number (1, 2, 3) or 0 for motion
    x: int                  // X coordinate (pixels)
    y: int                  // Y coordinate (pixels)
    modifiers: bitmask      // Modifier keys
    timestamp: uint         // Event timestamp
```

### Coordinate System

- **Origin**: Top-left corner (0, 0)
- **X-axis**: Increases rightward
- **Y-axis**: Increases downward
- **Units**: Pixels
- **Range**: 0 to DisplayWidth-1, 0 to DisplayHeight-1

## Button Mapping

### Two-Button Mouse

```pseudocode
function MapTwoButtonMouse(os_button):
    switch os_button:
        case LEFT_BUTTON:
            return 1  // Left button
        case RIGHT_BUTTON:
            return 2  // Right button
        case MIDDLE_BUTTON:
            // Simulate middle via modifier + right
            return 2  // Right button (with modifier)
```

### Three-Button Mouse

```pseudocode
function MapThreeButtonMouse(os_button):
    switch os_button:
        case LEFT_BUTTON:
            return 1  // Left button
        case MIDDLE_BUTTON:
            return 2  // Middle button
        case RIGHT_BUTTON:
            return 3  // Right button
```

## Mouse Event Processing

### Process Mouse Event

```pseudocode
function ProcessMouseEvent(os_event):
    // Translate button
    lisp_button = MapMouseButton(os_event.button)

    // Translate coordinates
    lisp_x = os_event.x
    lisp_y = os_event.y

    // Create Lisp event
    lisp_event = CreateMouseEvent(
        type: os_event.type,
        button: lisp_button,
        x: lisp_x,
        y: lisp_y,
        modifiers: os_event.modifiers,
        timestamp: GetTimestamp()
    )

    // Update mouse position
    UpdateMousePosition(lisp_x, lisp_y)

    // Queue event
    QueueMouseEvent(lisp_event)

    // Set interrupt flag
    SetInterruptFlag(IOInterrupt)
```

## Mouse Position Tracking

### Update Mouse Position

```pseudocode
function UpdateMousePosition(x, y):
    // Store in IOPage
    IOPage->dlmousex = x
    IOPage->dlmousey = y

    // Update last user action
    UpdateLastUserAction()
```

### Get Mouse Position

```pseudocode
function GetMousePosition():
    return (IOPage->dlmousex, IOPage->dlmousey)
```

## Button State Tracking

### Button State

```pseudocode
struct MouseButtonState:
    button1_pressed: boolean
    button2_pressed: boolean
    button3_pressed: boolean
    last_press_time: uint
    chord_ticks: uint
```

### Button Press/Release

```pseudocode
function HandleButtonPress(button):
    MouseButtonState["button" + button + "_pressed"] = true
    MouseButtonState.last_press_time = GetTimestamp()

    // Check for button chords
    CheckButtonChords()

function HandleButtonRelease(button):
    MouseButtonState["button" + button + "_pressed"] = false
```

## Mouse Motion

### Motion Event Processing

```pseudocode
function ProcessMotionEvent(x, y):
    // Update position
    UpdateMousePosition(x, y)

    // Create motion event
    event = CreateMouseEvent(
        type: MOTION,
        button: 0,
        x: x,
        y: y,
        modifiers: GetModifiers(),
        timestamp: GetTimestamp()
    )

    // Queue event
    QueueMouseEvent(event)
```

## Cursor Management

### Cursor Position

Cursor position tracked separately from mouse position:

- **Mouse Position**: Physical mouse position
- **Cursor Position**: Display cursor position (may differ)

### Cursor Update

```pseudocode
function UpdateCursorPosition(x, y):
    // Update cursor display position
    CursorX = x
    CursorY = y

    // Update display if needed
    if CursorVisible:
        RedrawCursor()
```

## Platform-Specific Handling

### X11 Mouse Events

```pseudocode
function ProcessX11MouseEvent(x_event):
    switch x_event.type:
        case ButtonPress:
            button = MapX11Button(x_event.xbutton.button)
            ProcessMouseEvent(BUTTON_PRESS, button, x_event.xbutton.x, x_event.xbutton.y)
        case ButtonRelease:
            button = MapX11Button(x_event.xbutton.button)
            ProcessMouseEvent(BUTTON_RELEASE, button, x_event.xbutton.x, x_event.xbutton.y)
        case MotionNotify:
            ProcessMotionEvent(x_event.xmotion.x, x_event.xmotion.y)
```

### SDL Mouse Events

```pseudocode
function ProcessSDLMouseEvent(sdl_event):
    switch sdl_event.type:
        case SDL_MOUSEBUTTONDOWN:
            button = MapSDLButton(sdl_event.button.button)
            ProcessMouseEvent(BUTTON_PRESS, button, sdl_event.button.x, sdl_event.button.y)
        case SDL_MOUSEBUTTONUP:
            button = MapSDLButton(sdl_event.button.button)
            ProcessMouseEvent(BUTTON_RELEASE, button, sdl_event.button.x, sdl_event.button.y)
        case SDL_MOUSEMOTION:
            ProcessMotionEvent(sdl_event.motion.x, sdl_event.motion.y)
```

## Related Documentation

- [Keyboard Protocol](keyboard-protocol.md) - Keyboard event handling
- [Event Protocols](../display/event-protocols.md) - General event handling
- [Display](../display/) - Display coordinate system
