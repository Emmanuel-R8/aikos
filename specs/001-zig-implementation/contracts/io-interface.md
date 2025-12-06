# I/O Interface Contract

**Version**: 1.0
**Date**: 2025-12-04
**Type**: Interface Specification
**Language**: Zig

## Overview

This contract specifies the interfaces for I/O subsystems (keyboard, mouse, file system) in the Zig implementation.

## Keyboard Interface

```zig
// Translate OS keycode to Lisp keycode
pub fn translateKeycode(os_keycode: u32, modifiers: u16) u16;

// Enqueue key event
pub fn enqueueKeyEvent(queue: *KeyEventQueue, event: KeyboardEvent) !void;

// Dequeue key event
pub fn dequeueKeyEvent(queue: *KeyEventQueue) ?KeyboardEvent;
```

**Preconditions**: Valid OS keycode

**Postconditions**: Keycode translated, event queued/dequeued

**Semantics**: Translate OS keycodes to Lisp keycodes and queue events.

## Mouse Interface

```zig
// Translate mouse event
pub fn translateMouseEvent(os_event: *sdl.SDL_Event) MouseEvent;

// Update mouse position
pub fn updateMousePosition(io: *IO, x: i32, y: i32) void;

// Get mouse position
pub fn getMousePosition(io: *IO) struct { x: i32, y: i32 };
```

**Preconditions**: Valid OS mouse event

**Postconditions**: Event translated, position updated

**Semantics**: Translate OS mouse events to Lisp mouse event format.

## File System Interface

```zig
// Translate Lisp pathname to platform path
pub fn lispToPlatformPathname(lisp_path: []const u8, versionp: bool) ![]u8;

// Translate platform path to Lisp pathname
pub fn platformToLispPathname(platform_path: []const u8, dirp: bool) ![]u8;

// Open file
pub fn openFile(pathname: []const u8, mode: FileMode) !FileHandle;

// Read file
pub fn readFile(handle: FileHandle, buffer: []u8) !usize;

// Write file
pub fn writeFile(handle: FileHandle, buffer: []const u8) !usize;

// Close file
pub fn closeFile(handle: FileHandle) void;
```

**Preconditions**: Valid pathname, file exists (for read)

**Postconditions**: File opened/read/written/closed

**Semantics**: Translate Lisp pathnames to native paths and perform file operations.

## Error Types

```zig
pub const IOError = error{
    InvalidPathname,
    FileNotFound,
    PermissionDenied,
    ReadFailed,
    WriteFailed,
    KeycodeTranslationFailed,
};
```

## Related Documentation

- `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` - Keyboard protocol
- `.ai_assistant_db/rewrite-spec/io/mouse-protocol.md` - Mouse protocol
- `.ai_assistant_db/rewrite-spec/io/file-system.md` - File system interface
