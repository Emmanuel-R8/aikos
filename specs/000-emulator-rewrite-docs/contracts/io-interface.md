# I/O Interface Contract

**Version**: 1.0
**Date**: 2025-12-04
**Type**: Interface Specification

## Overview

This contract specifies the interfaces for keyboard, mouse, file system, and network I/O. Implementations must fulfill these contracts for compatibility.

## Keyboard Interface

### Key Event Translation

**Operation**: `translate_keycode(os_keycode, modifiers) -> lisp_keycode`

**Preconditions**: Valid OS keycode

**Postconditions**: Returns corresponding Lisp keycode

**Semantics**: Translate OS-specific keycode to Lisp keycode using keymap.

**Keymap Requirements**:

- Must map all standard keys (letters, numbers, symbols)
- Must handle modifier combinations (Shift, Control, Meta)
- Must map special keys (function keys, arrow keys, etc.)

### Key Event Queue

**Operation**: `enqueue_key_event(event)`

**Operation**: `dequeue_key_event() -> Event | null`

**Semantics**: Queue-based event delivery to VM interrupt handler.

## Mouse Interface

### Mouse Event Translation

**Operation**: `translate_mouse_event(os_event) -> MouseEvent`

**Semantics**: Translate OS mouse events to Lisp mouse event format.

### Mouse Event Format

```
MouseEvent {
    type: BUTTON_PRESS | BUTTON_RELEASE | MOTION
    button: integer
    x: integer
    y: integer
    modifiers: bitmask
}
```

## File System Interface

### Pathname Operations

**Operation**: `resolve_pathname(lisp_pathname) -> native_path`

**Operation**: `list_directory(path) -> DirectoryEntry[]`

**Operation**: `file_exists(path) -> boolean`

**Semantics**: Translate Lisp pathnames to native paths and perform file operations.

### File I/O Operations

**Operation**: `open_file(path, mode) -> FileHandle`

**Operation**: `read_file(handle, buffer, length) -> bytes_read`

**Operation**: `write_file(handle, buffer, length) -> bytes_written`

**Operation**: `close_file(handle)`

**Semantics**: Standard file I/O operations with Lisp pathname abstraction.

### Pathname Translation Rules

- Lisp pathnames use specific format (device, directory, name, type, version)
- Must translate to platform-specific path format
- Must handle case sensitivity differences
- Must preserve pathname components

## Network Interface

### Ethernet Operations

**Operation**: `open_ethernet_device(device_name) -> DeviceHandle`

**Operation**: `send_packet(handle, packet_data) -> success`

**Operation**: `receive_packet(handle) -> Packet | null`

**Semantics**: Low-level Ethernet packet operations.

### Network Protocol Support

- DLPI (Data Link Provider Interface)
- NIT (Network Interface Tap)
- NETHUB (Network hub emulation)

**Platform Abstraction**: Implementation may use platform-specific network APIs as long as protocol semantics match.

## Serial Communication Interface

### Serial Port Operations

**Operation**: `open_serial_port(device, baud_rate, config) -> PortHandle`

**Operation**: `configure_serial_port(handle, config)`

**Operation**: `read_serial(handle, buffer, length) -> bytes_read`

**Operation**: `write_serial(handle, buffer, length) -> bytes_written`

**Semantics**: RS-232 serial port communication.

### Serial Configuration

- Baud rate: 300, 600, 1200, 2400, 4800, 9600, 19200, 38400, etc.
- Data bits: 5, 6, 7, or 8
- Stop bits: 1 or 2
- Parity: None, Even, Odd
- Flow control: None, XON/XOFF, Hardware

## Platform Abstraction

### Required Behaviors (Must Match)

- Keycode translation to Lisp keycodes
- Pathname translation rules
- File I/O semantics
- Network packet format

### Implementation Choices (May Differ)

- Specific OS APIs used
- Event delivery mechanism (polling vs callbacks)
- File system case sensitivity handling (if platform allows)
- Network device access method

## Error Handling

- Invalid parameters: Return error code
- Resource unavailable: Return error, do not block
- I/O errors: Return error code with error details
- Timeout: Return timeout error for blocking operations

## Compatibility Requirements

- Must support standard file operations (read, write, directory listing)
- Must handle Lisp pathname format correctly
- Must support at least one network protocol
- Must translate keyboard events correctly for all standard keys
