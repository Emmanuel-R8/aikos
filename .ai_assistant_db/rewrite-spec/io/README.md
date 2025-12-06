# I/O Subsystem Specification

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [Display](../display/) | [Platform Abstraction](../platform-abstraction/)

Complete specification of I/O subsystems, including keyboard, mouse, file system, and network protocols.

## Overview

The I/O subsystem handles all input/output operations, abstracting platform-specific I/O APIs behind consistent interfaces.

## Documentation Structure

- **[Keyboard Protocol](keyboard-protocol.md)** - Key event translation and handling
- **[Mouse Protocol](mouse-protocol.md)** - Mouse event handling
- **[File System](file-system.md)** - File I/O and pathname handling
- **[Network Protocol](network-protocol.md)** - Ethernet and Internet protocols

## Key Concepts

### I/O Interfaces

Platform-specific I/O is abstracted through interfaces:

- **Keyboard Interface**: Key event translation
- **Mouse Interface**: Mouse event handling
- **File Interface**: File system operations
- **Network Interface**: Network communication

### Event Translation

Platform events are translated to Lisp formats:

- **Keycodes**: OS keycodes → Lisp keycodes
- **Mouse Events**: Platform mouse events → Lisp mouse events
- **File Paths**: Platform paths → Lisp pathnames

### Platform Abstraction

I/O operations abstract platform differences:

- **File Systems**: Different path separators, case sensitivity
- **Network**: Different socket APIs
- **Serial Ports**: Different serial port interfaces

## Related Documentation

- [Display](../display/) - Display events
- [Platform Abstraction](../platform-abstraction/) - Required vs optional behaviors
- [VM Core](../vm-core/) - I/O interrupt handling
