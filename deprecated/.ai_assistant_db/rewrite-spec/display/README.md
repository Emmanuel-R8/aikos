---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Display Subsystem Specification

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [I/O](../io/) | [Platform Abstraction](../platform-abstraction/)

Complete specification of the display subsystem, including interface abstraction, graphics operations, and event handling.

## Overview

The display subsystem provides graphics output and input event handling. It abstracts platform-specific graphics libraries (X11, SDL) behind a common interface.

## Documentation Structure

- **[Interface Abstraction](interface-abstraction.md)** - Display interface contract
- **[Graphics Operations](graphics-operations.md)** - BitBLT and rendering semantics
- **[Event Protocols](event-protocols.md)** - Keyboard/mouse event handling

## Key Concepts

### Display Interface

The display interface (`DspInterface`) abstracts platform-specific display operations:

- **Initialization**: Open display connection
- **Window Management**: Create/manage windows
- **Graphics Operations**: BitBLT, drawing primitives
- **Event Handling**: Keyboard, mouse, window events
- **Color Management**: Color map operations

### Display Region

Memory-mapped display buffer:

- **Location**: `DisplayRegion68k` in Lisp memory
- **Format**: Pixel data (monochrome, color, true color)
- **Update**: BitBLT operations copy to screen

### Backend Abstraction

Multiple backends supported:

- **X11**: X Window System (primary)
- **SDL**: Simple DirectMedia Layer (alternative)
- **Platform-specific**: VGA, VESA (DOS)

## Related Documentation

- [I/O Systems](../io/) - Keyboard/mouse input
- [Platform Abstraction](../platform-abstraction/) - Required vs optional behaviors
- [VM Core](../vm-core/) - Display interrupt handling
