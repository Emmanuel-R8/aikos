= Display Subsystem Specification


Complete specification of the display subsystem, including interface abstraction, graphics operations, and event handling.

== Overview

The display subsystem provides graphics output and input event handling. It abstracts platform-specific graphics libraries (X11, SDL) behind a common interface.

== Documentation Structure
- *Interface Abstraction* - Display interface contract- *Graphics Operations* - BitBLT and rendering semantics- *Event Protocols - Keyboard/mouse event handling

== Key Concepts

=== Display Interface

The display interface (`DspInterface`) abstracts platform-specific display operations:
- *Initialization*: Open display connection
- *Window Management*: Create/manage windows
- *Graphics Operations*: BitBLT, drawing primitives
- *Event Handling*: Keyboard, mouse, window events
- *Color Management*: Color map operations

=== Display Region

Memory-mapped display buffer:
- *Location*: `DisplayRegion68k` in Lisp memory
- *Format*: Pixel data (monochrome, color, true color)
- *Update*: BitBLT operations copy to screen

=== Backend Abstraction

Multiple backends supported:
- *X11*: X Window System (primary)
- *SDL*: Simple DirectMedia Layer (alternative)
- *Platform-specific*: VGA, VESA* (DOS)

== Related Documentation

- I/O Systems - Keyboard/mouse input
- Platform Abstraction - Required vs optional behaviors
- VM Core* - Display interrupt handling
