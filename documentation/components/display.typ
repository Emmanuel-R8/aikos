= Display Component


The Display component provides graphics output abstraction across X11 and SDL subsystems.

*Related Components*: - VM Core - Uses display for output
- I/O Systems - Keyboard and mouse events from display

== Overview

#figure(
  caption: [Diagram],
  [Diagram: See original documentation for visual representation.],
)

  )
)

Maiko supports multiple display backends:
- *X11*: X Window System integration (primary) (see X11 Implementation)
- *SDL*: Simple DirectMedia Layer (alternative) (see SDL Implementation)

Both backends provide the same interface to the VM core through the display interface abstraction (see Display Interface Abstraction).

== Key Files

=== X11 Implementation - `src/xinit.c`: X11 initialization and window management
  - `Open_Display()`: Open X11 display connection
  - `X_init()`: Initialize X11 subsystem
  - `init_Xevent()`: Initialize X event handling
  - `lisp_Xexit()`: Cleanup X11 resources
  - `Xevent_before_raid()`: Disable events before URAID
  - `Xevent_after_raid()`: Re-enable events after URAID - `src/xlspwin.c`: Lisp window creation
  - `Create_LispWindow()`: Create main Lisp window
  - Window configuration - `src/xbbt.c`: X11 BitBLT operations
  - `clipping_Xbitblt()`: Bit-block transfer with clipping
  - Graphics rendering - `src/xcursor.c`: Cursor management
  - Cursor creation and manipulation
  - Hotspot handling - `src/xwinman.c`: Window management
  - Window operations
  - Event handling - `src/xscroll.c`: Scrollbar management
  - Scrollbar creation
  - Scrollbar events - `src/xmkicon.c`: Icon management
  - Icon creation
  - Icon display - `src/xrdopt.c`: X resource options
  - X resource parsing
  - Configuration

=== SDL Implementation - `src/sdl.c`: SDL initialization and rendering
  - `init_SDL()`: Initialize SDL subsystem
  - SDL window creation
  - SDL rendering (texture-based or surface-based)
  - Event handling
  - Key mapping

=== Display Interface Abstraction - `src/dspif.c`: Display interface abstraction
  - Generic display interface
  - Backend abstraction - `src/initdsp.c`: Display initialization
  - Display subsystem selection
  - Initialization coordination - `src/dspsubrs.c`: Display subroutines
  - Lisp-callable display functions

=== BitBLT Operations
- `src/bitblt.c`: Generic BitBLT implementation
- `src/blt.c`: Bit-block transfer core
- `src/xbbt.c`: X11-specific BitBLT
- `src/lineblt8.c`: Line drawing operations
- `src/draw.c`: Drawing primitives
- `src/picture.c`: Picture rendering

=== Color Management
- `src/llcolor.c`: Low-level color operations
- `src/rawcolor.c`: Raw color handling
- `src/truecolor.c`: True color support

== Display Architecture

=== Display Interface Structure

[`typedef struct dspinterface {`]
[`    Display pointerdisplay_id;        // X11 display (X11`] or SDL window (SDL)
    Window LispWindow;           // Main Lisp window
    Window DisplayWindow;        // Display area
    // ... scrollbars, gravity windows ...
    unsigned EnableEventMask;     // Event mask
    // ... other fields ...
} DspInterface;)

=== Display Region

The display region (`DisplayRegion68k`) is a memory-mapped area that represents the screen:
- *Memory Layout*: Pixel data stored in Lisp memory
- *Update Mechanism*: BitBLT operations copy from memory to screen
- *Format*: Depends on display mode (monochrome, color, true color)

== Initialization Sequence

1. *Display Selection*: Choose X11 or SDL based on build configuration
2. *Display Connection*: Open display (XOpenDisplay or SDL_Init)
3. *Window Creation*: Create main Lisp window
4. *Event Setup*: Configure event handling
5. *Color Setup*: Initialize color map
6. *BitBLT Setup*: Initialize graphics operations

== BitBLT Operations

BitBLT (Bit-Block Transfer) is the primary graphics operation:

=== BitBLT Parameters
- *Source*: Source rectangle in display memory
- *Destination*: Destination rectangle on screen
- *Operation*: Logical operation (copy, XOR, etc.)
- *Clipping*: Clip to window boundaries

=== BitBLT Implementation

1. *Generic BitBLT* (`bitblt.c`): Platform-independent implementation
2. *X11 BitBLT* (`xbbt.c`): X11-optimized using XPutImage
3. *SDL BitBLT* (`sdl.c`): SDL-optimized using texture rendering

== Event Handling

=== X11 Events
- *Key Events*: Keyboard input
- *Button Events*: Mouse button presses
- *Motion Events*: Mouse movement
- *Expose Events*: Window redraw requests
- *Configure Events*: Window resize/move

=== SDL Events
- *SDL_KeyboardEvent*: Keyboard input
- *SDL_MouseButtonEvent*: Mouse button presses
- *SDL_MouseMotionEvent*: Mouse movement
- *SDL_WindowEvent*: Window events

=== Event Processing

Events are processed in the main loop:

1. Check for pending events
2. Translate to Lisp keycodes
3. Queue for Lisp processing
4. Trigger interrupts if needed

== Window Management

=== Window Structure
- *LispWindow*: Outer window (title bar, borders)
- *DisplayWindow*: Inner window (actual display area)
- *ScrollBars*: Horizontal and vertical scrollbars
- *Gravity Windows*: Corner resize handles

=== Window Operations
- *Create*: Create new window
- *Resize*: Change window size
- *Move*: Change window position
- *Iconify*: Minimize window
- *Destroy*: Close window

== Cursor Management

=== Cursor Types
- *Text Cursor*: Text editing cursor
- *Mouse Cursor*: Mouse
- *Custom Cursors*: Application-defined cursors

=== Cursor Operations
- *Create*: Create cursor from bitmap
- *Set*: Set active cursor
- *Hide/Show*: Toggle cursor visibility
- *Hotspot*: Set cursor hotspot (click point)

== Color Management

=== Color Models
- *Monochrome*: 1-bit (black/white)
- *Indexed Color*: Color palette (8-bit, 16-bit)
- *True Color*: Direct color (24-bit, 32-bit)

=== Color Operations
- *Allocate Color*: Allocate color in colormap
- *Set Foreground*: Set foreground color
- *Set Background*: Set background color
- *Color Translation*: Convert between color formats

== Display Modes

=== Resolution
- *Default*: 1024x768 (configurable)
- *Scalable*: Pixel scaling support
- *Fullscreen*: Optional fullscreen mode

=== Pixel Formats
- *1-bit*: Monochrome
- *8-bit*: Indexed color (256 colors)
- *16-bit*: High color (65536 colors)
- *24-bit*: True color (16M colors)
- *32-bit*: True color with alpha

== Threading and Locking

=== X11 Locking
- *XLOCK*: Lock X11 operations
- *XUNLOCK*: Unlock X11 operations
- *XLocked*: Lock state flag
- *XNeedSignal*: Signal pending flag

Prevents signal handlers from interfering with X11 operations.

== Performance Considerations

1. *BitBLT Optimization*: Platform-specific optimizations
2. *Double Buffering*: Reduce flicker
3. *Clipping*: Efficient rectangle clipping
4. *Event Batching*: Batch multiple events

== Related Components

- I/O Systems - Keyboard and mouse input
- VM Core - Uses display for output

== See Also

- `maiko/inc/dspifdefs.h` - Display interface definitions
- `maiko/inc/xdefs.h` - X11 definitions
- `maiko/inc/sdldefs.h` - SDL definitions
- `maiko/inc/display.h` - Display constants
