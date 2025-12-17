= Source Code Mapping - Display & I/O


Mapping of Display and I/O subsystem source code files to documentation sections.

== Display Subsystem

=== Display Interface

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/dspif.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: `make_dsp_instance()`, display interface abstraction | | |
// | `maiko/src/initdsp.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Display initialization | | |
// | `maiko/src/dspsubrs.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Display subroutines | | |
// 

=== X11 Implementation

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/xinit.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | `.ai_assistant_db/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |
// | | **Functions*: `X_init()`, `init_Xevent()`, `lisp_Xexit()` | | |
// | `maiko/src/xlspwin.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: `Create_LispWindow()` | | |
// | `maiko/src/xbbt.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: X11 BitBLT operations | | |
// | `maiko/src/xcursor.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Cursor management | | |
// | `maiko/src/xwinman.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Window management | | |
// | `maiko/src/xscroll.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Scrollbar management | | |
// | `maiko/src/xmkicon.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Icon management | | |
// | `maiko/src/xrdopt.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: X resource options | | |
// 

=== SDL Implementation

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/sdl.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | `.ai_assistant_db/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |
// | | **Functions*: SDL initialization, rendering, event handling | | |
// 

=== Graphics Operations

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/bitblt.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: `N_OP_pilotbitblt()` - Main BitBLT opcode handler | | |
// | `maiko/src/blt.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: `N_OP_blt()` - BLT opcode | | |
// | `maiko/src/bbtsub.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: `bitbltsub()`, `bitblt_bitmap()`, BitBLT operations | | |
// | `maiko/src/lineblt8.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: Line drawing operations | | |
// | `maiko/src/draw.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: Drawing primitives | | |
// | `maiko/src/picture.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
// | | **Functions*: Picture rendering | | |
// | `maiko/src/asmbbt.c` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ⚠️ Assembly - platform-specific |
// | `maiko/src/bbt68k.s`, `src/bbtSPARC.s` | `.ai_assistant_db/rewrite-spec/display/graphics-operations.md` | ⚠️ Assembly - platform-specific |
// 

=== Color Management

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/llcolor.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Low-level color operations | | |
// | `maiko/src/rawcolor.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: Raw color handling | | |
// | `maiko/src/truecolor.c` | `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
// | | **Functions*: True color support | | |
// 

== I/O Subsystem

=== Keyboard

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/kbdif.c` | `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
// | | **Functions*: Keyboard interface abstraction | | |
// | `maiko/src/keyevent.c` | `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
// | | `.ai_assistant_db/rewrite-spec/display/event-protocols.md` | ✅ Complete |
// | | **Functions*: `process_io_events()`, keyboard event processing | | |
// | `maiko/src/kbdsubrs.c` | `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
// | | **Functions*: Keyboard subroutines | | |
// | `maiko/src/findkey.c` | `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
// | | **Functions*: Keycode lookup | | |
// | `maiko/src/initkbd.c` | `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
// | | **Functions*: Keyboard initialization | | |
// | `maiko/src/doskbd.c` | `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md` | ⚠️ DOS-specific |
// | | `.ai_assistant_db/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |
// 

=== Mouse

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/mouseif.c` | `.ai_assistant_db/rewrite-spec/io/mouse-protocol.md` | ✅ Complete |
// | | **Functions*: Mouse interface abstraction | | |
// | `maiko/src/mousesubrs.c` | `.ai_assistant_db/rewrite-spec/io/mouse-protocol.md` | ✅ Complete |
// | | **Functions*: Mouse subroutines | | |
// 

=== File System

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/fileio.c` | `.ai_assistant_db/rewrite-spec/io/file-system.md` | ✅ Complete |
// | | **Functions*: File I/O operations | | |
// | `maiko/src/dosfile.c` | `.ai_assistant_db/rewrite-spec/io/file-system.md` | ⚠️ DOS-specific |
// | | `.ai_assistant_db/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |
// 

=== Network

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/ether.c` | `.ai_assistant_db/rewrite-spec/io/network-protocol.md` | ✅ Complete |
// | | **Functions*: Ethernet operations | | |
// | `maiko/src/inet.c` | `.ai_assistant_db/rewrite-spec/io/network-protocol.md` | ✅ Complete |
// | | **Functions*: Internet operations | | |
// 

=== Serial Communication

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/rs232.c` | `.ai_assistant_db/rewrite-spec/io/` | ⚠️ **GAP*: Serial communication not documented |
// | | **Functions*: RS-232 operations | | |
// | `maiko/src/rawrs232.c` | `.ai_assistant_db/rewrite-spec/io/` | ⚠️ **GAP*: Serial communication not documented |
// | | **Functions*: Raw RS-232 operations | | |
// 

=== Unix IPC

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/unixcomm.c` | `.ai_assistant_db/rewrite-spec/io/` | ⚠️ **GAP*: Not explicitly documented |
// | | **Functions*: Unix IPC communication | | |
// | `maiko/src/unixfork.c` | `.ai_assistant_db/rewrite-spec/io/` | ⚠️ **GAP*: Not explicitly documented |
// | | **Functions*: Unix fork operations | | |
// 

== Related Documentation

- Source Code Mapping - Complete mapping index
- Display Specifications - Display subsystem specifications
- I/O Specifications - I/O subsystem specifications
