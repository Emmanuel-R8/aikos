 = Source Code Mapping - Display & I/O
 
 *Navigation*: Source Code Mapping | Main README
 
 Mapping of Display and I/O subsystem source code files to documentation sections.
 
 == Display Subsystem
 
 === Display Interface
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/dspif.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: `make_dsp_instance()`, display interface abstraction | | |
 // | `maiko/src/initdsp.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Display initialization | | |
 // | `maiko/src/dspsubrs.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Display subroutines | | |
 // 
 
 === X11 Implementation
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/xinit.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | `documentation/specifications/platform-abstraction/implementation-choices.typ` | ✅ Complete |
 // | | **Functions**: `X_init()`, `init_Xevent()`, `lisp_Xexit()` | | |
 // | `maiko/src/xlspwin.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: `Create_LispWindow()` | | |
 // | `maiko/src/xbbt.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: X11 BitBLT operations | | |
 // | `maiko/src/xcursor.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Cursor management | | |
 // | `maiko/src/xwinman.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Window management | | |
 // | `maiko/src/xscroll.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Scrollbar management | | |
 // | `maiko/src/xmkicon.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Icon management | | |
 // | `maiko/src/xrdopt.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: X resource options | | |
 // 
 
 === SDL Implementation
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/sdl.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | `documentation/specifications/platform-abstraction/implementation-choices.typ` | ✅ Complete |
 // | | **Functions**: SDL initialization, rendering, event handling | | |
 // 
 
 === Graphics Operations
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/bitblt.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: `N_OP_pilotbitblt()` - Main BitBLT opcode handler | | |
 // | `maiko/src/blt.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: `N_OP_blt()` - BLT opcode | | |
 // | `maiko/src/bbtsub.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: `bitbltsub()`, `bitblt_bitmap()`, BitBLT operations | | |
 // | `maiko/src/lineblt8.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: Line drawing operations | | |
 // | `maiko/src/draw.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: Drawing primitives | | |
 // | `maiko/src/picture.c` | `documentation/specifications/display/graphics-operations.typ` | ✅ Complete |
 // | | **Functions**: Picture rendering | | |
 // | `maiko/src/asmbbt.c` | `documentation/specifications/display/graphics-operations.typ` | ⚠️ Assembly - platform-specific |
 // | `maiko/src/bbt68k.s`, `src/bbtSPARC.s` | `documentation/specifications/display/graphics-operations.typ` | ⚠️ Assembly - platform-specific |
 // 
 
 === Color Management
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/llcolor.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Low-level color operations | | |
 // | `maiko/src/rawcolor.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: Raw color handling | | |
 // | `maiko/src/truecolor.c` | `documentation/specifications/display/interface-abstraction.typ` | ✅ Complete |
 // | | **Functions**: True color support | | |
 // 
 
 == I/O Subsystem
 
 === Keyboard
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/kbdif.c` | `documentation/specifications/io/keyboard-protocol.typ` | ✅ Complete |
 // | | **Functions**: Keyboard interface abstraction | | |
 // | `maiko/src/keyevent.c` | `documentation/specifications/io/keyboard-protocol.typ` | ✅ Complete |
 // | | `documentation/specifications/display/event-protocols.typ` | ✅ Complete |
 // | | **Functions**: `process_io_events()`, keyboard event processing | | |
 // | `maiko/src/kbdsubrs.c` | `documentation/specifications/io/keyboard-protocol.typ` | ✅ Complete |
 // | | **Functions**: Keyboard subroutines | | |
 // | `maiko/src/findkey.c` | `documentation/specifications/io/keyboard-protocol.typ` | ✅ Complete |
 // | | **Functions**: Keycode lookup | | |
 // | `maiko/src/initkbd.c` | `documentation/specifications/io/keyboard-protocol.typ` | ✅ Complete |
 // | | **Functions**: Keyboard initialization | | |
 // | `maiko/src/doskbd.c` | `documentation/specifications/io/keyboard-protocol.typ` | ⚠️ DOS-specific |
 // | | `documentation/specifications/platform-abstraction/implementation-choices.typ` | ✅ Complete |
 // 
 
 === Mouse
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/mouseif.c` | `documentation/specifications/io/mouse-protocol.typ` | ✅ Complete |
 // | | **Functions**: Mouse interface abstraction | | |
 // | `maiko/src/mousesubrs.c` | `documentation/specifications/io/mouse-protocol.typ` | ✅ Complete |
 // | | **Functions**: Mouse subroutines | | |
 // 
 
 === File System
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/fileio.c` | `documentation/specifications/io/file-system.typ` | ✅ Complete |
 // | | **Functions**: File I/O operations | | |
 // | `maiko/src/dosfile.c` | `documentation/specifications/io/file-system.typ` | ⚠️ DOS-specific |
 // | | `documentation/specifications/platform-abstraction/implementation-choices.typ` | ✅ Complete |
 // 
 
 === Network
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/ether.c` | `documentation/specifications/io/network-protocol.typ` | ✅ Complete |
 // | | **Functions**: Ethernet operations | | |
 // | `maiko/src/inet.c` | `documentation/specifications/io/network-protocol.typ` | ✅ Complete |
 // | | **Functions**: Internet operations | | |
 // 
 
 === Serial Communication
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/rs232.c` | `documentation/specifications/io/` | ⚠️ **GAP**: Serial communication not documented |
 // | | **Functions**: RS-232 operations | | |
 // | `maiko/src/rawrs232.c` | `documentation/specifications/io/` | ⚠️ **GAP**: Serial communication not documented |
 // | | **Functions**: Raw RS-232 operations | | |
 // 
 
 === Unix IPC
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/unixcomm.c` | `documentation/specifications/io/` | ⚠️ **GAP**: Not explicitly documented |
 // | | **Functions**: Unix IPC communication | | |
 // | `maiko/src/unixfork.c` | `documentation/specifications/io/` | ⚠️ **GAP**: Not explicitly documented |
 // | | **Functions**: Unix fork operations | | |
 // 
 
 == Related Documentation
 
 - Source Code Mapping - Complete mapping index
 - Display Specifications - Display subsystem specifications
 - I/O Specifications - I/O subsystem specifications
