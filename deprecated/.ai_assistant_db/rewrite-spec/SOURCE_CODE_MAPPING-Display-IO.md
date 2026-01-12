---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Source Code Mapping - Display & I/O

**Navigation**: [Source Code Mapping](SOURCE_CODE_MAPPING.md) | [Main README](../README.md)

Mapping of Display and I/O subsystem source code files to documentation sections.

## Display Subsystem

### Display Interface

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/dspif.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: `make_dsp_instance()`, display interface abstraction | | |
| `maiko/src/initdsp.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Display initialization | | |
| `maiko/src/dspsubrs.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Display subroutines | | |

### X11 Implementation

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/xinit.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | `documentation/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |
| | **Functions**: `X_init()`, `init_Xevent()`, `lisp_Xexit()` | | |
| `maiko/src/xlspwin.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: `Create_LispWindow()` | | |
| `maiko/src/xbbt.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: X11 BitBLT operations | | |
| `maiko/src/xcursor.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Cursor management | | |
| `maiko/src/xwinman.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Window management | | |
| `maiko/src/xscroll.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Scrollbar management | | |
| `maiko/src/xmkicon.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Icon management | | |
| `maiko/src/xrdopt.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: X resource options | | |

### SDL Implementation

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/sdl.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | `documentation/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |
| | **Functions**: SDL initialization, rendering, event handling | | |

### Graphics Operations

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/bitblt.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: `N_OP_pilotbitblt()` - Main BitBLT opcode handler | | |
| `maiko/src/blt.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: `N_OP_blt()` - BLT opcode | | |
| `maiko/src/bbtsub.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: `bitbltsub()`, `bitblt_bitmap()`, BitBLT operations | | |
| `maiko/src/lineblt8.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: Line drawing operations | | |
| `maiko/src/draw.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: Drawing primitives | | |
| `maiko/src/picture.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ✅ Complete |
| | **Functions**: Picture rendering | | |
| `maiko/src/asmbbt.c` | `documentation/rewrite-spec/display/graphics-operations.md` | ⚠️ Assembly - platform-specific |
| `maiko/src/bbt68k.s`, `src/bbtSPARC.s` | `documentation/rewrite-spec/display/graphics-operations.md` | ⚠️ Assembly - platform-specific |

### Color Management

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/llcolor.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Low-level color operations | | |
| `maiko/src/rawcolor.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: Raw color handling | | |
| `maiko/src/truecolor.c` | `documentation/rewrite-spec/display/interface-abstraction.md` | ✅ Complete |
| | **Functions**: True color support | | |

## I/O Subsystem

### Keyboard

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/kbdif.c` | `documentation/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
| | **Functions**: Keyboard interface abstraction | | |
| `maiko/src/keyevent.c` | `documentation/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
| | `documentation/rewrite-spec/display/event-protocols.md` | ✅ Complete |
| | **Functions**: `process_io_events()`, keyboard event processing | | |
| `maiko/src/kbdsubrs.c` | `documentation/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
| | **Functions**: Keyboard subroutines | | |
| `maiko/src/findkey.c` | `documentation/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
| | **Functions**: Keycode lookup | | |
| `maiko/src/initkbd.c` | `documentation/rewrite-spec/io/keyboard-protocol.md` | ✅ Complete |
| | **Functions**: Keyboard initialization | | |
| `maiko/src/doskbd.c` | `documentation/rewrite-spec/io/keyboard-protocol.md` | ⚠️ DOS-specific |
| | `documentation/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |

### Mouse

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/mouseif.c` | `documentation/rewrite-spec/io/mouse-protocol.md` | ✅ Complete |
| | **Functions**: Mouse interface abstraction | | |
| `maiko/src/mousesubrs.c` | `documentation/rewrite-spec/io/mouse-protocol.md` | ✅ Complete |
| | **Functions**: Mouse subroutines | | |

### File System

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/fileio.c` | `documentation/rewrite-spec/io/file-system.md` | ✅ Complete |
| | **Functions**: File I/O operations | | |
| `maiko/src/dosfile.c` | `documentation/rewrite-spec/io/file-system.md` | ⚠️ DOS-specific |
| | `documentation/rewrite-spec/platform-abstraction/implementation-choices.md` | ✅ Complete |

### Network

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/ether.c` | `documentation/rewrite-spec/io/network-protocol.md` | ✅ Complete |
| | **Functions**: Ethernet operations | | |
| `maiko/src/inet.c` | `documentation/rewrite-spec/io/network-protocol.md` | ✅ Complete |
| | **Functions**: Internet operations | | |

### Serial Communication

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/rs232.c` | `documentation/rewrite-spec/io/` | ⚠️ **GAP**: Serial communication not documented |
| | **Functions**: RS-232 operations | | |
| `maiko/src/rawrs232.c` | `documentation/rewrite-spec/io/` | ⚠️ **GAP**: Serial communication not documented |
| | **Functions**: Raw RS-232 operations | | |

### Unix IPC

| Source File | Documentation Section | Coverage Status |
|-------------|----------------------|-----------------|
| `maiko/src/unixcomm.c` | `documentation/rewrite-spec/io/` | ⚠️ **GAP**: Not explicitly documented |
| | **Functions**: Unix IPC communication | | |
| `maiko/src/unixfork.c` | `documentation/rewrite-spec/io/` | ⚠️ **GAP**: Not explicitly documented |
| | **Functions**: Unix fork operations | | |

## Related Documentation

- [Source Code Mapping](SOURCE_CODE_MAPPING.md) - Complete mapping index
- [Display Specifications](../display/) - Display subsystem specifications
- [I/O Specifications](../io/) - I/O subsystem specifications
