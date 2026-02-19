= C Emulator Obsolete Files Removal

*Date*: 2026-02-19 16:03 *Purpose*: Document the removal of obsolete platform-specific files from
the Maiko C implementation

== Overview

This document records the removal of obsolete platform-specific source files that were no longer
compiled or maintained as part of the Maiko C emulator. These files were remnants of historical
platform support that has been superseded by modern alternatives.

== Removed Files

=== DOS Platform Files (3 files)

The following DOS-specific platform files were removed. DOS is no longer a supported platform for
Medley Interlisp.

#table(
  columns: (auto, auto, 1fr),
  [*File*], [*Purpose*], [*Reason for Removal*],
  [`maiko/src/doscomm.c`], [DOS serial communications], [DOS platform no longer supported],
  [`maiko/src/doskbd.c`], [DOS keyboard handling], [DOS platform no longer supported],
  [`maiko/src/dosmouse.c`], [DOS mouse handling], [DOS platform no longer supported],
)

=== Legacy Assembly Files (6 files)

The following assembly language files were removed. These were platform-specific optimizations for
architectures that are no longer primary targets.

#table(
  columns: (auto, auto, auto, 1fr),
  [*File*], [*Architecture*], [*Purpose*], [*Reason for Removal*],
  [`maiko/src/bbt68k.s`], [Motorola 68000], [BitBLT optimization], [68k architecture deprecated],
  [`maiko/src/bbtSPARC.s`], [SPARC], [BitBLT optimization], [SPARC architecture deprecated],
  [`maiko/src/launch.asm`], [x86 (DOS)], [DOS program launcher], [DOS platform no longer supported],
  [`maiko/src/vesafns.asm`], [x86 (DOS)], [VESA VBE functions], [VESA display deprecated],
  [`maiko/src/dsp386.il`], [x86 (386)], [Inline assembly for display], [Superseded by SDL backend],
  [`maiko/src/dspSPARC.il`],
  [SPARC],
  [Inline assembly for display],
  [SPARC architecture deprecated],
)

=== VESA/VGA Display Files (2 files)

The following display initialization files were removed. SDL is now the primary display backend.

#table(
  columns: (auto, auto, 1fr),
  [*File*], [*Purpose*], [*Reason for Removal*],
  [`maiko/src/vesainit.c`],
  [VESA VBE display initialization],
  [VESA display deprecated; SDL is current backend],

  [`maiko/src/vgainit.c`],
  [VGA display initialization],
  [VGA display deprecated; SDL is current backend],
)

== Additional Removed Files (2026-02-19)

The following files were also removed after verifying they were not needed for the SDL2 build:

=== Unused Feature Modules (17 files)

#table(
  columns: (auto, 1fr),
  [*File*], [*Purpose*],
  [`atom.c`], [Atom cell operations (unused opcode handler)],
  [`chatter.c`], [Serial port communication],
  [`codeconv.c`], [Japanese EUC character code conversion],
  [`codetbl.c`], [Japanese code table lookup],
  [`ejlisp.c`], [Japanese EUC Lisp interface],
  [`imagefile.c`], [Sun raster image file handling],
  [`imagefile2.c`], [Raster file I/O],
  [`kbdif.c`], [Keyboard interface stub],
  [`ldeboot.c`], [Standalone boot program],
  [`ldeether.c`], [Standalone ethernet program],
  [`picture.c`], [Picture data structure operations],
  [`rawcolor.c`], [Raw color BitBLT operations],
  [`rawrs232c.c`], [Raw RS232C serial I/O],
  [`rs232c.c`], [RS232C serial port handling],
  [`truecolor.c`], [TrueColor display operations],
  [`unixfork.c`], [Unix subprocess forking],
)

=== X11 Display Backend (10 files)

These X11 display files were removed as SDL is now the primary backend:

#table(
  columns: (auto, 1fr),
  [*File*], [*Purpose*],
  [`xbbt.c`], [X11 BitBLT operations],
  [`xcursor.c`], [X11 cursor management],
  [`xinit.c`], [X11 display initialization],
  [`xlspwin.c`], [X11 Lisp window management],
  [`xmkicon.c`], [X11 icon creation],
  [`xrdopt.c`], [X11 option reading],
  [`xscroll.c`], [X11 scrolling],
  [`xwinman.c`], [X11 window manager interface],
  [`mnwevent.c`], [X11 event handling],
  [`mnxmeth.c`], [X11 method dispatch],
)

=== Linear Programming Module (9 files)

#table(
  columns: (auto, 1fr),
  [*File*], [*Purpose*],
  [`lpdual.c`], [LP dual simplex],
  [`lpkit.c`], [LP kit utilities],
  [`lplexyy.c`], [LP lexical analysis],
  [`lpmain.c`], [LP main module],
  [`lpread.c`], [LP input reading],
  [`lpsolve.c`], [LP solver],
  [`lptran.c`], [LP translation],
  [`lpwrite.c`], [LP output writing],
  [`lpytab.c`], [LP table handling],
)

=== Obsolete Network Backend (1 file)

#table(
  columns: (auto, 1fr),
  [*File*], [*Purpose*],
  [`ether_sunos.c`], [SunOS NIT ethernet (obsolete, replaced by Nethub)],
)

== Current Platform Support

After this cleanup, the Maiko C emulator supports the following platforms:

=== Display Backends
- *SDL2*: Primary display backend (recommended)
- *X11*: Legacy X Window System support (still available)

=== Network Backends
- *Nethub*: TCP-based network emulation (recommended)

=== Operating Systems
- Linux (primary)
- macOS
- Windows (via SDL)

== Build Verification

All removed files were already excluded from the CMake build configuration (`maiko/CMakeLists.txt`).
The build was verified after removal:

```text
cmake -DMAIKO_INTROSPECT_ENABLED=ON --fresh .
cmake --build .
```

Result: Build completed successfully (100% - ldesdl, setsout, tstsout built).

== Impact Assessment

- *No functional changes*: All removed files were already not being compiled
- *No API changes*: No public interfaces were affected
- *Documentation updates*: Platform abstraction documentation updated to reflect current support

== Related Documentation

- #link("../../specifications/platform-abstraction/implementation-choices.typ")[Platform
    Abstraction: Implementation Choices]
- #link("c-emulator-complete-analysis.typ")[C Emulator Complete Analysis]

== References

- `maiko/CMakeLists.txt` - Build configuration (files not listed were not compiled)
- Git history for removed files (preserved in repository history)
