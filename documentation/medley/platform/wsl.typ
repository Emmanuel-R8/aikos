= WSL Platform Documentation


== Overview

Windows Subsystem for Linux (WSL) is supported by Medley with special VNC support for better display scaling. WSL has unique behaviors for both WSL1 and WSL2.

== Script System

=== Script Used pointerPrimary Script: `medley_run.sh` (with VNC support)

*Location*: `medley/scripts/medley/medley_run.sh`

*Characteristics*: - Standard Linux shell script
- VNC support for better display scaling
- WSL1 vs WSL2 detection - Automation mode support pointerSource Code Reference: medley/scripts/medley/medley_run.sh

== Platform Detection

Scripts detect WSL using:

[`if [ -e "/proc/version" ] && grep --ignore-case --quiet Microsoft /proc/version`]
[`then`]
[`  platform=wsl`]
[`  wsl=true`]
[`  wsl_ver=0`]
[`  # WSL2`]
[`  grep --ignore-case --quiet wsl /proc/sys/kernel/osrelease`]
[`  if [ $? -eq 0 ];`]
[`  then`]
[`    wsl_ver=2`]
[`  else`]
[`    # WSL1`]
[`    grep --ignore-case --quiet microsoft /proc/sys/kernel/osrelease`]
[`    if [ $? -eq 0 ]`]
[`    then`]
[`      if [ "$(uname -m)" ] = "x86_64"`]
      then
        wsl_ver=1
      else
        [`# Error: WSL1 requires x86_64`]
      fi
    fi
  fi
fi)

*Source Code Reference*: medley/scripts/medley/medley_main.sh - WSL detection

== WSL1 vs WSL2

=== WSL1
- *Architecture*: Requires x86_64
- *VNC: Always uses VNC* (flag always set)
- *Display*: VNC window on Windows side

=== WSL2
- *Architecture*: Supports x86_64 and ARM64
- *VNC*: Optional (can use X11 or VNC)
- *Display*: VNC window on Windows side (recommended) or X11

== Display Backends

=== VNC (Recommended)

VNC is recommended on WSL for better display scaling.

*Usage*: Specify with `-v, --vnc` flag (or always on for WSL1)

*Benefits*: - Better scaling on high-resolution displays
- Follows Windows desktop scaling settings
- More usable than X11 on WSL pointerImplementation: - Xvnc server on Linux side
- VNC viewer on Windows side
- Scripts coordinate both sides pointerSource Code Reference: medley/scripts/medley/medley_vnc.sh - VNC setup

=== X11

X11 is available on WSL but scales poorly.

*Usage*: Specify with `-v -, --vnc -` to disable VNC pointerLimitations: - Poor scaling on high-resolution displays - Not recommended for WSL

== VNC Protocol

=== VNC Setup Flow

#figure(
  caption: [Sequence Diagram],
  [Diagram: See original documentation for visual representation.],
)
)

=== VNC Implementation

1. *Find Open Display*: Script finds available X display number
2. *Start Xvnc Server*: Script starts Xvnc server on Linux side
3. *Start VNC Viewer*: Script starts VNC viewer on Windows side
4. *Set DISPLAY*: Script sets DISPLAY environment variable
5. *Invoke Maiko*: Script invokes Maiko with VNC display pointerSource Code Reference: medley/scripts/medley/medley_vnc.sh - VNC implementation

== Automation Mode

=== Automation Flag

When `-am, --automation` flag is used:
- *Short Sessions*: Adjusts Xvnc error detection for very short sessions
- *Timing*: Adjusts timing for automation scripts
- *Error Handling*: Prevents false positives from short session detection pointerPurpose: Useful when calling Medley as part of automation scripts where Medley may run for very short time (< a couple of seconds).

*Source Code Reference*: medley/docs/man-page/medley.1.md - automation flag

== Path Handling

=== WSL Path Conventions

WSL uses Linux path conventions with Windows integration:
- *Linux paths*: `/path/to/file`
- *Windows paths*: Accessible via `/mnt/c/path/to/file`
- *Path separators*: `/`

=== VNC Viewer Location

VNC viewer is located on Windows side:
- *Location*: `%USERPROFILE%\AppData\Local\Interlisp\vncviewer.exe` - *Installation*: Scripts may copy VNC viewer to Windows directory if needed pointerSource Code Reference: medley/scripts/medley/medley_vnc.sh - VNC viewer location

== File System

=== WSL File System

WSL uses Linux file system with Windows integration:
- *File permissions*: Standard Linux permissions
- *Symbolic links*: Supported
- *Case sensitivity*: Case-sensitive (Linux)
- *Windows Access*: Windows drives accessible via `/mnt/`

== Script Behavior

=== WSL-Specific Behavior

WSL scripts include WSL-specific handling:
- *VNC Support*: Automatic VNC setup
- *Windows Integration*: VNC viewer on Windows side
- *WSL Version Detection*: WSL1 vs WSL2 handling
- *Automation Mode*: Special handling for automation

=== VNC Flag Behavior
- *WSL1*: VNC flag always set (cannot be disabled) - *WSL2*: VNC flag can be enabled/disabled with `-v, --vnc` flag

== Maiko Executable Location

Scripts locate Maiko executable in this order:

1. `MAIKODIR` environment variable: `<MAIKODIR>/linux.x86_64/lde`
2. `MEDLEYDIR/../maiko/`: `<MEDLEYDIR>/../maiko/linux.x86_64/lde`
3. `MEDLEYDIR/maiko/`: `<MEDLEYDIR>/maiko/linux.x86_64/lde`
4. PATH: `lde` on PATH pointerPlatform Identifier: `linux.x86_64` (same as Linux)

== Related Documentation
- *Platform Overview*: Platform Overview - Platform documentation overview
- *Scripts Component*: Scripts Component - Script system
- *Interface Documentation*: Interface Documentation - Interface mechanisms
- *Protocols*: Protocols - VNC protocol details

