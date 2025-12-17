= Linux Platform Documentation


== Overview

Linux is the standard Unix platform for Medley. Medley scripts on Linux use standard Unix conventions and support both X11 and SDL display backends.

== Script System

=== Script Used pointerPrimary Script: `medley_run.sh`

*Location*: `medley/scripts/medley/medley_run.sh`

*Characteristics*: - Bash/shell script
- Standard Unix behavior - Handles X11 and SDL display backends pointerSource Code Reference: medley/scripts/medley/medley_run.sh

== Platform Detection

Scripts detect Linux using:

[`if [ "$(uname)" ] != "Darwin" ] && \
   [ "$(uname -s | head --bytes 6)" != "CYGWIN" ] && \
      [ ! -e "/proc/version" ] || ! grep --ignore-case --quiet Microsoft /proc/version`]
then
  linux=true
  platform=linux
fi)

*Source Code Reference*: medley/scripts/medley/medley_main.sh - Linux detection

== Display Backends

=== X11

Default display backend on Linux.

*Usage*: Standard X11 display pointerSelection: Default, or specify with `-d :N,* --display:N`

=== SDL

Alternative display backend.

*Usage*: Specify with `-d SDL,* --display SDL`

*Platform Support*: Available on Linux

== Path Handling

=== Standard Unix Paths

Linux uses standard Unix path conventions:
- *Absolute paths*: `/path/to/file`
- *Home directory*: `~` or `$HOME`
- *Path separators*: `/`

=== MEDLEYDIR Resolution

MEDLEYDIR is computed from script location using standard Unix path resolution.

=== LOGINDIR Resolution

LOGINDIR defaults to `HOME/il` or can be specified with `-x DIR, --logindir DIR`.

== File System

=== Standard Unix File System

Linux uses standard Unix file system:
- *File permissions*: Standard Unix permissions
- *Symbolic links*: Supported
- *Case sensitivity*: Case-sensitive file system

== Script Behavior

=== Standard Behavior

Linux scripts follow standard Unix behavior:
- *Argument parsing*: Standard shell argument parsing
- *Environment variables*: Standard Unix environment
- *Process management*: Standard Unix process management

=== No Special Handling

Linux does not require special platform-specific handling beyond standard Unix behavior.

== Maiko Executable Location

Scripts locate Maiko executable in this order:

1. `MAIKODIR` environment variable: `<MAIKODIR>/linux.x86_64/lde`
2. `MEDLEYDIR/../maiko/`: `<MEDLEYDIR>/../maiko/linux.x86_64/lde`
3. `MEDLEYDIR/maiko/`: `<MEDLEYDIR>/maiko/linux.x86_64/lde`
4. PATH: `lde` on PATH pointerPlatform Identifier: `linux.x86_64` (or architecture-specific)

== Related Documentation
- *Platform Overview*: Platform Overview - Platform documentation overview
- *Scripts Component*: Scripts Component - Script system
- *Interface Documentation*: Interface Documentation - Interface mechanisms

