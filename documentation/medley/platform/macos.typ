= macOS Platform Documentation

*Navigation*: Medley README | Medley Index | Platform Overview

== Overview

macOS (Darwin) is supported by Medley with macOS-specific optimizations. Medley provides both standard shell scripts and macOS application bundle scripts.

== Script System

=== Scripts Used

*Primary Scripts*:

- `medley_run.sh`: Standard shell script (Linux/macOS)
- `medley.command`: macOS application bundle script

*Location*: `medley/scripts/medley/medley.command`

*Characteristics*:

- macOS-specific script
- Can be double-clicked in Finder
- Handles macOS-specific path conventions
- Similar functionality to `medley_run.sh` but macOS-optimized

*Source Code Reference*: medley/scripts/medley/medley.command

== Platform Detection

Scripts detect macOS using:

#codeblock(lang: "bash", [
if [ "$(uname)" = "Darwin" ]
then
  darwin=true
  platform=darwin
fi
])

*Source Code Reference*: medley/scripts/medley/medley_main.sh - macOS detection

== Display Backends

=== X11

X11 display backend available on macOS.

*Usage*: Standard X11 display (requires XQuartz)

*Selection*: Default, or specify with `-d :N, --display :N`

=== SDL

Alternative display backend.

*Usage*: Specify with `-d SDL, --display SDL`

*Platform Support*: Available on macOS

== Path Handling

=== macOS Path Conventions

macOS uses standard Unix paths with macOS-specific considerations:

- *Absolute paths*: `/path/to/file`
- *Home directory*: `~` or `$HOME`
- *Path separators*: `/`
- *Case sensitivity*: Case-insensitive by default (but case-preserving)

=== MEDLEYDIR Resolution

MEDLEYDIR is computed from script location using standard Unix path resolution.

=== LOGINDIR Resolution

LOGINDIR defaults to `HOME/il` or can be specified with `-x DIR, --logindir DIR`.

== File System

=== macOS File System

macOS uses HFS+ or APFS file system:

- *File permissions*: Standard Unix permissions
- *Symbolic links*: Supported
- *Case sensitivity*: Case-insensitive by default (but case-preserving)

== Script Behavior

=== macOS-Specific Behavior

macOS scripts may include macOS-specific optimizations:

- *Finder Integration*: `medley.command` can be double-clicked
- *Path Handling*: macOS-specific path handling
- *Display*: macOS display integration

=== Man Page Display

On macOS, man page display uses:

#codeblock(lang: "bash", [
/usr/bin/man "${MEDLEYDIR}/docs/man-page/medley.1.gz"
])

*Source Code Reference*: medley/scripts/medley/medley_args.sh - man page display

== Maiko Executable Location

Scripts locate Maiko executable in this order:

1. `MAIKODIR` environment variable: `<MAIKODIR>/darwin.x86_64/lde` or `<MAIKODIR>/darwin.aarch64/lde`
2. `MEDLEYDIR/../maiko/`: `<MEDLEYDIR>/../maiko/darwin.x86_64/lde` or `darwin.aarch64/lde`
3. `MEDLEYDIR/maiko/`: `<MEDLEYDIR>/maiko/darwin.x86_64/lde` or `darwin.aarch64/lde`
4. PATH: `lde` on PATH

*Platform Identifiers*:

- *Intel Macs*: `darwin.x86_64`
- *Apple Silicon Macs*: `darwin.aarch64`

== Related Documentation

- *Platform Overview*: Platform Overview - Platform documentation overview
- *Scripts Component*: Scripts Component - Script system
- *Interface Documentation*: Interface Documentation - Interface mechanisms

