= Environment Variables


== Overview

Medley scripts set environment variables that Maiko reads during initialization and execution. These variables communicate file paths, configuration, and runtime state between Medley and Maiko.

== Environment Variables

=== MEDLEYDIR pointerSet By: Medley scripts (computed from script location)

*Read By*: Maiko, Medley scripts pointerPurpose: Top-level directory of the Medley installation.

*Computation*: 1. Resolve all symbolic links in medley script path
2. MEDLEYDIR is the directory containing the resolved script
3. In standard global installation: `/usr/local/interlisp/medley`
4. Can be installed in multiple places on the same machine pointerUsage: Used by scripts and Maiko to locate Medley files (sysouts, greet files, etc.)

*Source Code Reference*: Scripts compute MEDLEYDIR from script location

=== LOGINDIR pointerSet By: Medley scripts pointerRead By: Maiko pointerPurpose: User-specific Medley directory where user files are stored.

*Default*: `MEDLEYDIR/logindir` or `HOME/il`

*Override*: `-x DIR, --logindir DIR` flag pointerUsage: - Location for vmem files: `LOGINDIR/vmem/`
- Location for user greet files: `LOGINDIR/INIT.LISP`
- Working directory for Medley sessions pointerSource Code Reference: medley/scripts/medley/medley_run.sh - LOGINDIR setup

=== LDESOURCESYSOUT pointerSet By: Medley scripts pointerRead By: Maiko pointerPurpose: Source sysout file path that Maiko should load.

*Value*: Resolved sysout file path (e.g., `MEDLEYDIR/loadups/full.sysout`)

*Note*: Temporary workaround for Maiko sysout arg processing (Issue #1702)

*Source Code Reference*: medley/scripts/medley/medley_run.sh - LDESOURCESYSOUT setup

=== LDEDESTSYSOUT pointerSet By: Medley scripts pointerRead By: Maiko pointerPurpose: Destination vmem file path where Maiko should save session state.

*Default*: `LOGINDIR/vmem/lisp_{run-id}.virtualmem` or `LOGINDIR/vmem/lisp.virtualmem` (if run ID is "default")

*Override*: `-p FILE, --vmem FILE` flag pointerUsage: Maiko saves session state to this file on exit.

*Source Code Reference*: medley/scripts/medley/medley_run.sh - LDEDESTSYSOUT setup

=== LDEINIT pointerSet By: Medley scripts pointerRead By: Maiko pointerPurpose: Greet file path that Maiko should execute during startup.

*Default*: - Standard sysout: `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT` - Apps sysout: `MEDLEYDIR/greetfiles/APPS-INIT`

*Override*: `-r FILE, --greet FILE` flag pointerSuppress: `-r -, --greet -` flag (LDEINIT not set)

*Usage*: Maiko executes this file during startup before main Lisp system starts.

*Source Code Reference*: medley/scripts/medley/medley_args.sh - LDEINIT setup

=== LDEREMCM pointerSet By: Medley scripts pointerRead By: Maiko pointerPurpose: REM.CM file path that Maiko should execute after greet files.

*Default*: Not set (no default REM.CM file)

*Override*: `-cm FILE, --rem.cm FILE` flag pointerSuppress: `-cm -, --rem.cm -` flag (LDEREMCM not set)

*Usage*: Maiko executes this file after greet files, typically used for loadup operations.

*Source Code Reference*: medley/scripts/medley/medley_args.sh - LDEREMCM setup

=== LDEREPEATCM pointerSet By: Medley scripts (when `-cc FILE, --repeat FILE` is used)

*Read By*: Medley scripts, Maiko pointerPurpose: Repeat file path for repeated Medley runs.

*Value*: Path to repeat file specified with `-cc FILE, --repeat FILE` flag pointerUsage: Medley scripts check if this file exists and is non-empty to determine if Medley should run again.

*Source Code Reference*: medley/scripts/medley/medley_args.sh - repeat file handling

== Environment Variable Lifecycle

=== Setting Variables

Environment variables are set by Medley scripts before invoking Maiko:

1. *MEDLEYDIR*: Computed from script location
2. *LOGINDIR*: Resolved from `-x` flag or defaults
3. *LDESOURCESYSOUT*: Set to resolved sysout file path
4. *LDEDESTSYSOUT*: Set based on run ID and LOGINDIR
5. *LDEINIT*: Set to resolved greet file path (if greet file specified)
6. *LDEREMCM*: Set to REM.CM file path (if `-cm` flag used)

*Source Code Reference*: medley/scripts/medley/medley_run.sh - environment variable setup

=== Exporting Variables

All environment variables are exported so Maiko can read them:

[`export MEDLEYDIR`]
[`export LOGINDIR`]
[`export LDESOURCESYSOUT`]
[`export LDEDESTSYSOUT`]
[`export LDEINIT`]
[`export LDEREMCM`]

=== Maiko Reading

Maiko reads environment variables during initialization:
- *Startup*: Maiko reads variables to locate files and configure behavior
- *File Loading*: Maiko uses variables to load sysout, vmem, greet files
- *File Saving*: Maiko uses LDEDESTSYSOUT to save vmem file on exit

== Variable Resolution

=== MEDLEYDIR Resolution

MEDLEYDIR is computed on each invocation:

1. Get script path (resolving symbolic links)
2. Extract directory containing script
3. Set MEDLEYDIR to that directory

=== LOGINDIR Resolution

LOGINDIR resolution order:

1. `-x DIR` flag: Use specified directory
2. `-x -` flag: Use `MEDLEYDIR/logindir`
3. `-x --` flag: Use `MEDLEYDIR/logindir`
4. Default: `HOME/il`

=== LDEDESTSYSOUT Resolution

LDEDESTSYSOUT resolution order:

1. `-p FILE` flag: Use specified file
2. Default: `LOGINDIR/vmem/lisp_{run-id}.virtualmem` or `LOGINDIR/vmem/lisp.virtualmem` (if run ID is "default")

=== LDEINIT Resolution

LDEINIT resolution order:

1. `-r FILE` flag: Use specified file
2. `-r -` flag: LDEINIT not set (no greet file)
3. Default: `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT` (or `APPS-INIT` for apps sysout)

=== LDEREMCM Resolution

LDEREMCM resolution order:

1. `-cm FILE` flag: Use specified file
2. `-cm -` flag: LDEREMCM not set (no REM.CM file)
3. Default: Not set

== Platform Considerations

=== Windows/Cygwin

On Windows/Cygwin, some environment variables may need special handling:
- *Path Format*: Windows/Cygwin path conventions - *File System*: Medley file system vs. host Windows file system pointerSee: Platform - Windows for Windows-specific details

=== WSL

On WSL, environment variables follow WSL conventions:
- *Path Format*: WSL path conventions - *VNC Mode*: Special handling when VNC is used pointerSee: Platform - WSL for WSL-specific details

== Related Documentation
- *Scripts Component*: Scripts Component - Script system and environment setup
- *Command-Line Interface*: Command-Line Interface - Command-line argument mapping
- *File Formats*: File Formats - File format specifications
- *Protocols*: Protocols - Runtime communication protocols
- *Virtual Memory Files*: Virtual Memory Files Component - Vmem file details
- *Greet Files*: Greet Files Component - Greet file details

