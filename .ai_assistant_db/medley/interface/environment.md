# Environment Variables

**Navigation**: [Medley README](../README.md) | [Medley Index](../INDEX.md) | [Interface Overview](README.md)

## Overview

Medley scripts set environment variables that Maiko reads during initialization and execution. These variables communicate file paths, configuration, and runtime state between Medley and Maiko.

## Environment Variables

### MEDLEYDIR

**Set By**: Medley scripts (computed from script location)

**Read By**: Maiko, Medley scripts

**Purpose**: Top-level directory of the Medley installation.

**Computation**:
1. Resolve all symbolic links in medley script path
2. MEDLEYDIR is the directory containing the resolved script
3. In standard global installation: `/usr/local/interlisp/medley`
4. Can be installed in multiple places on the same machine

**Usage**: Used by scripts and Maiko to locate Medley files (sysouts, greet files, etc.)

**Source Code Reference**: Scripts compute MEDLEYDIR from script location

### LOGINDIR

**Set By**: Medley scripts

**Read By**: Maiko

**Purpose**: User-specific Medley directory where user files are stored.

**Default**: `MEDLEYDIR/logindir` or `HOME/il`

**Override**: `-x DIR, --logindir DIR` flag

**Usage**: 
- Location for vmem files: `LOGINDIR/vmem/`
- Location for user greet files: `LOGINDIR/INIT.LISP`
- Working directory for Medley sessions

**Source Code Reference**: [medley/scripts/medley/medley_run.sh](medley/scripts/medley/medley_run.sh) - LOGINDIR setup

### LDESOURCESYSOUT

**Set By**: Medley scripts

**Read By**: Maiko

**Purpose**: Source sysout file path that Maiko should load.

**Value**: Resolved sysout file path (e.g., `MEDLEYDIR/loadups/full.sysout`)

**Note**: Temporary workaround for Maiko sysout arg processing (Issue #1702)

**Source Code Reference**: [medley/scripts/medley/medley_run.sh](medley/scripts/medley/medley_run.sh) - LDESOURCESYSOUT setup

### LDEDESTSYSOUT

**Set By**: Medley scripts

**Read By**: Maiko

**Purpose**: Destination vmem file path where Maiko should save session state.

**Default**: `LOGINDIR/vmem/lisp_{run-id}.virtualmem` or `LOGINDIR/vmem/lisp.virtualmem` (if run ID is "default")

**Override**: `-p FILE, --vmem FILE` flag

**Usage**: Maiko saves session state to this file on exit.

**Source Code Reference**: [medley/scripts/medley/medley_run.sh](medley/scripts/medley/medley_run.sh) - LDEDESTSYSOUT setup

### LDEINIT

**Set By**: Medley scripts

**Read By**: Maiko

**Purpose**: Greet file path that Maiko should execute during startup.

**Default**: 
- Standard sysout: `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT`
- Apps sysout: `MEDLEYDIR/greetfiles/APPS-INIT`

**Override**: `-r FILE, --greet FILE` flag

**Suppress**: `-r -, --greet -` flag (LDEINIT not set)

**Usage**: Maiko executes this file during startup before main Lisp system starts.

**Source Code Reference**: [medley/scripts/medley/medley_args.sh](medley/scripts/medley/medley_args.sh) - LDEINIT setup

### LDEREMCM

**Set By**: Medley scripts

**Read By**: Maiko

**Purpose**: REM.CM file path that Maiko should execute after greet files.

**Default**: Not set (no default REM.CM file)

**Override**: `-cm FILE, --rem.cm FILE` flag

**Suppress**: `-cm -, --rem.cm -` flag (LDEREMCM not set)

**Usage**: Maiko executes this file after greet files, typically used for loadup operations.

**Source Code Reference**: [medley/scripts/medley/medley_args.sh](medley/scripts/medley/medley_args.sh) - LDEREMCM setup

### LDEREPEATCM

**Set By**: Medley scripts (when `-cc FILE, --repeat FILE` is used)

**Read By**: Medley scripts, Maiko

**Purpose**: Repeat file path for repeated Medley runs.

**Value**: Path to repeat file specified with `-cc FILE, --repeat FILE` flag

**Usage**: Medley scripts check if this file exists and is non-empty to determine if Medley should run again.

**Source Code Reference**: [medley/scripts/medley/medley_args.sh](medley/scripts/medley/medley_args.sh) - repeat file handling

## Environment Variable Lifecycle

### Setting Variables

Environment variables are set by Medley scripts before invoking Maiko:

1. **MEDLEYDIR**: Computed from script location
2. **LOGINDIR**: Resolved from `-x` flag or defaults
3. **LDESOURCESYSOUT**: Set to resolved sysout file path
4. **LDEDESTSYSOUT**: Set based on run ID and LOGINDIR
5. **LDEINIT**: Set to resolved greet file path (if greet file specified)
6. **LDEREMCM**: Set to REM.CM file path (if `-cm` flag used)

**Source Code Reference**: [medley/scripts/medley/medley_run.sh](medley/scripts/medley/medley_run.sh) - environment variable setup

### Exporting Variables

All environment variables are exported so Maiko can read them:

```bash
export MEDLEYDIR
export LOGINDIR
export LDESOURCESYSOUT
export LDEDESTSYSOUT
export LDEINIT
export LDEREMCM
```

### Maiko Reading

Maiko reads environment variables during initialization:

- **Startup**: Maiko reads variables to locate files and configure behavior
- **File Loading**: Maiko uses variables to load sysout, vmem, greet files
- **File Saving**: Maiko uses LDEDESTSYSOUT to save vmem file on exit

## Variable Resolution

### MEDLEYDIR Resolution

MEDLEYDIR is computed on each invocation:

1. Get script path (resolving symbolic links)
2. Extract directory containing script
3. Set MEDLEYDIR to that directory

### LOGINDIR Resolution

LOGINDIR resolution order:

1. `-x DIR` flag: Use specified directory
2. `-x -` flag: Use `MEDLEYDIR/logindir`
3. `-x --` flag: Use `MEDLEYDIR/logindir`
4. Default: `HOME/il`

### LDEDESTSYSOUT Resolution

LDEDESTSYSOUT resolution order:

1. `-p FILE` flag: Use specified file
2. Default: `LOGINDIR/vmem/lisp_{run-id}.virtualmem` or `LOGINDIR/vmem/lisp.virtualmem` (if run ID is "default")

### LDEINIT Resolution

LDEINIT resolution order:

1. `-r FILE` flag: Use specified file
2. `-r -` flag: LDEINIT not set (no greet file)
3. Default: `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT` (or `APPS-INIT` for apps sysout)

### LDEREMCM Resolution

LDEREMCM resolution order:

1. `-cm FILE` flag: Use specified file
2. `-cm -` flag: LDEREMCM not set (no REM.CM file)
3. Default: Not set

## Platform Considerations

### Windows/Cygwin

On Windows/Cygwin, some environment variables may need special handling:

- **Path Format**: Windows/Cygwin path conventions
- **File System**: Medley file system vs. host Windows file system

**See**: [Platform - Windows](../platform/windows.md) for Windows-specific details

### WSL

On WSL, environment variables follow WSL conventions:

- **Path Format**: WSL path conventions
- **VNC Mode**: Special handling when VNC is used

**See**: [Platform - WSL](../platform/wsl.md) for WSL-specific details

## Related Documentation

- **Scripts Component**: [Scripts Component](../components/scripts.md) - Script system and environment setup
- **Command-Line Interface**: [Command-Line Interface](command-line.md) - Command-line argument mapping
- **File Formats**: [File Formats](file-formats.md) - File format specifications
- **Protocols**: [Protocols](protocols.md) - Runtime communication protocols
- **Virtual Memory Files**: [Virtual Memory Files Component](../components/vmem.md) - Vmem file details
- **Greet Files**: [Greet Files Component](../components/greetfiles.md) - Greet file details

