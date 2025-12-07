# Greet Files

**Navigation**: [Medley README](../README.md) | [Medley Index](../INDEX.md) | [Architecture](../architecture.md)

## Overview

Greet files are Lisp source files executed during Medley startup to initialize the environment. They are executed before the main Lisp system starts, allowing customization of the startup process. Greet files provide a way to set up the Lisp environment, load packages, configure system variables, and perform initialization tasks.

## Greet File Format

### Lisp Source Code

Greet files are plain text files containing Lisp source code:

```lisp
(SETQ INTERLISPMODE T)
(LOAD 'MYINIT)
```

### Format

- **File Format**: Plain text, Lisp source code
- **Encoding**: Platform-specific (typically UTF-8 or platform default)
- **Line Endings**: Platform-specific (LF on Unix, CRLF on Windows)

**See**: [Interface - File Formats](../interface/file-formats.md#greet-file-format) for format specification

## Greet File Execution

### Execution Order

Greet files are executed in this order:

1. **Greet file** (if specified): Executed first
2. **REM.CM file** (if specified): Executed after greet file
3. **Main Lisp system**: Starts after greet/REM.CM execution

### Execution Context

Greet files are executed:

- **Before**: Main Lisp system initialization
- **Context**: Early Lisp environment
- **Purpose**: Initialize environment before main system starts

**See**: [Interface - Protocols](../interface/protocols.md#startup-sequence) for startup sequence details

## Greet File Resolution

### Default Greet Files

Default greet file depends on sysout type:

- **Standard sysout** (`-f`, `-l`): `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT`
- **Apps sysout** (`-a`): `MEDLEYDIR/greetfiles/APPS-INIT`

**Source Code Reference**:

- [medley/scripts/medley/medley_args.sh](medley/scripts/medley/medley_args.sh) - Greet file resolution and LDEINIT setup
- [medley/greetfiles/README.md](medley/greetfiles/README.md) - Greet file directory documentation

### Custom Greet File

Greet file can be specified with `-r, --greet` flag:

```bash
medley -r /path/to/greet/file
```

### Suppressing Greet File

Greet file execution can be suppressed with `-r -, --greet -`:

```bash
medley -r -
# Start without greet file
```

## Standard Greet Files

### MEDLEYDIR-INIT

Default greet file for standard sysouts.

**Location**: `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT`

**Purpose**: Initialize MEDLEYDIR and standard environment.

### APPS-INIT

Default greet file for apps sysout.

**Location**: `MEDLEYDIR/greetfiles/APPS-INIT`

**Purpose**: Initialize apps environment.

### SIMPLE-INIT

Simple initialization greet file.

**Location**: `MEDLEYDIR/greetfiles/SIMPLE-INIT`

**Purpose**: Minimal initialization for git-directory relative structure. Contains INTERLISPMODE.

**Source Code Reference**: [medley/greetfiles/README.md](medley/greetfiles/README.md)

### NOGREET

Special file to disable greet file execution.

**Location**: `MEDLEYDIR/greetfiles/NOGREET`

**Purpose**: Used as system init when doing loadups that don't want personalization.

**Source Code Reference**: [medley/greetfiles/README.md](medley/greetfiles/README.md)

## REM.CM Files

### Purpose

REM.CM files are Lisp files executed after greet files, typically used for loadup operations.

### Specification

REM.CM file can be specified with `-cm, --rem.cm` flag:

```bash
medley -cm /path/to/rem.cm
```

**Note**: REM.CM file path must be absolute.

### Suppressing REM.CM

REM.CM execution can be suppressed with `-cm -, --rem.cm -`:

```bash
medley -cm -
# Start without REM.CM file
```

### Environment Variable

REM.CM file can also be specified via `LDEREMCM` environment variable.

**See**: [Interface - Environment Variables](../interface/environment.md#lderemcm) for environment variable details

## Greet File Usage

### Custom Initialization

Create custom greet file:

```lisp
;; ~/my-greet.lisp
(SETQ INTERLISPMODE T)
(LOAD 'MY-PACKAGE)
(SETQ MY-VARIABLE 'VALUE)
```

Then use it:

```bash
medley -r ~/my-greet.lisp
```

### User-Specific Greet File

Place greet file in LOGINDIR:

```bash
# Create LOGINDIR/INIT.LISP
medley
# Automatically uses LOGINDIR/INIT.LISP if present
```

### Loadup Operations

Use REM.CM file for loadup operations:

```bash
medley -cm /path/to/loadup.cm
```

## Error Handling

### Missing Greet File

If specified greet file doesn't exist:

- Maiko reports error during startup
- Medley may fail to start or start with errors

### Greet File Errors

If greet file contains errors:

- Lisp errors are reported during execution
- Medley may fail to start or start with errors
- Error messages indicate greet file execution problems

## Platform Considerations

### Windows/Cygwin

On Windows/Cygwin installations, greet file paths are specified in the Medley file system, not the host Windows file system.

**See**: [Platform - Windows](../platform/windows.md) for Windows-specific details

### Path Handling

Greet file paths follow platform conventions:

- **Linux/macOS**: Standard Unix paths
- **Windows/Cygwin**: Medley file system paths
- **WSL**: WSL path conventions

## Related Documentation

- **Architecture**: [Architecture Overview](../architecture.md) - System architecture
- **Scripts**: [Scripts Component](scripts.md) - Script system and greet file resolution
- **Directory Structure**: [Directory Structure Component](directory-structure.md) - Greet file locations
- **Interface - File Formats**: [File Formats](../interface/file-formats.md) - Greet file format specification
- **Interface - Environment Variables**: [Environment Variables](../interface/environment.md) - LDEINIT and LDEREMCM variables
- **Interface - Protocols**: [Protocols](../interface/protocols.md) - Startup sequence and execution
