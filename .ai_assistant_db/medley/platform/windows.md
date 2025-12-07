# Windows/Cygwin Platform Documentation

**Navigation**: [Medley README](../README.md) | [Medley Index](../INDEX.md) | [Platform Overview](README.md)

## Overview

Windows/Cygwin is supported by Medley through PowerShell scripts and optional Docker execution. Windows/Cygwin has unique path handling and file system considerations.

## Script System

### Script Used

**Primary Script**: `medley.ps1`

**Location**: `medley/scripts/medley/medley.ps1`

**Characteristics**:
- PowerShell script
- May use Docker for execution
- Handles Windows/Cygwin path conventions
- Windows-specific behaviors

**Source Code Reference**: [medley/scripts/medley/medley.ps1](medley/scripts/medley/medley.ps1)

## Platform Detection

Scripts detect Windows/Cygwin using:

```bash
if [ "$(uname -s | head --bytes 6)" = "CYGWIN" ]
then
  cygwin=true
  platform=cgwin
fi
```

**Source Code Reference**: [medley/scripts/medley/medley_main.sh](medley/scripts/medley/medley_main.sh) - Cygwin detection

## Display Backends

### SDL

SDL is the primary display backend on Windows/Cygwin.

**Usage**: SDL display backend

**Selection**: SDL is used by default

**Note**: X11 is not available on Windows/Cygwin.

### Pixel Scale

Pixel scale can be specified with `-ps N, --pixelscale N` flag (SDL only).

## Path Handling

### Windows/Cygwin Path Conventions

Windows/Cygwin uses Windows path conventions with Cygwin translation:

- **Windows paths**: `C:\path\to\file`
- **Cygwin paths**: `/cygdrive/c/path/to/file`
- **Path separators**: `/` (Cygwin) or `\` (Windows)

### MEDLEYDIR Resolution

MEDLEYDIR is computed from script location, with special handling for Cygwin paths.

### LOGINDIR Resolution

LOGINDIR defaults to `HOME/il` or can be specified with `-x DIR, --logindir DIR`.

### File Paths in Medley

On Windows/Cygwin, file paths specified in Medley (greet files, REM.CM files) are specified in the Medley file system, not the host Windows file system.

**Source Code Reference**: [medley/docs/man-page/medley.1.md](medley/docs/man-page/medley.1.md) - Windows file path note

## File System

### Windows/Cygwin File System

Windows/Cygwin uses Windows file system with Cygwin translation:

- **File permissions**: Windows permissions with Cygwin translation
- **Symbolic links**: Supported (Cygwin)
- **Case sensitivity**: Case-insensitive (Windows)

### Cygwin Workaround

There is a temporary workaround for Cygwin (Issue #1685):

```bash
if [ "${cygwin}" = true ]
then
  MEDLEYDIR="${MEDLEYDIR}/"
fi
```

**Source Code Reference**: [medley/scripts/medley/medley_run.sh](medley/scripts/medley/medley_run.sh) - Cygwin workaround

## Docker Execution

### Docker Support

Windows `medley.ps1` script may use Docker for execution:

- **Docker Image**: `interlisp/medley:${draft}`
- **Docker Entrypoint**: `medley --windows`
- **Volume Mounting**: LOGINDIR mounted as volume

**Source Code Reference**: [medley/scripts/medley/medley.ps1](medley/scripts/medley/medley.ps1) - Docker execution

## Script Behavior

### Windows-Specific Behavior

Windows scripts include Windows-specific handling:

- **PowerShell**: Uses PowerShell for script execution
- **Docker**: May use Docker for execution
- **Path Translation**: Handles Windows/Cygwin path translation
- **File System**: Medley file system vs. host Windows file system

### Internal Flag

Scripts use `--windows` flag internally when called from Windows `medley.ps1` via Docker:

```bash
--windows
# internal: called from Windows medley.ps1 (via docker)
windows=true
```

**Source Code Reference**: [medley/scripts/medley/medley_args.sh](medley/scripts/medley/medley_args.sh) - Windows flag

## Maiko Executable Location

Scripts locate Maiko executable in this order:

1. `MAIKODIR` environment variable: `<MAIKODIR>/cygwin.x86_64/lde`
2. `MEDLEYDIR/../maiko/`: `<MEDLEYDIR>/../maiko/cygwin.x86_64/lde`
3. `MEDLEYDIR/maiko/`: `<MEDLEYDIR>/maiko/cygwin.x86_64/lde`
4. PATH: `lde` on PATH

**Platform Identifier**: `cygwin.x86_64`

## Related Documentation

- **Platform Overview**: [Platform Overview](README.md) - Platform documentation overview
- **Scripts Component**: [Scripts Component](../components/scripts.md) - Script system
- **Interface Documentation**: [Interface Documentation](../interface/) - Interface mechanisms
