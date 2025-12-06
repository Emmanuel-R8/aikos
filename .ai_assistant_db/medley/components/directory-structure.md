# Medley Directory Structure

**Navigation**: [Medley README](../README.md) | [Medley Index](../INDEX.md) | [Architecture](../architecture.md)

## Overview

Medley organizes files in a standard directory structure. Understanding this structure is essential for understanding how Medley locates files, how scripts work, and how the system is organized.

## MEDLEYDIR Structure

MEDLEYDIR is the top-level directory of the Medley installation. It contains all Medley files except user-specific files (which are in LOGINDIR).

### Core Directories

#### `scripts/`

Medley scripts for running and building Medley.

**Key Subdirectories**:
- `scripts/medley/`: Main Medley startup scripts
  - `medley_run.sh`: Linux/macOS shell script
  - `medley.command`: macOS application bundle script
  - `medley.ps1`: Windows PowerShell script
  - `medley_vnc.sh`: VNC support script (WSL)
- `scripts/loadups/`: Loadup scripts for creating sysout files
  - `loadup-init.sh`: Stage 1 - Create init.dlinit
  - `loadup-mid-from-init.sh`: Stage 2 - Create init-mid.sysout
  - `loadup-lisp-from-mid.sh`: Stage 3 - Create lisp.sysout
  - `loadup-full-from-lisp.sh`: Stage 4 - Create full.sysout
  - `loadup-apps-from-full.sh`: Stage 5 - Create apps.sysout
  - `loadup-aux.sh`: Stage 6 - Create exports.all, whereis.hash
  - `loadup-db-from-full.sh`: Stage 7 - Create fuller.sysout, fuller.database
  - `loadup-all.sh`: Orchestrates stages 1-4 and 6 (optionally 5)
  - `loadup-db.sh`: Stage 7 only
  - `loadup-full.sh`: Stages 1-4 only

**See**: [Scripts Component](scripts.md) for script system documentation

#### `sources/`

Lisp source code for Interlisp and Common Lisp implementations.

**Contents**:
- Interlisp implementation source files (UPPERCASE, no extensions)
- Common Lisp implementation source files
- Compiled files (`.LCOM` for Interlisp, `.DFASL` for Common Lisp)

**Purpose**: Source code used during loadup to create sysout files.

#### `loadups/`

Sysout files and build artifacts created by the loadup process.

**Contents**:
- `lisp.sysout`: Minimal sysout
- `full.sysout`: Full sysout with development tools
- `apps.sysout`: Apps sysout with applications
- Other build artifacts (exports.all, whereis.hash, etc.)

**Purpose**: Standard location where Medley scripts look for sysout files.

**See**: [Sysout Files Component](sysout.md) for sysout file documentation

#### `greetfiles/`

Greet files executed during Medley startup.

**Contents**:
- `INIT.LISP`: Default greet file
- `APPS-INIT`: Greet file for apps sysout
- `MEDLEYDIR-INIT`: Greet file setting MEDLEYDIR
- `SIMPLE-INIT`: Simple greet file
- `NOGREET`: Disable greet file execution

**Purpose**: Initialize Lisp environment before main system starts.

**See**: [Greet Files Component](greetfiles.md) for greet file documentation

#### `library/`

Supported packages (historically maintained packages).

**Contents**:
- Package source files (UPPERCASE, no extensions)
- Compiled files (`.LCOM`)
- Documentation files (`.TEDIT`, `.TXT`)

**Purpose**: Standard packages that are part of the Medley distribution.

#### `lispusers/`

User-contributed packages (historically half-supported).

**Contents**:
- User-contributed package source files
- Compiled files (`.LCOM`)
- Documentation files (`.TEDIT`, `.TXT`)

**Purpose**: Community-contributed packages.

#### `docs/`

Documentation files in various formats.

**Subdirectories**:
- `docs/man-page/`: Man page source and generated files
  - `medley.1`: Man page source
  - `medley.1.md`: Markdown source
  - `medley.1.gz`: Compressed man page
- `docs/dinfo/`: TEdit documentation files
- `docs/primer/`: Primer documentation
- `docs/html-primer/`: HTML primer
- `docs/html-sunguide/`: Sun Users Guide HTML
- `docs/medley-irm/`: Medley IRM documentation
- `docs/ReleaseNote/`: Release notes
- `docs/Sun Users Guide/`: Sun Users Guide

**Purpose**: User and developer documentation.

#### `fonts/`

Font files for display, PostScript, Interpress, and press formats.

**Subdirectories**:
- `fonts/displayfonts/`: Display fonts (`.DISPLAYFONT`)
- `fonts/ipfonts/`: Interpress fonts (`.wd`)
- `fonts/postscriptfonts/`: PostScript fonts (`.PSCFONT`)
- `fonts/medleydisplayfonts/`: Medley display fonts (`.MEDLEYDISPLAYFONT`)
- `fonts/press/`: Press fonts (`.WIDTHS`)

**Purpose**: Font resources for Medley display and printing.

#### `clos/`

Early implementation of Common Lisp Object System (CLOS).

**Contents**:
- CLOS implementation source files (`.lisp`)
- Compiled files (`.dfasl`, `.DFASL`)
- Documentation (`.TEDIT`)

**Purpose**: CLOS implementation for Medley.

#### `rooms/`

Implementation of ROOMS window/desktop manager.

**Contents**:
- ROOMS source files
- Compiled files (`.DFASL`)
- Documentation (`.TEDIT`)

**Purpose**: ROOMS window manager for Medley.

#### `CLTL2/`

Files for Common Lisp, the Language 2nd edition conformance.

**Contents**:
- CLTL2 conformance source files
- Compiled files (`.LCOM`, `.DFASL`)

**Purpose**: CLTL2 conformance work (not ANSI standard).

#### `unicode/`

Data files for XCCS to and from Unicode mappings.

**Subdirectories**:
- `unicode/eastasia/`: East Asian character mappings
- `unicode/iso8859/`: ISO 8859 character mappings
- `unicode/vendors/`: Vendor-specific mappings
- `unicode/xerox/`: Xerox character mappings

**Purpose**: Unicode support data.

#### `internal/`

Internal implementation files (historically internal to Venue).

**Contents**:
- Internal implementation source files
- Compiled files (`.LCOM`)

**Purpose**: Internal implementation details.

#### `obsolete/`

Files that should be removed from the repository.

**Purpose**: Deprecated or obsolete files marked for removal.

#### `installers/`

Installation scripts and packages for various platforms.

**Subdirectories**:
- `installers/deb/`: Debian package installer
- `installers/macos/`: macOS installer
- `installers/cygwin/`: Cygwin installer
- `installers/win/`: Windows installer
- `installers/downloads_page/`: Downloads page generation

**Purpose**: Platform-specific installation tools.

### Root-Level Files

- `README.md`: Medley repository overview
- `BUILDING.md`: Instructions for building Medley and making releases
- `CONTRIBUTING.md`: Contribution guidelines
- `LICENSE`: License file
- `medley`: Symbolic link to medley script
- `run-medley`: Script to enhance medley options
- `loadup`: Symbolic link to loadup script

## LOGINDIR Structure

LOGINDIR is the user-specific Medley directory. Defaults to `MEDLEYDIR/logindir` but can be overridden with `-x` flag.

### User-Specific Files

#### Vmem Files

- `lisp.virtualmem`: Default vmem file (if run ID is "default")
- `{run-id}.virtualmem`: Run-specific vmem files

**Purpose**: Store persistent session state.

**See**: [Virtual Memory Files Component](vmem.md) for vmem file documentation

#### Configuration Files

- `.medley_config`: User-specific config file

**Purpose**: User-specific default command-line arguments.

**See**: [Configuration Files Component](configuration.md) for config file documentation

#### Greet Files

- `INIT.LISP`: User-specific greet file

**Purpose**: User-specific initialization.

**See**: [Greet Files Component](greetfiles.md) for greet file documentation

## File Naming Conventions

### Interlisp Source Files

- **Naming**: UPPERCASE names, no file extensions
- **Examples**: `INIT`, `MAIN`, `SYSOUT`

### Compiled Files

- **Interlisp**: `.LCOM` extension
- **Common Lisp**: `.DFASL` or `.dfasl` extension

### Documentation Files

- **TEdit Format**: `.TEDIT` or `.tedit` extension
- **Text Format**: `.TXT` or `.txt` extension
- **Markdown**: `.md` or `.MD` extension

### Font Files

- **Display Fonts**: `.DISPLAYFONT` or `.displayfont` extension
- **Interpress Fonts**: `.wd` or `.WD` extension
- **PostScript Fonts**: `.PSCFONT` extension
- **Medley Display Fonts**: `.MEDLEYDISPLAYFONT` extension
- **Press Fonts**: `.WIDTHS` extension

## Directory Resolution

### MEDLEYDIR Resolution

MEDLEYDIR is computed on each invocation of medley:

1. Resolve all symbolic links in the medley script path
2. MEDLEYDIR is the directory containing the resolved script
3. In standard global installation: `/usr/local/interlisp/medley`
4. Can be installed in multiple places on the same machine

### LOGINDIR Resolution

LOGINDIR resolution order:

1. `-x DIR` flag: Use specified directory
2. `-x -` flag: Use `MEDLEYDIR/logindir`
3. Default: `MEDLEYDIR/logindir`

### File Resolution

#### Sysout Files

1. Explicit sysout argument: Use specified file
2. `-f` flag: `MEDLEYDIR/loadups/full.sysout`
3. `-l` flag: `MEDLEYDIR/loadups/lisp.sysout`
4. `-a` flag: `MEDLEYDIR/loadups/apps.sysout`
5. No flag, no argument: Check for vmem file

#### Vmem Files

1. `-p FILE` flag: Use specified file
2. Default: `LOGINDIR/{run-id}.virtualmem` or `LOGINDIR/lisp.virtualmem`

#### Config Files

1. `-c FILE` flag: Use specified file
2. `~/.medley_config`: User home directory config
3. `MEDLEYDIR/.medley_config`: Medley directory config

#### Greet Files

1. `-r FILE` flag: Use specified file
2. `-r -` flag: No greet file
3. Default: `MEDLEYDIR/greetfiles/INIT.LISP`

## Related Documentation

- **Architecture**: [Architecture Overview](../architecture.md) - System architecture
- **Scripts**: [Scripts Component](scripts.md) - Script system
- **Sysout Files**: [Sysout Files Component](sysout.md) - Sysout files
- **Virtual Memory Files**: [Virtual Memory Files Component](vmem.md) - Vmem files
- **Configuration Files**: [Configuration Files Component](configuration.md) - Config files
- **Greet Files**: [Greet Files Component](greetfiles.md) - Greet files
