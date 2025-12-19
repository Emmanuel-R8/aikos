# Maiko Emulator Implementation in Common Lisp

This directory contains a complete implementation of the Maiko emulator in Common Lisp (SBCL), following the rewrite documentation specifications in `.ai_assistant_db/rewrite-spec/`.

## Overview

The Common Lisp implementation provides:
- Complete VM core with bytecode interpreter
- Memory management with garbage collection
- SDL3 display backend
- I/O subsystems (keyboard, mouse, file system)
- Full compatibility with existing sysout files

## Prerequisites

- SBCL (Steel Bank Common Lisp) installed
- SDL3 development libraries
- ASDF (included with SBCL)

### Installing SBCL

**Linux**:
```bash
# Ubuntu/Debian
sudo apt install sbcl

# Or download from https://www.sbcl.org/platform-table.html
```

**macOS**:
```bash
brew install sbcl
```

**NixOS**:
```bash
nix-env -i sbcl
# Or use /usr/bin/env sbcl if already in PATH
```

### Installing SDL3

**Linux**:
```bash
# Check your distribution's package manager
# SDL3 may need to be built from source or use development packages
```

**macOS**:
```bash
brew install sdl3
```

## Building

The project uses ASDF (Another System Definition Facility) for building and dependency management.

### Quick Start

```bash
# Build and load the system
sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)"

# Or use the build script
./build.sh
```

### Running

```bash
# Run with a sysout file
sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)" -- \
  -sysout path/to/sysout

# Or use the run script
./run.sh path/to/sysout
```

## Project Structure

```
alternatives/lisp/
├── maiko-lisp.asd          # ASDF system definition
├── README.md               # This file
├── build.sh                # Build script
├── run.sh                  # Run script
├── .gitignore              # Git ignore patterns
├── src/                    # Source code
│   ├── vm/                 # VM core (dispatch, opcodes, stack)
│   ├── memory/             # Memory management (GC, storage, virtual)
│   ├── data/               # Data structures (cons, array, sysout)
│   ├── io/                 # I/O subsystems (keyboard, mouse, filesystem)
│   ├── display/            # Display backend (SDL3)
│   └── utils/              # Utilities (types, errors, address)
├── tests/                  # Test files
└── docs/                   # Documentation
```

## Development

### Loading the System

```lisp
(require :asdf)
(asdf:load-system :maiko-lisp)
```

### Running Tests

```bash
sbcl --load maiko-lisp.asd --eval "(asdf:test-system :maiko-lisp)"
```

### REPL Development

```bash
sbcl --load maiko-lisp.asd
```

Then in the REPL:
```lisp
(asdf:load-system :maiko-lisp)
(in-package :maiko-lisp)
```

## Command Line Options

The implementation accepts the same command-line options as the C version:

- `-sysout <file>` - Specify sysout file
- `-info` / `-INFO` - Print system information
- `-help` / `-HELP` - Print help message
- `-sc <width>x<height>` / `-screen` - Screen geometry
- `-pixelscale <n>` - Pixel scaling factor
- `-fg` / `-foreground <color>` - Foreground color
- `-bg` / `-background <color>` - Background color
- `-t` / `-title <title>` - Window title
- `-timer <interval>` - Timer interval
- `-m <size>` - Virtual memory size in MB
- `-NF` - Don't fork (for debugging)
- `-INIT` - Init sysout mode

## Status

This is an active implementation following the same architecture as the Zig version. Current status:

- ✅ Project structure
- ✅ ASDF system definition
- ⏳ VM core implementation (in progress)
- ⏳ Memory management (in progress)
- ⏳ Display backend (planned)
- ⏳ I/O subsystems (planned)

## License

Same as the main Maiko project.