# Quickstart: Unified Build System for Medley

**Date**: 2025-01-27
**Feature**: Unified Build System
**Phase**: Phase 1 - Design & Contracts

## Overview

This guide helps developers get started with the unified build system for Medley that supports building all three Maiko emulator implementations (C, Zig, Lisp) and integrates emulator builds into the Medley loadup workflow.

## Prerequisites

### Required Tools

- **C Emulator**: C compiler (clang or gcc), CMake or Make, X11 or SDL2 libraries
- **Zig Emulator**: Zig compiler 0.11+, SDL2 libraries
- **Lisp Emulator**: SBCL (Steel Bank Common Lisp), SDL3 libraries

### Installation

**Linux (Ubuntu/Debian)**:
```bash
# C emulator prerequisites
sudo apt install clang make cmake libx11-dev libsdl2-dev

# Zig emulator prerequisites
# Download from https://ziglang.org/download/
# Or: sudo apt install zig  # if available

# Lisp emulator prerequisites
sudo apt install sbcl
# SDL3 may need to be built from source
```

**macOS**:
```bash
# C emulator prerequisites
brew install cmake sdl2

# Zig emulator prerequisites
brew install zig

# Lisp emulator prerequisites
brew install sbcl sdl3
```

## Quick Start

### 1. Build All Emulators

Build all three emulators from a single command:

```bash
cd medley/scripts/build
./build-all-emulators.sh
```

This will:
- Detect your platform automatically
- Check prerequisites for each emulator
- Build all available emulators
- Place executables in `maiko/build/<emulator>/<os>.<arch>/`

### 2. Build a Specific Emulator

Build only the C emulator:

```bash
./build-all-emulators.sh --emulator c
```

Build only the Zig emulator:

```bash
./build-all-emulators.sh --emulator zig
```

### 3. Use Emulator in Medley Loadup

Run a Medley loadup using a specific emulator:

```bash
cd medley/scripts
./loadup-all.sh --emulator zig
```

The system will:
- Automatically build the Zig emulator if not already built
- Use the Zig emulator for the loadup
- Create sysout files as usual

### 4. Set Default Emulator

Set environment variable to use Lisp emulator by default:

```bash
export MEDLEY_EMULATOR=lisp
./loadup-all.sh  # Will use Lisp emulator
```

## Common Workflows

### Development Workflow

1. **Build emulator you're working on**:
   ```bash
   ./build-emulator.sh --emulator zig --force
   ```

2. **Test with Medley loadup**:
   ```bash
   ./loadup-all.sh --emulator zig
   ```

3. **Iterate**: Make changes, rebuild, test again

### Release Workflow

1. **Build all emulators for release**:
   ```bash
   ./build-all-emulators.sh --optimize release
   ```

2. **Verify all emulators work**:
   ```bash
   ./loadup-all.sh --emulator c
   ./loadup-all.sh --emulator zig
   ./loadup-all.sh --emulator lisp
   ```

### Cross-Platform Testing

Build for a specific platform:

```bash
./build-all-emulators.sh --platform linux.x86_64
```

## Build Options

### Display Backend

Specify display backend (C emulator):

```bash
./build-all-emulators.sh --emulator c --display-backend sdl2
```

### Release Version

Specify release version (C emulator only):

```bash
./build-all-emulators.sh --emulator c --release 351
```

### Optimization Level

Build with debug symbols:

```bash
./build-all-emulators.sh --optimize debug
```

## Output Locations

Built executables are placed in:

```
maiko/build/
├── c/
│   └── <os>.<arch>/
│       ├── lde
│       ├── ldeinit
│       └── ldex
├── zig/
│   └── <os>.<arch>/
│       └── zaiko
└── lisp/
    └── <os>.<arch>/
        └── laiko
```

## Troubleshooting

### Prerequisites Missing

If you see errors about missing prerequisites:

```bash
# Check what's missing
./check-prerequisites.sh --emulator zig

# Install missing tools
# Then rebuild
```

### Build Failures

If a build fails:

1. **Check error messages**: They should indicate what went wrong
2. **Verify prerequisites**: Make sure all required tools are installed
3. **Try verbose mode**: `./build-all-emulators.sh --verbose --emulator <type>`
4. **Force rebuild**: `./build-all-emulators.sh --force --emulator <type>`

### Platform Detection Issues

If platform detection fails:

```bash
# Manually specify platform
./build-all-emulators.sh --platform linux.x86_64
```

## Integration with Existing Workflows

The unified build system is designed to be backward compatible:

- **Existing loadup scripts**: Continue to work as before
- **Existing Maiko locations**: Still supported (PATH, MAIKODIR, etc.)
- **New unified location**: Added to search order, doesn't break existing behavior

## Next Steps

- See [Build API Contracts](contracts/build-api.md) for detailed API documentation
- See [Data Model](data-model.md) for entity definitions
- See [Research](research.md) for design decisions

## Related Documentation

- [Feature Specification](spec.md) - Complete feature requirements
- [Implementation Plan](plan.md) - Technical implementation plan
- [Maiko Build System Documentation](../../.ai_assistant_db/build-system.md) - Existing build system docs
