# Build API Contracts: Unified Build System

**Date**: 2025-01-27
**Feature**: Unified Build System
**Phase**: Phase 1 - Design & Contracts

## Overview

This document defines the command-line and function interfaces for the unified build system. Since the system is implemented as shell scripts, "APIs" are command-line interfaces and shell function signatures.

## Command-Line Interfaces

### build-all-emulators.sh

Main orchestrator script that builds all available emulators.

**Usage**:
```bash
build-all-emulators.sh [OPTIONS]
```

**Options**:
- `--emulator <c|zig|lisp>`: Build only specified emulator (default: build all)
- `--platform <os.arch>`: Target platform (default: auto-detect)
- `--display-backend <x11|sdl2|sdl3>`: Display backend (default: x11 for C, sdl2 for Zig, sdl3 for Lisp)
- `--release <version>`: Release version for C emulator (default: 351)
- `--network-type <type>`: Network type for C emulator (default: NONE)
- `--optimize <debug|release>`: Optimization level (default: release)
- `--force`: Force rebuild even if executables exist
- `--skip-prereq-check`: Skip prerequisite checking
- `--verbose`: Verbose output
- `--help`: Show help message

**Exit Codes**:
- `0`: Success (all requested emulators built)
- `1`: General error
- `2`: Prerequisite missing
- `3`: Build failure
- `4`: Invalid arguments

**Output**:
- Success: Executables in `maiko/build/<emulator>/<os>.<arch>/`
- Errors: Error messages to stderr

---

### build-emulator.sh

Build a specific emulator.

**Usage**:
```bash
build-emulator.sh --emulator <c|zig|lisp> [OPTIONS]
```

**Options**:
- `--emulator <c|zig|lisp>`: **Required**. Emulator to build
- `--platform <os.arch>`: Target platform (default: auto-detect)
- `--display-backend <x11|sdl2|sdl3>`: Display backend
- `--release <version>`: Release version (C emulator only)
- `--network-type <type>`: Network type (C emulator only)
- `--optimize <debug|release>`: Optimization level
- `--force`: Force rebuild
- `--verbose`: Verbose output
- `--help`: Show help

**Exit Codes**:
- `0`: Success
- `1`: General error
- `2`: Prerequisite missing
- `3`: Build failure
- `4`: Invalid arguments

---

### build-c-emulator.sh

Build C emulator wrapper.

**Usage**:
```bash
build-c-emulator.sh [OPTIONS]
```

**Options**:
- `--build-system <cmake|make>`: Build system to use (default: auto-detect)
- `--platform <os.arch>`: Target platform (default: auto-detect)
- `--display-backend <x11|sdl2>`: Display backend (default: x11)
- `--release <version>`: Release version (default: 351)
- `--network-type <type>`: Network type (default: NONE)
- `--output-dir <path>`: Output directory (default: maiko/build/c/<os>.<arch>/)
- `--force`: Force rebuild
- `--verbose`: Verbose output

**Exit Codes**: Same as build-emulator.sh

---

### build-zig-emulator.sh

Build Zig emulator wrapper.

**Usage**:
```bash
build-zig-emulator.sh [OPTIONS]
```

**Options**:
- `--platform <os.arch>`: Target platform (default: auto-detect)
- `--display-backend <sdl2>`: Display backend (default: sdl2)
- `--optimize <debug|release>`: Optimization level (default: release)
- `--output-dir <path>`: Output directory (default: maiko/build/zig/<os>.<arch>/)
- `--force`: Force rebuild
- `--verbose`: Verbose output

**Exit Codes**: Same as build-emulator.sh

---

### build-lisp-emulator.sh

Build Lisp emulator wrapper.

**Usage**:
```bash
build-lisp-emulator.sh [OPTIONS]
```

**Options**:
- `--platform <os.arch>`: Target platform (default: auto-detect)
- `--display-backend <sdl3>`: Display backend (default: sdl3)
- `--output-dir <path>`: Output directory (default: maiko/build/lisp/<os>.<arch>/)
- `--force`: Force rebuild
- `--verbose`: Verbose output

**Exit Codes**: Same as build-emulator.sh

---

## Function Interfaces (common.sh)

### detect_platform()

Detect current platform.

**Signature**:
```bash
detect_platform() -> os.arch
```

**Returns**: Platform string in format `<os>.<arch>` (e.g., `"linux.x86_64"`)

**Implementation**:
- Uses `maiko/bin/osversion` and `maiko/bin/machinetype` if available
- Falls back to `uname` if Maiko utilities not available
- Returns platform string

---

### check_prerequisites()

Check if prerequisites are available for an emulator.

**Signature**:
```bash
check_prerequisites(emulator_type) -> exit_code
```

**Parameters**:
- `emulator_type`: One of `"c"`, `"zig"`, or `"lisp"`

**Returns**: Exit code
- `0`: All prerequisites available
- `1`: One or more prerequisites missing

**Output**: Error messages to stderr for missing prerequisites

---

### is_build_needed()

Check if emulator needs to be built.

**Signature**:
```bash
is_build_needed(emulator_type, platform, [force]) -> exit_code
```

**Parameters**:
- `emulator_type`: Emulator type
- `platform`: Platform identifier
- `force`: Optional flag to force rebuild

**Returns**: Exit code
- `0`: Build needed
- `1`: Build not needed (up to date)

**Logic**:
- If `force` flag set, return 0 (build needed)
- Check if executables exist in output directory
- Compare executable timestamps with source directory
- Return 0 if sources newer or executables missing

---

### build_emulator()

Build an emulator.

**Signature**:
```bash
build_emulator(emulator_type, platform, options...) -> exit_code
```

**Parameters**:
- `emulator_type`: Emulator to build
- `platform`: Target platform
- `options...`: Build options (display backend, release version, etc.)

**Returns**: Exit code
- `0`: Build successful
- `1`: Build failed

**Implementation**:
- Calls appropriate build wrapper (`build-c-emulator.sh`, etc.)
- Passes options through
- Returns exit code from build

---

### find_emulator_executable()

Find emulator executable for use by loadup scripts.

**Signature**:
```bash
find_emulator_executable(emulator_type, platform, executable_name) -> path
```

**Parameters**:
- `emulator_type`: Emulator type
- `platform`: Platform identifier
- `executable_name`: Name of executable (`"lde"`, `"maiko-zig"`, etc.)

**Returns**: Path to executable, or empty string if not found

**Search Order**:
1. `maiko/build/<emulator>/<platform>/<executable_name>`
2. Existing Maiko location resolution (for backward compatibility)

---

## Integration with Loadup Scripts

### Modified loadup-all.sh

**New Options**:
- `--emulator <c|zig|lisp>`: Use specified emulator (default: c)
- `--auto-build`: Automatically build emulator if not found (default: true)
- `--no-auto-build`: Don't automatically build emulator

**Behavior**:
- If `--emulator` specified, use that emulator
- Check `MEDLEY_EMULATOR` environment variable if `--emulator` not specified
- Default to C emulator if neither specified
- If `--auto-build` (default), check if emulator is built, build if needed
- Use `find_emulator_executable()` to locate emulator
- Fall back to existing location resolution for backward compatibility

---

## Error Handling

All scripts and functions should:
- Return appropriate exit codes
- Write error messages to stderr
- Provide actionable error messages
- Continue building other emulators if one fails (when building all)

---

## Environment Variables

- `MEDLEY_EMULATOR`: Default emulator to use (`c`, `zig`, `lisp`)
- `MAIKODIR`: Maiko directory (existing, for backward compatibility)
- `MEDLEYDIR`: Medley directory (existing)
- `LOADUP_WORKDIR`: Loadup work directory (existing)

---

## Notes

- All paths should be absolute or relative to repository root
- Scripts should be executable and have proper shebang (`#!/usr/bin/env bash`)
- Functions in `common.sh` should be sourced by other scripts
- Error messages should be clear and actionable
- All scripts should support `--help` option
