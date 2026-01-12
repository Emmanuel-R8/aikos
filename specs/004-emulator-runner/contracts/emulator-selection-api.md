# Contract: Emulator Selection API

**Date**: 2026-01-12
**Feature**: 004-emulator-runner

## Overview

This contract defines the command-line interface for selecting and running Interlisp emulators.

## Command-Line Interface

### run-medley Command

**Usage**:

```bash
./run-medley [options] [--emulator <type>] [sysout-file]
```

**New Options**:

- `--emulator <type>`: Select emulator (c, zig, lisp)
- `--auto-build`: Automatically build emulator if missing

**Existing Options Preserved**:

- All current run-medley options remain functional

### Environment Variables

- `MEDLEY_EMULATOR`: Default emulator selection (c, zig, lisp)

### Precedence Rules

1. Command-line `--emulator` flag (highest priority)
2. `MEDLEY_EMULATOR` environment variable
3. Default to 'c' emulator (lowest priority)

## Request/Response Format

### Input Validation

**Valid Inputs**:

- Emulator types: `c`, `zig`, `lisp` (case insensitive)
- Auto-build flag: `--auto-build` (optional)

**Invalid Inputs**:

- Unknown emulator types → Error exit code 1 with message
- Missing emulator when `--auto-build` not specified → Error exit code 1

### Output Format

**Success Output**:

- Interlisp starts normally
- No additional output from emulator selection

**Error Output**:

```
Error: Invalid emulator type 'invalid'. Valid options: c, zig, lisp
Error: Emulator 'zig' not found. Use --auto-build to build automatically.
Error: Another Interlisp session is running. Please wait or use a different emulator.
```

**Exit Codes**:

- 0: Success (Interlisp started)
- 1: Invalid arguments or missing emulator
- 2: Emulator not found or corrupted
- 3: Concurrent run detected
- 4: Build failed (with --auto-build)

## Platform-Specific Behavior

### Linux

- Unified build path: `maiko/build/<emulator>/linux.x86_64/`
- Fallback: Existing Maiko paths

### macOS

- Unified build path: `maiko/build/<emulator>/darwin.x86_64/` or `darwin.arm64/`
- Fallback: Existing Maiko paths

### Emulator Naming

- C emulator: `lde`, `ldex`, `ldesdl`
- Zig emulator: `zaiko`
- Lisp emulator: `laiko`

## Error Handling

### Validation Errors

- Check executable permissions
- Verify file exists and size > 0
- Validate executable header

### Lock Management

- Create lock file: `medley/.medley-<username>.lock`
- Content: `PID=<pid>\nTIMESTAMP=<unix-timestamp>`
- Auto-remove locks > 1 minute old

### Build Integration

- Trigger build command for missing emulators
- Wait for build completion
- Retry emulator location after build

## Backward Compatibility

- All existing command-line options work unchanged
- Default behavior (no --emulator) uses C emulator
- Existing scripts continue to function
