# Quick Start: Emulator Runner Scripts

**Date**: 2026-01-12
**Feature**: 004-emulator-runner

## Overview

The emulator runner scripts allow you to run Interlisp with your choice of emulator implementation (C, Zig, or Lisp).

## Basic Usage

### Run with C Emulator (Default)

```bash
./run-medley
```

### Run with Specific Emulator

```bash
./run-medley --emulator zig
./run-medley --emulator lisp
```

### Set Default Emulator

```bash
export MEDLEY_EMULATOR=zig
./run-medley  # Uses Zig emulator
```

### Auto-Build Missing Emulators

```bash
./run-medley --emulator zig --auto-build
```

## Examples

### Development Workflow

```bash
# Test with different emulators
./run-medley --emulator c
./run-medley --emulator zig
./run-medley --emulator lisp

# Use environment variable for frequent use
export MEDLEY_EMULATOR=zig
./run-medley  # Always uses Zig
```

### Troubleshooting

```bash
# If emulator is missing
./run-medley --emulator zig --auto-build

# Override environment setting
MEDLEY_EMULATOR=c ./run-medley --emulator zig
```

## Prerequisites

- Emulators built via unified build system (spec 003)
- Or use `--auto-build` flag
- Existing Medley installation

## Error Messages

### Invalid Emulator

```
Error: Invalid emulator type 'invalid'. Valid options: c, zig, lisp
```

### Missing Emulator

```
Error: Emulator 'zig' not found at maiko/build/zig/linux.x86_64/zaiko
Use --auto-build to build automatically.
```

### Concurrent Run

```
Error: Another Interlisp session is running (PID: 12345).
Please wait for it to finish or use a different emulator.
```

## Platform Notes

- **Linux**: Uses `linux.x86_64` build directory
- **macOS**: Uses `darwin.x86_64` or `darwin.arm64` based on architecture
- All existing run-medley options remain available

## Backward Compatibility

All existing functionality is preserved. The `--emulator` flag is optional and defaults to the C emulator.
