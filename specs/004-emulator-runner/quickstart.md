# Quick Start: Emulator Runner Scripts

**Date**: 2026-01-15
**Feature**: 004-emulator-runner

## Overview

This feature covers:

- Running Medley Interlisp with your choice of emulator implementation (C, Zig, or Lisp)
- Comparing C vs Zig execution traces and iterating until Zig matches C (staged: `starter.sysout` first, then `full.sysout`)

## Where to Run

These examples assume you run commands from the Medley directory:

```bash
cd medley
```

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

## Execution Trace Parity (C vs Zig)

### Stage 1: `starter.sysout`

From repo root:

```bash
./scripts/generate_debug_logs.sh medley/internal/loadups/starter.sysout
./scripts/compare_debug_logs.sh c_emulator_execution_log.txt zig_emulator_execution_log.txt
python3 ./scripts/analyze_execution_divergence.py c_emulator_execution_log.txt zig_emulator_execution_log.txt
```

As parity improves, prefer tooling that can auto-skip the already-matching prefix (longest common prefix / LCP) and focus on the next divergence.

#### Fast iteration: cap execution steps (`EMULATOR_MAX_STEPS`)

For faster iteration (especially when the logs get long), set `EMULATOR_MAX_STEPS` to cap how many instructions are executed/traced by **both** emulators:

```bash
EMULATOR_MAX_STEPS=1000 ./scripts/compare_emulator_execution.sh medley/internal/loadups/starter.sysout
```

Unset it (or set it to `0`) to run “to completion”.

### Stage 2: `full.sysout` (after Stage 1 completion parity)

```bash
./scripts/generate_debug_logs.sh medley/loadups/full.sysout
./scripts/compare_debug_logs.sh c_emulator_execution_log.txt zig_emulator_execution_log.txt
python3 ./scripts/analyze_execution_divergence.py c_emulator_execution_log.txt zig_emulator_execution_log.txt
```
