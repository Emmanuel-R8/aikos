# Quickstart: Emulator Runner Scripts for Interlisp

**Date**: 2025-01-27
**Feature**: Emulator Runner Scripts
**Phase**: Phase 1 - Design & Contracts

## Overview

This guide helps developers get started with running Interlisp using their choice of emulators (C, Zig, or Lisp) via enhanced Medley run scripts.

## Prerequisites

### Required
- Medley repository cloned
- Unified build system (spec 003) implemented
- At least one emulator built (or ability to build via --auto-build)

### Optional
- Multiple emulators built for comparison testing
- SBCL installed (for Lisp emulator)

## Quick Start

### 1. Run Interlisp with Selected Emulator

Run Interlisp using a specific emulator:

```bash
cd medley
./run-medley --emulator zig
```

This will:
- Select the Zig emulator
- Locate it in the unified build location
- Start Interlisp using the Zig emulator

### 2. Set Default Emulator

Set your preferred emulator as the default:

```bash
export MEDLEY_EMULATOR=lisp
./run-medley  # Will use Lisp emulator
```

The command-line argument takes precedence:

```bash
export MEDLEY_EMULATOR=lisp
./run-medley --emulator zig  # Uses Zig (command-line overrides environment)
```

### 3. Auto-Build Missing Emulator

Automatically build an emulator if it's not already built:

```bash
./run-medley --emulator zig --auto-build
```

This will:
- Check if Zig emulator is built
- Build it automatically if missing
- Start Interlisp with the newly built emulator

### 4. Use Different Sysout Files

Emulator selection works with all existing Medley options:

```bash
./run-medley --emulator c --full    # Use C emulator with full.sysout
./run-medley --emulator zig --lisp # Use Zig emulator with lisp.sysout
./run-medley --emulator lisp --apps --emulator zig  # Use Zig emulator with apps.sysout
```

### 5. Override Lock (Concurrent Runs)

By default, only one Interlisp instance can run at a time. To override:

```bash
./run-medley --emulator c --override-lock
```

**Warning**: Concurrent runs may cause conflicts with display, file locks, or sysout access.

## Common Scenarios

### Scenario 1: Testing All Emulators

Test the same sysout with different emulators:

```bash
# Test with C emulator
./run-medley --emulator c --full

# Test with Zig emulator
./run-medley --emulator zig --full

# Test with Lisp emulator
./run-medley --emulator lisp --full
```

### Scenario 2: Development Workflow

Set default emulator for your development session:

```bash
# In your shell session
export MEDLEY_EMULATOR=zig

# All subsequent runs use Zig emulator
./run-medley --full
./run-medley --lisp
./run-medley  # Uses default sysout
```

### Scenario 3: Quick Testing

Quickly test a new emulator build:

```bash
# Build and run in one command
./run-medley --emulator zig --auto-build --full
```

## Troubleshooting

### Emulator Not Found

**Error**: "Error: Emulator 'zig' not found."

**Solution**:
```bash
# Option 1: Build manually
cd scripts/build
./build-emulator.sh --emulator zig

# Option 2: Use auto-build
./run-medley --emulator zig --auto-build
```

### Invalid Emulator Name

**Error**: "Error: Invalid emulator type: invalid. Must be one of: c, zig, lisp"

**Solution**: Use one of the valid emulator types: `c`, `zig`, or `lisp`

### Lock File Held

**Error**: "Error: Another Medley instance is running (PID: 12345)."

**Solution**:
```bash
# Option 1: Wait for other instance to finish
# Option 2: Override lock (use with caution)
./run-medley --emulator c --override-lock
```

### Emulator Not Executable

**Error**: "Error: Emulator executable is not executable: /path/to/zaiko"

**Solution**:
```bash
# Fix permissions
chmod +x /path/to/zaiko

# Or rebuild
cd scripts/build
./build-emulator.sh --emulator zig --force
```

## Advanced Usage

### Using medley.command (macOS)

On macOS, you can use the medley.command script:

```bash
cd medley/scripts/medley
./medley.command --emulator zig --full
```

### Using medley_run.sh

For advanced scenarios, you can source medley_run.sh:

```bash
source medley/scripts/medley/medley_run.sh
# Then use functions directly
```

### Platform-Specific Emulators

Some emulators may not be available on all platforms:

```bash
# Check if emulator is available
ls maiko/build/zig/$(./scripts/build/detect-platform.sh)/

# If not available, build it
./scripts/build/build-emulator.sh --emulator zig
```

## Integration with Loadup

The emulator runner integrates with the loadup system:

```bash
# Build sysout using specific emulator
cd scripts/loadups
./loadup-all.sh --emulator zig

# The loadup will use the Zig emulator for all stages
```

## See Also

- `medley/BUILDING.md`: General Medley building instructions
- `medley/scripts/build/README.md`: Unified build system documentation
- `specs/003-unified-build-system/`: Unified build system specification
- `specs/004-emulator-runner/spec.md`: Full feature specification
