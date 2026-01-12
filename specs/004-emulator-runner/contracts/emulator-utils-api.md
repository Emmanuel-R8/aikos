# Emulator Utilities API Contract

**Date**: 2025-01-27
**Feature**: 004-emulator-runner
**Module**: `medley/scripts/medley/emulator_utils.sh`

## Overview

This document defines the function signatures and interfaces for emulator selection, validation, and locking utilities used by Medley run scripts.

## Function Contracts

### `select_emulator [--emulator TYPE] [--env-var VAR]`

**Purpose**: Determine emulator selection based on command-line arguments, environment variables, and defaults.

**Parameters**:
- `--emulator TYPE` (optional): Command-line emulator selection (`c`, `zig`, or `lisp`)
- `--env-var VAR` (optional): Environment variable name (default: `MEDLEY_EMULATOR`)

**Returns**: 
- Sets shell variable `selected_emulator` to `c`, `zig`, or `lisp`
- Sets shell variable `selection_source` to `command-line`, `environment`, or `default`
- Exit code: 0 on success, 1 on invalid selection

**Precedence**: Command-line > Environment > Default (`c`)

**Error Handling**:
- Invalid emulator type → Error message (user-friendly + technical), exit code 1
- Error messages follow FR-005 format

**Example**:
```bash
select_emulator --emulator zig
# Sets: selected_emulator=zig, selection_source=command-line

select_emulator --env-var MEDLEY_EMULATOR
# Sets: selected_emulator=lisp (if MEDLEY_EMULATOR=lisp), selection_source=environment
```

---

### `resolve_emulator_path EMULATOR_TYPE PLATFORM`

**Purpose**: Resolve absolute path to emulator executable using location resolution order.

**Parameters**:
- `EMULATOR_TYPE` (required): `c`, `zig`, or `lisp`
- `PLATFORM` (required): Platform identifier (`<os>.<arch>` format)

**Returns**:
- Sets shell variable `emulator_path` to absolute path of executable
- Sets shell variable `location_source` to `unified-build`, `existing-maiko`, or `path`
- Sets shell variable `executable_name` to name of executable found
- Exit code: 0 on success, 1 on not found

**Location Resolution Order** (FR-006, FR-007):
1. `maiko/build/<emulator>/<platform>/<executable_name>`
2. Existing Maiko location resolution (existing script logic)
3. System PATH search

**Error Handling**:
- Emulator not found → Error message (user-friendly + technical with searched paths), exit code 1
- Error messages follow FR-009 format

**Example**:
```bash
resolve_emulator_path zig linux.x86_64
# Sets: emulator_path=/path/to/maiko/build/zig/linux.x86_64/zaiko
#       location_source=unified-build
#       executable_name=zaiko
```

---

### `validate_emulator_executable EXECUTABLE_PATH`

**Purpose**: Validate emulator executable file before execution.

**Parameters**:
- `EXECUTABLE_PATH` (required): Absolute path to emulator executable

**Returns**:
- Exit code: 0 on valid, 1 on invalid

**Validation Checks** (FR-015):
1. File exists
2. File is executable (executable bit set)
3. File size > 0
4. File header/magic bytes match expected executable format

**Error Handling**:
- Validation failure → Error message (user-friendly + technical with file path and specific failure), exit code 1
- Error messages follow FR-015 format

**Example**:
```bash
validate_emulator_executable /path/to/zaiko
# Exit code: 0 if valid, 1 if invalid
```

---

### `acquire_lock [--lock-dir DIR]`

**Purpose**: Acquire user-specific lock file to prevent concurrent runs.

**Parameters**:
- `--lock-dir DIR` (optional): Directory for lock file (default: `medley/`)

**Returns**:
- Sets shell variable `lock_file_path` to path of lock file
- Creates lock file with PID and timestamp
- Exit code: 0 on success, 1 on active lock detected

**Lock File Format**:
- Path: `<lock-dir>/.medley-<username>.lock`
- Content: `PID:timestamp` (single line)
- Username: From `$USER` or `id -un`

**Stale Lock Handling** (FR-017):
- Lock file age > 60 seconds → Auto-remove with warning message, proceed
- Lock file age < 60 seconds → Error message, exit code 1

**Error Handling**:
- Active lock detected → Error message (user-friendly + technical with lock file path and PID), exit code 1
- Stale lock removed → Warning message (user-friendly + technical), continue
- Error messages follow FR-016, FR-017 format

**Example**:
```bash
acquire_lock --lock-dir /path/to/medley
# Sets: lock_file_path=/path/to/medley/.medley-username.lock
# Creates lock file with current PID and timestamp
```

---

### `release_lock [--lock-file PATH]`

**Purpose**: Release lock file on normal exit.

**Parameters**:
- `--lock-file PATH` (optional): Path to lock file (default: value of `lock_file_path` variable)

**Returns**:
- Removes lock file
- Exit code: 0 on success, 1 on error (non-fatal)

**Error Handling**:
- Lock file removal failure → Warning message (non-fatal), exit code 1
- Lock file doesn't exist → Silent success, exit code 0

**Implementation Notes**:
- Should be called in exit trap or cleanup function
- Non-fatal errors don't abort execution

**Example**:
```bash
release_lock --lock-file /path/to/medley/.medley-username.lock
# Removes lock file
```

---

### `build_emulator_if_missing EMULATOR_TYPE PLATFORM [--build-dir DIR]`

**Purpose**: Build emulator automatically if missing (when `--auto-build` flag used).

**Parameters**:
- `EMULATOR_TYPE` (required): `c`, `zig`, or `lisp`
- `PLATFORM` (required): Platform identifier (`<os>.<arch>` format)
- `--build-dir DIR` (optional): Build scripts directory (default: `medley/scripts/build/`)

**Returns**:
- Invokes appropriate build script from spec 003
- Exit code: 0 on success, 1 on build failure

**Build Scripts** (from spec 003):
- C: `medley/scripts/build/build-c-emulator.sh`
- Zig: `medley/scripts/build/build-zig-emulator.sh`
- Lisp: `medley/scripts/build/build-lisp-emulator.sh`

**Error Handling**:
- Build failure → Error message (user-friendly + technical with build output), exit code 1
- Build script not found → Error message, exit code 1
- Error messages follow FR-008 format

**Example**:
```bash
build_emulator_if_missing zig linux.x86_64
# Invokes: medley/scripts/build/build-zig-emulator.sh --platform linux.x86_64
```

---

## Integration Points

### With Existing Scripts

**run-medley**:
- Sources `emulator_utils.sh`
- Calls `select_emulator` during argument parsing
- Calls `resolve_emulator_path` after selection
- Calls `validate_emulator_executable` before execution
- Calls `acquire_lock` before execution
- Calls `release_lock` in exit trap

**medley.command**:
- Same integration pattern as `run-medley`

**medley_run.sh**:
- Same integration pattern as `run-medley`

### With Unified Build System (Spec 003)

- `resolve_emulator_path` checks `maiko/build/<emulator>/<platform>/` first
- `build_emulator_if_missing` invokes build scripts from `medley/scripts/build/`

## Error Message Format

All functions follow consistent error message format (from clarifications):

1. **User-friendly summary** (plain language explanation)
2. **Technical details** (file paths, error codes, specific validation failures)

Example:
```
Error: Invalid emulator selection 'invalid'

The emulator 'invalid' is not supported. Valid options are: c, zig, lisp.
Technical details: --emulator argument='invalid', valid options=['c', 'zig', 'lisp']
```

## Exit Codes

- `0`: Success
- `1`: Error (invalid input, validation failure, lock conflict, build failure)
- `2`: Reserved for existing Medley script error codes

## Environment Variables

- `MEDLEY_EMULATOR`: Default emulator selection (FR-002)
- `USER`: Username for lock file (fallback: `id -un`)
- `MEDLEYDIR`: Medley root directory (existing)

## Dependencies

- Platform detection: `detect-platform.sh` or `osversion`/`machinetype` utilities
- Build scripts: `medley/scripts/build/build-{c,zig,lisp}-emulator.sh` (spec 003)
- Standard Unix utilities: `file`, `stat`, `test`, `id`
