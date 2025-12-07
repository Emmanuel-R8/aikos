# Data Model: Emulator Runner Scripts for Interlisp

**Date**: 2025-01-27
**Feature**: Emulator Runner Scripts
**Phase**: Phase 1 - Design & Contracts

## Entities

### EmulatorSelection

Represents the user's choice of emulator with precedence rules.

**Attributes**:
- `emulator_type`: String - One of "c", "zig", "lisp"
- `source`: String - How selection was made: "command-line", "environment", "default"
- `precedence`: Integer - Selection priority (1 = command-line, 2 = environment, 3 = default)

**Validation Rules**:
- `emulator_type` MUST be one of: "c", "zig", "lisp"
- `source` MUST be one of: "command-line", "environment", "default"
- Command-line selection has highest precedence (1)
- Environment variable has medium precedence (2)
- Default has lowest precedence (3)

**State Transitions**:
- Initial: No selection
- Command-line provided → `source="command-line"`, `precedence=1`
- Environment variable set (no command-line) → `source="environment"`, `precedence=2`
- No selection → `source="default"`, `precedence=3`, `emulator_type="c"`

---

### EmulatorExecutable

Represents the emulator executable to be invoked, with location resolution logic.

**Attributes**:
- `emulator_type`: String - Emulator type ("c", "zig", "lisp")
- `executable_name`: String - Name of executable (e.g., "lde", "maiko-zig", "maiko-lisp")
- `platform`: String - Platform identifier (e.g., "linux.x86_64")
- `path`: String - Full path to executable
- `location_source`: String - Where executable was found: "unified-build", "maikodir", "path", "not-found"

**Validation Rules**:
- `executable_name` MUST match expected name for emulator type:
  - C: "lde", "ldeinit", "ldex", or "ldesdl"
  - Zig: "maiko-zig"
  - Lisp: "maiko-lisp"
- `path` MUST be absolute path if `location_source != "not-found"`
- `path` MUST point to executable file (permissions checked)
- Location resolution order: unified-build → maikodir → path → not-found

**State Transitions**:
- Search unified build location → If found: `location_source="unified-build"`
- Search MAIKODIR → If found: `location_source="maikodir"`
- Search PATH → If found: `location_source="path"`
- Not found → `location_source="not-found"`, `path=""`

---

### RunConfiguration

Represents the complete runtime configuration for running Interlisp.

**Attributes**:
- `emulator_selection`: EmulatorSelection - Selected emulator
- `emulator_executable`: EmulatorExecutable - Executable to invoke
- `sysout_file`: String - Path to sysout file (optional, may use default)
- `display_backend`: String - Display backend (X11, SDL2, SDL3) - inferred from emulator
- `memory_settings`: Object - Memory configuration (size, etc.)
- `display_settings`: Object - Display configuration (geometry, screen size, etc.)
- `other_options`: Array - Additional Medley options passed through

**Validation Rules**:
- `emulator_selection` MUST be valid EmulatorSelection
- `emulator_executable` MUST be valid EmulatorExecutable with `location_source != "not-found"`
- `sysout_file` MUST exist if specified
- All existing Medley run script options MUST be preserved

**State Transitions**:
- Configuration created → Validate all attributes
- Invalid configuration → Error state, do not proceed
- Valid configuration → Ready to execute

---

### RunLock

Represents the lock mechanism preventing concurrent Interlisp runs.

**Attributes**:
- `lock_file_path`: String - Path to lock file
- `process_id`: Integer - PID of process holding lock
- `lock_age`: Integer - Age of lock in seconds
- `is_stale`: Boolean - Whether lock is considered stale (>5 minutes or process dead)

**Validation Rules**:
- `lock_file_path` MUST be in accessible directory ($HOME/.medley/ or $TMPDIR/medley/)
- `process_id` MUST be valid PID if lock exists
- `is_stale` is true if `lock_age > 300` (5 minutes) OR process with `process_id` does not exist
- Lock MUST be released when script exits (trap handler)

**State Transitions**:
- No lock → Check for existing lock file
- Lock exists → Check if stale
- Stale lock → Remove lock file, proceed
- Active lock → Block or error (unless --override-lock)
- Lock acquired → Create lock file with current PID
- Script exit → Release lock (trap handler)

---

## Relationships

- **EmulatorSelection** → **EmulatorExecutable**: One selection resolves to one executable (1:1)
- **RunConfiguration** → **EmulatorSelection**: Contains one selection (1:1)
- **RunConfiguration** → **EmulatorExecutable**: Contains one executable (1:1)
- **RunConfiguration** → **RunLock**: May have one active lock (1:0..1)

## Validation Rules Summary

1. Emulator type validation: MUST be "c", "zig", or "lisp"
2. Executable existence: MUST exist and be executable before invocation
3. Platform detection: MUST succeed or provide clear error
4. Lock validation: MUST check for stale locks before blocking
5. Configuration completeness: All required attributes MUST be set before execution

