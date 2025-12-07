# API Contracts: Emulator Runner Scripts

**Date**: 2025-01-27
**Feature**: Emulator Runner Scripts
**Phase**: Phase 1 - Design & Contracts

## Command-Line Interface

### run-medley

**Script**: `medley/run-medley`

**New Arguments**:
- `--emulator <c|zig|lisp>`: Specify emulator to use
- `--auto-build`: Automatically build emulator if missing
- `--override-lock`: Override lock file to allow concurrent runs

**Existing Arguments** (preserved):
- All existing arguments remain unchanged (sysout selection, display options, memory settings, etc.)

**Exit Codes**:
- `0`: Success - Interlisp started successfully
- `1`: General error (invalid arguments, configuration error)
- `2`: Emulator not found (and --auto-build not specified)
- `3`: Emulator build failed (when --auto-build specified)
- `4`: Lock file held (and --override-lock not specified)
- `5`: Emulator executable validation failed (corrupted, not executable)
- `53`: Maiko directory not found (existing error code)
- `54`: Maiko executable not found (existing error code)

**Error Messages**:
- Invalid emulator: "Error: Invalid emulator type: <type>. Must be one of: c, zig, lisp"
- Emulator not found: "Error: Emulator '<type>' not found. Use --auto-build to build it, or run: ./scripts/build/build-emulator.sh --emulator <type>"
- Build failed: "Error: Failed to build <type> emulator. Check build output above for details."
- Lock held: "Error: Another Medley instance is running (PID: <pid>). Use --override-lock to override, or wait for it to finish."

---

### medley.command / medley_run.sh

**Scripts**: `medley/scripts/medley/medley.command`, `medley/scripts/medley/medley_run.sh`

**New Arguments**:
- `--emulator <c|zig|lisp>`: Specify emulator to use
- `--auto-build`: Automatically build emulator if missing
- `--override-lock`: Override lock file to allow concurrent runs

**Existing Arguments** (preserved):
- All existing arguments remain unchanged

**Exit Codes**: Same as run-medley (see above)

**Error Messages**: Same as run-medley (see above)

---

## Function Interfaces

### select_emulator()

**Purpose**: Determine which emulator to use based on command-line, environment, and defaults.

**Signature**:
```bash
select_emulator() -> (emulator_type, source)
```

**Parameters**: None (reads from command-line args and environment)

**Returns**:
- `emulator_type`: String - "c", "zig", or "lisp"
- `source`: String - "command-line", "environment", or "default"

**Preconditions**:
- Command-line arguments parsed (if any)
- Environment variables accessible

**Postconditions**:
- Returns valid emulator type
- Source indicates selection precedence

**Errors**:
- Invalid emulator type → Exit with code 1, error message

---

### find_emulator_executable()

**Purpose**: Locate emulator executable using unified build location and fallback logic.

**Signature**:
```bash
find_emulator_executable(emulator_type, platform, executable_name) -> path
```

**Parameters**:
- `emulator_type`: String - "c", "zig", or "lisp"
- `platform`: String - Platform identifier (e.g., "linux.x86_64")
- `executable_name`: String - Name of executable to find

**Returns**:
- `path`: String - Full path to executable, or empty string if not found

**Preconditions**:
- `emulator_type` is valid
- `platform` is valid (from detect_platform)
- `executable_name` matches expected name for emulator type

**Postconditions**:
- If found: Returns absolute path to executable
- If not found: Returns empty string

**Search Order**:
1. Unified build location: `maiko/build/<emulator>/<platform>/<executable_name>`
2. MAIKODIR: `$MAIKODIR/<platform>/<executable_name>`
3. MEDLEYDIR relative: `$MEDLEYDIR/../maiko/<platform>/<executable_name>` or `$MEDLEYDIR/maiko/<platform>/<executable_name>`
4. PATH: `command -v <executable_name>`

---

### validate_emulator_executable()

**Purpose**: Proactively validate emulator executable before attempting to run.

**Signature**:
```bash
validate_emulator_executable(executable_path) -> success
```

**Parameters**:
- `executable_path`: String - Path to executable file

**Returns**:
- `success`: Boolean - True if executable is valid, false otherwise

**Preconditions**:
- `executable_path` is non-empty

**Postconditions**:
- Returns true if file exists, is executable, and passes basic validation
- Returns false if file is missing, not executable, or corrupted

**Validation Checks**:
1. File exists
2. File is executable (`test -x`)
3. File is not empty (basic corruption check)
4. File is regular file (not directory, symlink issues)

**Errors**:
- Missing file → Returns false, error message to stderr
- Not executable → Returns false, error message with chmod suggestion
- Corrupted/empty → Returns false, error message suggesting rebuild

---

### acquire_run_lock()

**Purpose**: Acquire lock file to prevent concurrent Interlisp runs.

**Signature**:
```bash
acquire_run_lock(run_id, override_flag) -> success
```

**Parameters**:
- `run_id`: String - Run identifier (for lock file naming)
- `override_flag`: Boolean - Whether to override existing lock

**Returns**:
- `success`: Boolean - True if lock acquired, false otherwise

**Preconditions**:
- Lock file directory is accessible

**Postconditions**:
- If success: Lock file created with current PID
- If failure: Lock file not created, error message displayed

**Lock File**:
- Location: `$HOME/.medley/.medley-run-<run_id>.lock` or `$TMPDIR/medley/.medley-run-<run_id>.lock`
- Contents: Process ID (PID) as integer
- Stale detection: Lock age > 5 minutes OR process not running

**Errors**:
- Lock held by active process → Returns false, error message with PID
- Override flag set → Removes existing lock, creates new lock

---

### release_run_lock()

**Purpose**: Release lock file when script exits.

**Signature**:
```bash
release_run_lock(run_id)
```

**Parameters**:
- `run_id`: String - Run identifier (for lock file naming)

**Returns**: None

**Preconditions**:
- Lock file exists (created by acquire_run_lock)

**Postconditions**:
- Lock file removed if owned by current process

**Note**: Should be called via trap handler on script exit

---

### build_emulator_if_needed()

**Purpose**: Automatically build emulator if missing and --auto-build flag is set.

**Signature**:
```bash
build_emulator_if_needed(emulator_type, platform, auto_build_flag) -> success
```

**Parameters**:
- `emulator_type`: String - "c", "zig", or "lisp"
- `platform`: String - Platform identifier
- `auto_build_flag`: Boolean - Whether auto-build is enabled

**Returns**:
- `success`: Boolean - True if emulator is available (built or already exists), false otherwise

**Preconditions**:
- Build system scripts available (`medley/scripts/build/build-emulator.sh`)
- Platform detection successful

**Postconditions**:
- If emulator exists: Returns true immediately
- If auto-build enabled and emulator missing: Builds emulator, returns build result
- If auto-build disabled and emulator missing: Returns false

**Errors**:
- Build failure → Returns false, error message displayed
- Build script not found → Returns false, error message

---

## Integration Points

### With Unified Build System (Spec 003)

**Function**: `medley/scripts/build/build-emulator.sh`

**Usage**: Called when `--auto-build` flag is set and emulator is missing

**Interface**:
```bash
./scripts/build/build-emulator.sh --emulator <type> --platform <os>.<arch>
```

**Exit Codes**: 0 = success, non-zero = failure

---

### With Existing Medley Scripts

**Integration**: Modify existing emulator resolution logic in:
- `medley.command`: `check_unified_build_location()` function (already exists, enhance)
- `medley_run.sh`: `check_unified_build_location()` function (already exists, enhance)
- `run-medley`: Add unified build location checking (partially exists, complete)

**Backward Compatibility**: All existing Maiko location resolution must continue to work

---

## Error Handling Contract

All scripts MUST:
1. Return appropriate exit codes (see above)
2. Display error messages to stderr
3. Provide actionable guidance (suggested commands, flags)
4. Preserve existing error handling behavior for non-emulator errors

