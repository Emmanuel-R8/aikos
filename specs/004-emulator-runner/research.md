# Research: Emulator Runner Scripts for Interlisp

**Date**: 2025-01-27
**Feature**: Emulator Runner Scripts
**Phase**: Phase 0 - Outline & Research

## Research Questions

### 1. Existing Medley Run Script Architecture

**Question**: How do existing Medley run scripts (run-medley, medley.command, medley_run.sh) work and where should emulator selection be integrated?

**Research Findings**:

#### run-medley Script

- **Location**: `medley/run-medley`
- **Structure**: Simple shell script that sets up environment and invokes emulator
- **Emulator Resolution**:
  - Checks PATH for `lde` executable
  - Falls back to `MAIKODIR/<os>.<arch>/` directory
  - Already has partial unified build location checking (added in spec 003)
- **Key Functions**: Platform detection, PATH manipulation, direct emulator invocation
- **Integration Point**: Add `--emulator` argument parsing before emulator resolution

#### medley.command Script

- **Location**: `medley/scripts/medley/medley.command`
- **Structure**: Large compiled script (1600+ lines) combining multiple component scripts
- **Emulator Resolution**:
  - Uses `check_for_maiko_exe()` function to locate executables
  - Already has `check_unified_build_location()` function (added in spec 003)
  - Checks unified build location first, then falls back to existing locations
- **Key Functions**: Argument parsing, emulator location resolution, maiko invocation
- **Integration Point**: Emulator selection already partially integrated via `MEDLEY_EMULATOR` environment variable support

#### medley_run.sh Script

- **Location**: `medley/scripts/medley/medley_run.sh`
- **Structure**: Sourced script (not standalone) that handles actual maiko execution
- **Emulator Resolution**: Similar to medley.command, uses `check_unified_build_location()`
- **Key Functions**: `start_maiko()` function that invokes the emulator executable
- **Integration Point**: Already supports unified build location checking

**Decision**:

- Modify `run-medley` to add `--emulator` argument parsing and emulator selection logic
- Enhance `medley.command` and `medley_run.sh` to fully support `--emulator` command-line argument (currently only supports environment variable)
- Leverage existing `check_unified_build_location()` functions where available
- Add emulator selection argument parsing to all run scripts

**Rationale**:

- Existing scripts already have partial unified build location support from spec 003
- Adding `--emulator` argument provides explicit user control
- Environment variable support provides convenience for default preferences
- Command-line argument takes precedence over environment variable (standard Unix convention)

**Alternatives Considered**:

- Creating new wrapper scripts: Rejected - would duplicate functionality and break existing workflows
- Modifying only one script: Rejected - all run scripts need emulator selection for consistency

---

### 2. Lock File Implementation for Concurrent Run Prevention

**Question**: How should the system implement locking to prevent concurrent Interlisp runs?

**Research Findings**:

#### Existing Lock Patterns in Codebase

- **Loadup Scripts**: Use `LOADUP_LOCKFILE` with PID-based locking
  - Lock file contains process ID
  - Checks if process is still running before blocking
  - Supports `--override` flag to bypass lock
- **Build System**: Uses `acquire_build_lock()` and `release_build_lock()` functions
  - Lock file in `maiko/build/<emulator>/<platform>/.build.lock`
  - Stale lock detection (5 minute timeout)
  - PID-based with process existence check

#### Shell Script Locking Best Practices

- **Lock File Location**: Temporary directory or application-specific location
- **Lock File Contents**: Process ID (PID) for validation
- **Stale Lock Detection**: Check lock age and verify process still exists
- **Lock Release**: Automatic cleanup on script exit (trap handlers)
- **Error Handling**: Clear error messages when lock is held

**Decision**:

- Use lock file in `$HOME/.medley/` or `$TMPDIR/medley/` directory
- Lock file name: `.medley-run.lock` or `medley-run-<run-id>.lock` (if run-id specified)
- Lock file contains: Process ID (PID) of running instance
- Stale lock detection: Check if process exists, remove lock if process dead
- Lock timeout: 5 minutes (same as build system)
- Support `--override-lock` flag to bypass lock (similar to loadup scripts)
- Use trap handlers to ensure lock is released on script exit

**Rationale**:

- Consistent with existing lock patterns in codebase
- PID-based locking is standard Unix practice
- Stale lock detection prevents permanent blocking
- Override flag provides flexibility for debugging/emergency situations

**Alternatives Considered**:

- Using `flock` command: Rejected - not available on all platforms (Solaris, some BSD variants)
- Using semaphores: Rejected - too complex for shell scripts, platform-specific
- No locking: Rejected - violates FR-016 requirement

---

### 3. Emulator Executable Naming and Invocation

**Question**: How should different emulator executables be invoked, especially the Lisp emulator which requires SBCL?

**Research Findings**:

#### C Emulator

- **Executables**: `lde`, `ldeinit`, `ldex`, `ldesdl`
- **Invocation**: Direct execution (e.g., `./lde sysout.sysout`)
- **Arguments**: Standard Maiko command-line arguments

#### Zig Emulator

- **Executable**: `maiko-zig`
- **Invocation**: Direct execution (e.g., `./maiko-zig sysout.sysout`)
- **Arguments**: Should accept same arguments as C emulator (compatibility)

#### Lisp Emulator

- **Executable**: `maiko-lisp` (wrapper script created by build system)
- **Invocation**: Wrapper script executes `sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)"`
- **Arguments**: Wrapper script must pass through command-line arguments to SBCL/Lisp system
- **Note**: Wrapper script already created by unified build system (spec 003)

**Decision**:

- All emulators are invoked the same way: `$executable $sysout_file $arguments...`
- Lisp emulator wrapper script handles SBCL invocation internally
- All emulators receive same command-line arguments (sysout, display options, memory, etc.)
- Executable selection based on emulator type:
  - C emulator: `lde` (or `ldex`/`ldesdl` based on display backend)
  - Zig emulator: `maiko-zig`
  - Lisp emulator: `maiko-lisp` (wrapper script)

**Rationale**:

- Consistent invocation pattern simplifies script logic
- Wrapper script abstraction hides Lisp emulator complexity
- Same arguments ensure compatibility across emulators

**Alternatives Considered**:

- Special handling for Lisp emulator in run scripts: Rejected - wrapper script already handles this
- Different argument sets per emulator: Rejected - breaks compatibility and increases complexity

---

### 4. Auto-Build Integration

**Question**: How should automatic emulator building integrate with the run scripts?

**Research Findings**:

#### Unified Build System (Spec 003)

- **Build Script**: `medley/scripts/build/build-emulator.sh`
- **Arguments**: `--emulator <type> --platform <os>.<arch>`
- **Output**: Exit codes (0 = success, non-zero = failure)
- **Location**: Already integrated into loadup scripts

#### Integration Patterns

- **Loadup Scripts**: Call build script before loadup, check exit code
- **Error Handling**: Display clear error messages if build fails
- **Platform Detection**: Use existing `detect-platform.sh` utility

**Decision**:

- When `--auto-build` flag is specified and emulator is missing:
  1. Call `medley/scripts/build/build-emulator.sh --emulator <type>`
  2. Check exit code
  3. If success, proceed with emulator execution
  4. If failure, display error and exit
- Reuse existing build system infrastructure
- Display build progress to user (verbose output)

**Rationale**:

- Leverages existing unified build system
- Consistent with loadup script integration
- Clear separation of concerns (build vs. run)

**Alternatives Considered**:

- Inline build logic in run scripts: Rejected - violates DRY principle, duplicates build system code
- Separate auto-build script: Rejected - unnecessary complexity, build-emulator.sh already exists

---

### 5. Error Message Design

**Question**: What information should error messages contain for different failure scenarios?

**Research Findings**:

#### Existing Error Patterns

- **Loadup Scripts**: Use `output_error_msg()` function with colored output
- **Build Scripts**: Clear error messages with installation hints (spec 003)
- **Medley Scripts**: Standard error output with actionable guidance

#### Error Scenarios

1. Invalid emulator name → List valid options
2. Emulator not found → Suggest build command or --auto-build flag
3. Emulator not executable → Suggest permissions fix or rebuild
4. Emulator fails to start → Show emulator-specific error output
5. Lock file held → Show PID and suggest --override-lock or wait

**Decision**:

- Use consistent error message format across all scenarios
- Include actionable guidance (suggested commands, flags)
- Reference relevant documentation or help text
- Use existing `output_error_msg()` function where available
- For missing emulator: Suggest `./build-emulator.sh --emulator <type>` or `--auto-build` flag

**Rationale**:

- Consistent error messages improve user experience
- Actionable guidance reduces support burden
- Leverages existing error handling infrastructure

**Alternatives Considered**:

- Minimal error messages: Rejected - violates FR-009, FR-014, FR-015 requirements
- Interactive error resolution: Rejected - breaks script automation, adds complexity

---

## Summary of Decisions

1. **Script Integration**: Modify existing run scripts (run-medley, medley.command, medley_run.sh) to add `--emulator` argument parsing and selection logic
2. **Locking Mechanism**: Use PID-based lock file with stale detection, similar to existing loadup and build system patterns
3. **Emulator Invocation**: All emulators invoked consistently via executable path; Lisp emulator uses wrapper script
4. **Auto-Build**: Integrate with existing `build-emulator.sh` script from unified build system
5. **Error Messages**: Provide clear, actionable error messages with suggested commands and flags
