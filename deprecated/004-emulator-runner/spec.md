# Feature Specification: Emulator Runner Scripts for Interlisp

**Feature Branch**: `004-emulator-runner`
**Created**: 2025-01-27
**Status**: Draft
**Input**: User description: "I want scripts to run the Interlisp system from Medley using my choice of emulators."

## Clarifications

### Session 2025-01-27

- Q: If the selected emulator fails to start (e.g., crashes, can't load sysout, display error), should the system attempt automatic fallback to another emulator, or fail with an error? → A: Fail with clear error message (no fallback)
- Q: Should the system proactively detect corrupted/non-executable emulator files before attempting to run, or let the emulator fail naturally when invoked? → A: Proactively detect (check permissions, basic validation) and show clear error
- Q: Should the system allow or prevent multiple Interlisp instances running simultaneously with different emulators? → A: Prevent concurrent runs (implement locking mechanism)
- Q: When the locking mechanism detects an existing lock file, how should the system handle potentially stale locks (e.g., from a crashed previous run)? → A: Auto-remove stale locks (older than 1 minute) with warning message
- Q: When the system displays error messages, what level of technical detail should be included? → A: Technical details with user-friendly summary (include file paths, error codes, but lead with plain-language explanation)
- Q: When an Interlisp session ends (normal exit, user interrupt, or crash), how should the lock file be handled? → A: Always remove lock file on normal exit; rely on stale lock detection for crash cleanup
- Q: Where should the lock file be created and what information should it contain? → A: Per-user lock file (default: `$HOME/.medley/medley.lock`) containing PID and timestamp
- Q: For FR-015's "basic validation" of emulator files, what specific checks should be performed before attempting to run? → A: Check executable bit, file exists, file size > 0, and verify file header/magic bytes match expected executable format

### Session 2026-01-15

- Q: For `specs/spec.md`, how should we treat the “compare C vs Zig execution traces and make Zig match C” work? → A: Expand this spec to include execution-trace comparison + parity work (and update Out of Scope accordingly).
- Q: What is the canonical parity target for “Zig matches C” in this spec? → A: Full run to completion matches; comparison workflow may skip already-matching prefix lines as progress is made.
- Q: Which sysout/configuration is the canonical parity target for this spec (the one used for “match to completion”)? → A: Both `starter.sysout` and `full.sysout`, but `starter.sysout` must reach completion parity first before attempting `full.sysout`.
- Q: For the “skip already-matching prefix” workflow, what mechanism should be considered canonical? → A: Auto longest-common-prefix (LCP) detection with optional manual override (e.g., `--start-line N`).
- Q: For the “faster iteration” mode (cap instructions / trace length), what should be the canonical mechanism? → A: Use a runtime flag/env var honored by both emulators (no source patching).

## User Scenarios & Testing

### User Story 1 - Run Interlisp with Selected Emulator (Priority: P1)

A developer wants to run the Interlisp system using a specific emulator implementation (C, Zig, or Lisp) to test compatibility, performance, or development features.

**Why this priority**: This is the core functionality - without the ability to run Interlisp with a chosen emulator, the feature has no value. This enables developers to leverage the multiple emulator implementations built by the unified build system.

**Independent Test**: Can be fully tested by running `cd medley && ./run-medley --emulator zig` and verifying that the Zig emulator is used to start Interlisp, delivering the ability to choose emulators at runtime.

**Acceptance Scenarios**:

1. **Given** a developer has built multiple emulators using the unified build system, **When** they run `cd medley && ./run-medley --emulator c`, **Then** Interlisp starts using the C emulator and displays normally
2. **Given** a developer wants to test the Zig emulator, **When** they run `cd medley && ./run-medley --emulator zig`, **Then** Interlisp starts using the Zig emulator from the unified build location
3. **Given** a developer wants to use the Lisp emulator, **When** they run `cd medley && ./run-medley --emulator lisp`, **Then** Interlisp starts using the Lisp emulator wrapper script

---

### User Story 2 - Set Default Emulator Preference (Priority: P2)

A developer wants to set a default emulator preference so they don't have to specify it every time they run Interlisp.

**Why this priority**: Improves developer experience by reducing repetitive command-line arguments. While not critical for functionality, it significantly improves usability for regular development workflows.

**Independent Test**: Can be fully tested by setting `MEDLEY_EMULATOR=zig` and running `cd medley && ./run-medley` without the `--emulator` flag, verifying the Zig emulator is used by default.

**Acceptance Scenarios**:

1. **Given** a developer sets `export MEDLEY_EMULATOR=lisp`, **When** they run `cd medley && ./run-medley`, **Then** Interlisp starts using the Lisp emulator without requiring the `--emulator` flag
2. **Given** a developer has set `MEDLEY_EMULATOR=c` and runs `cd medley && ./run-medley --emulator zig`, **Then** the command-line argument takes precedence and the Zig emulator is used
3. **Given** no emulator is specified via command-line or environment variable, **When** a developer runs `cd medley && ./run-medley`, **Then** the system uses a sensible default (C emulator) and Interlisp starts normally

---

### User Story 3 - Automatic Emulator Building on First Run (Priority: P3)

A developer wants to run Interlisp with a specific emulator, and if that emulator isn't built yet, the system should automatically build it before running.

**Why this priority**: Enhances developer experience by removing manual build steps, but is not critical since developers can manually build emulators first. This provides convenience for quick testing scenarios.

**Independent Test**: Can be fully tested by removing a built emulator and running `cd medley && ./run-medley --emulator zig --auto-build`, verifying the Zig emulator is built automatically before Interlisp starts.

**Acceptance Scenarios**:

1. **Given** the Zig emulator is not built, **When** a developer runs `cd medley && ./run-medley --emulator zig --auto-build`, **Then** the system builds the Zig emulator first, then starts Interlisp with it
2. **Given** an emulator build fails, **When** a developer runs `cd medley && ./run-medley --emulator zig --auto-build`, **Then** the system displays a clear error message and does not start Interlisp
3. **Given** `--auto-build` is not specified and the emulator is missing, **When** a developer runs `cd medley && ./run-medley --emulator zig`, **Then** the system displays an error message indicating the emulator needs to be built first

---

### User Story 4 - Execution Trace Parity: Make Zig Match C (Priority: P0 - Absolute Priority)

A developer wants to compare execution traces/logs of the C and Zig emulators line-by-line and iteratively fix the Zig emulator until it matches the C emulator.

**Why this priority**: This is the absolute priority for the feature. Correct execution parity is required to trust the Zig emulator. Existing runner scripts and debugging automation should support repeatable trace generation and comparison. All other user stories depend on this parity work being completed first.

**Independent Test**: Can be tested by generating both traces for the same sysout/configuration and verifying they match to completion. This work proceeds in stages: get `starter.sysout` matching first, then apply the same parity workflow to `full.sysout`. The process is iterative: start from the 1st instruction, log a few execution steps and compare, then fix whatever needs fixing in the Zig emulator. Once fixed, execute but start logging a bit further from just before a mismatch occurs, and repeat until full parity is achieved.

**Acceptance Scenarios**:

1. **Given** both emulators are built, **When** a developer runs the trace generation workflow, **Then** both logs are produced for the same sysout/configuration and can be compared deterministically
2. **Given** a mismatch occurs, **When** the developer runs the comparison workflow, **Then** the first divergence is clearly reported (PC, opcode, instruction bytes, stack/frame fields as available)
3. **Given** fixes are applied to the Zig emulator, **When** the logs are regenerated and compared, **Then** the first divergence moves forward, allowing the developer to skip the already-matching prefix and focus on the next mismatch until completion parity is achieved

### Edge Cases

- What happens when the specified emulator doesn't exist in the unified build location?
- How does the system handle invalid emulator names (e.g., `--emulator invalid`)?
- What happens when multiple emulators are available but the selected one fails to start? → System fails with clear error message (no automatic fallback)
- How does the system handle platform-specific emulator availability (e.g., Lisp emulator not available on certain platforms)?
- What happens when the emulator executable exists but is corrupted or not executable? → System proactively detects (checks executable bit, file exists, file size > 0, and verifies file header/magic bytes match expected executable format) and shows clear error before attempting to run
- How does the system handle concurrent runs with different emulators? → System prevents concurrent runs per user using a per-user lock file (default: `$HOME/.medley/medley.lock`) containing PID and timestamp
- What happens when a lock file exists from a previous run? → System automatically removes stale locks older than 1 minute with a warning message; fails immediately if lock is less than 1 minute old (active session)
- What happens to the lock file when Interlisp exits normally? → System removes lock file on normal exit; crashed sessions leave stale locks that are cleaned up by stale lock detection (FR-017)
- What happens when the unified build location exists but contains stale or incomplete builds? → Stale builds (executables exist but outdated) are handled by build system; incomplete builds are detected by proactive validation (FR-015) or fail when executed

## Requirements

### Functional Requirements

- **FR-001**: System MUST allow users to specify which emulator to use via command-line argument `--emulator <c|zig|lisp>`
- **FR-002**: System MUST support setting default emulator via `MEDLEY_EMULATOR` environment variable
- **FR-003**: System MUST prioritize command-line arguments over environment variables when both are specified
- **FR-004**: System MUST use C emulator as default when no emulator is specified via command-line or environment variable
- **FR-005**: System MUST validate emulator selection and display clear error messages for invalid emulator names (user-friendly summary with technical details)
- **FR-006**: System MUST locate emulator executables in the unified build location (`maiko/build/<emulator>/<os>.<arch>/`) first
- **FR-007**: System MUST fall back to existing Maiko location resolution if emulator not found in unified build location (backward compatibility)
- **FR-008**: System MUST support automatic emulator building via `--auto-build` flag when emulator is missing
- **FR-009**: System MUST display clear error messages when emulator is missing and `--auto-build` is not specified (user-friendly summary with technical details including file paths)
- **FR-010**: System MUST preserve all existing run-medley script functionality (sysout selection, display options, memory settings, etc.)
- **FR-011**: System MUST work with all existing Medley run scripts (run-medley, medley.command, medley_run.sh)
- **FR-012**: System MUST handle platform-specific emulator availability gracefully (e.g., inform user if emulator not available for current platform)
- **FR-013**: System MUST maintain executable naming conventions expected by Medley scripts (lde, ldeinit, ldex, ldesdl, zaiko, laiko)
- **FR-014**: System MUST fail with clear error message when selected emulator fails to start (no automatic fallback to other emulators; user-friendly summary with technical details)
- **FR-015**: System MUST proactively detect corrupted or non-executable emulator files (check executable bit, file exists, file size > 0, and verify file header/magic bytes match expected executable format) and display clear error messages before attempting to run (user-friendly summary with technical details including file paths and permission/validation issues)
  - **Validation steps**:
    1. Check file exists
    2. Check file is executable (`test -x`)
    3. Check file size > 0
    4. Verify file header/magic bytes match expected executable format (ELF, Mach-O, PE/COFF, or shebang for scripts)
- **FR-016**: System MUST prevent concurrent runs using a locking mechanism to ensure only one Interlisp instance runs at a time per user (default lock file: `$HOME/.medley/medley.lock` containing PID and timestamp)
- **FR-017**: System MUST automatically remove stale lock files older than 1 minute and display a warning message when doing so (user-friendly summary with technical details)
- **FR-018**: System MUST remove lock file on normal exit of Interlisp session; crash cleanup relies on stale lock detection (FR-017)

#### Execution Trace Parity Requirements (Zaiko vs C)

- **FR-019**: System MUST provide a repeatable workflow to generate execution traces for both C and Zig emulators for the same sysout/configuration.
- **FR-020**: System MUST provide a repeatable workflow to compare C vs Zig traces line-by-line and identify the first divergence with actionable context (PC and key fields).
- **FR-021**: System MUST define a canonical trace format (fields + ordering) used by both emulators for parity comparison (or document any intentional differences).
- **FR-022**: System MUST support comparisons “to completion” and allow configuring a line/instruction budget for faster iteration during debugging.
- **FR-026**: System MUST support a runtime configuration knob (flag or environment variable) to cap trace length / executed instructions for faster iteration, without modifying source files.
  - **Canonical name**: `EMULATOR_MAX_STEPS` (integer; when unset/empty, run to completion)
  - **Performance criterion**: Execution time for capped runs should be under 10 seconds for standard development hardware.
- **FR-023**: System MUST treat the C emulator trace as the ground truth and drive Zig changes until parity is achieved.
- **FR-024**: System MUST support resuming comparisons by skipping the already-matching prefix to speed up iterative parity work:
  - Auto-detect the longest common prefix (LCP) and report the next line as the first divergence
  - Also allow an explicit manual override (e.g., `--start-line N`) when the developer already knows where they are working
- **FR-025**: System MUST stage parity work: `starter.sysout` must reach completion parity before `full.sysout` parity is required.

#### Executable / Script Header Validation (FR-015)

For FR-015’s “header/magic bytes” check, the system MUST treat the following as valid emulator launch targets (platform-dependent):

- Native binaries: ELF (Linux), Mach-O (macOS), PE/COFF (Windows/WSL environments where applicable)
- Script wrappers: files with a shebang (`#!...`) that are executable (e.g., a Lisp emulator wrapper script)

If the `file(1)` utility is available, it SHOULD be used to classify the file; otherwise a minimal byte-prefix check (ELF or shebang) is acceptable.

### Key Entities

- **EmulatorSelection**: Represents the user's choice of emulator (C, Zig, or Lisp), with precedence rules (command-line > environment > default)
- **EmulatorExecutable**: Represents the emulator executable to be invoked, with location resolution logic (unified build location > existing Maiko locations > PATH)
- **RunConfiguration**: Represents the complete runtime configuration including emulator selection, sysout file, display settings, and other Medley options

## Success Criteria

### Measurable Outcomes

- **SC-001**: Developers can successfully run Interlisp with any of the three emulators (C, Zig, Lisp) in under 5 seconds after emulator selection
- **SC-002**: Emulator selection via command-line argument works correctly in 100% of test cases across all supported platforms
- **SC-003**: Environment variable emulator selection works correctly in 100% of test cases when no command-line argument is provided
- **SC-004**: System displays helpful error messages within 1 second when invalid emulator is specified or emulator is missing
- **SC-005**: Automatic emulator building completes successfully and starts Interlisp in under 10 minutes for any emulator on standard development hardware
- **SC-006**: All existing Medley run script functionality remains fully functional with 100% backward compatibility
- **SC-007**: Developers can successfully switch between emulators for the same Interlisp session configuration (same sysout, same settings) without errors
- **SC-008**: For the target sysout/configuration, C and Zig execution traces match for the first 38 instructions (current divergence at instruction 38). Full completion parity pending further fixes.
- **SC-009**: `starter.sysout` reaches completion parity (C and Zig traces match to completion) before `full.sysout` parity work begins.
- **SC-010**: `full.sysout` reaches completion parity after `starter.sysout` parity is achieved.

## Assumptions

- The unified build system (spec 003) is already implemented and emulators are built in `maiko/build/<emulator>/<os>.<arch>/`
- Existing Medley run scripts (run-medley, medley.command, medley_run.sh) continue to function as before
- Emulator executables follow naming conventions: `lde`/`ldex`/`ldesdl` for C emulator, `zaiko` for Zig emulator, `laiko` for Lisp emulator
- Platform detection works correctly (leveraging existing Maiko utilities or uname fallback)
- Users have appropriate permissions to execute emulator binaries
- Display backends (X11, SDL2, SDL3) are available as needed by each emulator

## Dependencies

- **Spec 003 (Unified Build System)**: Required - provides the unified build location structure and emulator building capabilities
- **Existing Medley Run Scripts**: Required - must integrate with run-medley, medley.command, medley_run.sh
- **Maiko Platform Detection**: Required - uses existing osversion and machinetype utilities
- **Emulator Executables**: Required - emulators must be built (manually or via auto-build) before running
- **Trace/Comparison Tooling**: Utility scripts under `scripts/` (e.g., `generate_debug_logs.sh`, `compare_debug_logs.sh`, `analyze_execution_divergence.py`)

## Out of Scope

- Building emulators (handled by spec 003 unified build system)
- Modifying emulator implementations beyond what is required to (a) add trace/diagnostic output and (b) fix Zig execution to match C for parity
- Creating new emulator implementations
- Changing Medley sysout file format or structure
- Modifying Interlisp runtime behavior based on emulator choice (beyond fixing incorrect Zig behavior)
- Performance benchmarking or comparison tools between emulators
- Emulator-specific feature flags or configuration options
- Integration with IDE or development environments beyond command-line scripts
