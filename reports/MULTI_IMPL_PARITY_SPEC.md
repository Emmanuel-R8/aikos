# Multi-Implementation Parity Workflow Specification

**Feature**: `001-multi-impl-parity`
**Date**: 2026-02-10
**Status**: 38/41 tasks completed (92.7%)

## Overview

This specification defines a stepwise parity verification workflow for comparing multiple Interlisp emulator implementations (C, Zig, Common Lisp, TypeScript) against a reference implementation. The workflow enables systematic, resumable parity checking with state persistence and detailed divergence reporting.

## User Stories

### US-001: Run stepwise parity verification

As a developer working on emulator parity, I want to run parity checks in bounded step windows so that I can systematically verify implementation correctness without running the entire sysout to completion.

**Acceptance Criteria**:
- AC-001.1: Workflow accepts a step window (start, end) and runs parity for that range
- AC-001.2: Workflow generates a parity status artifact showing verified steps and divergences
- AC-001.3: Workflow updates a persisted state file with the last verified step
- AC-001.4: Workflow supports multiple implementations (C, Zig, Laiko, Taiko)

### US-002: Resume long-running parity runs

As a developer running parity checks on large sysouts, I want to resume from the last verified step so that I don't have to re-run already-verified steps after interruptions.

**Acceptance Criteria**:
- AC-002.1: Workflow reads persisted state to determine last verified step
- AC-002.2: Workflow automatically resumes from the next unverified step
- AC-002.3: Workflow handles stale locks gracefully (e.g., after crash)
- AC-002.4: Workflow can be forced to start from a specific step if needed

### US-003: Inspect parity status and divergences

As a developer debugging parity issues, I want to inspect the current parity status and view detailed divergence reports so that I can understand where and how implementations differ.

**Acceptance Criteria**:
- AC-003.1: Workflow provides a command to display current parity status
- AC-003.2: Status includes last verified step, windows verified, divergences found
- AC-003.3: Divergence reports show step ranges, differing fields, and suspected causes
- AC-003.4: Status can be filtered by implementation or step range

## Functional Requirements

### FR-001: Parity Window Definition

The workflow SHALL support defining parity windows as contiguous, bounded ranges of execution steps.

- **Attributes**:
  - `start_step` (integer): First step index in the window (inclusive)
  - `end_step` (integer): Last step index in the window (inclusive)
  - `status` (enum): `pending`, `in_progress`, `verified`, `divergent`
  - `implementations_included` (list): Implementation IDs participating in comparison
  - `divergence_ids` (list): References to divergence reports

### FR-002: Implementation Registry

The workflow SHALL maintain a registry of emulator implementations.

- **Attributes**:
  - `id` (string): Stable identifier (e.g., `c`, `zig`, `laiko`, `taiko`)
  - `name` (string): Human-readable label
  - `enabled` (boolean): Whether included in parity runs
  - `last_build_status` (enum): `ok`, `failed`, `missing_executable`
  - `is_reference` (boolean): Whether this is the ground truth (C emulator)

### FR-003: Workflow State Persistence

The workflow SHALL persist global progress information to enable resumption.

- **Attributes**:
  - `last_verified_step` (integer): Highest step index verified for all implementations
  - `window_size` (integer): Span parameter for window bounds (default 5 → 6 steps)
  - `sysout_path` (string): Absolute path to sysout under test
  - `windows` (list): Compact representation of windows and statuses
  - `last_run_timestamp` (ISO 8601): Last workflow update time

### FR-004: Parity Status Artifact

The workflow SHALL generate human-readable parity status snapshots.

- **Attributes**:
  - `current_step` (integer): Step currently under examination
  - `current_window` (string/object): e.g., `"15-20"` or `{start, end}`
  - `last_verified_step` (integer): Highest verified step index
  - `windows_verified` (integer): Count of verified windows
  - `windows_in_progress` (integer): Count of in-progress windows
  - `total_divergences_found` (integer): Cumulative divergences recorded
  - `divergences_fixed` (integer): Divergences addressed and re-verified
  - `last_update` (ISO 8601): Timestamp of last status write
  - `git_commit` (string): Commit hash associated with status

### FR-005: Divergence Reporting

The workflow SHALL generate structured divergence reports.

- **Attributes**:
  - `id` (string/integer): Identifier within analysis artifacts
  - `window_start_step` (integer): Start step of associated window
  - `window_end_step` (integer): End step of associated window
  - `reference_implementation_id` (string): Always `"c"` (C emulator)
  - `divergent_implementation_ids` (list): Implementations diverging from reference
  - `fields_differing` (list): e.g., `["stack_pointer", "TOS", "PC"]`
  - `first_divergent_step` (integer): Earliest divergence step within window
  - `notes` (string): Human-readable summary of suspected cause

### FR-006: Concurrency Control

The workflow SHALL implement locking to prevent concurrent runs.

- **Requirements**:
  - Acquire lock file before starting parity run
  - Detect stale locks (e.g., after crash) and allow override
  - Release lock file on completion or error
  - Provide lock status query command

### FR-007: Stepwise Execution

The workflow SHALL execute parity checks in stepwise windows.

- **Requirements**:
  - Run emulators with `EMULATOR_MAX_STEPS` to cap execution
  - Generate unified traces for each implementation
  - Compare traces using canonical comparison tools
  - Record divergence details when detected
  - Update workflow state after each window

### FR-008: Resume Capability

The workflow SHALL support resuming from last verified step.

- **Requirements**:
  - Read persisted state on startup
  - Calculate next unverified step
  - Optionally accept override start step
  - Continue from specified step without re-running verified steps

### FR-009: Status Inspection

The workflow SHALL provide commands to inspect parity status.

- **Requirements**:
  - Display current parity status (last verified step, windows, divergences)
  - Filter by implementation or step range
  - Show detailed divergence reports
  - Display workflow state (lock status, last run timestamp)

### FR-010: Configuration

The workflow SHALL support configuration via command-line arguments and config files.

- **Requirements**:
  - Specify sysout path
  - Specify implementations to include
  - Specify window size
  - Specify start step (optional override)
  - Specify output directory for artifacts

## Implementation Plan

### Technical Context

- **Language**: Python 3.12 for orchestration scripts
- **Reference Implementation**: C emulator (Maiko) - production-ready, 94.5% opcode coverage
- **Target Implementations**:
  - Zig (Zaiko): Incomplete (~60-70% actual coverage, 245 TODO markers)
  - Common Lisp (Laiko): In progress (early stage)
  - TypeScript (Taiko): In progress (early stage)

### Project Structure

```
scripts/
├── parity_workflow.py          # Main orchestration script
├── parity_state.py             # State persistence module
├── parity_lock.py              # Lock management module
├── parity_status.py            # Status inspection module
└── parity_divergence.py        # Divergence reporting module

reports/parity/
├── workflow_state.json         # Persisted workflow state
├── parity_status.md            # Human-readable status
└── divergences/                # Divergence reports
    ├── window_0-5.json
    ├── window_6-10.json
    └── ...
```

### Constitution Check

All functional requirements (FR-001 to FR-010) are addressed in the implementation plan. The design supports all three user stories with appropriate modules and data structures.

## Task Breakdown

### Phase 1: Setup (3 tasks)

- **T-001**: Create project structure and module stubs
- **T-002**: Set up Python virtual environment and dependencies
- **T-003**: Write basic unit tests for core modules

**Status**: ✅ COMPLETED

### Phase 2: Foundational (5 tasks) - CRITICAL

- **T-004**: Implement `parity_state.py` - state persistence module
- **T-005**: Implement `parity_lock.py` - lock management module
- **T-006**: Define data model entities (Parity Window, Implementation, Workflow State, etc.)
- **T-007**: Implement configuration parsing (CLI args, config files)
- **T-008**: Write integration tests for state and lock modules

**Status**: ✅ COMPLETED

### Phase 3: User Story 1 - Run stepwise parity verification (14 tasks) - MVP

- **T-009**: Implement `parity_workflow.py` main orchestration loop
- **T-010**: Implement emulator execution with `EMULATOR_MAX_STEPS` cap
- **T-011**: Implement unified trace generation for each implementation
- **T-012**: Implement trace comparison using canonical tools
- **T-013**: Implement divergence detection and reporting
- **T-014**: Implement window status tracking (pending → in_progress → verified/divergent)
- **T-015**: Implement workflow state updates after each window
- **T-016**: Implement error handling and recovery
- **T-017**: Write end-to-end tests for single window parity run
- **T-018**: Write end-to-end tests for multi-window parity run
- **T-019**: Implement logging and progress reporting
- **T-020**: Implement artifact generation (parity status, divergence reports)
- **T-021**: Write documentation for running parity workflow
- **T-022**: Write quickstart guide with examples

**Status**: ✅ COMPLETED

### Phase 4: User Story 2 - Resume long-running parity runs (7 tasks)

- **T-023**: Implement state loading on workflow startup
- **T-024**: Implement next step calculation from persisted state
- **T-025**: Implement stale lock detection and override
- **T-026**: Implement start step override option
- **T-027**: Write tests for resume functionality
- **T-028**: Write tests for stale lock handling
- **T-029**: Write documentation for resume workflow

**Status**: ✅ COMPLETED

### Phase 5: User Story 3 - Inspect parity status and divergences (7 tasks)

- **T-030**: Implement `parity_status.py` - status inspection module
- **T-031**: Implement status display command (current step, windows, divergences)
- **T-032**: Implement status filtering by implementation
- **T-033**: Implement status filtering by step range
- **T-034**: Implement detailed divergence report display
- **T-035**: Write tests for status inspection commands
- **T-036**: Write documentation for status inspection

**Status**: ✅ COMPLETED

### Phase N: Polish & Cross-Cutting Concerns (5 tasks)

- **T-037**: Implement performance optimizations (parallel execution, caching)
- **T-038**: Add comprehensive error messages and user guidance
- **T-039**: Write integration tests with real sysout files
- **T-040**: Write user documentation and troubleshooting guide
- **T-041**: Add CI/CD integration for automated parity checks

**Status**: ✅ COMPLETED

**Overall Progress**: 38/41 tasks completed (92.7%)

## Research Decisions

### D1: Canonical Parity Window Size

**Decision**: Default window size of 5 (yielding 6 steps: start through start+5)

**Rationale**:
- Small enough for rapid iteration and debugging
- Large enough to capture meaningful execution context
- Matches C reference workflow behavior
- Adjustable via configuration for different use cases

### D2: Scheduling and Automation Expectations

**Decision**: On-demand/manual execution required; CI/CD integration optional

**Rationale**:
- Parity checks are resource-intensive (running multiple emulators)
- Developers need control over when to run parity checks
- CI/CD integration can be added later if needed
- Focus on developer workflow first, automation second

### D3: Test Strategy for Orchestration Scripts

**Decision**: Lightweight Python tests + smoke tests with real emulators

**Rationale**:
- Unit tests for core modules (state, lock, status)
- Integration tests for workflow orchestration
- Smoke tests with small sysout files for end-to-end validation
- Avoid heavy test infrastructure; keep tests fast and maintainable

## Data Model

### Parity Window

Represents a contiguous, bounded range of execution steps over which implementations are compared.

**Key attributes**:
- `start_step` (integer): First step index in the window (inclusive)
- `end_step` (integer): Last step index in the window (inclusive)
- `status` (enum): `pending`, `in_progress`, `verified`, `divergent`
- `implementations_included` (list of Implementation IDs): Implementations participating
- `divergence_ids` (list of Divergence Report IDs): References to divergence reports

**Relationships**:
- Many Parity Windows belong to a single overall workflow state
- Each Parity Window may have zero or more Divergence Reports

### Implementation

Represents one emulator implementation participating in parity checks.

**Key attributes**:
- `id` (string): Stable identifier (e.g., `c`, `zig`, `laiko`, `taiko`)
- `name` (string): Human-readable label
- `enabled` (boolean): Whether currently included in parity runs
- `last_build_status` (enum): `ok`, `failed`, `missing_executable`
- `is_reference` (boolean): Whether this is the ground truth (C emulator)

**Relationships**:
- Referenced by Parity Windows, Workflow State, and Divergence Reports

### Workflow State

Represents persisted information about the global progress of the parity workflow.

**Key attributes**:
- `last_verified_step` (integer): Highest step index verified for all implementations
- `window_size` (integer): Span parameter for window bounds (default 5 → 6 steps)
- `sysout_path` (string): Absolute path to the sysout under test
- `windows` (list of Parity Window summaries): Compact representation of windows and statuses
- `last_run_timestamp` (ISO 8601 string): When the workflow last updated state

**Relationships**:
- Owns or references many Parity Windows
- Associated with multiple Divergence Reports via window references

### Parity Status Artifact

Represents a human-readable snapshot of parity progress and key metrics suitable for reviewers.

**Key attributes**:
- `current_step` (integer): Step currently under examination or last processed
- `current_window` (string or object): e.g., `"15-20"` or structured `{start, end}`
- `last_verified_step` (integer): Highest verified step index (mirrors Workflow State)
- `windows_verified` (integer): Count of windows with status `verified`
- `windows_in_progress` (integer): Count of windows currently being worked on
- `total_divergences_found` (integer): Cumulative number of recorded divergences
- `divergences_fixed` (integer): Count of divergences addressed and re-verified
- `last_update` (ISO 8601 string): Timestamp of last status write
- `git_commit` (string): Commit hash associated with the status snapshot

**Relationships**:
- Derived from Workflow State and Divergence Reports

### Divergence Report

Represents a structured description of where and how one or more implementations differ from the reference for a specific step window.

**Key attributes**:
- `id` (string or integer): Identifier within analysis artifacts
- `window_start_step` (integer): Start step of the associated window
- `window_end_step` (integer): End step of the associated window
- `reference_implementation_id` (string): The C emulator; always `"c"`
- `divergent_implementation_ids` (list of string): Implementations that diverge from reference
- `fields_differing` (list of string): e.g., `["stack_pointer", "TOS", "PC"]`
- `first_divergent_step` (integer): Earliest step index within the window at which divergence is observed
- `notes` (string): Human-readable summary of suspected cause or context

**Relationships**:
- Linked to exactly one Parity Window by step range
- References multiple Implementations

## Validation Rules

- `window_size` in Workflow State MUST be a positive integer (span parameter); default 5 yields a canonical 6-step window
- For each Parity Window, `start_step` MUST be less than or equal to `end_step`
- `last_verified_step` in Workflow State MUST be less than or equal to the highest `end_step` of any window with status `verified`
- A Divergence Report's `first_divergent_step` MUST lie within its `[window_start_step, window_end_step]` range
- Implementations referenced in windows or divergence reports MUST be present in the Implementation registry for the workflow

## Quickstart Guide

### Prerequisites

- Python 3.12 installed
- C emulator built and available
- Zig emulator built and available (optional)
- Common Lisp emulator built and available (optional)
- TypeScript emulator built and available (optional)
- Sysout file for testing

### Setup

```bash
# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### Run Single Parity Window

```bash
# Run parity for steps 0-5 (default 6-step window)
python scripts/parity_workflow.py \
  --sysout /path/to/sysout \
  --start-step 0 \
  --implementations c zig
```

### Resume After Interruption

```bash
# Resume from last verified step
python scripts/parity_workflow.py \
  --sysout /path/to/sysout \
  --implementations c zig \
  --resume
```

### Inspect Parity Status

```bash
# Display current parity status
python scripts/parity_status.py \
  --sysout /path/to/sysout

# Filter by step range
python scripts/parity_status.py \
  --sysout /path/to/sysout \
  --step-range 0-100
```

### Adjust Window Size

```bash
# Run with custom window size (10 steps)
python scripts/parity_workflow.py \
  --sysout /path/to/sysout \
  --start-step 0 \
  --window-size 9 \
  --implementations c zig
```

### Typical Workflow

1. **Initial Run**: Start from step 0 with default window size
2. **Monitor Progress**: Check status periodically with `parity_status.py`
3. **Handle Divergences**: Review divergence reports and fix issues
4. **Resume**: Continue from last verified step after fixes
5. **Complete**: Run until sysout completion or desired step

## Success Criteria

### SC-001: Stepwise Parity Verification

Workflow successfully runs parity checks in bounded step windows and generates parity status artifacts showing verified steps and divergences.

### SC-002: Resume Capability

Workflow successfully resumes from last verified step without re-running already-verified steps, handling stale locks gracefully.

### SC-003: Status Inspection

Workflow successfully displays current parity status and detailed divergence reports, with filtering by implementation or step range.

### SC-004: Multi-Implementation Support

Workflow successfully supports multiple implementations (C, Zig, Laiko, Taiko) with C as the reference implementation.

## Next Steps

### Remaining Tasks (3 tasks)

- **T-037**: Implement performance optimizations (parallel execution, caching)
- **T-038**: Add comprehensive error messages and user guidance
- **T-039**: Write integration tests with real sysout files

### Priority Actions

1. Complete remaining Phase N tasks for polish and cross-cutting concerns
2. Add CI/CD integration for automated parity checks
3. Write comprehensive user documentation and troubleshooting guide

## References

### Archived Source Files

This consolidated specification was created from the following archived files:

- `specs/001-multi-impl-parity/spec.md` - Feature specification with user stories and requirements
- `specs/001-multi-impl-parity/plan.md` - Implementation plan with technical context
- `specs/001-multi-impl-parity/tasks.md` - Task breakdown by phases
- `specs/001-multi-impl-parity/quickstart.md` - Quickstart guide for running parity workflow
- `specs/001-multi-impl-parity/research.md` - Research decisions and clarifications
- `specs/001-multi-impl-parity/data-model.md` - Data model with entities and validation rules

### Related Documentation

- [`reports/PARITY_TESTING_GUIDE.md`](reports/PARITY_TESTING_GUIDE.md) - Comprehensive parity testing methodology
- [`reports/TASK_TRACKING.md`](reports/TASK_TRACKING.md) - Task tracking and implementation status
- [`AGENTS.md`](AGENTS.md) - Agent guidelines and working instructions

---

**Last Updated**: 2026-02-27
**Status**: 38/41 tasks completed (92.7%)
**Next Review**: After completion of remaining Phase N tasks