# Data Model: Multi-Implementation Parity Workflow

**Feature**: `001-multi-impl-parity`
**Spec**: `specs/001-multi-impl-parity/spec.md`
**Date**: 2026-02-10

## Entities

### Parity Window

- **Represents**: A contiguous, bounded range of execution steps over which
  implementations are compared.
- **Key attributes**:
  - `start_step` (integer): First step index in the window (inclusive).
  - `end_step` (integer): Last step index in the window (inclusive).
  - `status` (enum): one of `pending`, `in_progress`, `verified`, `divergent`.
  - `implementations_included` (list of Implementation IDs): Implementations
    that participated in this window’s comparison run.
  - `divergence_ids` (list of Divergence Report IDs): References to divergence
    reports, if any, associated with this window.
- **Relationships**:
  - Many Parity Windows belong to a single overall workflow state.
  - Each Parity Window may have zero or more Divergence Reports.

### Implementation

- **Represents**: One emulator implementation participating in parity checks.
- **Key attributes**:
  - `id` (string): Stable identifier (e.g., `c`, `zig`, `laiko`, `taiko`).
  - `name` (string): Human-readable label.
  - `enabled` (boolean): Whether this implementation is currently included in
    parity runs.
  - `last_build_status` (enum or string): Last known build/run outcome (e.g.,
    `ok`, `failed`, `missing_executable`).
  - `is_reference` (boolean): Whether this implementation is the ground truth.
    The C emulator (`id` = `"c"`) is the reference; all parity judgments are
    relative to C. Other implementations are compared against C.
- **Relationships**:
  - Referenced by Parity Windows, Workflow State, and Divergence Reports.

### Workflow State

- **Represents**: Persisted information about the global progress of the parity
  workflow.
- **Key attributes**:
  - `last_verified_step` (integer): Highest step index for which all in-scope
    implementations are known to match the reference.
  - `window_size` (integer): Span parameter for window bounds, aligned with the
    workflow implementation. A window covers steps `start_step` through
    `start_step + window_size` (inclusive), yielding `window_size + 1` steps.
    Stored value: default 5 → 6 steps (e.g., steps 0–5). Matches C reference
    workflow behavior.
  - `sysout_path` (string): Absolute path to the sysout under test.
  - `windows` (list of Parity Window summaries): Optional compact representation
    of windows and their statuses (e.g., `[{"start": 0, "end": 5, "status": "verified"}]`).
  - `last_run_timestamp` (ISO 8601 string): When the workflow last updated
    state.
- **Relationships**:
  - Owns or references many Parity Windows.
  - Associated with multiple Divergence Reports via window references.

### Parity Status Artifact

- **Represents**: A human-readable snapshot of parity progress and key metrics
  suitable for reviewers.
- **Key attributes**:
  - `current_step` (integer): Step currently under examination or last
    processed.
  - `current_window` (string or object): e.g., `"15-20"` or structured
    `{start, end}`.
  - `last_verified_step` (integer): Highest verified step index (mirrors
    Workflow State).
  - `windows_verified` (integer): Count of windows with status `verified`.
  - `windows_in_progress` (integer): Count of windows currently being worked on.
  - `total_divergences_found` (integer): Cumulative number of recorded
    divergences.
  - `divergences_fixed` (integer): Count of divergences that have been
    addressed and re-verified.
  - `last_update` (ISO 8601 string): Timestamp of last status write.
  - `git_commit` (string): Commit hash associated with the status snapshot.
- **Relationships**:
  - Derived from Workflow State and Divergence Reports.

### Divergence Report

- **Represents**: A structured description of where and how one or more
  implementations differ from the reference for a specific step window.
- **Key attributes**:
  - `id` (string or integer): Identifier within analysis artifacts.
  - `window_start_step` (integer): Start step of the associated window.
  - `window_end_step` (integer): End step of the associated window.
  - `reference_implementation_id` (string): The C emulator; always `"c"`.
  - `divergent_implementation_ids` (list of string): Implementations that
    diverge from the reference in this window.
  - `fields_differing` (list of string): E.g., `["stack_pointer", "TOS", "PC"]`.
  - `first_divergent_step` (integer): Earliest step index within the window at
    which divergence is observed.
  - `notes` (string): Human-readable summary of suspected cause or context.
- **Relationships**:
  - Linked to exactly one Parity Window by step range.
  - References multiple Implementations.

## Validation Rules

- `window_size` in Workflow State MUST be a positive integer (span parameter);
  default 5 yields a canonical 6-step window.
- For each Parity Window, `start_step` MUST be less than or equal to
  `end_step`.
- `last_verified_step` in Workflow State MUST be less than or equal to the
  highest `end_step` of any window with status `verified`.
- A Divergence Report’s `first_divergent_step` MUST lie within its
  `[window_start_step, window_end_step]` range.
- Implementations referenced in windows or divergence reports MUST be present
  in the Implementation registry for the workflow.
