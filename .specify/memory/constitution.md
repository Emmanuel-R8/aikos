# Interlisp Specifications Constitution

This constitution governs the *specification artifacts* under `specs/` (including `spec.md`, `plan.md`, and `tasks.md`) and any changes derived from them.

## Core Principles

### I. Reference-First

- **MUST** treat the existing repository behavior and the C implementation as the ground truth when conflicts occur.
- **MUST** update documentation and specs when we discover they conflict with the implemented behavior (or explicitly decide to change the implementation).

### II. Backward Compatibility for Medley Run Scripts

- **MUST** preserve existing behavior of Medley run scripts unless a spec explicitly declares a breaking change.
- **MUST** keep default behavior unchanged when new flags/environment variables are not used.
- **MUST** document any behavior changes with clear before/after notes in the relevant `spec.md` and `quickstart.md`.

### III. Script Portability and Safety

- **MUST** use robust shell practices appropriate to the target script (`bash` vs POSIX `sh`), without breaking existing caller expectations.
- **MUST** avoid introducing interactive prompts into runner scripts.
- **MUST** use explicit, deterministic precedence rules for configuration sources (CLI flags > environment variables > defaults).

### IV. Operator-Facing Error Messages

- **MUST** present errors with a **user-facing summary** first, then **technical details** (paths, exit codes, failing checks).
- **MUST** provide actionable remediation guidance where feasible (e.g., “use `--auto-build`”, “check execute bit”).

### V. Concurrency Discipline

- **MUST** prevent accidental concurrent runs by default via a lock mechanism.
- **MUST** define “stale lock” precisely (time threshold and/or PID liveness) in `spec.md`.
- **MUST** ensure lock cleanup behavior is documented for normal exit vs crash scenarios.

### VI. Traceability: Requirements ↔ Tasks

- **MUST** ensure every Functional Requirement (FR) in `spec.md` has at least one clearly identified task in `tasks.md`.
- **MUST** ensure every task in `tasks.md` maps to a requirement and/or user story (or is explicitly labeled as “engineering hygiene”).
- **MUST NOT** leave placeholder/template text (e.g., “ACTION REQUIRED”, `[PRINCIPLE]`, TODO markers) in committed spec artifacts.

## Documentation & Contracts

- **MUST** keep CLI usage examples consistent across `spec.md`, `quickstart.md`, and any contract docs.
- **SHOULD** record notable design decisions in `research.md` and keep them aligned with `spec.md` clarifications.

## Governance

- This constitution supersedes any template text in generated artifacts.
- Amendments require updating this file and (when relevant) re-running consistency checks across affected specs.

**Version**: 1.0.0 | **Ratified**: 2026-01-15 | **Last Amended**: 2026-01-15
