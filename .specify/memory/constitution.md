<!--
Sync Impact Report
- Version change: 1.0.0 → 1.1.0
- Modified principles: VI. Traceability: Requirements ↔ Tasks (clarified placeholder prohibition language)
- Added sections: Governance → Versioning Policy; Governance → Compliance Review Expectations
- Removed sections: none
- Templates reviewed (no semantic changes required):
  - ✅ .specify/templates/plan-template.md
  - ✅ .specify/templates/spec-template.md
  - ✅ .specify/templates/tasks-template.md
  - ✅ .specify/templates/agent-file-template.md
  - ✅ .specify/templates/checklist-template.md
- Follow-up TODOs: none (no unresolved placeholders in this constitution)
-->

# Interlisp Specifications Constitution

This constitution governs the *specification artifacts* under `specs/` (including
`spec.md`, `plan.md`, and `tasks.md`) and any changes derived from them.

## Core Principles

### I. Reference-First

- **MUST** treat the existing repository behavior and the C implementation as the
  ground truth when conflicts occur.
- **MUST** update documentation and specs when we discover they conflict with the
  implemented behavior (or explicitly decide to change the implementation).

### II. Backward Compatibility for Medley Run Scripts

- **MUST** preserve existing behavior of Medley run scripts unless a spec
  explicitly declares a breaking change.
- **MUST** keep default behavior unchanged when new flags/environment variables
  are not used.
- **MUST** document any behavior changes with clear before/after notes in the
  relevant `spec.md` and any user-facing quickstart or runbook.

### III. Script Portability and Safety

- **MUST** use robust shell practices appropriate to the target script
  (`bash` vs POSIX `sh`), without breaking existing caller expectations.
- **MUST** avoid introducing interactive prompts into runner scripts.
- **MUST** use explicit, deterministic precedence rules for configuration
  sources (CLI flags > environment variables > defaults).

### IV. Operator-Facing Error Messages

- **MUST** present errors with a **user-facing summary** first, then **technical
  details** (paths, exit codes, failing checks).
- **MUST** provide actionable remediation guidance where feasible
  (e.g., “use `--auto-build`”, “check execute bit”).

### V. Concurrency Discipline

- **MUST** prevent accidental concurrent runs by default via a lock mechanism.
- **MUST** define “stale lock” precisely (time threshold and/or PID liveness) in
  `spec.md`.
- **MUST** ensure lock cleanup behavior is documented for normal exit vs crash
  scenarios.

### VI. Traceability: Requirements ↔ Tasks

- **MUST** ensure every Functional Requirement (FR) in `spec.md` has at least one
  clearly identified task in `tasks.md`.
- **MUST** ensure every task in `tasks.md` maps to a requirement and/or user
  story (or is explicitly labeled as “engineering hygiene”).
- **MUST NOT** allow placeholder/template text (e.g., “ACTION REQUIRED” comments,
  generic examples, or TODO markers) to ship in committed spec artifacts; all
  such text **MUST** be resolved, rewritten as project-specific content, or
  removed before merge.

## Documentation & Contracts

- **MUST** keep CLI usage examples consistent across `spec.md`, any quickstart
  documents, and contract docs.
- **SHOULD** record notable design decisions in `research.md` and keep them
  aligned with `spec.md` clarifications.

## Governance

- This constitution supersedes any template text in generated artifacts.
- Amendments require updating this file and (when relevant) re-running
  consistency checks across affected specs and templates.

### Versioning Policy

- **CONSTITUTION_VERSION** follows semantic versioning:
  - **MAJOR**: Backward-incompatible governance or principle removals or
    redefinitions that invalidate existing specs or plans.
  - **MINOR**: New principle or section added, or materially expanded guidance
    that affects how future specs/plans/tasks are authored.
  - **PATCH**: Clarifications, wording fixes, or non-semantic refinements that
    do not change obligations.

### Compliance Review Expectations

- Any change to `spec.md`, `plan.md`, or `tasks.md` **MUST** be reviewed for
  compliance with this constitution before merge.
- When a change intentionally violates a principle (e.g., to enable an
  experiment), the violation **MUST** be explicitly documented in the relevant
  spec or plan, with rationale and a follow-up task to restore compliance.
- At least once per significant feature (new `specs/[###-feature-name]/` tree),
  maintainers **SHOULD** re-check that:
  - Reference-first behavior is preserved.
  - Traceability from requirements to tasks is intact.
  - Backward compatibility and concurrency guarantees are honored where
    applicable.

**Version**: 1.1.0 | **Ratified**: 2026-01-15 | **Last Amended**: 2026-02-10
