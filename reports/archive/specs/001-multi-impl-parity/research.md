# Research: Multi-Implementation Parity Workflow

**Feature**: `001-multi-impl-parity`
**Spec**: `specs/001-multi-impl-parity/spec.md`
**Date**: 2026-02-10

## Decisions

### D1: Canonical parity window size

- **Decision**: Use a canonical default window size of 6 steps (STEP to STEP+5)
  for parity analysis, while allowing users to override this value per run via
  configuration.
- **Rationale**:
  - 6 steps matches the existing parity plans in `plan.md` and project reports.
  - It is large enough to capture short multi-instruction patterns (e.g., call
    + push + return) but small enough to keep analysis and reports focused.
  - A single canonical default improves documentation and examples; overrides
    can still support exceptional debugging needs.
  - Aligns with the existing multi-implementation parity plan language that
    repeatedly references “STEP to STEP+5” windows.
- **Alternatives considered**:
  - **Smaller windows (e.g., 1–3 steps)**: Simpler to reason about but require
    many more runs and can miss the context of multi-instruction idioms.
  - **Larger fixed windows (e.g., 10–20 steps)**: Provide more context but make
    diagnosis and divergence reporting heavier and more complex.
  - **No canonical size (always configured)**: Maximum flexibility but weaker
    standardization; documentation and tooling would be harder to keep aligned.

### D2: Scheduling and automation expectations

- **Decision**: Treat on-demand/manual invocation as the **required** execution
  mode, with CI/cron integration as an optional layer on top of the workflow.
- **Rationale**:
  - The primary user journeys in the spec focus on maintainers and contributors
    running parity analysis intentionally for specific ranges or during
    debugging, not continuous background monitoring.
  - Keeping scheduling outside this feature allows different environments
    (local dev, CI, nightly cron) to adopt their own policies without forcing a
    one-size-fits-all schedule.
  - The spec still requires that the workflow be safe and idempotent under
    repeated invocation, which makes CI integration straightforward later.
- **Alternatives considered**:
  - **Built-in daily scheduler**: Increases scope and entangles the workflow
    with environment-specific scheduling details; better handled at the CI or
    system level.
  - **Mandatory CI integration**: Useful, but not strictly necessary to deliver
    the core parity workflows described in the spec.

### D3: Test strategy for orchestration scripts

- **Decision**: Use lightweight, focused Python tests and smoke tests for the
  orchestration scripts, in addition to reusing existing emulator tests.
- **Rationale**:
  - The orchestration logic is primarily responsible for process management,
    file handling, and state updates; unit tests can cover argument parsing,
    state transitions, and file-path logic without running full emulators.
  - Smoke tests (e.g., short-step windows with known sysouts) are sufficient to
    validate end-to-end behavior without introducing heavy, brittle test
    matrices.
  - This keeps the testing load reasonable while still making the workflow
    verifiable.
- **Alternatives considered**:
  - **No explicit tests for orchestration**: Would make regressions harder to
    detect in the glue code that controls multi-implementation parity.
  - **Full-blown integration test suite per emulator**: Redundant with existing
    emulator-level tests; better to focus parity tests on the workflow itself.

## Clarifications Addressed

All `[NEEDS CLARIFICATION]` markers from the specification have been resolved
via decisions D1 and D2 above. No additional open clarifications remain for
Phase 1 design work.
