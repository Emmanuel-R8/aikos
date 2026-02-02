# Research Findings: Runner Scripts + Execution Trace Parity

**Date**: 2026-01-15
**Feature**: 004-emulator-runner

## Overview

This feature now includes both runner-script improvements and execution-trace parity work (C as ground truth, Zig must match). The repository already contains the necessary baseline tooling and debugging artifacts; the remaining work is primarily standardizing workflows and closing gaps (skip matching prefix, runtime step caps, staged sysout parity).

## Decisions Made

### Emulator Location Resolution

- **Decision**: Use unified build location (`maiko/build/<emulator>/<os>.<arch>/`) first, fallback to existing Maiko locations
- **Rationale**: Supports the unified build system (spec 003) while maintaining backward compatibility
- **Alternatives Considered**: Only unified build location (rejected due to breaking existing workflows)

### Locking Mechanism

- **Decision**: Per-user lock file at default location `$HOME/.medley/medley.lock` containing PID and timestamp
- **Rationale**: Prevents concurrent runs per user while allowing multiple users on same system; matches current `medley/scripts/medley/emulator_utils.sh` behavior
- **Alternatives Considered**: System-wide locks (rejected due to multi-user environments)

### Error Handling

- **Decision**: Fail with clear error messages, no automatic fallback to other emulators
- **Rationale**: Predictable behavior, user explicitly chooses emulator
- **Alternatives Considered**: Automatic fallback (rejected due to potential confusion and debugging difficulty)

## Technical Details Confirmed

- Bash/shell scripts are appropriate for the platform requirements
- Existing Medley scripts provide good integration points
- Manual testing sufficient for runner-script validation
- Performance goals achievable with shell script overhead

### Execution Trace Parity Workflow

- **Decision**: Parity target is **match to completion**, not a fixed first-N budget.
- **Decision**: Parity is **staged**: `starter.sysout` completion parity first, then `full.sysout`.
- **Decision**: Comparisons should **auto-skip the matching prefix** by computing the longest common prefix (LCP), with an optional manual `--start-line N` override.
- **Decision**: Fast iteration should use a **runtime step/trace cap knob** (flag/env var) honored by both emulators, not scripts that patch source.

**Rationale**:

- “To completion” prevents false confidence from early matching only.
- Staging keeps iteration time manageable and reduces confounding factors early.
- LCP skipping reduces wasted time re-checking already-matching regions.
- Runtime caps avoid dirty-tree churn and make automation deterministic.

**Related existing tooling**:

- `scripts/generate_debug_logs.sh` (log generation)
- `scripts/analyze_execution_divergence.py` / `scripts/enhanced_divergence_analysis.py` (first-divergence analysis)
- Existing execution-debugging artifacts under `specs/execution-*.md`

## Next Steps

Update Phase 1 design docs and contracts to include:

- Trace parity workflows (generation + comparison + LCP skipping)
- Runtime cap knob naming/behavior (shared across C and Zig)
- Staged sysout parity gate (`starter.sysout` then `full.sysout`)
