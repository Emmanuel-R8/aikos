# Research Findings: Emulator Runner Scripts

**Date**: 2026-01-12
**Feature**: 004-emulator-runner

## Overview

No critical unknowns identified in the technical context. All required information is available from existing specifications and project structure.

## Decisions Made

### Emulator Location Resolution

- **Decision**: Use unified build location (`maiko/build/<emulator>/<os>.<arch>/`) first, fallback to existing Maiko locations
- **Rationale**: Supports the unified build system (spec 003) while maintaining backward compatibility
- **Alternatives Considered**: Only unified build location (rejected due to breaking existing workflows)

### Locking Mechanism

- **Decision**: User-specific lock files in `medley/.medley-<username>.lock` with PID and timestamp
- **Rationale**: Prevents concurrent runs per user while allowing multiple users on same system
- **Alternatives Considered**: System-wide locks (rejected due to multi-user environments)

### Error Handling

- **Decision**: Fail with clear error messages, no automatic fallback to other emulators
- **Rationale**: Predictable behavior, user explicitly chooses emulator
- **Alternatives Considered**: Automatic fallback (rejected due to potential confusion and debugging difficulty)

## Technical Details Confirmed

- Bash/shell scripts are appropriate for the platform requirements
- Existing Medley scripts provide good integration points
- Manual testing sufficient for script validation
- Performance goals achievable with shell script overhead

## Next Steps

Proceed to Phase 1 design with data modeling and contract specification.
