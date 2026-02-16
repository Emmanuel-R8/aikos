# Contracts: Multi-Implementation Parity Workflow

**Purpose**: In Speckit, `contracts/` holds formal API or interface specifications
(e.g., CLI flags, function signatures, data formats) that implementation tasks
follow. Tasks can reference "Endpoints from contracts/" when generating task lists.

**This feature**: The parity workflow is orchestration glue—Python scripts that
run emulators and compare traces. It has no formal API beyond CLI interfaces.

- **CLI**: `scripts/iterative_parity_workflow.py` and `scripts/unified_test_harness.py`
  are documented in `quickstart.md` and `specs/001-multi-impl-parity/quickstart.md`.
- **Data schemas**: Workflow state and divergence reports are defined in
  `data-model.md`, not as separate contracts.
- **Trace format**: The unified trace format is described in existing project
  docs (e.g., `AGENTS.md` §4.2).

**Decision**: No formal contract files for this feature. The quickstart and
data-model provide sufficient interface documentation for orchestration scripts.

**When to add contracts**: If a future task introduces a new reusable API
(e.g., a Python module with a published interface), add a contract here.
