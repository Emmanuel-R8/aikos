# Laiko Parity Priorities Using the CL MCP Server

## Overview

Based on `AGENTS.md`, `reports/CURRENT_STATUS.md`, and `reports/STEP_COMPARISON_STATUS.md`, this plan proposes a **priority-ordered roadmap** to bring the Common Lisp emulator **Laiko** toward parity with the C reference **Maiko**, while explicitly leveraging the existing **CL MCP server** for navigation, analysis, and parity testing support.

## Priority 1 — Make Laiko Load Cleanly and Stably

- **Goal**: Laiko must compile and load via ASDF/`load-emulator.lisp` without undefined-function warnings that affect runtime.
- **Tasks**:
- **Stabilize ASDF system**: Identify and fix modules that trigger "undefined function" warnings (especially around op-list / list ops).
- **Fix load order**: Ensure packages and dependencies are loaded in a consistent order (packages first, then shared utilities, then VM/ops).
- **CL MCP usage**: Use CL MCP tools to:
- Search for undefined symbols across `laiko/src/`.
- Jump quickly between definition and use sites for problematic functions.

## Priority 2 — Fix Package/Export Issues (IFPAGE, Handlers, VM State)

- **Goal**: Ensure all VM-critical accessors and utility functions are properly exported and visible to the modules that use them.
- **Tasks**:
- **IFPAGE accessors**: Confirm the accessor functions exported in `src/package.lisp` are consistent with their usage across VM init and traces.
- **VM state structures**: Verify that the core VM structures (stack, PC, frame pointers, registers) and their mutators are exported and used consistently.
- **CL MCP usage**:
- Use a "where is this symbol used?" tool to audit IFPAGE-related symbols.
- Build a quick symbol→package map via CL MCP (e.g., a tool that scans packages and reports exports).

## Priority 3 — Ensure Opcode Registration Is Correct (Handler Count Bug)

- **Goal**: Fix the bug where the opcode handler count reports 0 instead of ~190+, and ensure every handler is registered and callable.
- **Tasks**:
- **Audit `src/vm/op-list.lisp`**: Verify each opcode symbol maps to a concrete handler function.
- **Debug registration path**: Trace the code path that builds the opcode dispatch table and the function that counts/prints the number of registered handlers.
- **Add sanity checks**: On startup, assert that the handler table length matches the expected baseline (e.g., 190+), otherwise log detailed diagnostics.
- **CL MCP usage**:
- Implement or use an MCP tool to list all registered opcode handlers at runtime and compare to the spec (`maiko/src/` and any opcode tables).
- Use MCP to jump between C opcodes and Laiko handlers for quick cross-reference.

## Priority 4 — Achieve First Correct VM Execution with Sysout

- **Goal**: Run Laiko against `starter.sysout` through at least the first N instructions **without crashing**, with coherent VM state.
- **Tasks**:
- **Validate VM initialization**: Mirror the C `start_lisp()` semantics in Laiko (stack pointers, frame pointers, IFPAGE, PC units), using the guidance from `reports/reports/STEP_COMPARISON_STATUS.md` (even though that file is focused on Zig, the invariants apply).
- **Minimal execution run**: Implement a small test harness (already sketched in `tests/run-parity.lisp`) that runs a fixed number of instructions and logs VM state.
- **CL MCP usage**:
- Add an MCP tool that can invoke the Laiko "run N steps" entrypoint and return a light summary (PC, SP, FP, key registers).
- Use MCP to compare the Laiko step log to the C trace (even if just manually at first).

## Priority 5 — Integrate Systematic Parity Testing (C vs Laiko)

- **Goal**: Reuse the existing parity philosophy (as in Zig) for a **Laiko vs Maiko** comparison loop.
- **Tasks**:
- **Trace format alignment**: Ensure Laiko’s trace format fully matches Maiko’s unified trace (same columns, units, and semantics as described in `AGENTS.md` and `reports/CURRENT_STATUS.md`).
- **Parity test runner**: Extend `tests/run-parity.lisp` to:
- Run Laiko and Maiko with the same `EMULATOR_MAX_STEPS`.
- Generate comparable trace files for each.
- **Comparison tooling**: Reuse or adapt `scripts/compare_unified_traces.awk` / `.py` for Laiko vs Maiko.
- **CL MCP usage**:
- Add tools to trigger a full parity test run from Cursor and return summarized divergence info (first differing instruction line, key fields).

## Priority 6 — Fill Implementation Gaps in Laiko Opcodes

- **Goal**: Move Laiko completeness from "infrastructure complete, partial handlers" toward the C implementation’s ~95% opcode coverage.
- **Tasks**:
- **Missing/placeholder handlers**: Identify opcodes that are stubbed, missing, or known-bad (e.g., `handle-nthcdr`, `handle-append`, and friends) and implement/repair them by referencing Maiko’s C code in `maiko/src/`.
- **Functional smoke tests**: For each newly implemented opcode cluster, add small, focused Lisp tests.
- **CL MCP usage**:
- Add or use a tool to list opcodes by status (implemented, stubbed, missing) based on code inspection.
- Provide a quick link from a C opcode implementation to the corresponding Laiko handler for side-by-side viewing.

## Priority 7 — Improve Laiko Debugging and Trace Ergonomics

- **Goal**: Make debugging Laiko parity issues as smooth as the Zig parity workflow described in `AGENTS.md` and `reports/STEP_COMPARISON_STATUS.md`.
- **Tasks**:
- **Debug hooks**: Implement helper functions to dump VM state (stack snapshot, registers, IFPAGE fields) at a particular instruction step.
- **Error reporting**: Ensure that when Laiko hits an unimplemented opcode or internal error, it logs a clear, structured error in the same style as C/Zig.
- **CL MCP usage**:
- Provide MCP tools that call these debug hooks and return structured data back to Cursor for inspection.

## Priority 8 — Documentation and Status Alignment

- **Goal**: Keep documentation in sync with Laiko’s actual progress, as required in `AGENTS.md` and `reports/CURRENT_STATUS.md`.
- **Tasks**:
- **Status updates**: Update `reports/CURRENT_STATUS.md` and related specs to track Laiko’s true opcode coverage, test status, and parity depth as it improves.
- **Implementation notes**: Add or extend Laiko-specific implementation documentation (mirroring Zig’s documentation structure) to capture stack/PC invariants, IFPAGE handling, and any Laiko-specific nuances.
- **CL MCP usage**:
- Use MCP tools for quick indexing of Laiko documentation and code when updating docs.

## How This Uses the CL MCP Server Effectively

- **Code navigation**: Rapidly move between Laiko and Maiko definitions, usage sites, and package/exports via MCP tools.
- **Runtime introspection**: Trigger Laiko runs, small step-executions, state dumps, and handler table inspections directly from Cursor.
- **Parity orchestration**: Kick off Laiko vs Maiko parity runs and get summarized divergence data back into Cursor for iterative debugging.

These priorities are ordered so you first stabilize Laiko’s load and handler registration, then achieve stable execution, and finally deepen parity testing and opcode coverage using the CL MCP server as your main control and inspection interface.
