---
name: Multi-Implementation Parity Analysis
overview: ""
todos: []
---

# Multi-Implementation Parity Analysis Plan

## Objective

Achieve systematic parity verification across all four emulator implementations (C, Zig, Common Lisp, TypeScript) by comparing execution traces, identifying divergences, documenting findings, and creating automated comparison infrastructure using an iterative STEP-by-STEP approach that runs autonomously for extended periods.

## Current State

- **C (maiko/)**: Production-ready reference (95% opcode coverage)
- **Zig (zaiko/)**: ~60-70% complete, 15-step parity achieved
- **Common Lisp (laiko/)**: Infrastructure complete, sysout loading may be blocked by `end-of-file` compilation error (verify in Phase 1.1)
- **TypeScript (taiko/)**: Implementation exists, status unclear

**Existing Infrastructure**:

- Complete detailed plan in [`reports/parity/multi_implementation_parity_plan.md`](reports/parity/multi_implementation_parity_plan.md)
- Unified trace format: [`documentation/specifications/vm-core/trace-and-logging-formats.typ`](documentation/specifications/vm-core/trace-and-logging-formats.typ)
- Comparison scripts: [`scripts/compare_unified_traces.py`](scripts/compare_unified_traces.py), [`scripts/compare_unified_traces.awk`](scripts/compare_unified_traces.awk)
- Parity check scripts: [`scripts/check_parity.sh`](scripts/check_parity.sh), [`scripts/check_parity_laiko.sh`](scripts/check_parity_laiko.sh)

## Pre-Flight Checklist

Before beginning implementation:

- [ ] Verify Laiko compiles: `cd laiko && sbcl --script load-emulator.lisp`
- [ ] Verify TypeScript builds: `cd taiko && npm run build` (or appropriate command)
- [ ] Confirm task template exists: `.specify/templates/tasks-template.md` or `medley/.specify/templates/tasks-template.md`
- [ ] Verify default sysout exists: `medley/internal/loadups/starter.sysout`
- [ ] Confirm Python 3.12+ available: `python3 --version`
- [ ] Verify comparison scripts exist and are executable

## Phase 0: Create Unified Test Harness and Iterative Workflow

### Task 0.1: Create Unified Test Harness

**File**: [`scripts/unified_test_harness.py`](scripts/unified_test_harness.py) (new)

**Requirements**:

- Accept parameters:
- `--implementation {c|zig|lisp|typescript}`: Which emulator to run
- `--end-step N`: Run emulator from step 0 to step N (inclusive)
- `--start-step M`: Extract/log only steps M to N from full trace (default: 0)
- `--sysout PATH`: Path to sysout file (default: `medley/internal/loadups/starter.sysout`)
- `--output FILE`: Output trace file path (default: `{implementation}_emulator_execution_log.txt`)
- `--json-metadata FILE`: Output execution statistics in JSON format
- Emulator always runs from 0 to end-step (provides full context)
- Extract and log only window from start-step to end-step for focused analysis
- Generate trace file in unified format matching specification
- Handle all four implementations with consistent interface
- Return exit codes: 0=success, 1=execution error, 2=trace format error, 3=emulator not found
- Support timeout: 30 seconds default, kill after 31 seconds
- **File Size Management**: If exceeds 300 lines, split into:
- `scripts/unified_test_harness/main.py` (orchestration)
- `scripts/unified_test_harness/runners.py` (implementation-specific runners)
- `scripts/unified_test_harness/trace_extractor.py` (trace window extraction)
- **Safe File Operations**: Never use `rm`; archive old trace files to `reports/parity/archive/{timestamp}/` instead

**Implementation Details**:

- For C: Set `EMULATOR_MAX_STEPS=end_step`, run emulator, extract lines start-step to end-step from trace
- For Zig: Pass `--max-steps end_step`, extract trace window
- For Lisp: Set `*max-execution-steps*` to end-step, extract trace window
- For TypeScript: Similar parameter passing, extract trace window
- Validate trace file format against specification before returning
- Handle missing executables gracefully

### Task 0.2: Create Execution Window Analysis Module

**File**: [`scripts/analyze_execution_window.py`](scripts/analyze_execution_window.py) (new)

**Purpose**: Deep analysis of execution within a STEP window (e.g., STEP to STEP+5)

**Input**: Trace file (unified format) for a specific step window

**Output**: Structured JSON analysis with:

- Opcodes executed with full details (instruction, operands, addressing modes)
- Memory address calculations and accesses (virtual to physical translation)
- Stack pointer and frame pointer changes per step
- Register state transitions (before/after each instruction)
- Memory content at key addresses (PC, stack top, frame pointers)
- Address translation details (vpage, offset, virtual address, physical address)
- Flag changes and condition evaluations
- Stack summary changes (TOS, N1, N2)
- Memory context changes

**File Size Management**: If exceeds 300 lines, split into parser and analyzer modules

### Task 0.3: Create Iterative Parity Workflow Script

**File**: [`scripts/iterative_parity_workflow.py`](scripts/iterative_parity_workflow.py) (new)

**Purpose**: Orchestrate iterative STEP-by-STEP parity verification process

**Workflow**:

1. Start at STEP=0 (or resume from last verified STEP)
2. Run C emulator from 0 to STEP+5 using `unified_test_harness.py`
3. Extract and log only steps STEP to STEP+5 from full trace
4. Analyze execution window using `analyze_execution_window.py` → generate structured analysis
5. Run all other emulators (Zig, Lisp, TypeScript) for same window (0 to STEP+5, extract STEP to STEP+5)
6. Compare traces field-by-field using existing comparison scripts
7. Identify which implementations are correct/incorrect
8. When multiple implementations match C, use them to inform fixes for non-matching ones
9. Fix incorrect implementations
10. Re-verify current window after fixes
11. Document findings in Typst and codebase (auto-generate from structured analysis)
12. Update `MULTI_IMPLEMENTATION_TASKS.md` with completion status
13. Commit state and progress to git
14. Update dashboard file
15. Increment STEP by 5 and repeat

**Features**:

- **State Management**: Maintain state file (`parity_workflow_state.json`) with atomic writes, change log (`parity_workflow_state_changelog.json`)
- **Checkpointing**: Git commit after each task completion
- **Recovery**: Resume from last git commit on restart
- **Retry Logic**: Exponential backoff for transient errors
- **Signal Handling**: Graceful shutdown on SIGTERM/SIGINT
- **Logging**: JSON logs with git commit references
- **Dashboard Updates**: Update dashboard file after each task
- \*\*Parallel
