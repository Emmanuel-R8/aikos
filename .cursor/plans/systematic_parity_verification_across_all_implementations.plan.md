---
name: Systematic Parity Verification Across All Implementations
overview: Create an autonomous, systematic framework to compare C, Zig, TypeScript, and Lisp emulator implementations, identify parity issues, document findings in Typst, and enable continuous verification without user intervention.
todos:
  - id: fix-laiko-compilation
    content: Fix Laiko end-of-file compilation error in sysout.lisp to enable trace generation
    status: completed
  - id: verify-trace-generation
    content: Verify all four implementations (C, Zig, TypeScript, Lisp) can generate unified trace format
    status: completed
    dependencies:
      - fix-laiko-compilation
  - id: create-multi-comparison-script
    content: Create scripts/compare_all_implementations.sh to run all emulators and collect traces
    status: completed
    dependencies:
      - verify-trace-generation
  - id: create-multi-comparison-tool
    content: Create scripts/compare_multi_implementation.py to compare traces from 4 implementations simultaneously
    status: completed
    dependencies:
      - create-multi-comparison-script
  - id: create-parity-suite
    content: Create scripts/run_parity_suite.sh for autonomous test runner with multiple step counts
    status: completed
    dependencies:
      - create-multi-comparison-tool
  - id: create-divergence-catalog
    content: Create documentation/implementations/parity-divergences.typ to systematically document all divergences
    status: completed
    dependencies:
      - create-multi-comparison-tool
  - id: create-field-analyzers
    content: Create field-specific analysis scripts (PC, stack, memory, opcode) for deep divergence analysis
    status: completed
    dependencies:
      - create-multi-comparison-tool
  - id: create-opcode-matrix
    content: Create documentation/implementations/opcode-coverage-matrix.typ to track opcode implementation status
    status: completed
    dependencies:
      - create-divergence-catalog
  - id: create-typst-generator
    content: Create scripts/generate_parity_report.py to generate Typst documentation from comparison results
    status: completed
    dependencies:
      - create-divergence-catalog
  - id: update-status-docs
    content: Update implementation status Typst files with current parity status and known issues
    status: completed
    dependencies:
      - create-typst-generator
  - id: create-progress-tracker
    content: Create documentation/implementations/parity-progress.typ to track parity improvements over time
    status: completed
    dependencies:
      - update-status-docs
  - id: create-continuous-script
    content: Create scripts/continuous_parity_check.sh for autonomous continuous verification
    status: completed
    dependencies:
      - create-parity-suite
  - id: create-regression-detection
    content: Create scripts/detect_regressions.py to compare current results with baseline and flag changes
    status: completed
    dependencies:
      - create-continuous-script
  - id: create-dashboard
    content: Create documentation/implementations/parity-dashboard.typ for high-level parity status view
    status: completed
    dependencies:
      - create-progress-tracker
---

# Systematic Parity Verification Plan

## Overview

This plan establishes an autonomous framework to systematically compare all four emulator implementations (C, Zig, TypeScript, Lisp) against the C reference implementation, identify divergences, document findings in Typst format, and enable continuous verification.

## Current State Assessment

### Implementations Status

- **C (maiko/)**: Production-ready reference (~95% opcode coverage)
- **Zig (zaiko/)**: ~60-70% complete, 15-step parity achieved, 245 TODO/FIXME markers
- **TypeScript (taiko/)**: Exists with unified trace format support
- **Lisp (laiko/)**: Infrastructure complete, sysout loading fixed, execution testing needed

### Existing Infrastructure

- Unified trace format: Pipe-delimited format defined in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Comparison tools: `scripts/compare_unified_traces.py`, `scripts/compare_emulator_execution.sh`
- Parity scripts: `scripts/check_parity.sh`, `scripts/check_parity_laiko.sh`
- Documentation: Typst format in `documentation/` directory

## Phase 1: Fix Immediate Blockers

### Task 1.1: Resolve Laiko Compilation Error

**File**: `laiko/src/data/sysout.lisp`
**Issue**: `end-of-file` condition handler compilation error
**Action**: Fix handler-case syntax to properly handle EOF conditions without function call interpretation
**Verification**: Laiko loads sysout and generates trace files successfully

### Task 1.2: Verify All Implementations Can Generate Traces

**Actions**:

- Test C emulator trace generation: `EMULATOR_MAX_STEPS=100 ./maiko/linux.x86_64/ldesdl starter.sysout`
- Test Zig emulator trace generation: `EMULATOR_MAX_STEPS=100 zig build run -- starter.sysout`
- Test TypeScript emulator trace generation: Verify `taiko/src/vm/trace.ts` produces unified format
- Test Lisp emulator trace generation: Verify `laiko/src/vm/trace.lisp` produces unified format
  **Output**: All implementations generate `*_emulator_execution_log.txt` files

## Phase 2: Create Comprehensive Comparison Framework

### Task 2.1: Extend Comparison Scripts for All Implementations

**File**: `scripts/compare_all_implementations.sh`
**Purpose**: Run all four emulators with same sysout and step count, collect traces, compare systematically
**Features**:

- Accepts `MAX_STEPS` and `SYSOUT_FILE` parameters
- Runs C, Zig, TypeScript, and Lisp emulators in sequence
- Collects all trace files
- Calls comparison tools for each pair
- Generates summary report

### Task 2.2: Create Multi-Implementation Comparison Tool

**File**: `scripts/compare_multi_implementation.py`
**Purpose**: Compare traces from 4 implementations simultaneously
**Features**:

- Parse unified trace format from all implementations
- Identify first divergence point for each pair (C vs Zig, C vs TS, C vs Lisp)
- Generate divergence report with field-by-field analysis
- Output machine-readable JSON for automated processing
- Generate Typst documentation snippets

### Task 2.3: Create Automated Parity Test Runner

**File**: `scripts/run_parity_suite.sh`
**Purpose**: Autonomous test runner that can run continuously
**Features**:

- Runs with different step counts (10, 100, 1000, 10000)
- Compares all implementations at each step count
- Logs results to timestamped files
- Continues even if one implementation fails
- Generates summary statistics

## Phase 3: Systematic Divergence Identification

### Task 3.1: Create Divergence Catalog

**File**: `documentation/implementations/parity-divergences.typ`
**Purpose**: Document all identified divergences systematically
**Structure**:

- Divergence ID (e.g., DIV-001)
- Affected implementations
- First occurrence (step number, PC address)
- Field(s) that diverge
- Root cause analysis
- Fix status
- Related files

### Task 3.2: Implement Field-by-Field Comparison

**Files**:

- `scripts/analyze_pc_divergence.py`
- `scripts/analyze_stack_divergence.py`
- `scripts/analyze_memory_divergence.py`
- `scripts/analyze_opcode_divergence.py`
  **Purpose**: Deep analysis of specific field categories
  **Output**: Typst-formatted analysis sections

### Task 3.3: Create Opcode Coverage Matrix

**File**: `documentation/implementations/opcode-coverage-matrix.typ`
**Purpose**: Track which opcodes are implemented/tested in each implementation
**Structure**: Table with opcodes as rows, implementations as columns, status (implemented/diverges/missing) as cells

## Phase 4: Documentation Automation

### Task 4.1: Create Typst Report Generator

**File**: `scripts/generate_parity_report.py`
**Purpose**: Generate Typst documentation from comparison results
**Features**:

- Reads comparison JSON output
- Generates Typst sections for divergences
- Updates existing Typst files with new findings
- Maintains chronological log of parity improvements

### Task 4.2: Update Implementation Status Documents

**Files**:

- `documentation/implementations/c/c-emulator-complete-verification.typ`
- `documentation/implementations/zig-implementation.typ`
- `documentation/implementations/lisp-implementation.typ`
- `documentation/implementations/typescript-implementation.typ` (create if missing)
  **Action**: Update with current parity status, known issues, completion percentages

### Task 4.3: Create Parity Progress Tracker

**File**: `documentation/implementations/parity-progress.typ`
**Purpose**: Track parity improvements over time
**Structure**: Timeline of parity achievements, step-by-step convergence metrics

## Phase 5: Continuous Verification Setup

### Task 5.1: Create Autonomous Test Script

**File**: `scripts/continuous_parity_check.sh`
**Purpose**: Run continuously without user intervention
**Features**:

- Runs parity checks at regular intervals
- Only reports new divergences
- Updates documentation automatically
- Logs to timestamped files
- Can be interrupted and resumed

### Task 5.2: Create Regression Detection

**File**: `scripts/detect_regressions.py`
**Purpose**: Compare current results with baseline
**Features**:

- Maintains baseline trace files
- Compares new runs against baseline
- Flags regressions (new divergences)
- Flags improvements (resolved divergences)

### Task 5.3: Create Summary Dashboard

**File**: `documentation/implementations/parity-dashboard.typ`
**Purpose**: High-level view of parity status
**Content**:

- Current parity percentages per implementation
- Recent improvements
- Known blockers
- Next priorities

## Phase 6: Implementation-Specific Fixes

### Task 6.1: Zig Implementation Fixes

**Priority**: Address 245 TODO/FIXME markers systematically
**Approach**:

- Categorize by subsystem (memory, opcodes, I/O, graphics)
- Fix one category at a time
- Verify parity after each category
- Document fixes in Typst

### Task 6.2: Lisp Implementation Fixes

**Priority**: Achieve first successful execution trace
**Approach**:

- Fix compilation errors
- Verify sysout loading
- Test basic opcode execution
- Compare traces with C reference

### Task 6.3: TypeScript Implementation Verification

**Priority**: Verify TypeScript implementation status
**Approach**:

- Test trace generation
- Compare with C reference
- Document current status
- Identify gaps

## Implementation Strategy

### Autonomous Operation

- All scripts use absolute paths
- No interactive prompts
- Error handling continues execution
- Results logged to files
- Can run in background

### Comparison Methodology

1. **Baseline**: C implementation is always the reference
2. **Trace Format**: Use unified pipe-delimited format
3. **Step-by-Step**: Compare instruction-by-instruction
4. **Field Granularity**: Compare sub-fields (TOS, SP, FP, registers)
5. **Documentation**: Update Typst files immediately after findings

### File Organization

- Comparison scripts: `scripts/`
- Generated reports: `reports/parity/` (timestamped)
- Baseline traces: `baselines/`
- Documentation: `documentation/implementations/`

## Success Criteria

1. All four implementations generate unified trace format
2. Automated comparison identifies all divergences
3. Divergences documented in Typst format
4. Continuous verification runs without user intervention
5. Parity improvements tracked over time
6. Implementation status accurately reflected in documentation

## Risk Mitigation

- **Missing implementations**: Scripts handle missing executables gracefully
- **Trace format differences**: Validation step ensures format compliance
- **Long execution times**: Step limits prevent infinite loops
- **Documentation drift**: Automated updates keep docs current
- **Build failures**: Scripts continue with available implementations

## Next Steps After Plan Approval

1. Fix Laiko compilation error (Task 1.1)
2. Create multi-implementation comparison script (Task 2.1)
3. Run initial comparison across all implementations
4. Document first set of divergences
5. Begin systematic fixes starting with highest-impact issues
