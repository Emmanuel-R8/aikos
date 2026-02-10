# Comprehensive Multi-Implementation Parity Analysis Plan

## Objective

Achieve systematic parity verification across all four emulator implementations (C, Zig, Common Lisp, TypeScript) by comparing execution traces, identifying divergences, documenting findings, and creating automated comparison infrastructure using an iterative STEP-by-STEP approach.

## Current State Assessment

### Implementations Status

- **C (maiko/)**: Production-ready reference (95% opcode coverage)
- **Zig (zaiko/)**: ~60-70% complete, 15-step parity achieved
- **Common Lisp (laiko/)**: Infrastructure complete, sysout loading blocked by `end-of-file` compilation error
- **TypeScript (taiko/)**: Implementation exists, status unclear

### Existing Infrastructure

- Unified trace format (pipe-delimited) defined in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Comparison scripts: `scripts/compare_unified_traces.py`, `scripts/compare_unified_traces.awk`
- Parity check scripts: `scripts/check_parity.sh` (C/Zig), `scripts/check_parity_laiko.sh` (C/Laiko)
- Documentation: Typst format in `documentation/` directory

## Phase 0: Create Unified Test Harness and Iterative Workflow

### Task 0.1: Create Unified Test Harness Python Script

**File**: `scripts/unified_test_harness.py` (new)
**Purpose**: Single Python script that can run any implementation from execution step 0 to a given end step, producing identical trace output format

**Requirements**:

- Accept parameters:
  - `--implementation {c|zig|lisp|typescript}`: Which emulator to run
  - `--end-step N`: Run emulator from step 0 to step N (inclusive)
  - `--start-step M`: Extract/log only steps M to N from the full trace (default: 0)
  - `--sysout PATH`: Path to sysout file
  - `--output FILE`: Output trace file path (default: `{implementation}_emulator_execution_log.txt`)
- Emulator always runs from step 0 to end-step (provides full context)
- Extract and log only the window from start-step to end-step for focused analysis
- Generate trace file in unified format matching specification
- Handle all four implementations with consistent interface
- Return exit codes: 0=success, 1=execution error, 2=trace format error, 3=emulator not found
- Support JSON metadata output: `--json-metadata FILE` for execution statistics

**Implementation Details**:

- Use subprocess to invoke each emulator with appropriate environment variables
- For C: Set `EMULATOR_MAX_STEPS=end_step`, run emulator, extract lines start-step to end-step from trace
- For Zig: Pass `--max-steps end_step`, extract trace window
- For Lisp: Set `*max-execution-steps*` to end-step, extract trace window
- For TypeScript: Similar parameter passing, extract trace window
- Validate trace file format against specification before returning
- Handle missing executables gracefully
- Support timeout for long-running executions

### Task 0.2: Create Execution Window Analysis Module

**File**: `scripts/analyze_execution_window.py` (new)
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

**Usage**:

- Feed C reference trace → generate structured analysis
- Use analysis for comparison with other implementations
- Auto-generate documentation from analysis

### Task 0.3: Create Iterative Parity Workflow Script

**File**: `scripts/iterative_parity_workflow.py` (new)
**Purpose**: Orchestrate the iterative STEP-by-STEP parity verification process

**Workflow**:

1. Start at STEP=0
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
13. Increment STEP by 5 and repeat

**Features**:

- Maintain state file (`parity_workflow_state.json`) tracking:
  - Current STEP being verified
  - Verification status per STEP range (0-4, 5-9, 10-14, etc.)
  - Last verified STEP
  - Divergences found per window
- Generate structured analysis JSON for each 5-step window
- Auto-generate Typst documentation from analysis
- Support resuming from last verified STEP
- Regression mode: re-verify previously verified ranges when implementations change
- Parallel execution: Run non-C emulators in parallel after C reference is established
- Only proceed to next window when current window matches across all implementations

**Parameters**:

- `--start-step N`: Resume from step N (default: 0)
- `--window-size M`: Size of analysis window (default: 5)
- `--max-step K`: Maximum step to verify (optional, for testing)
- `--sysout PATH`: Path to sysout file
- `--resume`: Resume from last verified step
- `--regression`: Re-verify all previously verified ranges

### Task 0.4: Create MULTI_IMPLEMENTATION_TASKS.md

**File**: `MULTI_IMPLEMENTATION_TASKS.md` (new)
**Purpose**: Track iterative parity verification tasks following tasks template format

**Structure** (following `.specify/templates/tasks-template.md`):

- Tasks organized by STEP ranges (0-4, 5-9, 10-14, 15-19, etc.)
- Each STEP range phase includes:
  - [ ] Run C emulator for window (steps STEP to STEP+5)
  - [ ] Generate structured analysis from C trace
  - [ ] Run Zig emulator for same window
  - [ ] Run Lisp emulator for same window
  - [ ] Run TypeScript emulator for same window
  - [ ] Compare all implementations field-by-field
  - [ ] Identify divergences
  - [ ] Fix divergences in non-matching implementations
  - [ ] Re-verify window after fixes
  - [ ] Document findings in Typst
  - [ ] Update codebase with fixes and comments
  - [ ] Mark window as verified
- Track completion status per STEP range
- Dependencies: later STEP ranges depend on earlier ones being verified
- Parallel opportunities: Non-C emulators can run in parallel after C reference is established

## Phase 1: Fix Immediate Blockers

### Task 1.1: Resolve Laiko Compilation Error

**File**: `laiko/src/data/sysout.lisp`
**Issue**: `end-of-file` condition type being interpreted as function call
**Action**:

- Verify `cl:end-of-file` usage in `handler-case` clauses
- Check for package shadowing or macro expansion issues
- Ensure all `handler-case` clauses use correct condition type syntax
- Test compilation and execution

### Task 1.2: Verify Laiko Trace Generation

**Files**: `laiko/src/vm/trace.lisp`, `laiko/src/main.lisp`
**Action**:

- Ensure trace format matches unified specification
- Verify trace file generation when `EMULATOR_MAX_STEPS` is set
- Test trace file location and format correctness
- Verify trace extraction works correctly (can extract step window from full trace)

## Phase 2: Establish TypeScript Comparison Infrastructure

### Task 2.1: Assess TypeScript Implementation Status

**Files**: `taiko/src/main.ts`, `taiko/src/vm/trace.ts`, `taiko/src/vm/execution.ts`
**Action**:

- Review TypeScript implementation completeness
- Verify trace format matches unified specification
- Check if sysout loading works
- Verify trace extraction works correctly
- Document current status in `reports/CURRENT_STATUS.md`

### Task 2.2: Create TypeScript Parity Check Script

**File**: `scripts/check_parity_taiko.sh` (new)
**Action**:

- Model after `scripts/check_parity_laiko.sh`
- Handle TypeScript execution (Node.js/deno/bun)
- Generate trace file in unified format
- Integrate with comparison scripts
- Support trace window extraction

### Task 2.3: Verify TypeScript Trace Format Compliance

**File**: `taiko/src/vm/trace.ts`
**Action**:

- Compare trace output format with specification in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Ensure all 13 fields match exactly
- Verify comma-separated sub-fields format
- Test trace file generation
- Verify trace extraction works correctly

## Phase 3: Create Comprehensive Comparison Framework

### Task 3.1: Enhance Comparison Scripts for Window-Based Analysis

**Files**: `scripts/compare_unified_traces.py`, `scripts/compare_unified_traces.awk`
**Action**:

- Extend to support comparing 3-4 implementations simultaneously
- Support comparing specific step windows (not just full traces)
- Generate divergence matrix (which implementations match on which fields)
- Identify common divergence patterns
- Create HTML/JSON report format
- Integrate with structured analysis from `analyze_execution_window.py`

### Task 3.2: Create Multi-Implementation Comparison Script

**File**: `scripts/compare_all_implementations.sh` (new)
**Action**:

- Run all four emulators using `unified_test_harness.py` with same parameters
- Collect trace files for specific step windows
- Generate comparison matrix showing pairwise differences
- Output summary report
- Support window-based comparison (not just full traces)

### Task 3.3: Create Automated Parity Test Suite

**File**: `scripts/run_all_parity_tests.sh` (new)
**Action**:

- Run parity checks for all implementation pairs
- Test with multiple step counts (10, 100, 1000, 10000)
- Support window-based testing (verify specific step ranges)
- Generate comprehensive report
- Exit codes: 0=all match, 1=divergences found, 2=execution errors

## Phase 4: Systematic Divergence Identification (Iterative Approach)

### Task 4.1: Iterative STEP-by-STEP Verification

**Action**:

- Use `iterative_parity_workflow.py` to orchestrate the process
- Start at STEP=0
- For each 5-step window:
  - Run C emulator from 0 to STEP+5, extract steps STEP to STEP+5
  - Generate structured analysis using `analyze_execution_window.py`
  - Run all other emulators for same window
  - Compare traces field-by-field
  - Identify which implementations match C reference
  - When multiple implementations match, use them to inform fixes
  - Fix divergences in non-matching implementations
  - Re-verify current window after fixes
  - Document findings (auto-generate from structured analysis)
  - Update `MULTI_IMPLEMENTATION_TASKS.md`
  - Only proceed when all implementations match for current window
  - Increment STEP by 5
- Continue until target step count reached or all implementations match

### Task 4.2: Execution Window Analysis

**Action**:

- For each 5-step window, generate deep analysis:
  - Opcode execution details (instruction, operands, addressing modes)
  - Memory address calculations (virtual to physical translation)
  - Stack and frame pointer changes
  - Register state transitions
  - Memory content analysis at key addresses
  - Address translation details
  - Flag changes and condition evaluations
- Store analysis in structured JSON format
- Use analysis for comparison and documentation generation

### Task 4.3: Multi-Implementation Comparison Per Window

**Action**:

- Compare all implementations within each 5-step window
- Identify which implementations match C reference
- When multiple match, use them to inform fixes for non-matching ones
- Document divergence patterns per window
- Track which fields diverge (PC, registers, stack, memory, etc.)
- Identify root causes (opcode implementation, memory management, initialization, etc.)

### Task 4.4: Incremental Fix and Verification

**Action**:

- Fix identified divergences before moving to next window
- Re-run verification for current window after fixes
- Only proceed to next window when current window matches across all implementations
- Track fix history per STEP range
- Document fixes in codebase with comments referencing STEP range
- Update Typst documentation with fix details

## Phase 5: Documentation and Reporting

### Task 5.1: Create Parity Status Document

**File**: `documentation/implementations/multi-implementation-parity-status.typ` (new)
**Action**:

- Document current parity status for all implementation pairs
- List verified STEP ranges (0-4, 5-9, etc.)
- List known divergences with step numbers and fields
- Track progress over time
- Include comparison methodology
- Auto-update from workflow state and analysis data

### Task 5.2: Document Divergence Patterns

**File**: `documentation/implementations/divergence-patterns.typ` (new)
**Action**:

- Categorize divergence types (initialization, opcode implementation, memory management, etc.)
- Document common causes (unit confusion, endianness, initialization order)
- Provide examples and fixes per STEP range
- Reference C implementation as ground truth
- Auto-generate from structured analysis data

### Task 5.3: Update Implementation Status Documents

**Files**: `reports/CURRENT_STATUS.md`, `documentation/implementations/*.typ`
**Action**:

- Update completion percentages based on actual parity testing
- Document which opcodes are verified vs. implemented (by STEP range)
- Update known issues lists
- Add parity test results
- Track verified step ranges per implementation

### Task 5.4: Create Comparison Methodology Documentation

**File**: `documentation/specifications/vm-core/parity-testing-methodology.typ` (new)
**Action**:

- Document unified trace format specification
- Explain comparison tools and scripts
- Provide step-by-step parity testing workflow (iterative approach)
- Document window-based analysis methodology
- Include troubleshooting guide
- Explain structured analysis format

### Task 5.5: Auto-Generate Documentation from Analysis

**File**: `scripts/generate_typst_from_analysis.py` (new)
**Action**:

- Read structured analysis JSON from execution windows
- Generate Typst documentation sections automatically
- Update existing Typst files with new findings
- Maintain chronological log of parity improvements
- Generate divergence reports per STEP range

## Phase 6: Continuous Improvement Loop

### Task 6.1: Create Automated Daily Parity Check

**File**: `scripts/daily_parity_check.sh` (new)
**Action**:

- Run comprehensive parity tests daily using iterative workflow
- Generate summary report
- Track divergence trends over time
- Alert on new divergences
- Verify previously verified ranges (regression testing)

### Task 6.2: Create Divergence Fix Workflow

**File**: `scripts/fix_divergence_workflow.sh` (enhance existing)
**Action**:

- Identify first divergence in current window
- Show C reference implementation trace
- Show structured analysis for divergence point
- Suggest fix locations based on analysis
- Verify fix with re-run of current window
- Document fix in Typst and codebase
- Update `MULTI_IMPLEMENTATION_TASKS.md`

### Task 6.3: Integration with CI/CD

**Action**:

- Create GitHub Actions workflow (if applicable)
- Run parity tests on commits using iterative workflow
- Block merges if new divergences introduced in verified ranges
- Generate comparison reports as artifacts
- Track parity progress over time

## Implementation Strategy

### Autonomous Operation

- All scripts use absolute paths for reliability
- No interactive prompts - fully automated execution
- Error handling continues execution even if one implementation fails
- Results logged to timestamped files for historical tracking
- Can run in background or as scheduled jobs
- Exit codes clearly indicate success/failure/partial completion
- State file (`parity_workflow_state.json`) enables resuming and regression testing

### Comparison Methodology

1. **Baseline Reference**: C implementation (maiko) is always the reference ground truth
2. **Trace Format**: Use unified pipe-delimited format from `documentation/specifications/vm-core/trace-and-logging-formats.typ`
3. **Iterative Approach**: Verify in 5-step windows, starting from STEP=0
4. **Window Extraction**: Emulator runs from 0 to STEP+5, but only steps STEP to STEP+5 are extracted for analysis (provides context while focusing analysis)
5. **Step-by-Step Comparison**: Compare instruction-by-instruction, field-by-field within each window
6. **Field Granularity**: Compare sub-fields (TOS, SP, FP, registers, flags) not just major fields
7. **Structured Analysis**: Generate deep analysis (opcodes, memory, addresses) for each window
8. **Multi-Implementation Learning**: When multiple implementations match C, use them to inform fixes
9. **Fix Before Proceed**: Only move to next window when current window matches across all implementations
10. **Documentation**: Auto-generate Typst files from structured analysis immediately after findings

### File Organization

- **Test Harness**: `scripts/unified_test_harness.py`
- **Analysis Tools**: `scripts/analyze_execution_window.py`, `scripts/generate_typst_from_analysis.py`
- **Workflow Orchestration**: `scripts/iterative_parity_workflow.py`
- **Comparison Scripts**: `scripts/` directory
- **Generated Reports**: `reports/parity/` (timestamped subdirectories)
- **Structured Analysis**: `reports/parity/analysis/` (JSON files per step window)
- **Workflow State**: `parity_workflow_state.json` (in repo root)
- **Task Tracking**: `MULTI_IMPLEMENTATION_TASKS.md` (in repo root)
- **Baseline Traces**: `baselines/` directory for reference traces
- **Documentation**: `documentation/implementations/` for Typst files
- **Trace Files**: Named as `{implementation}_emulator_execution_log.txt`

### Execution Priority

1. Phase 0: Create unified test harness and iterative workflow (foundation for everything)
2. Fix immediate blockers (Phase 1) before any comparison work
3. Establish infrastructure (Phases 2-3) before systematic analysis
4. Systematic divergence identification (Phase 4) using iterative approach
5. Documentation (Phase 5) tracks progress and findings (auto-generated)
6. Continuous improvement (Phase 6) maintains long-term parity

### Iterative Workflow Details

**Window Size**: 5 steps (STEP to STEP+4) provides enough context while keeping analysis focused
**Step Increment**: 5 steps per iteration (can be adjusted if needed)
**Emulator Execution**: Always runs from 0 to STEP+5 to provide full context
**Trace Extraction**: Only steps STEP to STEP+5 are extracted and analyzed
**Verification Gate**: All implementations must match before proceeding to next window
**Documentation**: Auto-generated from structured analysis after each window is verified

## Success Criteria

1. **Unified Test Harness**: Single Python script can run any implementation and extract step windows
2. **Structured Analysis**: Deep execution analysis generated for each step window
3. **Iterative Workflow**: Automated workflow verifies parity in 5-step windows
4. **Trace Generation**: All four implementations successfully generate unified trace format files
5. **Automated Comparison**: Multi-implementation comparison identifies all divergences automatically
6. **Divergence Documentation**: All identified divergences documented in Typst format with root cause analysis
7. **Incremental Verification**: Each 5-step window verified before proceeding to next
8. **Continuous Verification**: Daily parity check runs autonomously without user intervention
9. **Progress Tracking**: Parity improvements tracked over time with metrics and trends (by step range)
10. **Status Accuracy**: Implementation status documents accurately reflect current parity state (verified step ranges)
11. **Regression Detection**: New divergences in previously verified ranges are automatically detected and flagged
12. **Fix Workflow**: Clear process for identifying, fixing, and verifying divergence resolutions
13. **Task Tracking**: `MULTI_IMPLEMENTATION_TASKS.md` accurately reflects verification status

## Risk Mitigation

### Missing Implementations

- **Risk**: One or more implementations may not compile or run
- **Mitigation**: Scripts handle missing executables gracefully, continue with available implementations, report which are missing

### Trace Format Differences

- **Risk**: Implementations may generate traces in slightly different formats
- **Mitigation**: Validation step ensures format compliance before comparison, format errors reported clearly

### Long Execution Times

- **Risk**: Large step counts may cause very long execution times
- **Mitigation**: Window-based approach limits analysis scope, progress indicators show status, can interrupt and resume from last verified step

### Documentation Drift

- **Mitigation**: Automated Typst generation from structured analysis keeps documentation current, timestamped reports maintain history

### Build Failures

- **Risk**: Implementation builds may fail during parity testing
- **Mitigation**: Scripts continue with available implementations, build errors logged separately from parity results

### Compilation Errors Blocking Progress

- **Risk**: Compilation errors prevent trace generation (e.g., Laiko end-of-file issue)
- **Mitigation**: Phase 1 prioritizes fixing blockers, clear error messages guide resolution

### Divergence Analysis Complexity

- **Risk**: Large number of divergences may be overwhelming
- **Mitigation**: Window-based approach focuses on 5 steps at a time, systematic categorization (by field, opcode, implementation pair), prioritization by impact, incremental fixes

### State File Corruption

- **Risk**: `parity_workflow_state.json` may become corrupted
- **Mitigation**: Backup state file, validate JSON structure, support manual state editing, resume from last known good step

### Window Size Too Small/Large

- **Risk**: 5-step window may not provide enough context or may be too large to analyze
- **Mitigation**: Window size is configurable, can be adjusted based on experience, structured analysis provides full context

## Dependencies

### Prerequisites

- All implementations must be buildable (or at least identifiable as missing)
- Unified trace format specification must be finalized in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Comparison scripts (`compare_unified_traces.py`, `compare_unified_traces.awk`) must exist or be created
- Documentation structure (`documentation/implementations/`) must be established
- Python 3.x with standard library (subprocess, json, argparse)

### Task Dependencies

- **Phase 0 → Phase 1**: Unified test harness must exist before fixing blockers (can test fixes)
- **Phase 0 → Phase 2**: Test harness needed to verify TypeScript trace generation
- **Phase 0 → Phase 3**: Test harness needed for comprehensive comparison framework
- **Phase 0 → Phase 4**: Iterative workflow depends on unified test harness and analysis module
- **Phase 1 → Phase 4**: Must fix Laiko compilation before iterative verification
- **Phase 2 → Phase 4**: Must verify TypeScript trace generation before iterative verification
- **Phase 3 → Phase 4**: Must have comparison framework before systematic divergence identification
- **Phase 4 → Phase 5**: Must identify divergences before documenting them
- **Phase 5 → Phase 6**: Must have documentation structure before continuous improvement automation

### External Dependencies

- Python 3.x for test harness and workflow scripts
- Awk for text processing (if using awk-based comparison)
- Typst for documentation generation
- Build tools for each implementation (gcc/clang for C, zig for Zig, sbcl/cl for Lisp, node/deno/bun for TypeScript)

## Milestones

### Milestone 0: Unified Test Harness Complete

- **Deliverables**:
  - `scripts/unified_test_harness.py` created and tested
  - Can run all four implementations with consistent interface
  - Trace window extraction working correctly
  - Structured analysis module (`analyze_execution_window.py`) created
- **Verification**: Successfully run each implementation and extract step windows

### Milestone 1: Iterative Workflow Infrastructure Complete

- **Deliverables**:
  - `scripts/iterative_parity_workflow.py` created and tested
  - `MULTI_IMPLEMENTATION_TASKS.md` created with initial structure
  - Workflow state management working
  - Can run first iteration (STEP=0, window 0-4)
- **Verification**: Successfully complete first 5-step window verification

### Milestone 2: All Implementations Generating Traces

- **Deliverables**:
  - Laiko compilation error fixed
  - All four implementations successfully generate unified trace format
  - Trace files verified for format compliance
  - Trace window extraction working for all implementations
- **Verification**: Run each implementation with test harness and verify trace generation

### Milestone 3: Multi-Implementation Comparison Infrastructure

- **Deliverables**:
  - Enhanced comparison scripts support 3-4 implementations
  - Window-based comparison working
  - TypeScript parity check script created
  - Comparison integrates with structured analysis
- **Verification**: Successfully compare all implementations for a step window

### Milestone 4: First Verified Step Ranges

- **Deliverables**:
  - Steps 0-4 verified across all implementations
  - Steps 5-9 verified across all implementations
  - Structured analysis generated for verified ranges
  - Documentation auto-generated for verified ranges
- **Verification**: All implementations match C reference for first two windows

### Milestone 5: Automated Documentation Generation

- **Deliverables**:
  - Typst documentation auto-generated from structured analysis
  - Parity status document created and auto-updated
  - Divergence patterns document created
  - Implementation status documents updated
- **Verification**: Documentation accurately reflects verified step ranges

### Milestone 6: Continuous Improvement Automation

- **Deliverables**:
  - Daily parity check script operational
  - Divergence fix workflow documented and tested
  - CI/CD integration (if applicable) configured
  - Regression testing working
- **Verification**: Automated checks run successfully without user intervention

### Milestone 7: Extended Parity Verification

- **Deliverables**:
  - Steps 0-99 verified across all implementations (20 windows)
  - All divergences in verified ranges fixed
  - Comprehensive documentation for verified ranges
- **Verification**: All implementations match C reference for first 100 steps

## Next Steps After Plan Approval

1. **Immediate**: Create unified test harness (`scripts/unified_test_harness.py`) - Phase 0, Task 0.1
2. **Immediate**: Create execution analysis module (`scripts/analyze_execution_window.py`) - Phase 0, Task 0.2
3. **Immediate**: Create iterative workflow script (`scripts/iterative_parity_workflow.py`) - Phase 0, Task 0.3
4. **Immediate**: Create `MULTI_IMPLEMENTATION_TASKS.md` - Phase 0, Task 0.4
5. **Short-term**: Fix Laiko compilation error (Task 1.1) - resolve `end-of-file` condition handling
6. **Short-term**: Verify all implementations can generate traces using test harness (Tasks 1.2, 2.1-2.3)
7. **Short-term**: Enhance comparison scripts for window-based analysis (Task 3.1)
8. **Medium-term**: Run first iterative verification window (STEP=0, steps 0-4) - Phase 4, Task 4.1
9. **Medium-term**: Begin systematic divergence documentation (Task 5.1-5.5)
10. **Long-term**: Establish continuous improvement loop (Phase 6) - automate ongoing verification

## References and Resources

### Key Files

- **Trace Format Specification**: `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- **Comparison Scripts**:
  - `scripts/compare_unified_traces.py`
  - `scripts/compare_unified_traces.awk`
- **Existing Parity Scripts**:
  - `scripts/check_parity.sh` (C/Zig)
  - `scripts/check_parity_laiko.sh` (C/Laiko)
- **Implementation Directories**:
  - C: `maiko/`
  - Zig: `zaiko/`
  - Common Lisp: `laiko/`
  - TypeScript: `taiko/`

### Documentation Structure

- Implementation status: `documentation/implementations/`
- Specifications: `documentation/specifications/vm-core/`
- Reports: `reports/parity/` (to be created)
- Structured analysis: `reports/parity/analysis/` (to be created)

### Task Tracking

- **Task Template**: `.specify/templates/tasks-template.md`
- **Plan Template**: `.specify/templates/plan-template.md`
- **Task File**: `MULTI_IMPLEMENTATION_TASKS.md` (to be created)

### Related Plans

- `systematic_parity_verification_across_all_implementations.plan.md` - Related autonomous verification framework
- `update_trace_format_for_awk-friendly_parsing.plan.md` - Trace format standardization

### External Resources

- C reference implementation documentation
- Unified trace format specification (Typst)
- Comparison tool documentation