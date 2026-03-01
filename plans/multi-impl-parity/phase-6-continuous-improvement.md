## Phase 6: Continuous Improvement Loop

### Task 6.1: Create Automated Daily Parity Check

**File**: `scripts/daily_parity_check.sh` (new)

**Action**:

- Run comprehensive parity tests daily using iterative workflow
- Generate summary report
- Track divergence trends over time
- Alert on new divergences
- Verify previously verified ranges (regression testing)
- **Safe File Operations**: Archive instead of delete
- **Documentation**: Comprehensive comments
- **Git Commit**: Commit reports

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
- **Documentation**: Update comments
- **Git Commit**: Commit fixes

### Task 6.3: Integration with CI/CD

**Action**:

- Create GitHub Actions workflow (if applicable)
- Run parity tests on commits using iterative workflow
- Block merges if new divergences introduced in verified ranges
- Generate comparison reports as artifacts
- Track parity progress over time
- **Git Commit**: Commit CI/CD configuration

## Implementation Strategy

### Autonomous Operation

- All scripts use absolute paths for reliability
- No interactive prompts - fully automated execution
- Error handling continues execution even if one implementation fails
- Results logged to timestamped files for historical tracking
- Can run in background or as scheduled jobs (screen/tmux)
- Exit codes clearly indicate success/failure/partial completion
- State file (`parity_workflow_state.json`) enables resuming and regression testing
- Atomic state file writes prevent corruption
- Git commits after each task enable recovery
- Signal handlers enable graceful shutdown

### Comparison Methodology

1. **Baseline Reference**: C implementation (maiko) is always the reference ground truth
2. **Trace Format**: Use unified pipe-delimited format from `documentation/specifications/vm-core/trace-and-logging-formats.typ`
3. **Iterative Approach**: Verify in 6-step windows (STEP to STEP+5), starting from STEP=0
4. **Window Extraction**: Emulator runs from 0 to STEP+5, but only steps STEP to STEP+5 are extracted for analysis (provides context while focusing analysis)
5. **Step-by-Step Comparison**: Compare instruction-by-instruction, field-by-field within each window
6. **Field Granularity**: Compare sub-fields (TOS, SP, FP, registers, flags) not just major fields
7. **Structured Analysis**: Generate deep analysis (opcodes, memory, addresses) for each window
8. **Multi-Implementation Learning**: When multiple implementations match C, use them to inform fixes
9. **Fix Before Proceed**: Only move to next window when current window matches across all implementations
10. **Documentation**: Auto-generate Typst files from structured analysis immediately after findings

### File Organization

- **Test Harness**: `scripts/unified_test_harness.py` (or package if >300 lines)
- **Analysis Tools**: `scripts/analyze_execution_window.py`, `scripts/generate_typst_from_analysis.py`
- **Workflow Orchestration**: `scripts/iterative_parity_workflow.py`
- **Recovery Tools**: `scripts/recover_workflow_state.sh`
- **Dashboard**: `scripts/update_dashboard.py`
- **Comparison Scripts**: `scripts/` directory
- **Generated Reports**: `reports/parity/` (timestamped subdirectories)
- **Structured Analysis**: `reports/parity/analysis/` (JSON files per step window, compressed after verification)
- **Archives**: `reports/parity/archive/{timestamp}/` (old traces, logs, analysis)
- **Workflow State**: `parity_workflow_state.json` (in repo root, atomic writes)
- **State Change Log**: `parity_workflow_state_changelog.json` (in repo root)
- **Dashboard File**: `parity_workflow_dashboard.json` (in repo root, atomic writes)
- **Task Tracking**: `MULTI_IMPLEMENTATION_TASKS.md` (in repo root)
- **Logs**: `logs/parity_workflow_YYYYMMDD.jsonl` (rotated daily, compressed after 7 days)
- **Baseline Traces**: `baselines/` directory for reference traces
- **Documentation**: `documentation/implementations/` for Typst files
- **Trace Files**: Named as `{implementation}_emulator_execution_log.txt` (archived or cleared after verification)

### Execution Priority

1. Phase 0: Create unified test harness and iterative workflow (foundation for everything)
2. Fix immediate blockers (Phase 1) before any comparison work
3. Establish infrastructure (Phases 2-3) before systematic analysis
4. Systematic divergence identification (Phase 4) using iterative approach
5. Documentation (Phase 5) tracks progress and findings (auto-generated)
6. Continuous improvement (Phase 6) maintains long-term parity

### Iterative Workflow Details

**Window Size**: 6 steps (STEP to STEP+5 inclusive) balances context vs. analysis complexity

- Provides sufficient context for multi-instruction patterns (function calls, loops, conditionals)
- Keeps analysis focused and manageable (not overwhelming with too many steps)
- Allows systematic progression through execution trace
- Can be adjusted via `--window-size` parameter if needed

**Step Increment**: 6 steps per iteration (can be adjusted if needed)

**Emulator Execution**: Always runs from 0 to STEP+5 to provide full context

**Trace Extraction**: Only steps STEP to STEP+5 are extracted and analyzed

**Verification Gate**: All implementations must match before proceeding to next window

**Documentation**: Auto-generated from structured analysis after each window is verified

**Checkpointing**: Git commit after each task completion

**Recovery**: Resume from last git commit on restart

**Partial Completion**: Break down tasks if partial completion detected

## Success Criteria

1. **Unified Test Harness**: Single Python script can run any implementation and extract step windows
2. **Structured Analysis**: Deep execution analysis generated for each step window
3. **Iterative Workflow**: Automated workflow verifies parity in 6-step windows (STEP to STEP+5)
4. **Long-Running Execution**: Workflow runs autonomously for hours/days without losing context
5. **State Persistence**: State file maintained with atomic writes and change log
6. **Recovery**: Can recover from last git commit after interruption
7. **Trace Generation**: All four implementations successfully generate unified trace format files
8. **Automated Comparison**: Multi-implementation comparison identifies all divergences automatically
9. **Divergence Documentation**: All identified divergences documented in Typst format with root cause analysis
10. **Incremental Verification**: Each 6-step window (STEP to STEP+5) verified before proceeding to next
11. **Continuous Verification**: Daily parity check runs autonomously without user intervention
12. **Progress Tracking**: Parity improvements tracked over time with metrics and trends (by step range)
13. **Status Accuracy**: Implementation status documents accurately reflect current parity state (verified step ranges)
14. **Regression Detection**: New divergences in previously verified ranges are automatically detected and flagged
15. **Fix Workflow**: Clear process for identifying, fixing, and verifying divergence resolutions
16. **Task Tracking**: `MULTI_IMPLEMENTATION_TASKS.md` accurately reflects verification status
17. **File Management**: All files under 500 lines, no `rm` commands used
18. **Documentation**: Comprehensive documentation and comments maintained throughout
19. **Git Commits**: Regular commits after each task with descriptive messages

## Risk Mitigation

### Missing Implementations

- **Risk**: One or more implementations may not compile or run
- **Mitigation**: Scripts handle missing executables gracefully, continue with available implementations, report which are missing

### Trace Format Differences

- **Risk**: Implementations may generate traces in slightly different formats
- **Mitigation**: Validation step ensures format compliance before comparison, format errors reported clearly

### Long Execution Times

- **Risk**: Large step counts may cause very long execution times
- **Mitigation**: Window-based approach limits analysis scope, progress indicators show status, can interrupt and resume from last verified step, timeout handling prevents hangs

### Documentation Drift

- **Mitigation**: Automated Typst generation from structured analysis keeps documentation current, timestamped reports maintain history, regular documentation updates required

### Build Failures

- **Risk**: Implementation builds may fail during parity testing
- **Mitigation**: Scripts continue with available implementations, build errors logged separately from parity results

### Compilation Errors Blocking Progress

- **Risk**: Compilation errors prevent trace generation (e.g., Laiko end-of-file issue)
- **Mitigation**: Phase 1 prioritizes fixing blockers, clear error messages guide resolution

### Divergence Analysis Complexity

- **Risk**: Large number of divergences may be overwhelming
- **Mitigation**: Window-based approach focuses on 6 steps at a time (STEP to STEP+5), systematic categorization (by field, opcode, implementation pair), prioritization by impact, incremental fixes, task breakdown for partial completion

### State File Corruption

- **Risk**: `parity_workflow_state.json` may become corrupted
- **Mitigation**: Atomic writes prevent corruption, change log tracks recent changes, git commits provide recovery points, recovery script restores from last commit

### Window Size Too Small/Large

- **Risk**: 6-step window (STEP to STEP+5) may not provide enough context or may be too large to analyze
- **Mitigation**: Window size is configurable, can be adjusted based on experience, structured analysis provides full context

### Process Interruption

- **Risk**: Process may be interrupted (power loss, system crash, manual stop)
- **Mitigation**: Git commits after each task enable recovery, state file with atomic writes, recovery script restores from last commit, signal handlers enable graceful shutdown

### File Size Growth

- **Risk**: Files may grow beyond 500 lines
- **Mitigation**: Monitor file sizes, refactor at 400 lines, split into modules/packages, pre-commit checks

## Dependencies

### Prerequisites

- All implementations must be buildable (or at least identifiable as missing)
- Unified trace format specification must be finalized in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Comparison scripts (`compare_unified_traces.py`, `compare_unified_traces.awk`) must exist or be created
- Documentation structure (`documentation/implementations/`) must be established
- Python 3.12+ with standard library (subprocess, json, argparse, signal, tempfile, shutil, gzip)
- Git for version control and recovery
- Screen or tmux for long-running sessions

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

- Python 3.12+ for test harness and workflow scripts
- Awk for text processing (if using awk-based comparison)
- Typst for documentation generation
- Build tools for each implementation (gcc/clang for C, zig for Zig, sbcl/cl for Lisp, node/deno/bun for TypeScript)
- Git for version control
- Screen or tmux for long-running sessions

## Milestones

### Milestone 0: Unified Test Harness Complete

- **Deliverables**: - `scripts/unified_test_harness.py` created and tested (or package if >300 lines) - Can run all four implementations with consistent interface - Trace window extraction working correctly - Structured analysis module (`analyze_execution_window.py`) created - All files under 500 lines - Comprehensive documentation and comments
- **Verification**: Successfully run each implementation and extract step windows

### Milestone 1: Iterative Workflow Infrastructure Complete

- **Deliverables**: - `scripts/iterative_parity_workflow.py` created and tested (or package if >300 lines) - `scripts/recover_workflow_state.sh` created - `scripts/update_dashboard.py` created - `MULTI_IMPLEMENTATION_TASKS.md` created with initial structure following template - Workflow state management working (atomic writes, change log) - Recovery mechanism working (from git commits) - Can run first iteration (STEP=0, window 0-5) - All files under 500 lines - Comprehensive documentation
- **Verification**: Successfully complete first 6-step window verification (0-5) with git commits

### Milestone 2: All Implementations Generating Traces

- **Deliverables**: - Laiko compilation error fixed - All four implementations successfully generate unified trace format - Trace files verified for format compliance - Trace window extraction working for all implementations - Documentation updated - Git commits for all fixes
- **Verification**: Run each implementation with test harness and verify trace generation

### Milestone 3: Multi-Implementation Comparison Infrastructure

- **Deliverables**: - Enhanced comparison scripts support 3-4 implementations - Window-based comparison working - TypeScript parity check script created - Comparison integrates with structured analysis - All files under 500 lines - Documentation updated
- **Verification**: Successfully compare all implementations for a step window

### Milestone 4: First Verified Step Ranges

- **Deliverables**:
- Steps 0-5 verified across all implementations
- Steps 6-11 verified across all implementations - Structured analysis generated for verified ranges - Documentation auto-generated for verified ranges - All fixes committed to git - Dashboard updated
- **Verification**: All implementations match C reference for first two windows

### Milestone 5: Automated Documentation Generation

- **Deliverables**: - Typst documentation auto-generated from structured analysis - Parity status document created and auto-updated - Divergence patterns document created - Implementation status documents updated - All files under 500 lines
- **Verification**: Documentation accurately reflects verified step ranges

### Milestone 6: Continuous Improvement Automation

- **Deliverables**: - Daily parity check script operational - Divergence fix workflow documented and tested - CI/CD integration (if applicable) configured - Regression testing working - Long-running execution tested (hours/days)
- **Verification**: Automated checks run successfully without user intervention, recovery tested

### Milestone 7: Extended Parity Verification

- **Deliverables**: - Steps 0-95 verified across all implementations (16 windows of 6 steps each) - All divergences in verified ranges fixed - Comprehensive documentation for verified ranges - All state files, logs, and analysis files maintained - Recovery tested multiple times
- **Verification**: All implementations match C reference for first 100 steps

## Pre-Flight Checklist

Before beginning implementation, verify:

- [ ] **Laiko Compilation**: Test if Laiko compiles successfully (Phase 1.1 may be unnecessary if already fixed)
  ```bash
  cd laiko && sbcl --script load-emulator.lisp
  ```
- [ ] **TypeScript Build**: Verify TypeScript implementation builds (Phase 2.1 assessment)
  ```bash
  cd taiko && npm run build  # or appropriate build command
  ```
- [ ] **Task Template Location**: Confirm `.specify/templates/tasks-template.md` exists (or use `medley/.specify/templates/tasks-template.md`)
- [ ] **Default Sysout Exists**: Verify `medley/internal/loadups/starter.sysout` exists and is accessible
- [ ] **Python Version**: Confirm Python 3.12+ is available (`python3 --version`)
- [ ] **Comparison Scripts**: Verify `scripts/compare_unified_traces.py` and `scripts/compare_unified_traces.awk` exist and are executable

## Next Steps After Plan Approval

1. **Immediate**: Create unified test harness (`scripts/unified_test_harness.py`) - Phase 0, Task 0.1
2. **Immediate**: Create execution analysis module (`scripts/analyze_execution_window.py`) - Phase 0, Task 0.2
3. **Immediate**: Create iterative workflow script (`scripts/iterative_parity_workflow.py`) - Phase 0, Task 0.3
4. **Immediate**: Create recovery script (`scripts/recover_workflow_state.sh`) - Phase 0, Task 0.4
5. **Immediate**: Create dashboard generator (`scripts/update_dashboard.py`) - Phase 0, Task 0.5
6. **Immediate**: Create `MULTI_IMPLEMENTATION_TASKS.md` - Phase 0, Task 0.6
7. **Short-term**: Fix Laiko compilation error (Task 1.1) - resolve `end-of-file` condition handling
8. **Short-term**: Verify all implementations can generate traces using test harness (Tasks 1.2, 2.1-2.3)
9. **Short-term**: Enhance comparison scripts for window-based analysis (Task 3.1)
10. **Medium-term**: Run first iterative verification window (STEP=0, steps 0-5) - Phase 4, Task 4.1
11. **Medium-term**: Begin systematic divergence documentation (Task 5.1-5.5)
12. **Long-term**: Establish continuous improvement loop (Phase 6) - automate ongoing verification

## References and Resources

### Key Files

- **Trace Format Specification**: `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- **Comparison Scripts**: - `scripts/compare_unified_traces.py` - `scripts/compare_unified_traces.awk`
- **Existing Parity Scripts**: - `scripts/check_parity.sh` (C/Zig) - `scripts/check_parity_laiko.sh` (C/Laiko)
- **Implementation Directories**: - C: `maiko/` - Zig: `zaiko/` - Common Lisp: `laiko/` - TypeScript: `taiko/`

### Documentation Structure

- Implementation status: `documentation/implementations/`
- Specifications: `documentation/specifications/vm-core/`
- Reports: `reports/parity/` (to be created)
- Structured analysis: `reports/parity/analysis/` (to be created)
- Archives: `reports/parity/archive/` (to be created)
- Logs: `logs/` (to be created)

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
