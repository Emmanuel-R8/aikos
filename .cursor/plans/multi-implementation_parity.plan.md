# Comprehensive Multi-Implementation Parity Analysis Plan

## Objective

Achieve systematic parity verification across all four emulator implementations (C, Zig, Common Lisp, TypeScript) by comparing execution traces, identifying divergences, documenting findings, and creating automated comparison infrastructure using an iterative STEP-by-STEP approach that runs autonomously for extended periods.

## Current State Assessment

### Implementations Status

- **C (maiko/)**: Production-ready reference (95% opcode coverage)
- **Zig (zaiko/)**: ~60-70% complete, 15-step parity achieved
- **Common Lisp (laiko/)**: Infrastructure complete, sysout loading blocked by `end-of-file` compilation error
- **TypeScript (taiko/)**: Implementation exists, status unclear

### Existing Infrastructure

- Detailed plan in `reports/parity/`
- Unified trace format (pipe-delimited) defined in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Comparison scripts: `scripts/compare_unified_traces.py`, `scripts/compare_unified_traces.awk`
- Parity check scripts: `scripts/check_parity.sh` (C/Zig), `scripts/check_parity_laiko.sh` (C/Laiko)
- Documentation: Typst format in `documentation/` directory

## Development Constraints and Standards

### File Management

- **Never use `rm` command**: Use safe alternatives:
                                                                - Python: `os.remove()` with error handling, or move to archive directory
                                                                - Shell scripts: Use `mv` to archive files to `archive/` or `old/` directories with timestamps
                                                                - Trace files: Archive old traces to `reports/parity/archive/{timestamp}/` instead of deleting
                                                                - Temporary files: Use `tempfile` module in Python, or move to `tmp/` directory
                                                                - Successful trace files: Write empty JSON `{}` to file or move to archive
- **File Size Limit**: All source files must be kept under 500 lines
                                                                - If a file exceeds 400 lines, plan refactoring before it reaches 500
                                                                - Split large scripts into logical components (e.g., `unified_test_harness.py` → `unified_test_harness/` with `main.py`, `c_runner.py`, `zig_runner.py`, etc.)
                                                                - Monitor file sizes in pre-commit checks or CI
- **File Organization**:
                                                                - Python scripts: Split into modules if >300 lines
                                                                - Typst documentation: Split large documents into sections/files
                                                                - Keep functions focused and single-purpose

### Documentation and Comments

- **Comprehensive Documentation Updates**: Required at these checkpoints:
                                                                - After each task completion: Update relevant documentation
                                                                - After each 6-step window verification: Update Typst docs with findings
                                                                - After each divergence fix: Document fix in code comments and Typst
                                                                - Weekly: Review and update all documentation for accuracy
- **Code Comments**:
                                                                - Every function must have docstring (Python) or comment block (other languages)
                                                                - Complex logic must have inline comments explaining reasoning
                                                                - Reference STEP ranges in comments when fixing divergences (e.g., `# Fixed for STEP 0-4 window`)
                                                                - Document assumptions and design decisions
- **Documentation Files**:
                                                                - Update `MULTI_IMPLEMENTATION_TASKS.md` after each task
                                                                - Update Typst files after each window verification
                                                                - Update implementation status docs after each milestone

### Git Commit Practices

- **Regular Comprehensive Commits**: Required at these checkpoints:
                                                                - After each task completion: Commit with descriptive message
                                                                - After each 6-step window verification: Commit with window details
                                                                - After each divergence fix: Commit with fix description and STEP range
                                                                - Daily: Commit progress even if incomplete (use WIP prefix)
- **Commit Message Format**:
  ```
  [Phase X.Task Y] Brief description
  
  - What was done
  - STEP range affected (if applicable)
  - Files changed
  - Related issue/divergence ID
  ```

- **Commit Frequency**:
                                                                - Minimum: One commit per task
                                                                - Preferred: One commit per logical unit of work
                                                                - Never: Large commits spanning multiple unrelated changes
- **Commit Contents**: Include state file, task list updates, generated files, log entries

### Task List Maintenance

- **MULTI_IMPLEMENTATION_TASKS.md Format**: Must follow `.specify/templates/tasks-template.md`:
                                                                - Use format: `[ID] [P?] [Story] Description`
                                                                - Include exact file paths in descriptions
                                                                - Mark parallel tasks with `[P]`
                                                                - Group by STEP ranges (0-4, 5-9, etc.) as phases
                                                                - Include checkpoints after each STEP range
                                                                - Update task status immediately after completion
                                                                - Maintain dependencies section
- **Task Updates**:
                                                                - Mark tasks complete `[x]` immediately upon completion
                                                                - Add notes for any deviations from plan
                                                                - Update dependencies if task order changes
                                                                - Commit task list updates after each change

## Long-Running Autonomous Execution

### State Persistence & Recovery

**State File Management**:

- **Atomic Writes**: Always write state files using atomic pattern:
  ```python
  # Write to temp file first, then rename (atomic on most filesystems)
  temp_file = f"{state_file}.tmp"
  with open(temp_file, 'w') as f:
      json.dump(state, f, indent=2)
  os.rename(temp_file, state_file)  # Atomic on Unix
  ```

- **Change Log**: Maintain `parity_workflow_state_changelog.json` with recent changes:
                                                                - Keep last 50 state changes
                                                                - Each entry: `{timestamp, step_range, change_type, details, git_commit}`
                                                                - Rotate when log exceeds 50 entries (archive old entries to `reports/parity/archive/`)
- **Git Commits**: Commit state file after each task completion
                                                                - Commit message: `[State] Updated workflow state after [task description]`
                                                                - Enables recovery from last known good state

**Checkpoint Strategy**:

- **After Each Task**: Create checkpoint (git commit) with:
                                                                - State file
                                                                - Task list updates
                                                                - Any generated files
                                                                - Log entries
                                                                - Dashboard file
- **Checkpoint Contents**:
                                                                - Current STEP being verified
                                                                - Last completed task
                                                                - Verification status
                                                                - Any partial progress

**Recovery Mechanism**:

- **From Last Git Commit**: On restart, workflow:

                                                                1. Checkout last commit (or use current state if git is clean)
                                                                2. Read `parity_workflow_state.json` from last commit
                                                                3. Resume from last verified STEP
                                                                4. If state file is missing, start from STEP=0

- **Recovery Script**: `scripts/recover_workflow_state.sh`:
                                                                - Finds last commit with state file
                                                                - Restores state file
                                                                - Reports recovery point
                                                                - Allows manual override if needed

### Process Management

**Execution Environment**:

- **Screen Session**: Run in screen/tmux for long-running execution:
  ```bash
  screen -S parity_workflow
  # Run workflow script
  # Detach with Ctrl+A, D
  # Reattach with: screen -r parity_workflow
  ```

- **Timeout Handling**: Always use `timeout --kill-after X+1 X` for subprocess calls:
  ```python
  # Example: 30 second timeout, 31 second kill
  subprocess.run(..., timeout=30)  # Python timeout
  # Or in shell:
  timeout --kill-after 31 30 command
  ```

- **Process Monitoring**: Script should handle its own process health:
                                                                - Log heartbeat every N operations
                                                                - Detect if stuck (no progress for extended period)
                                                                - Self-restart capability (optional)

### Error Handling & Retry Logic

**Retry Strategy**:

- **Exponential Backoff**: For transient errors:
  ```python
  max_retries = 5
  base_delay = 1  # seconds
  for attempt in range(max_retries):
      try:
          # operation
          break
      except TransientError as e:
          if attempt < max_retries - 1:
              delay = base_delay * (2 ** attempt)
              time.sleep(delay)
              continue
          raise
  ```

- **Retryable Errors**: Network issues, temporary file locks, subprocess timeouts
- **Non-Retryable Errors**: Syntax errors, missing files, configuration errors

**Partial Completion Handling**:

- **Detection**: If a STEP range is partially completed:

                                                                1. Stop current workflow
                                                                2. Review C reference implementation in deeper detail
                                                                3. Break down remaining tasks into smaller sub-components
                                                                4. Update `MULTI_IMPLEMENTATION_TASKS.md` with new sub-tasks
                                                                5. Commit breakdown and resume from beginning of current window

- **Task Breakdown Process**:
                                                                - Analyze what failed
                                                                - Identify root cause
                                                                - Create granular sub-tasks (each < 1 hour work)
                                                                - Document breakdown in task list
                                                                - Resume with smaller tasks

### Logging & Context Preservation

**Logging Strategy**:

- **JSON Logs**: All logs in JSON format for parsing:
  ```json
  {
    "timestamp": "2024-01-01T12:00:00Z",
    "level": "INFO",
    "step_range": "0-4",
    "task": "T001",
    "message": "Completed C emulator run",
    "git_commit": "abc123",
    "details": {...}
  }
  ```

- **Log Files**:
                                                                - `logs/parity_workflow_YYYYMMDD.jsonl` (one file per day)
                                                                - Rotate daily
                                                                - Compress old logs after 7 days (move to `logs/archive/`)
- **Git Commit References**: Include git commit hash in log entries:
                                                                - Enables tracing log entries to code state
                                                                - Links logs to specific workflow state

**Analysis File Management**:

- **Compression**: Compress analysis JSON files after verification:
                                                                - Keep uncompressed for current window
                                                                - Compress previous windows: `analysis_STEP0-4.json.gz`
                                                                - Use gzip compression
                                                                - Move compressed files to `reports/parity/analysis/archive/`
- **Retention**: Keep all compressed analysis files for historical reference

**Trace File Management**:

- **After Successful Verification**: Clear trace files (avoid `rm`):
  ```python
  # Instead of rm, write empty JSON
  with open(trace_file, 'w') as f:
      f.write('{}')  # Empty JSON
  # Or move to archive
  archive_path = f"reports/parity/archive/{timestamp}/{trace_file}"
  os.makedirs(os.path.dirname(archive_path), exist_ok=True)
  shutil.move(trace_file, archive_path)
  ```

- **Archive Strategy**: Move successful traces to `reports/parity/archive/{timestamp}/`
- **Failed Verification**: Keep trace files for debugging

### Progress Tracking

**Dashboard File**:

- **File**: `parity_workflow_dashboard.json`
- **Contents**:
  ```json
  {
    "current_step": 15,
    "current_window": "15-19",
    "last_verified_step": 14,
    "windows_verified": 3,
    "windows_in_progress": 1,
    "total_divergences_found": 5,
    "divergences_fixed": 3,
    "last_update": "2024-01-01T12:00:00Z",
    "git_commit": "abc123",
    "status": "running",
    "recent_activity": [
      { "timestamp": "...", "action": "...", "step_range": "..." }
    ]
  }
  ```

- **Update Frequency**: After each task completion
- **Atomic Write**: Use same atomic write pattern as state file

### Graceful Shutdown & Recovery

**Signal Handling**:

- **SIGTERM/SIGINT**:

                                                                1. Complete current task if possible
                                                                2. Save state file atomically
                                                                3. Commit current progress to git
                                                                4. Exit gracefully

- **Implementation**:
  ```python
  import signal
  import sys
  
  def signal_handler(sig, frame):
      save_state_atomically()
      git_commit("Workflow interrupted, saving state")
      sys.exit(0)
  
  signal.signal(signal.SIGTERM, signal_handler)
  signal.signal(signal.SIGINT, signal_handler)
  ```


**Recovery After Interruption**:

- **Automatic**: On restart, workflow:

                                                                1. Checks for incomplete operations
                                                                2. Reads state from last git commit
                                                                3. Resumes from last verified STEP
                                                                4. Reports recovery status in dashboard

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
                                                                - `--json-metadata FILE`: Output execution statistics in JSON format
- Emulator always runs from step 0 to end-step (provides full context)
- Extract and log only the window from start-step to end-step for focused analysis
- Generate trace file in unified format matching specification
- Handle all four implementations with consistent interface
- Return exit codes: 0=success, 1=execution error, 2=trace format error, 3=emulator not found
- Support timeout for long-running executions using `timeout --kill-after X+1 X`

**Implementation Details**:

- Use subprocess to invoke each emulator with appropriate environment variables
- For C: Set `EMULATOR_MAX_STEPS=end_step`, run emulator, extract lines start-step to end-step from trace
- For Zig: Pass `--max-steps end_step`, extract trace window
- For Lisp: Set `*max-execution-steps*` to end-step, extract trace window
- For TypeScript: Similar parameter passing, extract trace window
- Validate trace file format against specification before returning
- Handle missing executables gracefully
- **File Size Management**: If script exceeds 300 lines, split into:
                                                                - `scripts/unified_test_harness/main.py` (orchestration)
                                                                - `scripts/unified_test_harness/runners.py` (implementation-specific runners)
                                                                - `scripts/unified_test_harness/trace_extractor.py` (trace window extraction)
- **Documentation**: Include comprehensive docstrings for all functions, module-level docstring explaining architecture
- **Safe File Operations**: Never use `rm`; archive old trace files to `reports/parity/archive/{timestamp}/` instead
- **Git Commit**: Commit after implementation with message: `[Phase 0.1] Create unified test harness with support for all implementations`

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

**Implementation Details**:

- **File Size Management**: If exceeds 300 lines, split into parser and analyzer modules
- **Documentation**: Comprehensive docstrings for all analysis functions
- **Git Commit**: Commit after implementation

### Task 0.3: Create Iterative Parity Workflow Script

**File**: `scripts/iterative_parity_workflow.py` (new)

**Purpose**: Orchestrate the iterative STEP-by-STEP parity verification process

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

- **State Management**:
                                                                - Maintain state file (`parity_workflow_state.json`) with atomic writes
                                                                - Maintain change log (`parity_workflow_state_changelog.json`)
                                                                - Track: current STEP, verification status per STEP range, last verified STEP, divergences found
- **Checkpointing**: Git commit after each task completion
- **Recovery**: Resume from last git commit on restart
- **Retry Logic**: Exponential backoff for transient errors
- **Signal Handling**: Graceful shutdown on SIGTERM/SIGINT
- **Logging**: JSON logs with git commit references
- **Dashboard Updates**: Update dashboard file after each task
- **Parallel Execution**: Run non-C emulators in parallel after C reference is established
- **Verification Gate**: Only proceed to next window when current window matches across all implementations
- **Partial Completion Handling**: Break down tasks if partial completion detected

**Parameters**:

- `--start-step N`: Resume from step N (default: 0)
- `--window-size M`: Size of analysis window (default: 5)
- `--max-step K`: Maximum step to verify (optional, for testing)
- `--sysout PATH`: Path to sysout file
- `--resume`: Resume from last verified step (reads from git)
- `--regression`: Re-verify all previously verified ranges

**Implementation Details**:

- **File Size Management**: If exceeds 300 lines, split into workflow orchestrator and task handlers
- **Documentation**: Comprehensive docstrings and inline comments
- **Timeout Handling**: Use `timeout --kill-after X+1 X` for all subprocess calls
- **Git Commits**: After each task with format: `[Phase X.Task Y] [STEP range] Description`
- **Safe File Operations**: Never use `rm`; archive or write empty JSON

### Task 0.4: Create Recovery Script

**File**: `scripts/recover_workflow_state.sh` (new)

**Purpose**: Restore workflow state from last git commit

**Action**:

- Find last commit containing `parity_workflow_state.json`
- Restore state file from that commit
- Report recovery point (STEP, timestamp, commit hash)
- Allow manual override if needed
- Update dashboard with recovery information

**Documentation**: Include usage instructions and examples

**Git Commit**: Commit after implementation

### Task 0.5: Create Dashboard Generator

**File**: `scripts/update_dashboard.py` (new)

**Purpose**: Update dashboard file with current progress

**Action**:

- Read current state file
- Read recent log entries
- Calculate metrics (windows verified, divergences found/fixed, etc.)
- Write dashboard file atomically
- Include git commit reference

**Documentation**: Comprehensive docstrings

**Git Commit**: Commit after implementation

### Task 0.6: Create MULTI_IMPLEMENTATION_TASKS.md

**File**: `MULTI_IMPLEMENTATION_TASKS.md` (new)

**Purpose**: Track iterative parity verification tasks following tasks template format

**Structure** (following `.specify/templates/tasks-template.md`):

- **Header**: Include frontmatter and prerequisites section per template
- **Format**: Use `[ID] [P?] [Story] Description` format with exact file paths
- Tasks organized by STEP ranges (0-5, 6-11, 12-17, 18-23, etc.) as phases
- Each STEP range phase includes:
                                                                - [ ] TXXX [P][step0-4] Run C emulator for window (steps STEP to STEP+5) using `scripts/unified_test_harness.py --implementation c --end-step STEP+5 --start-step STEP --sysout PATH`
                                                                - [ ] TXXX [STEP0-4] Generate structured analysis from C trace using `scripts/analyze_execution_window.py`
                                                                - [ ] TXXX [P][step0-4] Run Zig emulator for same window using `scripts/unified_test_harness.py --implementation zig --end-step STEP+5 --start-step STEP --sysout PATH`
                                                                - [ ] TXXX [P][step0-4] Run Lisp emulator for same window using `scripts/unified_test_harness.py --implementation lisp --end-step STEP+5 --start-step STEP --sysout PATH`
                                                                - [ ] TXXX [P][step0-4] Run TypeScript emulator for same window using `scripts/unified_test_harness.py --implementation typescript --end-step STEP+5 --start-step STEP --sysout PATH`
                                                                - [ ] TXXX [STEP0-4] Compare all implementations field-by-field using enhanced comparison scripts
                                                                - [ ] TXXX [STEP0-4] Identify divergences and document in structured format
                                                                - [ ] TXXX [STEP0-4] Fix divergences in non-matching implementations (with code comments referencing STEP range)
                                                                - [ ] TXXX [STEP0-4] Re-verify window after fixes
                                                                - [ ] TXXX [STEP0-4] Document findings in Typst (auto-generate from structured analysis)
                                                                - [ ] TXXX [STEP0-4] Update codebase with fixes and comments (reference STEP range)
                                                                - [ ] TXXX [STEP0-4] Commit changes with descriptive message: `[Phase 4] Verified STEP range X-Y: [summary]`
                                                                - [ ] TXXX [STEP0-4] Update `MULTI_IMPLEMENTATION_TASKS.md` with completion status
                                                                - [ ] TXXX [STEP0-4] Update dashboard file
                                                                - [ ] TXXX [STEP0-4] Mark window as verified in state file
- **Task Breakdown Section**: When partial completion detected, add sub-tasks:
                                                                - [ ] TXXX.1 [STEP0-4] Sub-task 1: [specific action]
                                                                - [ ] TXXX.2 [STEP0-4] Sub-task 2: [specific action]
- Track completion status per STEP range
- Dependencies: later STEP ranges depend on earlier ones being verified
- Parallel opportunities: Non-C emulators can run in parallel after C reference is established
- **Maintenance**: Update task status immediately after each task completion, commit updates

## Phase 1: Fix Immediate Blockers

### Task 1.1: Resolve Laiko Compilation Error

**File**: `laiko/src/data/sysout.lisp`

**Issue**: `end-of-file` condition type being interpreted as function call

**Action**:

- Verify `cl:end-of-file` usage in `handler-case` clauses
- Check for package shadowing or macro expansion issues
- Ensure all `handler-case` clauses use correct condition type syntax
- Test compilation and execution
- **Documentation**: Add comments explaining the fix
- **Git Commit**: Commit with message: `[Phase 1.1] Fix Laiko end-of-file compilation error`

### Task 1.2: Verify Laiko Trace Generation

**Files**: `laiko/src/vm/trace.lisp`, `laiko/src/main.lisp`

**Action**:

- Ensure trace format matches unified specification
- Verify trace file generation when `EMULATOR_MAX_STEPS` is set
- Test trace file location and format correctness
- Verify trace extraction works correctly (can extract step window from full trace)
- **Documentation**: Update code comments and documentation
- **Git Commit**: Commit after verification

## Phase 2: Establish TypeScript Comparison Infrastructure

### Task 2.1: Assess TypeScript Implementation Status

**Files**: `taiko/src/main.ts`, `taiko/src/vm/trace.ts`, `taiko/src/vm/execution.ts`

**Action**:

- Review TypeScript implementation completeness
- Verify trace format matches unified specification
- Check if sysout loading works
- Verify trace extraction works correctly
- Document current status in `CURRENT_STATUS.md`
- **Documentation**: Update status documents
- **Git Commit**: Commit assessment results

### Task 2.2: Create TypeScript Parity Check Script

**File**: `scripts/check_parity_taiko.sh` (new)

**Action**:

- Model after `scripts/check_parity_laiko.sh`
- Handle TypeScript execution (Node.js/deno/bun)
- Generate trace file in unified format
- Integrate with comparison scripts
- Support trace window extraction
- **Safe File Operations**: Never use `rm`; archive old files
- **Documentation**: Comprehensive comments
- **Git Commit**: Commit after implementation

### Task 2.3: Verify TypeScript Trace Format Compliance

**File**: `taiko/src/vm/trace.ts`

**Action**:

- Compare trace output format with specification in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Ensure all 13 fields match exactly
- Verify comma-separated sub-fields format
- Test trace file generation
- Verify trace extraction works correctly
- **Documentation**: Update code comments
- **Git Commit**: Commit verification results

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
- **File Size Management**: Split if exceeds 300 lines
- **Documentation**: Update docstrings and comments
- **Git Commit**: Commit enhancements

### Task 3.2: Create Multi-Implementation Comparison Script

**File**: `scripts/compare_all_implementations.sh` (new)

**Action**:

- Run all four emulators using `unified_test_harness.py` with same parameters
- Collect trace files for specific step windows
- Generate comparison matrix showing pairwise differences
- Output summary report
- Support window-based comparison (not just full traces)
- **Safe File Management**: Archive old trace files to `reports/parity/archive/{timestamp}/` instead of deleting
- **Documentation**: Include comprehensive comments explaining comparison logic
- **Git Commit**: Commit after implementation

### Task 3.3: Create Automated Parity Test Suite

**File**: `scripts/run_all_parity_tests.sh` (new)

**Action**:

- Run parity checks for all implementation pairs
- Test with multiple step counts (10, 100, 1000, 10000)
- Support window-based testing (verify specific step ranges)
- Generate comprehensive report
- Exit codes: 0=all match, 1=divergences found, 2=execution errors
- **Safe File Operations**: Archive instead of delete
- **Documentation**: Comprehensive comments
- **Git Commit**: Commit after implementation

## Phase 4: Systematic Divergence Identification (Iterative Approach)

### Task 4.1: Iterative STEP-by-STEP Verification

**Action**:

- Use `iterative_parity_workflow.py` to orchestrate the process
- Start at STEP=0 (or resume from last verified STEP)
- For each 6-step window (STEP to STEP+5):
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
                                                                - Commit state and progress to git
                                                                - Update dashboard file
                                                                - Only proceed when all implementations match for current window
                                                                - Increment STEP by 5
- Continue until target step count reached or all implementations match
- **Partial Completion Handling**: If partial completion detected, break down tasks and resume

### Task 4.2: Execution Window Analysis

**Action**:

- For each 6-step window (STEP to STEP+5), generate deep analysis:
                                                                - Opcode execution details (instruction, operands, addressing modes)
                                                                - Memory address calculations (virtual to physical translation)
                                                                - Stack and frame pointer changes
                                                                - Register state transitions
                                                                - Memory content analysis at key addresses
                                                                - Address translation details
                                                                - Flag changes and condition evaluations
- Store analysis in structured JSON format
- Compress previous window analysis files
- Use analysis for comparison and documentation generation

### Task 4.3: Multi-Implementation Comparison Per Window

**Action**:

- Compare all implementations within each 6-step window (STEP to STEP+5)
- Identify which implementations match C reference
- When multiple match, use them to inform fixes for non-matching ones
- Document divergence patterns per window
- Track which fields diverge (PC, registers, stack, memory, etc.)
- Identify root causes (opcode implementation, memory management, initialization, etc.)
- Log findings in JSON format with git commit reference

### Task 4.4: Incremental Fix and Verification

**Action**:

- Fix identified divergences before moving to next window
- Re-run verification for current window after fixes
- Only proceed to next window when current window matches across all implementations
- Track fix history per STEP range
- Document fixes in codebase with comments referencing STEP range (e.g., `# Fixed for STEP 0-4 window`)
- Update Typst documentation with fix details
- Commit fixes with descriptive messages

## Phase 5: Documentation and Reporting

### Task 5.1: Create Parity Status Document

**File**: `documentation/implementations/multi-implementation-parity-status.typ` (new)

**Action**:

- Document current parity status for all implementation pairs
- List verified STEP ranges (0-5, 6-11, etc.)
- List known divergences with step numbers and fields
- Track progress over time
- Include comparison methodology
- Auto-update from workflow state and analysis data
- **File Size Management**: Split if exceeds 400 lines
- **Git Commit**: Commit after each update

### Task 5.2: Document Divergence Patterns

**File**: `documentation/implementations/divergence-patterns.typ` (new)

**Action**:

- Categorize divergence types (initialization, opcode implementation, memory management, etc.)
- Document common causes (unit confusion, endianness, initialization order)
- Provide examples and fixes per STEP range
- Reference C implementation as ground truth
- Auto-generate from structured analysis data
- **File Size Management**: Split if exceeds 400 lines
- **Git Commit**: Commit after each update

### Task 5.3: Update Implementation Status Documents

**Files**: `CURRENT_STATUS.md`, `documentation/implementations/*.typ`

**Action**:

- Update completion percentages based on actual parity testing
- Document which opcodes are verified vs. implemented (by STEP range)
- Update known issues lists
- Add parity test results
- Track verified step ranges per implementation
- **Git Commit**: Commit after each update

### Task 5.4: Create Comparison Methodology Documentation

**File**: `documentation/specifications/vm-core/parity-testing-methodology.typ` (new)

**Action**:

- Document unified trace format specification
- Explain comparison tools and scripts
- Provide step-by-step parity testing workflow (iterative approach)
- Document window-based analysis methodology
- Include troubleshooting guide
- Explain structured analysis format
- **Git Commit**: Commit after creation and updates

### Task 5.5: Auto-Generate Documentation from Analysis

**File**: `scripts/generate_typst_from_analysis.py` (new)

**Action**:

- Read structured analysis JSON from execution windows
- Generate Typst documentation sections automatically
- Update existing Typst files with new findings
- Maintain chronological log of parity improvements
- Generate divergence reports per STEP range
- **File Size Management**: Split if exceeds 300 lines
- **Documentation**: Comprehensive docstrings
- **Git Commit**: Commit after implementation and updates

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

**Window Size**: 6 steps (STEP to STEP+5 inclusive) provides enough context while keeping analysis focused

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
- Python 3.x with standard library (subprocess, json, argparse, signal, tempfile, shutil, gzip)
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

- Python 3.x for test harness and workflow scripts
- Awk for text processing (if using awk-based comparison)
- Typst for documentation generation
- Build tools for each implementation (gcc/clang for C, zig for Zig, sbcl/cl for Lisp, node/deno/bun for TypeScript)
- Git for version control
- Screen or tmux for long-running sessions

## Milestones

### Milestone 0: Unified Test Harness Complete

- **Deliverables**:
                                                                - `scripts/unified_test_harness.py` created and tested (or package if >300 lines)
                                                                - Can run all four implementations with consistent interface
                                                                - Trace window extraction working correctly
                                                                - Structured analysis module (`analyze_execution_window.py`) created
                                                                - All files under 500 lines
                                                                - Comprehensive documentation and comments
- **Verification**: Successfully run each implementation and extract step windows

### Milestone 1: Iterative Workflow Infrastructure Complete

- **Deliverables**:
                                                                - `scripts/iterative_parity_workflow.py` created and tested (or package if >300 lines)
                                                                - `scripts/recover_workflow_state.sh` created
                                                                - `scripts/update_dashboard.py` created
                                                                - `MULTI_IMPLEMENTATION_TASKS.md` created with initial structure following template
                                                                - Workflow state management working (atomic writes, change log)
                                                                - Recovery mechanism working (from git commits)
                                                                - Can run first iteration (STEP=0, window 0-5)
                                                                - All files under 500 lines
                                                                - Comprehensive documentation
- **Verification**: Successfully complete first 6-step window verification (0-5) with git commits

### Milestone 2: All Implementations Generating Traces

- **Deliverables**:
                                                                - Laiko compilation error fixed
                                                                - All four implementations successfully generate unified trace format
                                                                - Trace files verified for format compliance
                                                                - Trace window extraction working for all implementations
                                                                - Documentation updated
                                                                - Git commits for all fixes
- **Verification**: Run each implementation with test harness and verify trace generation

### Milestone 3: Multi-Implementation Comparison Infrastructure

- **Deliverables**:
                                                                - Enhanced comparison scripts support 3-4 implementations
                                                                - Window-based comparison working
                                                                - TypeScript parity check script created
                                                                - Comparison integrates with structured analysis
                                                                - All files under 500 lines
                                                                - Documentation updated
- **Verification**: Successfully compare all implementations for a step window

### Milestone 4: First Verified Step Ranges

- **Deliverables**:
- Steps 0-5 verified across all implementations
- Steps 6-11 verified across all implementations
                                                                - Structured analysis generated for verified ranges
                                                                - Documentation auto-generated for verified ranges
                                                                - All fixes committed to git
                                                                - Dashboard updated
- **Verification**: All implementations match C reference for first two windows

### Milestone 5: Automated Documentation Generation

- **Deliverables**:
                                                                - Typst documentation auto-generated from structured analysis
                                                                - Parity status document created and auto-updated
                                                                - Divergence patterns document created
                                                                - Implementation status documents updated
                                                                - All files under 500 lines
- **Verification**: Documentation accurately reflects verified step ranges

### Milestone 6: Continuous Improvement Automation

- **Deliverables**:
                                                                - Daily parity check script operational
                                                                - Divergence fix workflow documented and tested
                                                                - CI/CD integration (if applicable) configured
                                                                - Regression testing working
                                                                - Long-running execution tested (hours/days)
- **Verification**: Automated checks run successfully without user intervention, recovery tested

### Milestone 7: Extended Parity Verification

- **Deliverables**:
                                                                - Steps 0-95 verified across all implementations (16 windows of 6 steps each)
                                                                - All divergences in verified ranges fixed
                                                                - Comprehensive documentation for verified ranges
                                                                - All state files, logs, and analysis files maintained
                                                                - Recovery tested multiple times
- **Verification**: All implementations match C reference for first 100 steps

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