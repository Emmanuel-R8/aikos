## Phase 0: Create Unified Test Harness and Iterative Workflow

### Task 0.1: Create Unified Test Harness Python Script

**File**: `scripts/unified_test_harness.py` (new)

**Purpose**: Single Python script that can run any implementation from execution step 0 to a given end step, producing identical trace output format

**Requirements**:

- Accept parameters: - `--implementation {c|zig|lisp|typescript}`: Which emulator to run - `--end-step N`: Run emulator from step 0 to step N (inclusive) - `--start-step M`: Extract/log only steps M to N from the full trace (default: 0) - `--sysout PATH`: Path to sysout file (default: `medley/internal/loadups/starter.sysout`) - `--output FILE`: Output trace file path (default: `{implementation}_emulator_execution_log.txt`) - `--json-metadata FILE`: Output execution statistics in JSON format
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
- Default sysout path: `medley/internal/loadups/starter.sysout` (if not specified)
- Validate trace file format against specification before returning
- Handle missing executables gracefully
- **File Size Management**: If script exceeds 300 lines, split into: - `scripts/unified_test_harness/main.py` (orchestration) - `scripts/unified_test_harness/runners.py` (implementation-specific runners) - `scripts/unified_test_harness/trace_extractor.py` (trace window extraction)
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

- **State Management**: - Maintain state file (`parity_workflow_state.json`) with atomic writes - Maintain change log (`parity_workflow_state_changelog.json`) - Track: current STEP, verification status per STEP range, last verified STEP, divergences found
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
- `--sysout PATH`: Path to sysout file (default: `medley/internal/loadups/starter.sysout`)
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

- **Note**: If template not found at `.specify/templates/tasks-template.md`, check `medley/.specify/templates/tasks-template.md` as alternative location

- **Header**: Include frontmatter and prerequisites section per template
- **Format**: Use `[ID] [P?] [Story] Description` format with exact file paths
- Tasks organized by STEP ranges (0-5, 6-11, 12-17, 18-23, etc.) as phases
- Each STEP range phase includes: - [ ] TXXX [P][step0-4] Run C emulator for window (steps STEP to STEP+5) using `scripts/unified_test_harness.py --implementation c --end-step STEP+5 --start-step STEP --sysout PATH` - [ ] TXXX [STEP0-4] Generate structured analysis from C trace using `scripts/analyze_execution_window.py` - [ ] TXXX [P][step0-4] Run Zig emulator for same window using `scripts/unified_test_harness.py --implementation zig --end-step STEP+5 --start-step STEP --sysout PATH` - [ ] TXXX [P][step0-4] Run Lisp emulator for same window using `scripts/unified_test_harness.py --implementation lisp --end-step STEP+5 --start-step STEP --sysout PATH` - [ ] TXXX [P][step0-4] Run TypeScript emulator for same window using `scripts/unified_test_harness.py --implementation typescript --end-step STEP+5 --start-step STEP --sysout PATH` - [ ] TXXX [STEP0-4] Compare all implementations field-by-field using enhanced comparison scripts - [ ] TXXX [STEP0-4] Identify divergences and document in structured format - [ ] TXXX [STEP0-4] Fix divergences in non-matching implementations (with code comments referencing STEP range) - [ ] TXXX [STEP0-4] Re-verify window after fixes - [ ] TXXX [STEP0-4] Document findings in Typst (auto-generate from structured analysis) - [ ] TXXX [STEP0-4] Update codebase with fixes and comments (reference STEP range) - [ ] TXXX [STEP0-4] Commit changes with descriptive message: `[Phase 4] Verified STEP range X-Y: [summary]` - [ ] TXXX [STEP0-4] Update `MULTI_IMPLEMENTATION_TASKS.md` with completion status - [ ] TXXX [STEP0-4] Update dashboard file - [ ] TXXX [STEP0-4] Mark window as verified in state file
- **Task Breakdown Section**: When partial completion detected, add sub-tasks: - [ ] TXXX.1 [STEP0-4] Sub-task 1: [specific action] - [ ] TXXX.2 [STEP0-4] Sub-task 2: [specific action]
- Track completion status per STEP range
- Dependencies: later STEP ranges depend on earlier ones being verified
- Parallel opportunities: Non-C emulators can run in parallel after C reference is established
- **Maintenance**: Update task status immediately after each task completion, commit updates

