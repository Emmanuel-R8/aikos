# Comprehensive Multi-Implementation Parity Analysis Plan

## Objective

Achieve systematic parity verification across all four emulator implementations (C, Zig, Common Lisp, TypeScript) by comparing execution traces, identifying divergences, documenting findings, and creating automated comparison infrastructure using an iterative STEP-by-STEP approach that runs autonomously for extended periods.

## Current State Assessment

### Implementations Status

- **C (maiko/)**: Production-ready reference (95% opcode coverage)
- **Zig (zaiko/)**: ~60-70% complete, 15-step parity achieved
- **Common Lisp (laiko/)**: Infrastructure complete, sysout loading may be blocked by `end-of-file` compilation error (verify in Phase 1.1)
- **TypeScript (taiko/)**: Implementation exists, status unclear

### Existing Infrastructure

- Unified trace format (pipe-delimited) defined in `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Comparison scripts: `scripts/compare_unified_traces.py`, `scripts/compare_unified_traces.awk`
- Parity check scripts: `scripts/check_parity.sh` (C/Zig), `scripts/check_parity_laiko.sh` (C/Laiko)
- Documentation: Typst format in `documentation/` directory

## Development Constraints and Standards

### File Management

- **Never use `rm` command**: Use safe alternatives: - Python: `os.remove()` with error handling, or move to archive directory - Shell scripts: Use `mv` to archive files to `archive/` or `old/` directories with timestamps - Trace files: Archive old traces to `reports/parity/archive/{timestamp}/` instead of deleting - Temporary files: Use `tempfile` module in Python, or move to `tmp/` directory - Successful trace files: Write empty JSON `{}` to file or move to archive
- **File Size Limit**: All source files must be kept under 500 lines - If a file exceeds 400 lines, plan refactoring before it reaches 500 - Split large scripts into logical components (e.g., `unified_test_harness.py` → `unified_test_harness/` with `main.py`, `c_runner.py`, `zig_runner.py`, etc.) - Monitor file sizes in pre-commit checks or CI
- **File Organization**: - Python scripts: Split into modules if >300 lines - Typst documentation: Split large documents into sections/files - Keep functions focused and single-purpose

### Documentation and Comments

- **Comprehensive Documentation Updates**: Required at these checkpoints: - After each task completion: Update relevant documentation - After each 6-step window verification: Update Typst docs with findings - After each divergence fix: Document fix in code comments and Typst - Weekly: Review and update all documentation for accuracy
- **Code Comments**: - Every function must have docstring (Python) or comment block (other languages) - Complex logic must have inline comments explaining reasoning - Reference STEP ranges in comments when fixing divergences (e.g., `# Fixed for STEP 0-4 window`) - Document assumptions and design decisions
- **Documentation Files**: - Update `MULTI_IMPLEMENTATION_TASKS.md` after each task - Update Typst files after each window verification - Update implementation status docs after each milestone

### Git Commit Practices

- **Regular Comprehensive Commits**: Required at these checkpoints: - After each task completion: Commit with descriptive message - After each 6-step window verification: Commit with window details - After each divergence fix: Commit with fix description and STEP range - Daily: Commit progress even if incomplete (use WIP prefix)
- **Commit Message Format**:

  ```
  [Phase X.Task Y] Brief description

  - What was done
  - STEP range affected (if applicable)
  - Files changed
  - Related issue/divergence ID
  ```

- **Commit Frequency**: - Minimum: One commit per task - Preferred: One commit per logical unit of work - Never: Large commits spanning multiple unrelated changes
- **Commit Contents**: Include state file, task list updates, generated files, log entries

### Task List Maintenance

- **MULTI_IMPLEMENTATION_TASKS.md Format**: Must follow `.specify/templates/tasks-template.md`: - Use format: `[ID] [P?] [Story] Description` - Include exact file paths in descriptions - Mark parallel tasks with `[P]` - Group by STEP ranges (0-4, 5-9, etc.) as phases - Include checkpoints after each STEP range - Update task status immediately after completion - Maintain dependencies section
- **Task Updates**: - Mark tasks complete `[x]` immediately upon completion - Add notes for any deviations from plan - Update dependencies if task order changes - Commit task list updates after each change

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

- **Change Log**: Maintain `parity_workflow_state_changelog.json` with recent changes: - Keep last 50 state changes - Each entry: `{timestamp, step_range, change_type, details, git_commit}` - Rotate when log exceeds 50 entries (archive old entries to `reports/parity/archive/`)
- **Git Commits**: Commit state file after each task completion - Commit message: `[State] Updated workflow state after [task description]` - Enables recovery from last known good state

**Checkpoint Strategy**:

- **After Each Task**: Create checkpoint (git commit) with: - State file - Task list updates - Any generated files - Log entries - Dashboard file
- **Checkpoint Contents**: - Current STEP being verified - Last completed task - Verification status - Any partial progress

**Recovery Mechanism**:

- **From Last Git Commit**: On restart, workflow:

        1. Checkout last commit (or use current state if git is clean)
        2. Read `parity_workflow_state.json` from last commit
        3. Resume from last verified STEP
        4. If state file is missing, start from STEP=0

- **Recovery Script**: `scripts/recover_workflow_state.sh`: - Finds last commit with state file - Restores state file - Reports recovery point - Allows manual override if needed

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

- **Default Timeout Values**:

  ```python
  # Default: 30 second timeout, 31 second kill
  DEFAULT_TIMEOUT = 30  # seconds
  DEFAULT_KILL_AFTER = 31  # seconds
  ```

  - Adjust based on step count (larger step counts may need longer timeouts)
  - For window-based execution (0 to STEP+5), 30 seconds is typically sufficient

- **Process Monitoring**: Script should handle its own process health: - Log heartbeat every N operations - Detect if stuck (no progress for extended period) - Self-restart capability (optional)

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

- **Task Breakdown Process**: - Analyze what failed - Identify root cause - Create granular sub-tasks (each < 1 hour work) - Document breakdown in task list - Resume with smaller tasks

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

- **Log Files**: - `logs/parity_workflow_YYYYMMDD.jsonl` (one file per day) - Rotate daily - Compress old logs after 7 days (move to `logs/archive/`)
- **Git Commit References**: Include git commit hash in log entries: - Enables tracing log entries to code state - Links logs to specific workflow state

**Analysis File Management**:

- **Compression**: Compress analysis JSON files after verification: - Keep uncompressed for current window - Compress previous windows: `analysis_STEP0-4.json.gz` - Use gzip compression - Move compressed files to `reports/parity/analysis/archive/`
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

