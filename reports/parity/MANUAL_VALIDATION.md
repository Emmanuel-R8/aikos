# Manual Validation Guide

This document provides step-by-step instructions for completing the remaining manual validation tasks (T001, T022, T029, T036, T039).

## Prerequisites

Ensure you're in the correct directory and branch:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
git checkout 001-multi-impl-parity
```

## T001: Verify Emulators Build and Run

**Goal**: Confirm that C, Zig, Laiko, and Taiko emulators can be built and run basic smoke tests.

### C Emulator

```bash
# Build C emulator (adjust path based on your build system)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/maiko/
# Option 1: CMake build
mkdir -p build-cmake && cd build-cmake
cmake .. && make
# Option 2: Traditional build
cd ../linux.x86_64
make

# Test run (from repo root)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
EMULATOR_MAX_STEPS=5 ./maiko/build-cmake/ldesdl medley/internal/loadups/starter.sysout
# Or if using traditional build:
# EMULATOR_MAX_STEPS=5 ./maiko/linux.x86_64/ldesdl medley/internal/loadups/starter.sysout
# Should produce c_emulator_execution_log.txt in repo root
```

**Success criteria**:

- ✅ Emulator builds without errors
- ✅ Can load sysout and produce trace log
- ✅ Trace log contains unified format lines

### Zig Emulator

```bash
# Build from zaiko directory (set cache directory to avoid permission issues)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/zaiko/
export ZIG_GLOBAL_CACHE_DIR=/tmp/zig-cache
# Or use a local cache directory:
# export ZIG_GLOBAL_CACHE_DIR=$(pwd)/.zig-cache
zig build

# Test run (from zaiko directory, use absolute path for sysout)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/zaiko/
export ZIG_GLOBAL_CACHE_DIR=/tmp/zig-cache
zig build run -- --max-steps 5 /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/medley/internal/loadups/starter.sysout
# Or use relative path from repo root:
# cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
# cd zaiko && export ZIG_GLOBAL_CACHE_DIR=/tmp/zig-cache && zig build run -- --max-steps 5 ../medley/internal/loadups/starter.sysout
# Should produce zig_emulator_execution_log.txt in zaiko/ directory
```

**Success criteria**:

- ✅ `zig build` completes without errors
- ✅ Emulator runs and produces trace log
- ✅ Trace log contains unified format lines

### Laiko (Common Lisp) Emulator

```bash
# Test run (from laiko directory, use absolute path for sysout)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/laiko/
EMULATOR_MAX_STEPS=5 ./run.sh /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/medley/internal/loadups/starter.sysout
# Or use relative path from repo root:
# cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
# cd laiko && EMULATOR_MAX_STEPS=5 ./run.sh ../medley/internal/loadups/starter.sysout
# Should produce lisp_emulator_execution_log.txt in repo root or laiko/
```

**Success criteria**:

- ✅ SBCL is available (`sbcl --version`)
- ✅ `run.sh` executes without errors
- ✅ Produces trace log with unified format

### Taiko (TypeScript) Emulator

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/taiko/
npm install  # if needed
npm run build  # if needed
# Execution not yet implemented - should return clear diagnostic
```

**Success criteria**:

- ✅ TypeScript emulator directory exists
- ✅ Returns clear "not yet implemented" message (expected for now)

---

## T022: Validate End-to-End Window 0-5 Parity Run

**Goal**: Run a single parity window (steps 0-5) and verify the workflow completes successfully.

### Step 1: Clean State (Optional)

### Step 2: Run Single Window

```bash
python3 scripts/iterative_parity_workflow.py \
  --start-step 0 \
  --window-size 5 \
  --max-step 5 \
  --sysout medley/internal/loadups/starter.sysout
```

**Expected output**:

- Script runs C emulator for steps 0-5
- Script runs other available emulators (Zig, Laiko, etc.)
- Compares traces against C reference
- Updates `parity_workflow_state.json`
- Updates `parity_workflow_dashboard.json`

### Step 3: Verify Results

```bash
# Check state file
cat parity_workflow_state.json
# Should show: "last_verified_step": 5 (or -1 if divergence found)

# Check dashboard
cat parity_workflow_dashboard.json
# Should show: "windows_verified": 1 (if successful), "current_window": "0-5"

# Check for trace files
ls -lh c_emulator_unified_trace.txt zig_emulator_unified_trace.txt 2>/dev/null
# Should exist with trace data

# Check analysis outputs
ls -lh reports/parity/analysis/
# Should contain JSON analysis files
```

**Success criteria**:

- ✅ Workflow completes without errors
- ✅ `last_verified_step` is 5 (or appropriate if divergence detected)
- ✅ Dashboard shows 1 window verified (or divergence count if found)
- ✅ Trace files exist and contain valid unified format data
- ✅ Analysis JSON files are generated

**If divergence detected**:

- ✅ Script stops and reports which implementations diverged
- ✅ Divergence log entry created in `reports/parity/divergence_reports.jsonl`
- ✅ You can inspect the divergence details

---

## T029: Validate Resume Behavior

**Goal**: Verify that the workflow can resume from the last verified step after interruption.

### Step 1: Run Multiple Windows

```bash
# Run 3 windows (steps 0-5, 6-11, 12-17)
python3 scripts/iterative_parity_workflow.py \
  --start-step 0 \
  --window-size 5 \
  --max-step 17 \
  --sysout medley/internal/loadups/starter.sysout
```

**Note**: Let this run to completion, or interrupt it manually (Ctrl+C) after at least one window completes.

### Step 2: Check State

```bash
cat parity_workflow_state.json
# Note the last_verified_step value (e.g., 5, 11, or 17)
```

### Step 3: Resume from State

```bash
# Resume using the --resume flag
python3 scripts/iterative_parity_workflow.py \
  --resume \
  --window-size 5 \
  --max-step 30 \
  --sysout medley/internal/loadups/starter.sysout
```

**Expected behavior**:

- Script reads `parity_workflow_state.json`
- Starts from `last_verified_step + 1` (e.g., if last was 5, starts at 6)
- Does NOT re-run windows 0-5
- Continues with windows 6-11, 12-17, etc.

### Step 4: Verify Resume Behavior

```bash
# Check that state was updated correctly
cat parity_workflow_state.json
# last_verified_step should advance beyond the previous value

# Check dashboard
cat parity_workflow_dashboard.json
# windows_verified should increase
```

**Success criteria**:

- ✅ Resume starts from `last_verified_step + 1`, not from step 0
- ✅ Previously verified windows are NOT re-run
- ✅ New windows are processed correctly
- ✅ State file is updated with new `last_verified_step`
- ✅ Dashboard reflects increased `windows_verified` count

### Step 5: Test Missing/Corrupted State

```bash
# Test graceful handling of missing state
mv parity_workflow_state.json parity_workflow_state.json.bak
python3 scripts/iterative_parity_workflow.py \
  --resume \
  --window-size 5 \
  --max-step 5 \
  --sysout medley/internal/loadups/starter.sysout
# Should start from step 0 with explanatory message

# Restore state
mv parity_workflow_state.json.bak parity_workflow_state.json
```

**Success criteria**:

- ✅ Missing state file handled gracefully
- ✅ Starts from step 0 with clear message
- ✅ Does not crash or produce confusing errors

---

## T036: Validate Status and Divergence Artifacts

**Goal**: Verify that status artifacts and divergence reports are generated correctly after multiple windows, including at least one divergence.

### Step 1: Run Multiple Windows (Expect Some Divergences)

```bash
# Run enough windows to likely encounter a divergence
python3 scripts/iterative_parity_workflow.py \
  --start-step 0 \
  --window-size 5 \
  --max-step 30 \
  --sysout medley/internal/loadups/starter.sysout
```

**Note**: If all windows match perfectly, you may need to intentionally introduce a divergence or use a sysout/implementation combination known to have differences.

### Step 2: Inspect Dashboard

```bash
cat parity_workflow_dashboard.json | python3 -m json.tool
```

**Expected structure**:

```json
{
  "current_step": 30,
  "current_window": "25-30",
  "last_verified_step": 17,
  "windows_verified": 3,
  "total_divergences_found": 1,
  "last_update": "2026-02-11T12:34:56.789Z"
}
```

**Success criteria**:

- ✅ Dashboard contains all expected fields
- ✅ `windows_verified` matches number of successful windows
- ✅ `total_divergences_found` matches number of divergent windows
- ✅ `last_verified_step` is correct
- ✅ `last_update` timestamp is recent

### Step 3: Inspect Divergence Reports

```bash
# View divergence log (JSONL format - one JSON object per line)
cat reports/parity/divergence_reports.jsonl | python3 -m json.tool
```

**Expected structure** (one entry per divergent window):

```json
{
  "window_start_step": 18,
  "window_end_step": 23,
  "reference_implementation_id": "c",
  "divergent_implementation_ids": ["zig"],
  "timestamp": "2026-02-11T12:34:56.789Z"
}
```

**Success criteria**:

- ✅ Divergence log exists (even if empty if no divergences)
- ✅ Each divergent window has one JSON entry
- ✅ Entries contain correct window boundaries
- ✅ Divergent implementation IDs are listed correctly
- ✅ Timestamps are present and valid

### Step 4: Verify State File Consistency

```bash
cat parity_workflow_state.json | python3 -m json.tool
```

**Expected structure**:

```json
{
  "current_step": 30,
  "window_size": 5,
  "last_verified_step": 17,
  "windows_verified": 3,
  "total_divergences_found": 1,
  "last_update": "2026-02-11T12:34:56.789Z"
}
```

**Success criteria**:

- ✅ State file matches dashboard values
- ✅ `last_verified_step` is last successfully verified step (not beyond divergences)
- ✅ Counts are consistent between state and dashboard

### Step 5: Verify Analysis Artifacts

```bash
# Check analysis directory
ls -lh reports/parity/analysis/
# Should contain JSON files for each analyzed window
```

**Success criteria**:

- ✅ Analysis JSON files exist for processed windows
- ✅ Files contain structured analysis data
- ✅ Files are named consistently (e.g., by window range)

---

## T039: Run Quickstart.md Validation End-to-End

**Goal**: Follow the quickstart guide from start to finish and verify all documented steps work.

### Step 1: Follow Quickstart Section 1 (Single Window)

```bash
# From quickstart.md section 1
python3 scripts/iterative_parity_workflow.py \
  --max-iterations 1 \
  --window-size 6 \
  --sysout medley/internal/loadups/starter.sysout
```

**Note**: The quickstart uses `--max-iterations` but the actual script uses `--max-step`. Verify which parameter works, or update quickstart if needed.

**Alternative** (if `--max-iterations` doesn't exist):

```bash
python3 scripts/iterative_parity_workflow.py \
  --start-step 0 \
  --window-size 5 \
  --max-step 5 \
  --sysout medley/internal/loadups/starter.sysout
```

**Success criteria**:

- ✅ Command executes successfully
- ✅ Produces expected outputs (state, dashboard, traces)
- ✅ Matches quickstart.md description

### Step 2: Follow Quickstart Section 2 (Resume)

```bash
# From quickstart.md section 2
python3 scripts/iterative_parity_workflow.py \
  --max-iterations 2 \
  --window-size 6 \
  --sysout medley/internal/loadups/starter.sysout
```

**Or with --resume flag**:

```bash
python3 scripts/iterative_parity_workflow.py \
  --resume \
  --window-size 5 \
  --max-step 11 \
  --sysout medley/internal/loadups/starter.sysout
```

**Success criteria**:

- ✅ Resume behavior works as documented
- ✅ State file is read correctly
- ✅ Continues from last verified step

### Step 3: Follow Quickstart Section 3 (Inspect Status)

```bash
# Check state file
cat parity_workflow_state.json

# Check dashboard (if it exists)
cat parity_workflow_dashboard.json

# Check divergence log
cat reports/parity/divergence_reports.jsonl
```

**Success criteria**:

- ✅ All files exist and are readable
- ✅ JSON is valid and well-formed
- ✅ Content matches quickstart.md examples

### Step 4: Follow Quickstart Section 4 (Adjust Window Size)

```bash
# Test different window size
python3 scripts/iterative_parity_workflow.py \
  --start-step 0 \
  --window-size 10 \
  --max-step 10 \
  --sysout medley/internal/loadups/starter.sysout
```

**Success criteria**:

- ✅ Window size parameter is respected
- ✅ Correct number of steps are processed
- ✅ Window boundaries are correct

### Step 5: Follow Quickstart Section 5 (Typical Workflow)

Execute the typical workflow sequence:

```bash
# Window 0-5
python3 scripts/iterative_parity_workflow.py \
  --start-step 0 --window-size 5 --max-step 5 \
  --sysout medley/internal/loadups/starter.sysout

# Window 6-11
python3 scripts/iterative_parity_workflow.py \
  --resume --window-size 5 --max-step 11 \
  --sysout medley/internal/loadups/starter.sysout

# Window 12-17
python3 scripts/iterative_parity_workflow.py \
  --resume --window-size 5 --max-step 17 \
  --sysout medley/internal/loadups/starter.sysout

# Extend to step 95
python3 scripts/iterative_parity_workflow.py \
  --resume --window-size 5 --max-step 95 \
  --sysout medley/internal/loadups/starter.sysout
```

**Success criteria**:

- ✅ Each window processes correctly
- ✅ Resume works between windows
- ✅ State accumulates correctly
- ✅ Dashboard reflects progress
- ✅ Can extend to larger step ranges

---

## Troubleshooting

### Script Not Found

```bash
# Ensure you're in the repo root
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
# Check script exists
ls -l scripts/iterative_parity_workflow.py
```

### Permission Denied

```bash
# Make script executable
chmod +x scripts/iterative_parity_workflow.py
```

### Emulator Not Found

```bash
# Check emulator paths
ls -l maiko/build-cmake/ldesdl
ls -l zaiko/zig-out/bin/zaiko  # or wherever Zig emulator is
ls -l laiko/run.sh
```

### State File Issues

```bash
# Backup and reset state
mv parity_workflow_state.json parity_workflow_state.json.bak
# Run fresh
python3 scripts/iterative_parity_workflow.py --start-step 0 ...
```

### Trace Files Not Generated

```bash
# Check emulator execution
# C emulator should produce: c_emulator_execution_log.txt
# Zig emulator should produce: zig_emulator_execution_log.txt
# Check EMULATOR_MAX_STEPS environment variable is set correctly
```

---

## Validation Checklist

After completing all tasks, verify:

- [ ] T001: All emulators build and run smoke tests
- [ ] T022: Single window (0-5) parity run completes successfully
- [ ] T029: Resume behavior works correctly after interruption
- [ ] T036: Status and divergence artifacts are generated correctly
- [ ] T039: Quickstart.md steps all work as documented

Once all items are checked, update `specs/001-multi-impl-parity/tasks.md` to mark tasks T001, T022, T029, T036, and T039 as complete.
