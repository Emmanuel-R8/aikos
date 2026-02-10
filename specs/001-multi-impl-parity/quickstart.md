# Quickstart: Multi-Implementation Parity Workflow

This guide explains how to run the multi-implementation parity workflow for a
given sysout and interpret the results.

## Prerequisites

- The C reference emulator and at least one alternate implementation (e.g., Zig)
  build and run successfully.
- The default sysout exists, typically
  `medley/internal/loadups/starter.sysout`.
- You are on the feature branch:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
git checkout 001-multi-impl-parity
```

## 1. Run a single parity window

Start by verifying the very first window (steps 0–5) across implementations:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
python3 scripts/iterative_parity_workflow.py \
  --max-iterations 1 \
  --window-size 6 \
  --sysout medley/internal/loadups/starter.sysout
```

This:

- Runs the C reference and other available implementations for the first window.
- Compares their traces using the unified comparison tools.
- Updates `parity_workflow_state.json` with the last verified step.
- Writes or updates any status artifacts the workflow maintains.

If a divergence is detected in this window, the script stops so you can inspect
the divergence reports and fix the underlying implementation.

## 2. Resume after an interruption

If the workflow is interrupted (manual stop, machine reboot, etc.), restart it
in the same way:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
python3 scripts/iterative_parity_workflow.py \
  --max-iterations 2 \
  --window-size 6 \
  --sysout medley/internal/loadups/starter.sysout
```

The workflow:

- Reads `parity_workflow_state.json`.
- Resumes from the next unverified window.
- Continues until it has processed the requested number of windows or until a
  divergence is found.

## 3. Inspect parity status

After one or more runs, check the current parity status by opening the parity
status artifact (for example, `parity_workflow_dashboard.json` if configured)
and the workflow state file:

- `parity_workflow_state.json`: shows `last_verified_step`, `window_size`, and
  the sysout under test.
- Any dashboard or status JSON: summarizes windows verified, divergences found
  and fixed, and recent activity.

If divergences have been found, you can also inspect the divergence log:

- `reports/parity/divergence_reports.jsonl`: one JSON object per line, each
  describing a window and which implementations diverged from the C reference,
  for example:

  ```json
  {
    "window_start_step": 0,
    "window_end_step": 5,
    "reference_implementation_id": "c",
    "divergent_implementation_ids": ["zig"],
    "timestamp": "2026-02-10T12:34:56.789Z"
  }
  ```

Together, these files tell you:

- How far into execution parity has been verified.
- Which windows (if any) still diverge.
- Whether recent fixes improved parity.

## 4. Adjust window size (optional)

The canonical default window size is 6 steps, but you can override it per run:

```bash
python3 scripts/iterative_parity_workflow.py \
  --max-iterations 1 \
  --window-size 10 \
  --sysout medley/internal/loadups/starter.sysout
```

Larger windows provide more context but may be harder to analyze; smaller
windows make diagnosis simpler at the cost of more runs.

## 5. Typical workflow

1. Verify early windows (e.g., steps 0–5, 6–11, 12–17) and fix any divergences.
2. Once early windows are clean, gradually extend coverage (e.g., up to at
   least step 95).
3. Use parity status artifacts to communicate progress and decide where to
   focus further debugging work.
