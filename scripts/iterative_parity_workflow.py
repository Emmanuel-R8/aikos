#!/usr/bin/env python3
"""
Iterative Parity Workflow Script

Orchestrates the iterative STEP-by-STEP parity verification process using:
- scripts/unified_test_harness.py
- scripts/analyze_execution_window.py
- scripts/compare_unified_traces.py

State is persisted to `parity_workflow_state.json` in the repo root so runs
can resume from the last verified STEP. A high-level dashboard summary is
written to `parity_workflow_dashboard.json`.

Lock: Prevents concurrent runs. Stale lock = PID dead or lock older than 24h.
Release: normal exit (try/finally), SIGINT/SIGTERM. Crash leaves lock; next
run removes stale lock per spec.md "Concurrency and Locking".
"""

from __future__ import annotations

import argparse
import atexit
import json
import os
import signal
import subprocess
import sys
import time
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional


REPO_ROOT = Path(__file__).resolve().parent.parent
STATE_FILE = REPO_ROOT / "parity_workflow_state.json"
CHANGELOG_FILE = REPO_ROOT / "parity_workflow_state_changelog.json"
DASHBOARD_FILE = REPO_ROOT / "parity_workflow_dashboard.json"
REPORTS_DIR = REPO_ROOT / "reports" / "parity"
ANALYSIS_DIR = REPORTS_DIR / "analysis"
DIVERGENCE_LOG = REPORTS_DIR / "divergence_reports.jsonl"
LOCK_FILE = REPORTS_DIR / ".parity_workflow.lock"
STALE_LOCK_AGE_SECONDS = 24 * 60 * 60  # 24 hours

_lock_held = False


def _pid_alive(pid: int) -> bool:
  """Check if process with given PID is still running."""
  try:
    os.kill(pid, 0)
    return True
  except OSError:
    return False


def _is_lock_stale() -> bool:
  """Stale if PID dead OR lock older than 24h (per spec.md)."""
  if not LOCK_FILE.exists():
    return False
  try:
    data = json.loads(LOCK_FILE.read_text())
    pid = data.get("pid")
    ts = data.get("timestamp", 0)
    if pid is not None and _pid_alive(pid):
      return False
    return (
      (pid is not None and not _pid_alive(pid))
      or (time.time() - ts >= STALE_LOCK_AGE_SECONDS)
    )
  except Exception:
    return True


def _remove_lock() -> None:
  """Remove lock file if we hold it."""
  global _lock_held
  if _lock_held and LOCK_FILE.exists():
    try:
      LOCK_FILE.unlink()
      _lock_held = False
    except OSError:
      pass


def acquire_lock() -> bool:
  """Acquire workflow lock. Remove stale lock first. Return True if acquired."""
  global _lock_held
  REPORTS_DIR.mkdir(parents=True, exist_ok=True)
  if _is_lock_stale():
    try:
      LOCK_FILE.unlink()
    except OSError:
      pass
  if LOCK_FILE.exists():
    return False
  try:
    LOCK_FILE.write_text(json.dumps({
      "pid": os.getpid(),
      "timestamp": time.time(),
      "hostname": os.environ.get("HOSTNAME", ""),
    }))
    _lock_held = True
    atexit.register(_remove_lock)
    return True
  except OSError:
    return False


def release_lock() -> None:
  """Release workflow lock."""
  _remove_lock()
  try:
    atexit.unregister(_remove_lock)
  except Exception:
    pass


@dataclass
class WorkflowState:
  current_step: int = 0
  window_size: int = 5  # STEP..STEP+window_size (inclusive) => window_size+1 steps
  last_verified_step: int = -1
  windows_verified: int = 0
  total_divergences_found: int = 0
  last_update: str = ""


def load_state() -> WorkflowState:
  """Load workflow state from JSON file, or return default."""
  if not STATE_FILE.exists():
    return WorkflowState()
  try:
    data = json.loads(STATE_FILE.read_text())
    return WorkflowState(
      current_step=data.get("current_step", 0),
      window_size=data.get("window_size", 5),
      last_verified_step=data.get("last_verified_step", -1),
      windows_verified=data.get("windows_verified", 0),
      total_divergences_found=data.get("total_divergences_found", 0),
      last_update=data.get("last_update", ""),
    )
  except Exception:
    # On corrupt state, start fresh (safer than crashing indefinitely)
    return WorkflowState()


def atomic_write_json(path: Path, data: Dict[str, Any]) -> None:
  """Write JSON atomically (write to temp then rename)."""
  path.parent.mkdir(parents=True, exist_ok=True)
  tmp = path.with_suffix(path.suffix + ".tmp")
  tmp.write_text(json.dumps(data, indent=2))
  tmp.replace(path)


def append_changelog_entry(state: WorkflowState, step_start: int, step_end: int) -> None:
  """Append a changelog entry for the current state."""
  entry = {
    "timestamp": datetime.now().isoformat(),
    "step_range": f"{step_start}-{step_end}",
    "current_step": state.current_step,
    "last_verified_step": state.last_verified_step,
    "windows_verified": state.windows_verified,
    "total_divergences_found": state.total_divergences_found,
  }
  if CHANGELOG_FILE.exists():
    try:
      log = json.loads(CHANGELOG_FILE.read_text())
    except Exception:
      log = {"entries": []}
  else:
    log = {"entries": []}
  log.setdefault("entries", []).append(entry)
  # Keep only last 50 entries
  log["entries"] = log["entries"][-50:]
  atomic_write_json(CHANGELOG_FILE, log)


def run_cmd(cmd: list[str], cwd: Path, timeout: int) -> subprocess.CompletedProcess:
  """Run a subprocess command with timeout, capturing output."""
  return subprocess.run(
    cmd,
    cwd=str(cwd),
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    timeout=timeout,
  )


def run_unified_harness(
  implementation: str,
  start_step: int,
  end_step: int,
  sysout: Path,
  output: Path,
  json_metadata: Optional[Path],
  timeout: int,
) -> int:
  """Invoke scripts/unified_test_harness.py with given parameters."""
  harness = REPO_ROOT / "scripts" / "unified_test_harness.py"
  if not harness.exists():
    print(f"ERROR: unified_test_harness.py not found at {harness}", file=sys.stderr)
    return 3

  cmd: list[str] = [
    str(harness),
    "--implementation",
    implementation,
    "--end-step",
    str(end_step),
    "--start-step",
    str(start_step),
    "--sysout",
    str(sysout),
    "--output",
    str(output),
    "--timeout",
    str(timeout),
  ]
  if json_metadata is not None:
    cmd.extend(["--json-metadata", str(json_metadata)])

  try:
    result = run_cmd(cmd, REPO_ROOT, timeout=timeout + 1)
  except subprocess.TimeoutExpired:
    print(
      f"ERROR: unified_test_harness timed out for {implementation} window {start_step}-{end_step}",
      file=sys.stderr,
    )
    return 1

  if result.returncode != 0:
    print(
      f"ERROR: unified_test_harness failed for {implementation} window {start_step}-{end_step}",
      file=sys.stderr,
    )
    print(result.stderr, file=sys.stderr)
  return result.returncode


def analyze_window(trace_file: Path, output_json: Path) -> int:
  """Run analyze_execution_window.py on a trace file."""
  analyzer = REPO_ROOT / "scripts" / "analyze_execution_window.py"
  if not analyzer.exists():
    print(f"ERROR: analyze_execution_window.py not found at {analyzer}", file=sys.stderr)
    return 1

  cmd = [
    str(analyzer),
    str(trace_file),
    "--output",
    str(output_json),
  ]
  try:
    result = run_cmd(cmd, REPO_ROOT, timeout=60)
  except subprocess.TimeoutExpired:
    print(
      f"ERROR: analyze_execution_window timed out for {trace_file}",
      file=sys.stderr,
    )
    return 1

  if result.returncode != 0:
    print(
      f"ERROR: analyze_execution_window failed for {trace_file}",
      file=sys.stderr,
    )
    print(result.stderr, file=sys.stderr)
  return result.returncode


def compare_with_c(c_trace: Path, other_trace: Path, label: str) -> bool:
  """Compare another implementation's trace with C.

  Returns True if traces match (no divergence), False if divergence or error.
  """
  comp = REPO_ROOT / "scripts" / "compare_unified_traces.py"
  if not comp.exists():
    print(
      f"WARNING: compare_unified_traces.py not found, treating {label} as non-comparable",
      file=sys.stderr,
    )
    return False

  cmd = [str(comp), str(c_trace), str(other_trace)]
  try:
    result = run_cmd(cmd, REPO_ROOT, timeout=60)
  except subprocess.TimeoutExpired:
    print(
      f"ERROR: compare_unified_traces timed out for {label}",
      file=sys.stderr,
    )
    return False

  if result.returncode != 0:
    print(
      f"WARNING: compare_unified_traces reported divergence or error for {label}",
      file=sys.stderr,
    )
    return False

  return True


def write_dashboard(state: WorkflowState, current_window: tuple[int, int]) -> None:
  """Write a simple parity status dashboard JSON."""
  start_step, end_step = current_window
  dashboard = {
    "current_step": state.current_step,
    "current_window": f"{start_step}-{end_step}",
    "last_verified_step": state.last_verified_step,
    "windows_verified": state.windows_verified,
    "total_divergences_found": state.total_divergences_found,
    "last_update": state.last_update,
  }
  atomic_write_json(DASHBOARD_FILE, dashboard)


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Iterative STEP-by-STEP parity workflow orchestrator",
  )
  parser.add_argument(
    "--start-step",
    type=int,
    default=0,
    help="Start step (default: 0, overridden by --resume)",
  )
  parser.add_argument(
    "--window-size",
    type=int,
    default=5,
    help="Window size M => window STEP..STEP+M (default: 5 => 6 steps)",
  )
  parser.add_argument(
    "--max-step",
    type=int,
    default=50,
    help="Maximum step to verify (inclusive, default: 50)",
  )
  parser.add_argument(
    "--sysout",
    type=str,
    default="medley/internal/loadups/starter.sysout",
    help="Path to sysout file",
  )
  parser.add_argument(
    "--resume",
    action="store_true",
    help="Resume from last verified step using parity_workflow_state.json",
  )
  parser.add_argument(
    "--regression",
    action="store_true",
    help="Re-verify all previously verified ranges from step 0",
  )

  args = parser.parse_args()

  if not acquire_lock():
    print(
      "ERROR: Another parity workflow is running, or stale lock present. "
      f"Lock file: {LOCK_FILE}. If no other run is active, remove the lock file.",
      file=sys.stderr,
    )
    return 2

  def _signal_handler(signum: int, frame: object) -> None:
    release_lock()
    sys.exit(128 + (signum if signum < 128 else 0))

  signal.signal(signal.SIGINT, _signal_handler)
  signal.signal(signal.SIGTERM, _signal_handler)

  try:
    return _run_workflow(args)
  finally:
    release_lock()


def _run_workflow(args: argparse.Namespace) -> int:
  """Main workflow logic (called after lock acquired)."""
  state = load_state()
  state.window_size = args.window_size

  # Determine starting step
  if args.regression:
    current_step = 0
  elif args.resume and state.last_verified_step >= 0:
    current_step = state.last_verified_step + 1
  else:
    current_step = args.start_step

  sysout_path = Path(args.sysout)
  if not sysout_path.is_absolute():
    sysout_path = REPO_ROOT / sysout_path
  if not sysout_path.exists():
    print(f"ERROR: Sysout file not found: {sysout_path}", file=sys.stderr)
    return 1

  REPORTS_DIR.mkdir(parents=True, exist_ok=True)
  ANALYSIS_DIR.mkdir(parents=True, exist_ok=True)
  # Do not create DIVERGENCE_LOG yet; only append when divergences occur.

  print("=== ITERATIVE PARITY WORKFLOW ===")
  print(f"Repo root: {REPO_ROOT}")
  print(f"Sysout: {sysout_path}")
  print(f"Start step: {current_step}")
  print(f"Window size: {args.window_size} (window STEP..STEP+{args.window_size})")
  print(f"Max step: {args.max_step}")
  print("")

  while current_step <= args.max_step:
    end_step = current_step + args.window_size
    print(f"--- Window {current_step}-{end_step} ---")

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    window_prefix = f"window_{current_step}_{end_step}_{timestamp}"

    # 1) Run C emulator (reference)
    c_trace = REPORTS_DIR / f"c_{window_prefix}.txt"
    c_meta = REPORTS_DIR / f"c_{window_prefix}.json"
    rc = run_unified_harness(
      "c",
      current_step,
      end_step,
      sysout_path,
      c_trace,
      c_meta,
      timeout=30,
    )
    if rc != 0:
      print("ERROR: C emulator failed; aborting workflow", file=sys.stderr)
      return rc

    # 2) Analyze C window
    c_analysis = ANALYSIS_DIR / f"c_{window_prefix}.json"
    analyze_window(c_trace, c_analysis)

    # 3) Run other implementations (best-effort)
    impls = ["zig", "lisp", "typescript"]
    traces: Dict[str, Path] = {}
    for impl in impls:
      trace_path = REPORTS_DIR / f"{impl}_{window_prefix}.txt"
      meta_path = REPORTS_DIR / f"{impl}_{window_prefix}.json"
      rc = run_unified_harness(
        impl,
        current_step,
        end_step,
        sysout_path,
        trace_path,
        meta_path,
        timeout=60,
      )
      if rc == 0:
        traces[impl] = trace_path
        print(f"{impl}: trace captured for window {current_step}-{end_step}")
      elif rc == 3:
        print(f"{impl}: emulator not available or not implemented")
      else:
        print(f"{impl}: execution error (code {rc})")

    # 4) Compare with C where possible
    divergence_found = False
    diverging_impls: list[str] = []
    for impl, trace_path in traces.items():
      matches = compare_with_c(c_trace, trace_path, impl)
      if not matches:
        divergence_found = True
        diverging_impls.append(impl)

    # 5) Update state and dashboard
    state.current_step = current_step
    state.last_update = datetime.now().isoformat()

    if divergence_found:
      state.total_divergences_found += 1
      # Append a simple divergence report entry for this window.
      entry = {
        "window_start_step": current_step,
        "window_end_step": end_step,
        "reference_implementation_id": "c",
        "divergent_implementation_ids": diverging_impls,
        "timestamp": state.last_update,
      }
      with DIVERGENCE_LOG.open("a", encoding="utf-8") as f:
        f.write(json.dumps(entry) + "\n")

      atomic_write_json(STATE_FILE, asdict(state))
      append_changelog_entry(state, current_step, end_step)
      write_dashboard(state, (current_step, end_step))
      print(
        f"Divergence detected in window {current_step}-{end_step}; "
        "stopping so fixes can be applied.",
        file=sys.stderr,
      )
      break

    state.last_verified_step = end_step
    state.windows_verified += 1

    atomic_write_json(STATE_FILE, asdict(state))
    append_changelog_entry(state, current_step, end_step)
    write_dashboard(state, (current_step, end_step))

    print(
      f"Window {current_step}-{end_step} processed. "
      f"windows_verified={state.windows_verified}, "
      f"last_verified_step={state.last_verified_step}",
      file=sys.stderr,
    )
    print("")

    current_step = end_step + 1

  print("=== Iterative parity workflow completed ===")
  return 0


if __name__ == "__main__":
  sys.exit(main())
