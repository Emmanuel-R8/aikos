from __future__ import annotations

import argparse
import json
import os
import shutil
import sys
from datetime import datetime
from pathlib import Path

from .runners import (
  run_c_emulator,
  run_zig_emulator,
  run_lisp_emulator,
  run_typescript_emulator,
)
from .trace_extractor import validate_trace_format, archive_file


DEFAULT_TIMEOUT = 30
DEFAULT_SYSOUT = "medley/internal/loadups/starter.sysout"


def get_repo_root() -> Path:
  """Get the repository root directory."""
  script_dir = Path(__file__).resolve().parent
  # main.py lives in scripts/unified_test_harness/ -> parent=scripts/ -> parent.parent=repo root
  return script_dir.parent.parent


def _resolve_sysout(repo_root: Path, sysout_arg: str) -> Path:
  sysout_path = Path(sysout_arg)
  if not sysout_path.is_absolute():
    sysout_path = repo_root / sysout_path
  return sysout_path


def _determine_output_path(repo_root: Path, implementation: str, output_arg: str | None) -> Path:
  if output_arg:
    return Path(output_arg)
  return repo_root / f"{implementation}_emulator_execution_log.txt"


def _run_implementation(
  implementation: str,
  repo_root: Path,
  sysout_path: Path,
  end_step: int,
  output_file: Path,
  timeout: int,
) -> tuple[int, str | None]:
  repo_str = str(repo_root)
  sysout_str = str(sysout_path)
  out_str = str(output_file)

  if implementation == "c":
    return run_c_emulator(repo_str, sysout_str, end_step, out_str, timeout)
  if implementation == "zig":
    return run_zig_emulator(repo_str, sysout_str, end_step, out_str, timeout)
  if implementation == "lisp":
    return run_lisp_emulator(repo_str, sysout_str, end_step, out_str, timeout)
  if implementation == "typescript":
    return run_typescript_emulator(repo_str, sysout_str, end_step, out_str, timeout)

  return 3, f"Unknown implementation: {implementation}"


def main(argv: list[str] | None = None) -> int:
  """Main entry point for unified test harness."""
  parser = argparse.ArgumentParser(
    description="Unified test harness for multi-implementation parity analysis",
  )

  parser.add_argument(
    "--implementation",
    choices=["c", "zig", "lisp", "typescript"],
    required=True,
    help="Which emulator to run",
  )
  parser.add_argument(
    "--end-step",
    type=int,
    required=True,
    help="Run emulator from step 0 to step N (inclusive)",
  )
  parser.add_argument(
    "--start-step",
    type=int,
    default=0,
    help="Extract/log only steps M to N from full trace (default: 0)",
  )
  parser.add_argument(
    "--sysout",
    type=str,
    default=DEFAULT_SYSOUT,
    help=f"Path to sysout file (default: {DEFAULT_SYSOUT})",
  )
  parser.add_argument(
    "--output",
    type=str,
    help="Output trace file path (default: {implementation}_emulator_execution_log.txt)",
  )
  parser.add_argument(
    "--json-metadata",
    type=str,
    help="Output execution statistics in JSON format",
  )
  parser.add_argument(
    "--timeout",
    type=int,
    default=DEFAULT_TIMEOUT,
    help=f"Timeout in seconds (default: {DEFAULT_TIMEOUT})",
  )

  args = parser.parse_args(argv)

  repo_root = get_repo_root()
  sysout_path = _resolve_sysout(repo_root, args.sysout)

  if not sysout_path.exists():
    print(f"ERROR: Sysout file not found: {sysout_path}", file=sys.stderr)
    return 1

  output_file = _determine_output_path(repo_root, args.implementation, args.output)
  if output_file.parent != Path("."):
    output_file.parent.mkdir(parents=True, exist_ok=True)

  # Run emulator
  exit_code, error_msg = _run_implementation(
    args.implementation,
    repo_root,
    sysout_path,
    args.end_step,
    output_file,
    args.timeout,
  )
  if exit_code != 0:
    if error_msg:
      print(f"ERROR: {error_msg}", file=sys.stderr)
    return exit_code

  # Extract window if start_step > 0
  if args.start_step > 0:
    temp_file = output_file.with_suffix(output_file.suffix + ".tmp")
    shutil.move(str(output_file), str(temp_file))

    from .trace_extractor import extract_trace_window  # local import to avoid cycle

    extract_trace_window(str(temp_file), str(output_file), args.start_step, args.end_step)

    archive_dir = repo_root / "reports" / "parity" / "archive" / datetime.now().strftime("%Y%m%d")
    archive_file(str(temp_file), str(archive_dir))

  # Validate trace format
  is_valid, validation_error = validate_trace_format(str(output_file))
  if not is_valid:
    print(f"ERROR: Trace format validation failed: {validation_error}", file=sys.stderr)
    return 2

  # Generate JSON metadata if requested
  if args.json_metadata:
    metadata = {
      "implementation": args.implementation,
      "start_step": args.start_step,
      "end_step": args.end_step,
      "sysout": str(sysout_path),
      "output_file": str(output_file),
      "timestamp": datetime.now().isoformat(),
    }
    with open(args.json_metadata, "w") as f:
      json.dump(metadata, f, indent=2)

  return 0


if __name__ == "__main__":  # pragma: no cover
  sys.exit(main())
