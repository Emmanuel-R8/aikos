#!/usr/bin/env python3
"""
Execution Window Analysis Module (wrapper).

This script now delegates to the `execution_window_analysis` package, which
contains the actual parser and analyzer logic in smaller, focused modules.
The CLI remains compatible with the previous version.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from execution_window_analysis.analyzer import analyze_execution_window, write_analysis_to_file


def main(argv: list[str] | None = None) -> int:
  parser = argparse.ArgumentParser(
    description="Analyze execution window from unified trace format",
  )
  parser.add_argument(
    "trace_file",
    type=str,
    help="Path to unified trace format file",
  )
  parser.add_argument(
    "--output",
    type=str,
    help="Output JSON file path (default: print to stdout)",
  )

  args = parser.parse_args(argv)

  trace_path = Path(args.trace_file)
  if not trace_path.exists():
    print(f"ERROR: Trace file not found: {trace_path}", file=sys.stderr)
    return 1

  try:
    analysis = analyze_execution_window(str(trace_path))
    if args.output:
      write_analysis_to_file(analysis, args.output)
      print(f"Analysis written to: {args.output}", file=sys.stderr)
    else:
      import json

      print(json.dumps(analysis, indent=2))
    return 0
  except Exception as exc:  # pragma: no cover - defensive
    print(f"ERROR: Analysis failed: {exc}", file=sys.stderr)
    import traceback

    traceback.print_exc()
    return 1


if __name__ == "__main__":  # pragma: no cover
  sys.exit(main())
