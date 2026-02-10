from __future__ import annotations

import os
from datetime import datetime
from pathlib import Path
from typing import Tuple


def extract_trace_window(input_file: str, output_file: str, start_step: int, end_step: int) -> None:
  """Extract a window of steps from a trace file.

  Args:
      input_file: Path to full trace file
      output_file: Path to write extracted window
      start_step: First step to include (0-indexed, inclusive)
      end_step: Last step to include (0-indexed, inclusive)
  """
  with open(input_file, "r") as f_in:
    lines = f_in.readlines()

  # Extract lines from start_step to end_step (inclusive)
  # Trace lines are 0-indexed, so line N corresponds to step N
  window_lines = lines[start_step : end_step + 1]

  out_path = Path(output_file)
  out_path.parent.mkdir(parents=True, exist_ok=True)
  with open(out_path, "w") as f_out:
    f_out.writelines(window_lines)


def archive_file(file_path: str, archive_dir: str) -> str | None:
  """Archive a file instead of deleting it (safe file operations)."""
  if not os.path.exists(file_path):
    return None

  os.makedirs(archive_dir, exist_ok=True)
  timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
  filename = os.path.basename(file_path)
  archive_path = os.path.join(archive_dir, f"{timestamp}_{filename}")
  os.replace(file_path, archive_path)
  return archive_path


def validate_trace_format(trace_file: str) -> Tuple[bool, str | None]:
  """Validate trace file format against specification.

  Returns:
      (is_valid, error_message)
  """
  if not os.path.exists(trace_file):
    return False, "Trace file does not exist"

  with open(trace_file, "r") as f:
    lines = f.readlines()

  if not lines:
    return False, "Trace file is empty"

  # Check first few lines for format compliance
  # Format: LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
  for i, line in enumerate(lines[:10]):  # Check first 10 lines
    line = line.strip()
    if not line:
      continue

    fields = line.split("|")
    if len(fields) < 13:
      return False, f"Line {i + 1}: Expected at least 13 pipe-delimited fields, got {len(fields)}"

  return True, None
