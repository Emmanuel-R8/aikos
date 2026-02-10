from __future__ import annotations

import os
import shutil
import subprocess
from datetime import datetime
from typing import Tuple

from .trace_extractor import extract_trace_window, archive_file


DEFAULT_ZIG_CACHE_DIR = "/tmp/zig-cache"


def find_c_emulator(repo_root: str) -> str | None:
  """Find the C emulator executable."""
  possible_paths = [
    os.path.join(repo_root, "maiko", "build-cmake", "ldesdl"),
    os.path.join(repo_root, "maiko", "linux.x86_64", "ldesdl"),
    os.path.join(repo_root, "maiko", "build", "c", "linux.x86_64", "ldesdl"),
  ]

  for path in possible_paths:
    if os.path.isfile(path) and os.access(path, os.X_OK):
      return path

  return None


def _archive_log_if_exists(repo_root: str, log_path: str) -> None:
  if not os.path.exists(log_path):
    return
  archive_dir = os.path.join(
    repo_root,
    "reports",
    "parity",
    "archive",
    datetime.now().strftime("%Y%m%d"),
  )
  archive_file(log_path, archive_dir)


def run_c_emulator(
  repo_root: str,
  sysout_path: str,
  end_step: int,
  output_file: str,
  timeout: int,
) -> Tuple[int, str | None]:
  """Run the C emulator and extract trace window.
  
  Args:
    repo_root: Repository root directory path.
    sysout_path: Path to the sysout file to load.
    end_step: Maximum step number to execute (inclusive).
    output_file: Path where the extracted trace window will be written.
    timeout: Maximum execution time in seconds.
  
  Returns:
    Tuple of (exit_code, error_message):
      - (0, None) on success
      - (1, error_msg) on execution failure or timeout
      - (3, error_msg) if emulator executable not found
  
  The function:
    1. Locates the C emulator executable (ldesdl)
    2. Sets EMULATOR_MAX_STEPS environment variable
    3. Archives any existing trace log
    4. Runs the emulator with the specified sysout
    5. Extracts the trace window (steps 0 to end_step) to output_file
  """
  emulator = find_c_emulator(repo_root)
  if not emulator:
    return 3, "C emulator (ldesdl) not found"

  # Set environment variable for max steps
  env = os.environ.copy()
  env["EMULATOR_MAX_STEPS"] = str(end_step)

  # C emulator writes to c_emulator_execution_log.txt in repo root
  c_log_path = os.path.join(repo_root, "c_emulator_execution_log.txt")
  _archive_log_if_exists(repo_root, c_log_path)

  try:
    subprocess.run(
      [emulator, sysout_path],
      cwd=repo_root,
      env=env,
      stdout=subprocess.PIPE,
      stderr=subprocess.PIPE,
      timeout=timeout,
      check=False,
    )

    if not os.path.exists(c_log_path):
      return 1, f"C emulator did not generate trace file: {c_log_path}"

    extract_trace_window(c_log_path, output_file, 0, end_step)
    return 0, None

  except subprocess.TimeoutExpired:
    return 1, f"C emulator timed out after {timeout} seconds"
  except Exception as e:  # pragma: no cover - defensive
    return 1, f"C emulator execution error: {str(e)}"


def run_zig_emulator(
  repo_root: str,
  sysout_path: str,
  end_step: int,
  output_file: str,
  timeout: int,
) -> Tuple[int, str | None]:
  """Run the Zig emulator and extract trace window.
  
  Args:
    repo_root: Repository root directory path.
    sysout_path: Path to the sysout file to load.
    end_step: Maximum step number to execute (inclusive).
    output_file: Path where the extracted trace window will be written.
    timeout: Maximum execution time in seconds.
  
  Returns:
    Tuple of (exit_code, error_message):
      - (0, None) on success
      - (1, error_msg) on execution failure or timeout
      - (3, error_msg) if Zig directory not found or zig command unavailable
  
  The function:
    1. Verifies the zaiko directory exists
    2. Archives any existing trace log
    3. Runs `zig build run` with --max-steps flag
    4. Locates the generated trace log (checks multiple possible locations)
    5. Extracts the trace window (steps 0 to end_step) to output_file
  """
  zig_dir = os.path.join(repo_root, "zaiko")
  if not os.path.isdir(zig_dir):
    return 3, "Zig emulator directory not found"

  # Zig emulator writes to zaiko/zig_emulator_execution_log.txt
  zig_log_path = os.path.join(zig_dir, "zig_emulator_execution_log.txt")
  _archive_log_if_exists(repo_root, zig_log_path)

  try:
    env = os.environ.copy()
    env.setdefault("ZIG_GLOBAL_CACHE_DIR", DEFAULT_ZIG_CACHE_DIR)

    subprocess.run(
      ["zig", "build", "run", "--", "--max-steps", str(end_step), sysout_path],
      cwd=zig_dir,
      env=env,
      stdout=subprocess.PIPE,
      stderr=subprocess.PIPE,
      timeout=timeout,
      check=False,
    )

    possible_logs = [
      zig_log_path,
      os.path.join(repo_root, "zig_emulator_execution_log.txt"),
    ]

    log_found = next((p for p in possible_logs if os.path.exists(p)), None)

    if not log_found:
      return 1, "Zig emulator did not generate trace file"

    extract_trace_window(log_found, output_file, 0, end_step)
    return 0, None

  except subprocess.TimeoutExpired:
    return 1, f"Zig emulator timed out after {timeout} seconds"
  except FileNotFoundError:
    return 3, "Zig compiler not found (zig command not available)"
  except Exception as e:  # pragma: no cover - defensive
    return 1, f"Zig emulator execution error: {str(e)}"


def run_lisp_emulator(
  repo_root: str,
  sysout_path: str,
  end_step: int,
  output_file: str,
  timeout: int,
) -> Tuple[int, str | None]:
  """Run the Laiko (Common Lisp) emulator and extract trace window.
  
  Args:
    repo_root: Repository root directory path.
    sysout_path: Path to the sysout file to load.
    end_step: Maximum step number to execute (inclusive).
    output_file: Path where the extracted trace window will be written.
    timeout: Maximum execution time in seconds.
  
  Returns:
    Tuple of (exit_code, error_message):
      - (0, None) on success
      - (1, error_msg) on execution failure or timeout
      - (3, error_msg) if laiko directory not found or sbcl command unavailable
  
  The function:
    1. Verifies the laiko directory exists
    2. Archives any existing trace log
    3. Runs the laiko run.sh script with EMULATOR_MAX_STEPS environment variable
    4. Locates the generated trace log
    5. Extracts the trace window (steps 0 to end_step) to output_file
  """
  lisp_dir = os.path.join(repo_root, "laiko")
  if not os.path.isdir(lisp_dir):
    return 3, "Lisp emulator directory not found"

  run_script = os.path.join(lisp_dir, "run.sh")
  if not os.path.isfile(run_script):
    return 3, "Lisp emulator run script not found"

  lisp_log_path = os.path.join(repo_root, "lisp_emulator_execution_log.txt")
  _archive_log_if_exists(repo_root, lisp_log_path)

  try:
    env = os.environ.copy()
    env["EMULATOR_MAX_STEPS"] = str(end_step)

    subprocess.run(
      ["bash", run_script, sysout_path],
      cwd=lisp_dir,
      env=env,
      stdout=subprocess.PIPE,
      stderr=subprocess.PIPE,
      timeout=timeout,
      check=False,
    )

    possible_logs = [
      lisp_log_path,
      os.path.join(lisp_dir, "lisp_emulator_execution_log.txt"),
    ]
    log_found = next((p for p in possible_logs if os.path.exists(p)), None)

    if not log_found:
      return 1, "Lisp emulator did not generate trace file"

    extract_trace_window(log_found, output_file, 0, end_step)
    return 0, None

  except subprocess.TimeoutExpired:
    return 1, f"Lisp emulator timed out after {timeout} seconds"
  except FileNotFoundError:
    return 3, "SBCL not found (sbcl command not available)"
  except Exception as e:  # pragma: no cover - defensive
    return 1, f"Lisp emulator execution error: {str(e)}"


def run_typescript_emulator(
  repo_root: str,
  sysout_path: str,
  end_step: int,
  output_file: str,
  timeout: int,
) -> Tuple[int, str | None]:
  """Run the Taiko (TypeScript) emulator and extract trace window.
  
  Args:
    repo_root: Repository root directory path.
    sysout_path: Path to the sysout file to load.
    end_step: Maximum step number to execute (inclusive).
    output_file: Path where the extracted trace window would be written.
    timeout: Maximum execution time in seconds.
  
  Returns:
    Tuple of (exit_code, error_message):
      - (3, error_msg) if taiko directory not found or execution not yet implemented
  
  Note:
    Execution is currently not implemented; this is a placeholder that
    returns a clear diagnostic so callers can handle the absence of TS support.
    Future implementation will follow the same pattern as other emulator runners.
  """
  ts_dir = os.path.join(repo_root, "taiko")
  if not os.path.isdir(ts_dir):
    return 3, "TypeScript emulator directory not found"

  package_json = os.path.join(ts_dir, "package.json")
  if not os.path.isfile(package_json):
    return 3, "TypeScript emulator package.json not found"

  ts_log_path = os.path.join(repo_root, "typescript_emulator_execution_log.txt")
  _archive_log_if_exists(repo_root, ts_log_path)

  # For now we only signal that TS execution is not wired up.
  # This keeps behavior consistent with the previous monolithic script.
  return 3, "TypeScript emulator execution not yet implemented"
