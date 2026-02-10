#!/usr/bin/env python3
"""
Unified Test Harness for Multi-Implementation Parity Analysis.

This thin wrapper preserves the original CLI while delegating the actual
implementation to the `unified_test_harness` package so the code can be
maintained in smaller, focused modules.
"""

from __future__ import annotations

import sys

from unified_test_harness.main import main


if __name__ == "__main__":  # pragma: no cover
  sys.exit(main())
