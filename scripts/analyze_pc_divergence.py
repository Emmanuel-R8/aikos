#!/usr/bin/env python3
"""
PC Divergence Analysis
Analyzes program counter (PC) divergences between implementations.
"""

import sys
import argparse
from pathlib import Path


def parse_trace_line(line: str):
    """Parse unified trace line."""
    fields = line.strip().split("|")
    if len(fields) < 13:
        return None
    return {
        "line": fields[0],
        "pc": fields[1],
        "instruction": fields[2],
        "opcode": fields[3],
    }


def analyze_pc_divergence(c_trace_file: Path, other_trace_file: Path, impl_name: str):
    """Analyze PC divergences between C and another implementation."""
    print(f"=== PC DIVERGENCE ANALYSIS: C vs {impl_name} ===\n")

    with open(c_trace_file) as f:
        c_lines = [l.strip() for l in f if l.strip()]
    with open(other_trace_file) as f:
        other_lines = [l.strip() for l in f if l.strip()]

    min_len = min(len(c_lines), len(other_lines))
    divergences = []

    for i in range(min_len):
        c_parsed = parse_trace_line(c_lines[i])
        other_parsed = parse_trace_line(other_lines[i])

        if not c_parsed or not other_parsed:
            continue

        if c_parsed["pc"] != other_parsed["pc"]:
            divergences.append({
                "step": i,
                "c_pc": c_parsed["pc"],
                "other_pc": other_parsed["pc"],
                "instruction": c_parsed["instruction"],
                "opcode": c_parsed["opcode"]
            })

    if divergences:
        print(f"Found {len(divergences)} PC divergences:\n")
        for div in divergences[:20]:  # Show first 20
            print(f"Step {div['step']}: PC C={div['c_pc']} {impl_name}={div['other_pc']} "
                  f"(Instruction: {div['instruction']}, Opcode: {div['opcode']})")
        if len(divergences) > 20:
            print(f"\n... and {len(divergences) - 20} more")
    else:
        print("✓ No PC divergences found")

    if len(c_lines) != len(other_lines):
        print(f"\n⚠️ Trace length mismatch: C={len(c_lines)} lines, {impl_name}={len(other_lines)} lines")

    return divergences


def main():
    parser = argparse.ArgumentParser(description="Analyze PC divergences")
    parser.add_argument("c_trace", type=Path, help="C reference trace")
    parser.add_argument("other_trace", type=Path, help="Other implementation trace")
    parser.add_argument("--impl-name", default="Other", help="Implementation name")

    args = parser.parse_args()
    analyze_pc_divergence(args.c_trace, args.other_trace, args.impl_name)


if __name__ == "__main__":
    main()
