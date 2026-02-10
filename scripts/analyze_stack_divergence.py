#!/usr/bin/env python3
"""
Stack Divergence Analysis
Analyzes stack pointer, frame pointer, and stack content divergences.
"""

import sys
import argparse
from pathlib import Path


def parse_trace_line(line: str):
    """Parse unified trace line."""
    fields = line.strip().split("|")
    if len(fields) < 13:
        return None

    # Parse SP_FP field (comma-separated)
    sp_fp = {}
    if len(fields) > 7 and fields[7]:
        for item in fields[7].split(','):
            if ':' in item:
                key, value = item.split(':', 1)
                sp_fp[key.strip()] = value.strip()

    # Parse STACK_SUMMARY field (comma-separated)
    stack = {}
    if len(fields) > 8 and fields[8]:
        for item in fields[8].split(','):
            if ':' in item:
                key, value = item.split(':', 1)
                stack[key.strip()] = value.strip()

    return {
        "line": fields[0],
        "pc": fields[1],
        "instruction": fields[2],
        "sp_fp": sp_fp,
        "stack": stack,
    }


def analyze_stack_divergence(c_trace_file: Path, other_trace_file: Path, impl_name: str):
    """Analyze stack divergences."""
    print(f"=== STACK DIVERGENCE ANALYSIS: C vs {impl_name} ===\n")

    with open(c_trace_file) as f:
        c_lines = [l.strip() for l in f if l.strip()]
    with open(other_trace_file) as f:
        other_lines = [l.strip() for l in f if l.strip()]

    min_len = min(len(c_lines), len(other_lines))
    sp_divergences = []
    fp_divergences = []
    tos_divergences = []

    for i in range(min_len):
        c_parsed = parse_trace_line(c_lines[i])
        other_parsed = parse_trace_line(other_lines[i])

        if not c_parsed or not other_parsed:
            continue

        # Check SP
        c_sp = c_parsed["sp_fp"].get("SP", "")
        other_sp = other_parsed["sp_fp"].get("SP", "")
        if c_sp != other_sp:
            sp_divergences.append({
                "step": i,
                "c_sp": c_sp,
                "other_sp": other_sp,
                "pc": c_parsed["pc"],
                "instruction": c_parsed["instruction"]
            })

        # Check FP
        c_fp = c_parsed["sp_fp"].get("FP", "")
        other_fp = other_parsed["sp_fp"].get("FP", "")
        if c_fp != other_fp:
            fp_divergences.append({
                "step": i,
                "c_fp": c_fp,
                "other_fp": other_fp,
                "pc": c_parsed["pc"],
                "instruction": c_parsed["instruction"]
            })

        # Check TOS
        c_tos = c_parsed["stack"].get("TOS", "")
        other_tos = other_parsed["stack"].get("TOS", "")
        if c_tos != other_tos:
            tos_divergences.append({
                "step": i,
                "c_tos": c_tos,
                "other_tos": other_tos,
                "pc": c_parsed["pc"],
                "instruction": c_parsed["instruction"]
            })

    print(f"Stack Pointer Divergences: {len(sp_divergences)}")
    if sp_divergences:
        for div in sp_divergences[:10]:
            print(f"  Step {div['step']}: SP C={div['c_sp']} {impl_name}={div['other_sp']} "
                  f"(PC: {div['pc']}, Instruction: {div['instruction']})")

    print(f"\nFrame Pointer Divergences: {len(fp_divergences)}")
    if fp_divergences:
        for div in fp_divergences[:10]:
            print(f"  Step {div['step']}: FP C={div['c_fp']} {impl_name}={div['other_fp']} "
                  f"(PC: {div['pc']}, Instruction: {div['instruction']})")

    print(f"\nTop-of-Stack Divergences: {len(tos_divergences)}")
    if tos_divergences:
        for div in tos_divergences[:10]:
            print(f"  Step {div['step']}: TOS C={div['c_tos']} {impl_name}={div['other_tos']} "
                  f"(PC: {div['pc']}, Instruction: {div['instruction']})")

    if not sp_divergences and not fp_divergences and not tos_divergences:
        print("\n✓ No stack divergences found")

    return {
        "sp": sp_divergences,
        "fp": fp_divergences,
        "tos": tos_divergences
    }


def main():
    parser = argparse.ArgumentParser(description="Analyze stack divergences")
    parser.add_argument("c_trace", type=Path, help="C reference trace")
    parser.add_argument("other_trace", type=Path, help="Other implementation trace")
    parser.add_argument("--impl-name", default="Other", help="Implementation name")

    args = parser.parse_args()
    analyze_stack_divergence(args.c_trace, args.other_trace, args.impl_name)


if __name__ == "__main__":
    main()
