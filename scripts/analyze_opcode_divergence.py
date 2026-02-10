#!/usr/bin/env python3
"""
Opcode Divergence Analysis
Analyzes opcode execution and instruction divergences.
"""

import sys
import argparse
from pathlib import Path
from collections import defaultdict


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
        "operands": fields[4],
    }


def analyze_opcode_divergence(c_trace_file: Path, other_trace_file: Path, impl_name: str):
    """Analyze opcode divergences."""
    print(f"=== OPCODE DIVERGENCE ANALYSIS: C vs {impl_name} ===\n")

    with open(c_trace_file) as f:
        c_lines = [l.strip() for l in f if l.strip()]
    with open(other_trace_file) as f:
        other_lines = [l.strip() for l in f if l.strip()]

    min_len = min(len(c_lines), len(other_lines))
    opcode_divergences = []
    instruction_divergences = []
    opcode_counts = defaultdict(int)

    for i in range(min_len):
        c_parsed = parse_trace_line(c_lines[i])
        other_parsed = parse_trace_line(other_lines[i])

        if not c_parsed or not other_parsed:
            continue

        opcode_counts[c_parsed["opcode"]] += 1

        # Check opcode
        if c_parsed["opcode"] != other_parsed["opcode"]:
            opcode_divergences.append({
                "step": i,
                "c_opcode": c_parsed["opcode"],
                "other_opcode": other_parsed["opcode"],
                "pc": c_parsed["pc"],
                "c_instruction": c_parsed["instruction"],
                "other_instruction": other_parsed["instruction"]
            })

        # Check instruction name
        if c_parsed["instruction"] != other_parsed["instruction"]:
            instruction_divergences.append({
                "step": i,
                "c_instruction": c_parsed["instruction"],
                "other_instruction": other_parsed["instruction"],
                "pc": c_parsed["pc"],
                "opcode": c_parsed["opcode"]
            })

    print(f"Opcode Divergences: {len(opcode_divergences)}")
    if opcode_divergences:
        for div in opcode_divergences[:10]:
            print(f"  Step {div['step']}: Opcode C={div['c_opcode']} {impl_name}={div['other_opcode']} "
                  f"(PC: {div['pc']}, C Instruction: {div['c_instruction']}, "
                  f"{impl_name} Instruction: {div['other_instruction']})")

    print(f"\nInstruction Name Divergences: {len(instruction_divergences)}")
    if instruction_divergences:
        for div in instruction_divergences[:10]:
            print(f"  Step {div['step']}: Instruction C={div['c_instruction']} {impl_name}={div['other_instruction']} "
                  f"(PC: {div['pc']}, Opcode: {div['opcode']})")

    print(f"\nOpcode Coverage:")
    print(f"  Total unique opcodes in C trace: {len(opcode_counts)}")
    print(f"  Most common opcodes:")
    for opcode, count in sorted(opcode_counts.items(), key=lambda x: x[1], reverse=True)[:10]:
        print(f"    {opcode}: {count} occurrences")

    if not opcode_divergences and not instruction_divergences:
        print("\n✓ No opcode divergences found")

    return {
        "opcode": opcode_divergences,
        "instruction": instruction_divergences,
        "coverage": opcode_counts
    }


def main():
    parser = argparse.ArgumentParser(description="Analyze opcode divergences")
    parser.add_argument("c_trace", type=Path, help="C reference trace")
    parser.add_argument("other_trace", type=Path, help="Other implementation trace")
    parser.add_argument("--impl-name", default="Other", help="Implementation name")

    args = parser.parse_args()
    analyze_opcode_divergence(args.c_trace, args.other_trace, args.impl_name)


if __name__ == "__main__":
    main()
