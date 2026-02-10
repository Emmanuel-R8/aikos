#!/usr/bin/env python3
"""
Memory Divergence Analysis
Analyzes memory context, virtual page, and address mapping divergences.
"""

import sys
import argparse
from pathlib import Path


def parse_trace_line(line: str):
    """Parse unified trace line."""
    fields = line.strip().split("|")
    if len(fields) < 13:
        return None

    # Parse MEMORY_CONTEXT field
    memory = {}
    if len(fields) > 9 and fields[9]:
        for item in fields[9].split(','):
            if ':' in item:
                key, value = item.split(':', 1)
                memory[key.strip()] = value.strip()

    # Parse FP_VP_FO_VA field
    mapping = {}
    if len(fields) > 10 and fields[10]:
        for item in fields[10].split(','):
            if ':' in item:
                key, value = item.split(':', 1)
                mapping[key.strip()] = value.strip()

    return {
        "line": fields[0],
        "pc": fields[1],
        "instruction": fields[2],
        "memory": memory,
        "mapping": mapping,
    }


def analyze_memory_divergence(c_trace_file: Path, other_trace_file: Path, impl_name: str):
    """Analyze memory divergences."""
    print(f"=== MEMORY DIVERGENCE ANALYSIS: C vs {impl_name} ===\n")

    with open(c_trace_file) as f:
        c_lines = [l.strip() for l in f if l.strip()]
    with open(other_trace_file) as f:
        other_lines = [l.strip() for l in f if l.strip()]

    min_len = min(len(c_lines), len(other_lines))
    vpage_divergences = []
    mapping_divergences = []

    for i in range(min_len):
        c_parsed = parse_trace_line(c_lines[i])
        other_parsed = parse_trace_line(other_lines[i])

        if not c_parsed or not other_parsed:
            continue

        # Check virtual page
        c_vpage = c_parsed["memory"].get("vpage", "")
        other_vpage = other_parsed["memory"].get("vpage", "")
        if c_vpage != other_vpage:
            vpage_divergences.append({
                "step": i,
                "c_vpage": c_vpage,
                "other_vpage": other_vpage,
                "pc": c_parsed["pc"],
                "instruction": c_parsed["instruction"]
            })

        # Check mapping
        c_vp = c_parsed["mapping"].get("VP", "")
        other_vp = other_parsed["mapping"].get("VP", "")
        if c_vp != other_vp:
            mapping_divergences.append({
                "step": i,
                "c_vp": c_vp,
                "other_vp": other_vp,
                "pc": c_parsed["pc"],
                "instruction": c_parsed["instruction"]
            })

    print(f"Virtual Page Divergences: {len(vpage_divergences)}")
    if vpage_divergences:
        for div in vpage_divergences[:10]:
            print(f"  Step {div['step']}: vpage C={div['c_vpage']} {impl_name}={div['other_vpage']} "
                  f"(PC: {div['pc']}, Instruction: {div['instruction']})")

    print(f"\nVP Mapping Divergences: {len(mapping_divergences)}")
    if mapping_divergences:
        for div in mapping_divergences[:10]:
            print(f"  Step {div['step']}: VP C={div['c_vp']} {impl_name}={div['other_vp']} "
                  f"(PC: {div['pc']}, Instruction: {div['instruction']})")

    if not vpage_divergences and not mapping_divergences:
        print("\n✓ No memory divergences found")

    return {
        "vpage": vpage_divergences,
        "mapping": mapping_divergences
    }


def main():
    parser = argparse.ArgumentParser(description="Analyze memory divergences")
    parser.add_argument("c_trace", type=Path, help="C reference trace")
    parser.add_argument("other_trace", type=Path, help="Other implementation trace")
    parser.add_argument("--impl-name", default="Other", help="Implementation name")

    args = parser.parse_args()
    analyze_memory_divergence(args.c_trace, args.other_trace, args.impl_name)


if __name__ == "__main__":
    main()
