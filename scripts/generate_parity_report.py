#!/usr/bin/env python3
"""
Parity Report Generator
Generates Typst documentation from comparison results.
Reads JSON output from compare_multi_implementation.py and generates Typst sections.
"""

import sys
import json
import argparse
from pathlib import Path
from datetime import datetime


def generate_divergence_entry(divergence_id: str, impl_name: str, divergence: dict) -> str:
    """Generate Typst entry for a single divergence."""
    lines = []
    lines.append(f"=== {divergence_id}: {impl_name} - {divergence['instruction']}")
    lines.append("")
    lines.append(f"*Status*: OPEN")
    lines.append(f"*Date Identified*: {datetime.now().strftime('%Y-%m-%d')}")
    lines.append(f"*Affected Implementations*: {impl_name}")
    lines.append(f"*First Occurrence*: Step {divergence['step']}, PC {divergence['pc']}")
    lines.append(f"*Fields*: {', '.join([d['field'] for d in divergence['differences'][:5]])}")
    lines.append("")
    lines.append("*Root Cause*: TBD - Requires investigation")
    lines.append("")
    lines.append("*Related Files*: TBD")
    lines.append("")
    return "\n".join(lines)


def generate_typst_report(json_file: Path, output_file: Path):
    """Generate Typst report from JSON comparison results."""
    with open(json_file) as f:
        data = json.load(f)

    lines = []
    lines.append("= Parity Comparison Report")
    lines.append("")
    lines.append(f"*Date*: {datetime.now().strftime('%Y-%m-%d %H:%M')}")
    lines.append(f"*C Reference Lines*: {data['c_reference']['lines']}")
    lines.append("")

    # Summary section
    summary = data.get("summary", {})
    lines.append("== Summary")
    lines.append("")
    lines.append(f"*Total Implementations*: {summary.get('total_implementations', 0)}")
    lines.append(f"*Matching*: {summary.get('matching', 0)}")
    lines.append(f"*Diverging*: {summary.get('diverging', 0)}")
    lines.append(f"*Missing/Empty*: {summary.get('missing_or_empty', 0)}")
    lines.append("")

    # Divergences section
    lines.append("== Identified Divergences")
    lines.append("")

    divergence_count = 0
    for impl_name, comp in data.get("comparisons", {}).items():
        if comp.get("status") == "diverges":
            divergence_count += 1
            div_id = f"DIV-{divergence_count:03d}"
            divergence = comp.get("first_divergence", {})
            lines.append(generate_divergence_entry(div_id, impl_name, divergence))
            lines.append("")

    if divergence_count == 0:
        lines.append("No divergences identified in this comparison.")
        lines.append("")

    # Implementation status section
    lines.append("== Implementation Status")
    lines.append("")

    for impl_name, comp in data.get("comparisons", {}).items():
        lines.append(f"=== {impl_name}")
        status = comp.get("status", "unknown")
        if status == "matches":
            lines.append("*Status*: ✅ MATCHES")
            lines.append(f"*Matched Lines*: {comp.get('matched_lines', 0)}")
        elif status == "diverges":
            lines.append("*Status*: ❌ DIVERGES")
            div = comp.get("first_divergence", {})
            lines.append(f"*First Divergence*: Step {div.get('step', 'N/A')}, PC {div.get('pc', 'N/A')}")
        elif status == "not_found":
            lines.append("*Status*: ⚠️ NOT FOUND")
        elif status == "empty_trace":
            lines.append("*Status*: ⚠️ EMPTY TRACE")
        else:
            lines.append(f"*Status*: ⚠️ {status.upper()}")
        lines.append("")

    # Write output
    with open(output_file, 'w') as f:
        f.write("\n".join(lines))

    print(f"Generated Typst report: {output_file}")
    print(f"  Divergences found: {divergence_count}")


def update_existing_typst(typst_file: Path, json_file: Path):
    """Update existing Typst file with new findings."""
    # Read existing file
    if typst_file.exists():
        with open(typst_file) as f:
            existing_content = f.read()
    else:
        existing_content = ""

    # Generate new section
    with open(json_file) as f:
        data = json.load(f)

    new_section = []
    new_section.append(f"== Update: {datetime.now().strftime('%Y-%m-%d %H:%M')}")
    new_section.append("")

    for impl_name, comp in data.get("comparisons", {}).items():
        if comp.get("status") == "diverges":
            div = comp.get("first_divergence", {})
            new_section.append(f"*{impl_name}*: Diverges at step {div.get('step', 'N/A')}, PC {div.get('pc', 'N/A')}")

    new_section.append("")
    new_section.append("")

    # Append to file
    with open(typst_file, 'a') as f:
        f.write("\n".join(new_section))

    print(f"Updated Typst file: {typst_file}")


def main():
    parser = argparse.ArgumentParser(description="Generate Typst reports from comparison JSON")
    parser.add_argument("json_file", type=Path, help="JSON output from compare_multi_implementation.py")
    parser.add_argument("--output", type=Path, help="Output Typst file (default: report.typ)")
    parser.add_argument("--update", type=Path, help="Update existing Typst file instead of creating new")

    args = parser.parse_args()

    if args.update:
        update_existing_typst(args.update, args.json_file)
    else:
        output_file = args.output or Path("parity_report.typ")
        generate_typst_report(args.json_file, output_file)


if __name__ == "__main__":
    main()
