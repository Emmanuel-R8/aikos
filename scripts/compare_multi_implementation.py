#!/usr/bin/env python3
"""
Multi-Implementation Trace Comparison Tool
Compares traces from 4 implementations (C, Zig, TypeScript, Lisp) simultaneously.
Identifies first divergence point for each pair and generates comprehensive reports.
"""

import sys
import argparse
import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from collections import defaultdict


def parse_trace_line(line: str) -> Optional[Dict[str, str]]:
    """Parse a unified trace line into fields."""
    fields = line.strip().split("|")
    # Accept 13-14 fields (13 data fields + optional empty at end)
    if len(fields) < 13:
        return None

    return {
        "line": fields[0],
        "pc": fields[1],
        "instruction": fields[2],
        "opcode": fields[3],
        "operands": fields[4],
        "registers": fields[5],
        "flags": fields[6],
        "sp_fp": fields[7],
        "stack": fields[8],
        "memory": fields[9],
        "mapping": fields[10],
        "byteswap": fields[11],
        "notes": fields[12],
    }


def parse_sub_fields(field_str: str) -> Dict[str, str]:
    """Parse comma-separated sub-fields into a dictionary."""
    if not field_str or field_str.strip() == "":
        return {}

    result = {}
    for item in field_str.split(','):
        item = item.strip()
        if ':' in item:
            key, value = item.split(':', 1)
            result[key.strip()] = value.strip()
    return result


def compare_fields(c_field: str, other_field: str, field_name: str) -> List[Dict]:
    """Compare two fields and return differences."""
    differences = []

    # For comma-separated fields, compare sub-fields
    if ',' in c_field or ',' in other_field:
        c_sub = parse_sub_fields(c_field)
        other_sub = parse_sub_fields(other_field)
        all_keys = set(c_sub.keys()) | set(other_sub.keys())

        for key in sorted(all_keys):
            c_val = c_sub.get(key, None)
            other_val = other_sub.get(key, None)

            if c_val != other_val:
                differences.append({
                    "field": f"{field_name}.{key}",
                    "c_value": c_val,
                    "other_value": other_val
                })
    else:
        # Simple field comparison
        if c_field.strip() != other_field.strip():
            differences.append({
                "field": field_name,
                "c_value": c_field.strip(),
                "other_value": other_field.strip()
            })

    return differences


def find_first_divergence(c_trace: List[str], other_trace: List[str], impl_name: str) -> Optional[Dict]:
    """Find first divergence point between C and another implementation."""
    min_len = min(len(c_trace), len(other_trace))

    for i in range(min_len):
        c_line = parse_trace_line(c_trace[i])
        other_line = parse_trace_line(other_trace[i])

        if not c_line or not other_line:
            continue

        differences = []

        # Compare all fields
        for field_name in ["pc", "instruction", "opcode", "operands", "registers",
                          "flags", "sp_fp", "stack", "memory", "mapping", "byteswap", "notes"]:
            field_diffs = compare_fields(c_line[field_name], other_line[field_name], field_name)
            differences.extend(field_diffs)

        if differences:
            return {
                "step": i,
                "line_number": c_line["line"],
                "pc": c_line["pc"],
                "instruction": c_line["instruction"],
                "differences": differences,
                "c_line": c_trace[i],
                "other_line": other_trace[i]
            }

    # Check if one trace is longer
    if len(c_trace) != len(other_trace):
        return {
            "step": min_len,
            "line_number": str(min_len),
            "pc": "N/A",
            "instruction": "LENGTH_MISMATCH",
            "differences": [{
                "field": "trace_length",
                "c_value": str(len(c_trace)),
                "other_value": str(len(other_trace))
            }],
            "c_line": f"Total lines: {len(c_trace)}",
            "other_line": f"Total lines: {len(other_trace)}"
        }

    return None


def load_trace_file(filepath: Path) -> List[str]:
    """Load trace file and return list of lines."""
    try:
        with open(filepath, 'r') as f:
            lines = [line.strip() for line in f if line.strip()]
        return lines
    except FileNotFoundError:
        return []
    except Exception as e:
        print(f"Error loading {filepath}: {e}", file=sys.stderr)
        return []


def compare_all_implementations(
    c_trace_file: Path,
    zig_trace_file: Optional[Path] = None,
    ts_trace_file: Optional[Path] = None,
    lisp_trace_file: Optional[Path] = None,
    max_lines: Optional[int] = None
) -> Dict:
    """Compare all implementations against C reference."""

    # Load C reference trace
    c_trace = load_trace_file(c_trace_file)
    if max_lines:
        c_trace = c_trace[:max_lines]

    if not c_trace:
        return {
            "error": "C reference trace is empty or not found",
            "c_file": str(c_trace_file)
        }

    results = {
        "c_reference": {
            "file": str(c_trace_file),
            "lines": len(c_trace)
        },
        "comparisons": {},
        "summary": {}
    }

    # Compare each implementation
    implementations = [
        ("Zig", zig_trace_file),
        ("TypeScript", ts_trace_file),
        ("Lisp", lisp_trace_file)
    ]

    for impl_name, trace_file in implementations:
        if not trace_file or not trace_file.exists():
            results["comparisons"][impl_name] = {
                "status": "not_found",
                "file": str(trace_file) if trace_file else "not_provided"
            }
            continue

        other_trace = load_trace_file(trace_file)
        if max_lines:
            other_trace = other_trace[:max_lines]

        if not other_trace:
            results["comparisons"][impl_name] = {
                "status": "empty_trace",
                "file": str(trace_file)
            }
            continue

        divergence = find_first_divergence(c_trace, other_trace, impl_name)

        if divergence:
            results["comparisons"][impl_name] = {
                "status": "diverges",
                "file": str(trace_file),
                "lines": len(other_trace),
                "first_divergence": divergence
            }
        else:
            results["comparisons"][impl_name] = {
                "status": "matches",
                "file": str(trace_file),
                "lines": len(other_trace),
                "matched_lines": min(len(c_trace), len(other_trace))
            }

    # Generate summary
    total_impls = len([impl for impl, _ in implementations])
    matching = sum(1 for comp in results["comparisons"].values()
                   if comp.get("status") == "matches")
    diverging = sum(1 for comp in results["comparisons"].values()
                    if comp.get("status") == "diverges")
    missing = total_impls - matching - diverging

    results["summary"] = {
        "total_implementations": total_impls,
        "matching": matching,
        "diverging": diverging,
        "missing_or_empty": missing,
        "c_reference_lines": len(c_trace)
    }

    return results


def print_report(results: Dict, verbose: bool = False):
    """Print human-readable comparison report."""
    print("=== MULTI-IMPLEMENTATION PARITY REPORT ===\n")

    print(f"C Reference: {results['c_reference']['file']}")
    print(f"  Lines: {results['c_reference']['lines']}\n")

    print("Implementation Status:")
    for impl_name, comp in results["comparisons"].items():
        status = comp["status"]
        if status == "matches":
            print(f"  ✓ {impl_name}: MATCHES ({comp['matched_lines']} lines)")
        elif status == "diverges":
            div = comp["first_divergence"]
            print(f"  ✗ {impl_name}: DIVERGES at step {div['step']} (PC: {div['pc']}, Instruction: {div['instruction']})")
            if verbose:
                print(f"    Differences:")
                for diff in div["differences"][:5]:  # Show first 5 differences
                    print(f"      {diff['field']}: C={diff['c_value']}, {impl_name}={diff['other_value']}")
        elif status == "not_found":
            print(f"  - {impl_name}: NOT FOUND ({comp['file']})")
        elif status == "empty_trace":
            print(f"  - {impl_name}: EMPTY TRACE ({comp['file']})")

    print(f"\nSummary:")
    summary = results["summary"]
    print(f"  Matching: {summary['matching']}/{summary['total_implementations']}")
    print(f"  Diverging: {summary['diverging']}/{summary['total_implementations']}")
    print(f"  Missing/Empty: {summary['missing_or_empty']}/{summary['total_implementations']}")


def generate_typst_snippet(results: Dict) -> str:
    """Generate Typst documentation snippet from comparison results."""
    lines = []
    lines.append("== Multi-Implementation Parity Comparison")
    lines.append("")
    lines.append(f"*Date*: Generated automatically")
    lines.append(f"*C Reference Lines*: {results['c_reference']['lines']}")
    lines.append("")

    for impl_name, comp in results["comparisons"].items():
        lines.append(f"=== {impl_name} Implementation")
        if comp["status"] == "matches":
            lines.append(f"*Status*: ✅ MATCHES")
            lines.append(f"*Matched Lines*: {comp['matched_lines']}")
        elif comp["status"] == "diverges":
            div = comp["first_divergence"]
            lines.append(f"*Status*: ❌ DIVERGES")
            lines.append(f"*First Divergence*: Step {div['step']}, PC {div['pc']}, Instruction {div['instruction']}")
            lines.append(f"*Differences*:")
            for diff in div["differences"][:10]:  # Limit to 10 differences
                lines.append(f"  - {diff['field']}: C={diff['c_value']}, {impl_name}={diff['other_value']}")
        else:
            lines.append(f"*Status*: ⚠️ {comp['status'].upper()}")
        lines.append("")

    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Compare traces from multiple emulator implementations"
    )
    parser.add_argument("--c-trace", required=True, type=Path,
                       help="C reference trace file")
    parser.add_argument("--zig-trace", type=Path,
                       help="Zig emulator trace file")
    parser.add_argument("--ts-trace", type=Path,
                       help="TypeScript emulator trace file")
    parser.add_argument("--lisp-trace", type=Path,
                       help="Lisp emulator trace file")
    parser.add_argument("--max-lines", type=int,
                       help="Maximum number of lines to compare")
    parser.add_argument("--json", action="store_true",
                       help="Output results as JSON")
    parser.add_argument("--typst", action="store_true",
                       help="Generate Typst documentation snippet")
    parser.add_argument("--verbose", "-v", action="store_true",
                       help="Verbose output")

    args = parser.parse_args()

    results = compare_all_implementations(
        args.c_trace,
        args.zig_trace,
        args.ts_trace,
        args.lisp_trace,
        args.max_lines
    )

    if "error" in results:
        print(f"ERROR: {results['error']}", file=sys.stderr)
        sys.exit(1)

    if args.json:
        print(json.dumps(results, indent=2))
    elif args.typst:
        print(generate_typst_snippet(results))
    else:
        print_report(results, args.verbose)


if __name__ == "__main__":
    main()
