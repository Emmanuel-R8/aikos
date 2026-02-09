#!/usr/bin/env python3
"""
Unified Trace Comparison Script
Compares C and Zig emulator unified traces for divergences.
Enhanced with sub-field parsing for comma-separated fields.
"""

import sys
import argparse


def parse_trace_line(line):
    """Parse a unified trace line into fields."""
    fields = line.strip().split("|")
    # Accept 13-14 fields (13 data fields + optional empty at end)
    # Old format may have fewer fields, new format has 14
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


def parse_sub_fields(field_str, field_name):
    """Parse comma-separated sub-fields into a dictionary.
    
    Examples:
        "r1:0x1234,r2:0x5678,r3:0x9a" -> {"r1": "0x1234", "r2": "0x5678", "r3": "0x9a"}
        "SP:0x012e8a,FP:0x012e72" -> {"SP": "0x012e8a", "FP": "0x012e72"}
        "TOS:0x00000000,N1:0x00000000,N2:0x00000000" -> {"TOS": "0x00000000", "N1": "0x00000000", "N2": "0x00000000"}
    """
    if not field_str or field_str.strip() == "":
        return {}
    
    result = {}
    for item in field_str.split(','):
        item = item.strip()
        if ':' in item:
            key, value = item.split(':', 1)
            result[key.strip()] = value.strip()
    return result


def compare_sub_fields(c_field, zig_field, field_name, verbose=False):
    """Compare two comma-separated sub-fields and return differences."""
    c_sub = parse_sub_fields(c_field, field_name)
    zig_sub = parse_sub_fields(zig_field, field_name)
    
    differences = []
    all_keys = set(c_sub.keys()) | set(zig_sub.keys())
    
    for key in sorted(all_keys):
        c_val = c_sub.get(key, None)
        zig_val = zig_sub.get(key, None)
        
        if c_val != zig_val:
            differences.append({
                "field": f"{field_name}.{key}",
                "c_value": c_val,
                "zig_value": zig_val
            })
        elif verbose:
            differences.append({
                "field": f"{field_name}.{key}",
                "c_value": c_val,
                "zig_value": zig_val,
                "match": True
            })
    
    return differences


def compare_traces(c_file, zig_file, max_lines=None, verbose=False):
    """Compare unified traces between C and Zig emulators."""

    print("=== UNIFIED TRACE COMPARISON ===")
    print(f"C file: {c_file}")
    print(f"Zig file: {zig_file}")
    print()

    try:
        with open(c_file, "r") as f:
            c_lines = f.readlines()
        with open(zig_file, "r") as f:
            zig_lines = f.readlines()
    except FileNotFoundError as e:
        print(f"ERROR: {e}")
        return

    if max_lines:
        c_lines = c_lines[:max_lines]
        zig_lines = zig_lines[:max_lines]

    total_lines = min(len(c_lines), len(zig_lines))
    differences_found = 0

    for i in range(total_lines):
        c_line = c_lines[i].strip()
        zig_line = zig_lines[i].strip()

        c_parsed = parse_trace_line(c_line)
        zig_parsed = parse_trace_line(zig_line)

        if not c_parsed or not zig_parsed:
            print(f"Line {i + 1}: Malformed trace line")
            continue

        # Compare critical fields
        differences = []
        critical_diffs = 0

        # PC (critical)
        if c_parsed["pc"] != zig_parsed["pc"]:
            differences.append(f"PC: C={c_parsed['pc']} Zig={zig_parsed['pc']}")
            critical_diffs += 1

        # Instruction (critical)
        if c_parsed["instruction"] != zig_parsed["instruction"]:
            differences.append(
                f"INSTR: C={c_parsed['instruction']} Zig={zig_parsed['instruction']}"
            )
            critical_diffs += 1

        # Opcode (critical)
        if c_parsed["opcode"] != zig_parsed["opcode"]:
            differences.append(
                f"OPCODE: C={c_parsed['opcode']} Zig={zig_parsed['opcode']}"
            )
            critical_diffs += 1

        # Memory context (critical for memory issues)
        if c_parsed["memory"] != zig_parsed["memory"]:
            differences.append(
                f"MEMORY: C={c_parsed['memory']} Zig={zig_parsed['memory']}"
            )
            critical_diffs += 1

        # File/virtual mapping (critical for paging)
        if c_parsed["mapping"] != zig_parsed["mapping"]:
            differences.append(
                f"MAPPING: C={c_parsed['mapping']} Zig={zig_parsed['mapping']}"
            )
            critical_diffs += 1

        # Stack (important)
        if c_parsed["stack"] != zig_parsed["stack"]:
            differences.append(
                f"STACK: C={c_parsed['stack']} Zig={zig_parsed['stack']}"
            )

        # Operands (may differ in formatting)
        if c_parsed["operands"] != zig_parsed["operands"]:
            differences.append(
                f"OPERANDS: C={c_parsed['operands']} Zig={zig_parsed['operands']}"
            )

        if differences:
            print(
                f"Line {c_parsed['line']}: {len(differences)} differences ({critical_diffs} critical)"
            )
            for diff in differences:
                print(f"  {diff}")
            differences_found += 1

    print()
    print("=== SUMMARY ===")
    print(f"Total lines compared: {total_lines}")
    print(f"Lines with differences: {differences_found}")
    print(f"C trace total lines: {len(c_lines)}")
    print(f"Zig trace total lines: {len(zig_lines)}")

    if len(c_lines) != len(zig_lines):
        print("WARNING: Different number of trace lines!")

    if differences_found == 0:
        print("✅ No differences found - traces match!")
    else:
        print(f"❌ Found {differences_found} lines with differences")


def main():
    parser = argparse.ArgumentParser(description="Compare unified emulator traces with sub-field precision")
    parser.add_argument("c_trace", help="C emulator unified trace file")
    parser.add_argument("zig_trace", help="Zig emulator unified trace file")
    parser.add_argument("--max-lines", type=int, help="Maximum lines to compare")
    parser.add_argument("--verbose", "-v", action="store_true", help="Show all fields including matches")

    args = parser.parse_args()
    compare_traces(args.c_trace, args.zig_trace, args.max_lines, args.verbose)


if __name__ == "__main__":
    main()
