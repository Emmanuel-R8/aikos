#!/usr/bin/env python3
"""
Visual Diff Tool for Trace Comparison
Generates HTML report with side-by-side comparison and color-coded differences
Usage: python3 visual_diff.py c_trace.txt zig_trace.txt [output.html]
"""

import sys
import argparse
from html import escape


def parse_trace_line(line):
    """Parse a unified trace line into fields."""
    fields = line.strip().split("|")
    if len(fields) != 14:
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


def parse_sub_fields(field_str):
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


def compare_sub_fields(c_field, zig_field):
    """Compare two comma-separated sub-fields and return differences."""
    c_sub = parse_sub_fields(c_field)
    zig_sub = parse_sub_fields(zig_field)
    
    differences = {}
    all_keys = set(c_sub.keys()) | set(zig_sub.keys())
    
    for key in sorted(all_keys):
        c_val = c_sub.get(key, None)
        zig_val = zig_sub.get(key, None)
        if c_val != zig_val:
            differences[key] = {"c": c_val, "zig": zig_val}
    
    return differences


def generate_html_report(c_file, zig_file, output_file, max_lines=None):
    """Generate HTML comparison report."""
    
    with open(c_file, "r") as f:
        c_lines = f.readlines()
    with open(zig_file, "r") as f:
        zig_lines = f.readlines()
    
    if max_lines:
        c_lines = c_lines[:max_lines]
        zig_lines = zig_lines[:max_lines]
    
    total_lines = min(len(c_lines), len(zig_lines))
    differences_found = 0
    
    html = []
    html.append("""<!DOCTYPE html>
<html>
<head>
    <title>Trace Comparison Report</title>
    <style>
        body { font-family: monospace; margin: 20px; background: #1e1e1e; color: #d4d4d4; }
        h1 { color: #4ec9b0; }
        h2 { color: #569cd6; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th { background: #2d2d30; color: #cccccc; padding: 8px; text-align: left; border: 1px solid #3e3e42; }
        td { padding: 4px 8px; border: 1px solid #3e3e42; }
        .match { background: #1e3a1e; }
        .diff { background: #3a1e1e; }
        .sub-diff { color: #f48771; font-weight: bold; }
        .match-field { color: #89d185; }
        .line-num { color: #858585; }
        .summary { margin: 20px 0; padding: 10px; background: #252526; border: 1px solid #3e3e42; }
    </style>
</head>
<body>
    <h1>Trace Comparison Report</h1>
    <div class="summary">
        <p><strong>C Trace:</strong> %s</p>
        <p><strong>Zig Trace:</strong> %s</p>
        <p><strong>Lines Compared:</strong> %d</p>
    </div>
    <table>
        <tr>
            <th>Line</th>
            <th>Field</th>
            <th>C Value</th>
            <th>Zig Value</th>
            <th>Status</th>
        </tr>
""" % (escape(c_file), escape(zig_file), total_lines))
    
    for i in range(total_lines):
        c_line = c_lines[i].strip()
        zig_line = zig_lines[i].strip()
        
        c_parsed = parse_trace_line(c_line)
        zig_parsed = parse_trace_line(zig_line)
        
        if not c_parsed or not zig_parsed:
            continue
        
        line_num = c_parsed["line"]
        row_class = "match"
        has_diff = False
        
        # Compare fields
        fields_to_check = [
            ("PC", "pc"),
            ("INSTRUCTION", "instruction"),
            ("OPCODE", "opcode"),
        ]
        
        # Check sub-fields
        sub_fields_to_check = [
            ("REGISTERS", "registers"),
            ("FLAGS", "flags"),
            ("SP_FP", "sp_fp"),
            ("STACK", "stack"),
            ("MEMORY", "memory"),
            ("MAPPING", "mapping"),
            ("BS_MEM", "byteswap"),
        ]
        
        for field_name, field_key in fields_to_check:
            c_val = c_parsed[field_key]
            zig_val = zig_parsed[field_key]
            if c_val != zig_val:
                html.append(f"""
        <tr class="diff">
            <td class="line-num">{escape(line_num)}</td>
            <td>{escape(field_name)}</td>
            <td>{escape(c_val)}</td>
            <td>{escape(zig_val)}</td>
            <td class="sub-diff">❌ DIFFER</td>
        </tr>""")
                has_diff = True
                differences_found += 1
        
        for field_name, field_key in sub_fields_to_check:
            c_field = c_parsed[field_key]
            zig_field = zig_parsed[field_key]
            sub_diffs = compare_sub_fields(c_field, zig_field)
            
            if sub_diffs:
                for sub_key, vals in sub_diffs.items():
                    html.append(f"""
        <tr class="diff">
            <td class="line-num">{escape(line_num)}</td>
            <td>{escape(field_name)}.{escape(sub_key)}</td>
            <td>{escape(vals['c'] or '<missing>')}</td>
            <td>{escape(vals['zig'] or '<missing>')}</td>
            <td class="sub-diff">❌ DIFFER</td>
        </tr>""")
                    has_diff = True
                    differences_found += 1
        
        if has_diff:
            row_class = "diff"
    
    html.append("""
    </table>
    <div class="summary">
        <h2>Summary</h2>
        <p><strong>Total Differences:</strong> %d</p>
        <p><strong>Parity Status:</strong> %s</p>
    </div>
</body>
</html>
""" % (differences_found, "✅ PASS" if differences_found == 0 else "❌ FAIL"))
    
    with open(output_file, "w") as f:
        f.write("".join(html))
    
    print(f"HTML report generated: {output_file}")
    print(f"Total differences: {differences_found}")


def main():
    parser = argparse.ArgumentParser(description="Generate HTML visual diff report")
    parser.add_argument("c_trace", help="C emulator unified trace file")
    parser.add_argument("zig_trace", help="Zig emulator unified trace file")
    parser.add_argument("output", nargs="?", default="trace_comparison.html", help="Output HTML file")
    parser.add_argument("--max-lines", type=int, help="Maximum lines to compare")
    
    args = parser.parse_args()
    generate_html_report(args.c_trace, args.zig_trace, args.output, args.max_lines)


if __name__ == "__main__":
    main()
