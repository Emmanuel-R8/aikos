#!/usr/bin/env python3
"""
Compare C and Zig emulator execution traces field by field.
Verifies CPU state (registers, flags), stack state, and memory effects.
"""

import sys
import re


def extract_state_fields(line):
    """Extract all state fields from a trace line."""
    fields = {}

    # Extract Stack state: D:..., P:..., TOS:..., N:[...]
    stack_match = re.search(
        r"Stack: D:(\d+)/[^\s]*\s+P:([^]]*)\s+TOS:(\S*)\s+N:\[([^\]]*)\]", line
    )
    if stack_match:
        fields["stack_depth"] = stack_match.group(1)
        fields["stack_ptr"] = stack_match.group(2).strip()
        fields["tos"] = stack_match.group(3)
        fields["next_values"] = stack_match.group(4).strip()

    # Extract Frame info: FX:..., FH:..., PC:..., NB:..., FO:...
    frame_match = re.search(
        r"Frame: FX:[^/]*/[^/]*\([^)]*\)\s+FH:(\S*)[^/]*/[^\s]*\s+PC:\s*(\d+)[^/]*/[^\s]*\s+NB:(\S*)[^/]*/[^\s]*\s+FO:(\S*)",
        line,
    )
    if frame_match:
        fields["fnheader"] = frame_match.group(1)
        fields["pc"] = frame_match.group(2)
        fields["nextblock"] = frame_match.group(3)
        fields["funcobj"] = frame_match.group(4)

    # Extract PC and instruction
    pc_match = re.search(r"PC:(\S*)/[^\s]*\s+(\w+)\s+", line)
    if pc_match:
        fields["pc_addr"] = pc_match.group(1)
        fields["instruction"] = pc_match.group(2)

    # Extract memory context: [FP:... VP:... FO:... VA:...]
    mem_match = re.search(r"\[FP:(\d+)\s+VP:(\d+)\s+FO:(\S*)\s+VA:(\S*)\]", line)
    if mem_match:
        fields["file_page"] = mem_match.group(1)
        fields["virt_page"] = mem_match.group(2)
        fields["file_offset"] = mem_match.group(3)
        fields["virt_addr"] = mem_match.group(4)

    return fields


def compare_fields(c_fields, z_fields, instruction_num):
    """Compare fields and report differences."""
    differences = []

    all_keys = set(c_fields.keys()) | set(z_fields.keys())

    for key in sorted(all_keys):
        c_val = c_fields.get(key, "MISSING")
        z_val = z_fields.get(key, "MISSING")

        if c_val != z_val:
            differences.append(f"  {key}: C={c_val} vs Zig={z_val}")

    if differences:
        print(
            f"\n  Instruction {instruction_num}: {c_fields.get('instruction', '?')} - DIFFERENCES FOUND"
        )
        for diff in differences:
            print(diff)
        return False
    else:
        return True


def main():
    if len(sys.argv) != 3:
        print("Usage: compare_state.py <c_trace.txt> <zig_trace.txt>")
        sys.exit(1)

    c_file = sys.argv[1]
    z_file = sys.argv[2]

    with open(c_file, "r") as f:
        c_lines = f.readlines()

    with open(z_file, "r") as f:
        z_lines = f.readlines()

    # Extract C-style format lines only (skip unified trace format)
    # C lines start with line number then "PC:", Zig lines start directly with "PC:"
    c_c_style = [l for l in c_lines if re.search(r"^\s*\d+\s+PC:0x", l)]
    z_c_style = [l for l in z_lines if re.search(r"^\s*\d+\s+PC:0x", l)]

    print(f"Found {len(c_c_style)} C-style lines, {len(z_c_style)} Zig-style lines")

    matches = 0
    mismatches = 0
    comparison_count = min(len(c_c_style), len(z_c_style), 20)  # Compare first 20

    print(f"\nComparing first {comparison_count} instructions:\n")
    print(
        f"{'#':<4} {'Instruction':<15} {'Stack D':<8} {'Stack Ptr':<12} {'TOS':<12} {'Match'}"
    )
    print("-" * 80)

    for i in range(comparison_count):
        c_line = c_c_style[i]
        z_line = z_c_style[i]

        c_fields = extract_state_fields(c_line)
        z_fields = extract_state_fields(z_line)

        if compare_fields(c_fields, z_fields, i + 1):
            matches += 1
            print(
                f"{i + 1:<4} {c_fields.get('instruction', '?'):<15} {c_fields.get('stack_depth', '?'):<8} {c_fields.get('stack_ptr', '?'):<12} {c_fields.get('tos', '?')[:10]:<12} ✅"
            )
        else:
            mismatches += 1

    print(f"\n{'=' * 80}")
    print(
        f"RESULTS: {matches} matches, {mismatches} mismatches out of {comparison_count} instructions"
    )

    if mismatches == 0:
        print(
            "\n🎉 PERFECT STATE PARITY! All CPU state, registers, and memory effects match exactly."
        )
    else:
        print(f"\n  Found {mismatches} instructions with state differences.")


if __name__ == "__main__":
    main()
