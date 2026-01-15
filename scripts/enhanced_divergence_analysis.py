#!/usr/bin/env python3
"""
Enhanced execution divergence analysis
Identifies root causes of emulator differences
"""

import argparse
import re
import sys
from typing import Dict, List, Tuple, Optional

def parse_c_emulator_line(line: str) -> Optional[Dict]:
    """Parse C emulator execution line"""
    line = line.rstrip()
    if not line or not line.strip():
        return None

    # Parse instruction number
    match = re.match(r'\s*(\d+)\s+', line)
    if not match:
        return None
    instruction_num = int(match.group(1))

    # Extract key fields using regex
    patterns = {
        'pc': r'PC:\s+(0x[0-9a-f]+)',
        'opcode_bytes': r'\s+([0-9a-f]{16})\s+',
        'opcode_name': r'\s+([A-Z0-9_]+)\s+',
        'stack_depth': r'Stack:\s+D:\s*(\d+)',
        'tos': r'TOS:0x([0-9a-f]+)',
        'fx_offset': r'FX:\s*(\d+)',
        'fnheader': r'FH:0x([0-9a-f]+)'
    }

    result = {'instruction_num': instruction_num, 'raw_line': line}

    for field, pattern in patterns.items():
        match = re.search(pattern, line)
        if match:
            if field == 'stack_depth':
                result[field] = int(match.group(1))
            else:
                result[field] = match.group(1)
        else:
            result[field] = ""

    return result

def parse_zig_emulator_line(line: str) -> Optional[Dict]:
    """Parse Zig emulator execution line"""
    line = line.rstrip()
    if not line or not line.strip():
        return None

    # Parse instruction number
    match = re.match(r'\s*(\d+)\s+', line)
    if not match:
        return None
    instruction_num = int(match.group(1))

    # Extract key fields using regex
    patterns = {
        'pc': r'PC:\s+(0x[0-9a-f]+)',
        'opcode_bytes': r'\s+([0-9a-f]{16})\s+',
        'opcode_name': r'\s+([A-Z0-9_]+)\s+',
        'stack_depth': r'Stack:\s+D:\s*(\d+)',
        'tos': r'TOS:0x([0-9a-f]+)',
        'fx_offset': r'FX:\s*(\d+)',
        'fnheader': r'FH:0x([0-9a-f]+)'
    }

    result = {'instruction_num': instruction_num, 'raw_line': line}

    for field, pattern in patterns.items():
        match = re.search(pattern, line)
        if match:
            if field == 'stack_depth':
                result[field] = int(match.group(1))
            else:
                result[field] = match.group(1)
        else:
            result[field] = ""

    return result

def analyze_memory_at_pc(pc_address: str, c_inst: Dict, zig_inst: Dict) -> Dict:
    """Analyze what bytes each emulator reads at the same PC for one divergence point."""
    print(f"\n=== MEMORY ANALYSIS AT PC {pc_address} ===")
    print(f"C emulator reads:    {c_inst.get('opcode_bytes', 'N/A')}")
    print(f"Zig emulator reads:  {zig_inst.get('opcode_bytes', 'N/A')}")

    c_bytes = c_inst.get('opcode_bytes', '')
    zig_bytes = zig_inst.get('opcode_bytes', '')

    analysis = {
        'pc': pc_address,
        'c_bytes': c_bytes,
        'zig_bytes': zig_bytes,
        'c_first_byte': c_bytes[:2] if c_bytes else '',
        'zig_first_byte': zig_bytes[:2] if zig_bytes else '',
        'c_opcode_name': c_inst.get('opcode_name', ''),
        'zig_opcode_name': zig_inst.get('opcode_name', ''),
        'identical_bytes': c_bytes == zig_bytes,
        'identical_first_byte': c_bytes[:2] == zig_bytes[:2] if c_bytes and zig_bytes else False
    }

    print(f"C first byte:        0x{analysis['c_first_byte']} = {analysis['c_opcode_name']}")
    print(f"Zig first byte:      0x{analysis['zig_first_byte']} = {analysis['zig_opcode_name']}")
    print(f"Bytes identical:     {analysis['identical_bytes']}")
    print(f"First byte identical: {analysis['identical_first_byte']}")

    return analysis

def main():
    parser = argparse.ArgumentParser(description="Enhanced divergence analysis (LCP + first divergence).")
    parser.add_argument("c_log", help="Path to C emulator log")
    parser.add_argument("zig_log", help="Path to Zig emulator log")
    parser.add_argument(
        "--start-line",
        type=int,
        default=1,
        help="1-based line number to start comparison from (manual resume override)",
    )
    args = parser.parse_args()

    # Load C emulator log
    print(f"Loading C emulator log: {args.c_log}")
    c_instructions = []
    with open(args.c_log, 'r') as f:
        for line in f:
            parsed = parse_c_emulator_line(line)
            if parsed:
                c_instructions.append(parsed)

    # Load Zig emulator log
    print(f"Loading Zig emulator log: {args.zig_log}")
    zig_instructions = []
    with open(args.zig_log, 'r') as f:
        for line in f:
            parsed = parse_zig_emulator_line(line)
            if parsed:
                zig_instructions.append(parsed)

    print(f"C emulator: {len(c_instructions)} instructions")
    print(f"Zig emulator: {len(zig_instructions)} instructions")

    if not c_instructions or not zig_instructions:
        print("ERROR: One or both logs contain no parseable instruction lines")
        return

    start_line = max(1, args.start_line)
    start_idx = start_line - 1
    max_instructions = min(len(c_instructions), len(zig_instructions))
    if start_idx >= max_instructions:
        print("ERROR: start line beyond available overlap of the two logs")
        return

    # Auto LCP
    lcp = 0
    for i in range(start_idx, max_instructions):
        if c_instructions[i]["raw_line"] == zig_instructions[i]["raw_line"]:
            lcp += 1
        else:
            break

    divergence_idx = start_idx + lcp
    print(f"\n=== PREFIX SKIP ===")
    print(f"Start line: {start_line}")
    print(f"Longest common prefix (from start): {lcp} line(s)")

    if divergence_idx >= max_instructions:
        if len(c_instructions) == len(zig_instructions):
            print("\n✅ No divergence found: logs match to completion (same length).")
        else:
            print("\n⚠️ No divergence found in overlap, but logs have different lengths:")
            print(f"  C lines:   {len(c_instructions)}")
            print(f"  Zig lines: {len(zig_instructions)}")
        return

    c_first = c_instructions[divergence_idx]
    zig_first = zig_instructions[divergence_idx]

    print(f"\n=== FIRST DIVERGENCE (after LCP) ===")
    print(f"At log line: {divergence_idx + 1}")
    print(f"C:   {c_first['raw_line'][:140]}...")
    print(f"Zig: {zig_first['raw_line'][:140]}...")

    # Analyze memory at PC
    pc_addr = c_first.get("pc", "0x0")
    memory_analysis = analyze_memory_at_pc(pc_addr, c_first, zig_first)

    print(f"\n=== ROOT CAUSE ANALYSIS ===")

    if not memory_analysis.get("identical_first_byte", False):
        print("CRITICAL: Different bytes read at same PC address.")
        print("Likely causes: memory loading differences, address translation differences, sysout interpretation mismatch.")
    else:
        print("Same first byte at PC, but other fields differ.")
        print("Likely causes: opcode decode/length differences, stack/frame state divergence.")

    # Stack comparison (guard against divide-by-zero)
    c_stack = c_first.get("stack_depth", 0) or 0
    zig_stack = zig_first.get("stack_depth", 0) or 0
    if c_stack:
        print(f"\nStack depth ratio: {zig_stack / c_stack:.2f}x (Zig/C)")
    else:
        print("\nStack depth ratio: N/A (C stack depth not parsed)")

    # Frame header comparison
    c_fh = c_first.get("fnheader", "")
    zig_fh = zig_first.get("fnheader", "")
    if c_fh and zig_fh and c_fh != zig_fh:
        print(f"Frame header mismatch: C={c_fh} Zig={zig_fh}")

if __name__ == "__main__":
    main()