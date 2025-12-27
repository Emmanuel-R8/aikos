#!/usr/bin/env python3
"""
Enhanced execution divergence analysis
Identifies root causes of emulator differences
"""

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

def analyze_memory_at_pc(pc_address: str, c_instructions: List[Dict], zig_instructions: List[Dict]) -> Dict:
    """Analyze what bytes each emulator reads at the same PC"""
    if not c_instructions or not zig_instructions:
        return {}

    c_first = c_instructions[0]
    zig_first = zig_instructions[0]

    print(f"\n=== MEMORY ANALYSIS AT PC {pc_address} ===")
    print(f"C emulator reads:    {c_first.get('opcode_bytes', 'N/A')}")
    print(f"Zig emulator reads:  {zig_first.get('opcode_bytes', 'N/A')}")

    # Decode bytes to see what they represent
    c_bytes = c_first.get('opcode_bytes', '')
    zig_bytes = zig_first.get('opcode_bytes', '')

    analysis = {
        'pc': pc_address,
        'c_bytes': c_bytes,
        'zig_bytes': zig_bytes,
        'c_first_byte': c_bytes[:2] if c_bytes else '',
        'zig_first_byte': zig_bytes[:2] if zig_bytes else '',
        'c_opcode_name': c_first.get('opcode_name', ''),
        'zig_opcode_name': zig_first.get('opcode_name', ''),
        'identical_bytes': c_bytes == zig_bytes,
        'identical_first_byte': c_bytes[:2] == zig_bytes[:2] if c_bytes and zig_bytes else False
    }

    print(f"C first byte:        0x{analysis['c_first_byte']} = {analysis['c_opcode_name']}")
    print(f"Zig first byte:      0x{analysis['zig_first_byte']} = {analysis['zig_opcode_name']}")
    print(f"Bytes identical:     {analysis['identical_bytes']}")
    print(f"First byte identical: {analysis['identical_first_byte']}")

    return analysis

def main():
    if len(sys.argv) != 3:
        print("Usage: python enhanced_divergence_analysis.py <c_log> <zig_log>")
        sys.exit(1)

    c_log_file = sys.argv[1]
    zig_log_file = sys.argv[2]

    # Load C emulator log
    print(f"Loading C emulator log: {c_log_file}")
    c_instructions = []
    with open(c_log_file, 'r') as f:
        for line in f:
            parsed = parse_c_emulator_line(line)
            if parsed:
                c_instructions.append(parsed)

    # Load Zig emulator log
    print(f"Loading Zig emulator log: {zig_log_file}")
    zig_instructions = []
    with open(zig_log_file, 'r') as f:
        for line in f:
            parsed = parse_zig_emulator_line(line)
            if parsed:
                zig_instructions.append(parsed)

    print(f"C emulator: {len(c_instructions)} instructions")
    print(f"Zig emulator: {len(zig_instructions)} instructions")

    # Analyze first instruction divergence
    if c_instructions and zig_instructions:
        c_first = c_instructions[0]
        zig_first = zig_instructions[0]

        print(f"\n=== FIRST INSTRUCTION COMPARISON ===")
        print(f"C:   {c_first['raw_line'][:100]}...")
        print(f"Zig: {zig_first['raw_line'][:100]}...")

        # Analyze memory at PC
        pc_addr = c_first.get('pc', '0x0')
        memory_analysis = analyze_memory_at_pc(pc_addr, c_instructions, zig_instructions)

        print(f"\n=== ROOT CAUSE ANALYSIS ===")

        if not memory_analysis['identical_first_byte']:
            print("🚨 CRITICAL: Different bytes read at same PC address!")
            print("   This indicates:")
            print("   1. Memory loading differences between emulators")
            print("   2. XOR addressing implementation differences")
            print("   3. Virtual memory mapping issues")
            print("   4. Different sysout file interpretation")
        else:
            print("✅ Same bytes read, but different opcode interpretation")
            print("   This indicates:")
            print("   1. Opcode decoding differences")
            print("   2. Instruction length calculation issues")

        # Stack comparison
        c_stack = c_first.get('stack_depth', 0)
        zig_stack = zig_first.get('stack_depth', 0)
        print(f"\nStack depth ratio: {zig_stack / c_stack:.2f}x (Zig/C)")
        if abs(zig_stack - c_stack) > 1000:
            print("🚨 Large stack difference suggests different initialization")

        # Frame header comparison
        c_fh = c_first.get('fnheader', '')
        zig_fh = zig_first.get('fnheader', '')
        if c_fh and zig_fh and c_fh != zig_fh:
            print(f"Frame header mismatch: C={c_fh} Zig={zig_fh}")
            print("🚨 Different function headers suggest different code loaded")

if __name__ == "__main__":
    main()