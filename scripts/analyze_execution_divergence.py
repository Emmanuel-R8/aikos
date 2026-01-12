#!/usr/bin/env python3
"""
Analyze execution divergence between C and Zig emulators
Detailed state-by-state comparison to identify root causes of differences
"""

import re
import sys
from typing import Dict, List, Tuple, Optional

class ExecutionLogParser:
    """Parse execution logs from both emulators"""

    def __init__(self):
        self.instructions = []

    def parse_line(self, line: str) -> Optional[Dict]:
        """Parse a single instruction line"""
        # Remove trailing whitespace
        line = line.rstrip()
        if not line:
            return None

        # Parse instruction count
        match = re.match(r'\s*(\d+)\s+', line)
        if not match:
            return None

        instruction_num = int(match.group(1))

        # Parse PC information
        pc_match = re.search(r'PC:\s+(0x[0-9a-f]+)', line)
        pc = pc_match.group(1) if pc_match else ""

        # Parse opcode bytes (16 hex chars = 8 bytes)
        bytes_match = re.search(r'\s+([0-9a-f]{16})\s+', line)
        opcode_bytes = bytes_match.group(1) if bytes_match else ""

        # Parse opcode name (between bytes and Stack field)
        opcode_match = re.search(r'\s+([A-Z0-9_]+)', line)
        opcode_name = opcode_match.group(1) if opcode_match else ""

        # Parse stack information
        stack_match = re.search(r'Stack:\s+D:\s*(\d+)\s+P:\s*(\d+)\s+TOS:0x([0-9a-f]+)', line)
        if stack_match:
            stack_depth = int(stack_match.group(1))
            stack_ptr = int(stack_match.group(2))
            tos = stack_match.group(3)
        else:
            stack_depth = stack_ptr = 0
            tos = ""

        # Parse frame information
        frame_match = re.search(r'Frame:\s+FX:\s*(\d+)\s+FH:0x([0-9a-f]+)', line)
        if frame_match:
            fx_offset = int(frame_match.group(1))
            fnheader = frame_match.group(2)
        else:
            fx_offset = 0
            fnheader = ""

        return {
            'instruction_num': instruction_num,
            'pc': pc,
            'opcode_bytes': opcode_bytes,
            'opcode_name': opcode_name,
            'stack_depth': stack_depth,
            'stack_ptr': stack_ptr,
            'tos': tos,
            'fx_offset': fx_offset,
            'fnheader': fnheader,
            'raw_line': line
        }

class DivergenceAnalyzer:
    """Analyze differences between C and Zig execution logs"""

    def __init__(self, c_log_file: str, zig_log_file: str):
        self.c_parser = ExecutionLogParser()
        self.zig_parser = ExecutionLogParser()
        self.c_log_file = c_log_file
        self.zig_log_file = zig_log_file

    def load_logs(self):
        """Load and parse both execution logs"""
        print(f"Loading C emulator log: {self.c_log_file}")
        with open(self.c_log_file, 'r') as f:
            for line in f:
                parsed = self.c_parser.parse_line(line)
                if parsed:
                    self.c_parser.instructions.append(parsed)

        print(f"Loading Zig emulator log: {self.zig_log_file}")
        with open(self.zig_log_file, 'r') as f:
            for line in f:
                parsed = self.zig_parser.parse_line(line)
                if parsed:
                    self.zig_parser.instructions.append(parsed)

        print(f"C emulator: {len(self.c_parser.instructions)} instructions")
        print(f"Zig emulator: {len(self.zig_parser.instructions)} instructions")

    def analyze_divergence(self):
        """Analyze where and why the emulators diverge"""
        max_instructions = min(len(self.c_parser.instructions), len(self.zig_parser.instructions))

        print(f"\n=== DIVERGENCE ANALYSIS ===")
        print(f"Analyzing first {max_instructions} instructions\n")

        for i in range(max_instructions):
            c_inst = self.c_parser.instructions[i]
            zig_inst = self.zig_parser.instructions[i]

            # Check for differences
            differences = []

            if c_inst['opcode_name'] != zig_inst['opcode_name']:
                differences.append(f"Opcode: {c_inst['opcode_name']} vs {zig_inst['opcode_name']}")

            if c_inst['opcode_bytes'] != zig_inst['opcode_bytes']:
                differences.append(f"Bytes: {c_inst['opcode_bytes']} vs {zig_inst['opcode_bytes']}")

            if c_inst['pc'] != zig_inst['pc']:
                differences.append(f"PC: {c_inst['pc']} vs {zig_inst['pc']}")

            if c_inst['stack_depth'] != zig_inst['stack_depth']:
                differences.append(f"Stack depth: {c_inst['stack_depth']} vs {zig_inst['stack_depth']}")

            if c_inst['tos'] != zig_inst['tos']:
                differences.append(f"TOS: {c_inst['tos']} vs {zig_inst['tos']}")

            if differences:
                print(f"--- INSTRUCTION {i+1} ---")
                print(f"C:   {c_inst['raw_line'][:120]}...")
                print(f"Zig: {zig_inst['raw_line'][:120]}...")
                print(f"DIFFERENCES: {'; '.join(differences)}")
                print()

                # Show detailed byte comparison for first few differences
                if i < 5:  # Only for first 5 divergences to avoid spam
                    self._show_detailed_comparison(c_inst, zig_inst)
                break  # Stop at first divergence for detailed analysis

    def _show_detailed_comparison(self, c_inst: Dict, zig_inst: Dict):
        """Show detailed comparison of specific instruction"""
        print(f"  DETAILED COMPARISON:")
        print(f"    PC:        C={c_inst['pc']} Zig={zig_inst['pc']}")
        print(f"    Opcode:    C={c_inst['opcode_name']} Zig={zig_inst['opcode_name']}")
        print(f"    Bytes:     C={c_inst['opcode_bytes']}")
        print(f"               Zig={zig_inst['opcode_bytes']}")
        print(f"    Stack:     C=D{c_inst['stack_depth']} P{c_inst['stack_ptr']} TOS={c_inst['tos']}")
        print(f"               Zig=D{zig_inst['stack_depth']} P{zig_inst['stack_ptr']} TOS={zig_inst['tos']}")
        print(f"    Frame:     C=FX{c_inst['fx_offset']} FH={c_inst['fnheader']}")
        print(f"               Zig=FX{zig_inst['fx_offset']} FH={zig_inst['fnheader']}")
        print()

def main():
    if len(sys.argv) != 3:
        print("Usage: python analyze_execution_divergence.py <c_log> <zig_log>")
        sys.exit(1)

    c_log_file = sys.argv[1]
    zig_log_file = sys.argv[2]

    analyzer = DivergenceAnalyzer(c_log_file, zig_log_file)
    analyzer.load_logs()
    analyzer.analyze_divergence()

if __name__ == "__main__":
    main()