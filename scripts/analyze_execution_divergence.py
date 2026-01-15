#!/usr/bin/env python3
"""
Analyze execution divergence between C and Zig emulators.

Primary use:
- Identify the first divergence between two execution trace logs.
- Support fast iteration by skipping the already-matching prefix (auto LCP).
- Support manual resume by starting comparison at a given line number.
"""

import argparse
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

        # Parse PC information (allow `PC:0x...` and `PC: 0x...`)
        pc_match = re.search(r'PC:\s*(0x[0-9a-f]+)', line)
        pc = pc_match.group(1) if pc_match else ""

        # Parse opcode bytes (16 hex chars = 8 bytes)
        bytes_match = re.search(r'\b([0-9a-f]{16})\b', line)
        opcode_bytes = bytes_match.group(1) if bytes_match else ""

        # Parse opcode name (token immediately following the 16-hex-byte field)
        opcode_name = ""
        if opcode_bytes:
            opcode_match = re.search(rf'\b{opcode_bytes}\b\s+([A-Z0-9_]+)\b', line)
            opcode_name = opcode_match.group(1) if opcode_match else ""

        # Parse stack information
        stack_match = re.search(r'Stack:\s+D:\s*(\d+).*?P:\s*(?:0x)?([0-9a-f]+).*?TOS:0x([0-9a-f]+)', line)
        if stack_match:
            stack_depth = int(stack_match.group(1))
            stack_ptr_raw = stack_match.group(2) or "0"
            base = 16 if re.search(r"[a-f]", stack_ptr_raw) else 10
            stack_ptr = int(stack_ptr_raw, base)
            tos = stack_match.group(3)
        else:
            stack_depth = stack_ptr = 0
            tos = ""

        # Parse frame information
        frame_match = re.search(r'Frame:\s+FX:\s*(\d+).*?FH:0x([0-9a-f]+)', line)
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

    def __init__(self, c_log_file: str, zig_log_file: str, start_line: int = 1):
        self.c_parser = ExecutionLogParser()
        self.zig_parser = ExecutionLogParser()
        self.c_log_file = c_log_file
        self.zig_log_file = zig_log_file
        self.start_line = max(1, start_line)

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
        c_len = len(self.c_parser.instructions)
        z_len = len(self.zig_parser.instructions)
        max_instructions = min(c_len, z_len)

        print(f"\n=== DIVERGENCE ANALYSIS ===")
        print(f"C emulator: {c_len} instructions")
        print(f"Zig emulator: {z_len} instructions")
        print(f"Start line: {self.start_line}\n")

        # Convert 1-based "line" to 0-based index
        start_idx = self.start_line - 1
        if start_idx >= max_instructions:
            print("ERROR: start line beyond available overlap of the two logs")
            return

        # Auto longest-common-prefix (LCP) from the chosen start line
        lcp = 0
        for i in range(start_idx, max_instructions):
            if self.c_parser.instructions[i]["raw_line"] == self.zig_parser.instructions[i]["raw_line"]:
                lcp += 1
            else:
                break

        print(f"Longest common prefix (from start line): {lcp} line(s)\n")

        divergence_idx = start_idx + lcp
        if divergence_idx >= max_instructions:
            # No divergence within the overlap. Determine whether both logs ended together.
            if c_len == z_len:
                print("✅ No divergence found: logs match to completion (same length).")
            else:
                print("⚠️ No divergence found in overlap, but logs have different lengths:")
                print(f"  C lines:   {c_len}")
                print(f"  Zig lines: {z_len}")
                print("  This may indicate early stop or extra output in one emulator.")
            return

        c_inst = self.c_parser.instructions[divergence_idx]
        zig_inst = self.zig_parser.instructions[divergence_idx]

        # Check for differences (field-level)
        differences = []

        if c_inst["opcode_name"] != zig_inst["opcode_name"]:
            differences.append(f"Opcode: {c_inst['opcode_name']} vs {zig_inst['opcode_name']}")

        if c_inst["opcode_bytes"] != zig_inst["opcode_bytes"]:
            differences.append(f"Bytes: {c_inst['opcode_bytes']} vs {zig_inst['opcode_bytes']}")

        if c_inst["pc"] != zig_inst["pc"]:
            differences.append(f"PC: {c_inst['pc']} vs {zig_inst['pc']}")

        if c_inst["stack_depth"] != zig_inst["stack_depth"]:
            differences.append(f"Stack depth: {c_inst['stack_depth']} vs {zig_inst['stack_depth']}")

        if c_inst["tos"] != zig_inst["tos"]:
            differences.append(f"TOS: {c_inst['tos']} vs {zig_inst['tos']}")

        print(f"--- FIRST DIVERGENCE ---")
        print(f"At log line: {divergence_idx + 1}")
        print(f"C:   {c_inst['raw_line'][:140]}...")
        print(f"Zig: {zig_inst['raw_line'][:140]}...")
        print(f"DIFFERENCES: {'; '.join(differences) if differences else '(unable to parse differences)'}")
        print()

        self._show_detailed_comparison(c_inst, zig_inst)

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
    parser = argparse.ArgumentParser(description="Find first divergence between C and Zig execution logs.")
    parser.add_argument("c_log", help="Path to C emulator log")
    parser.add_argument("zig_log", help="Path to Zig emulator log")
    parser.add_argument(
        "--start-line",
        type=int,
        default=1,
        help="1-based line number to start comparison from (manual resume override)",
    )
    args = parser.parse_args()

    analyzer = DivergenceAnalyzer(args.c_log, args.zig_log, start_line=args.start_line)
    analyzer.load_logs()
    analyzer.analyze_divergence()

if __name__ == "__main__":
    main()