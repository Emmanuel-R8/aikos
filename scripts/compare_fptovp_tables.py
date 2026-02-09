#!/usr/bin/env python3
import re

def parse_log(filename):
    mappings = {}
    with open(filename) as f:
        for line in f:
            match = re.search(r'FPtoVP\[(\d+)\] = file_page \d+ -> virtual_page (\d+)', line)
            if match:
                idx, vpage = int(match.group(1)), int(match.group(2))
                mappings[idx] = vpage
    return mappings

c_mappings = parse_log('c_fptovp_log.txt')
zig_mappings = parse_log('zig_fptovp_log.txt')

print("First 20 FPtoVP mappings:")
for i in range(min(20, max(len(c_mappings), len(zig_mappings)))):
    c_val = c_mappings.get(i, 'MISSING')
    z_val = zig_mappings.get(i, 'MISSING')
    status = "MATCH" if c_val == z_val else "DIFFER"
    print(f"[{i}] C:{c_val} Z:{z_val} {status}")

# Find first difference
for i in range(max(len(c_mappings), len(zig_mappings))):
    c_val = c_mappings.get(i)
    z_val = zig_mappings.get(i)
    if c_val != z_val:
        print(f"\nFirst difference at index {i}: C={c_val}, Z={z_val}")
        break
