#!/usr/bin/awk -f
# Memory Context Analysis Script
# Analyzes memory context (vpage, offset) across traces
# Usage: awk -f analyze_memory.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt

BEGIN {
    FS = "|"
    print "=== MEMORY CONTEXT ANALYSIS ==="
    print ""
}

# Function to parse memory context field
function parse_memory(mem_field,    parts, i, key_val, key, value, result) {
    n = split(mem_field, parts, ",")
    for (i = 1; i <= n; i++) {
        if (match(parts[i], /^[^:]+:/)) {
            key_val = parts[i]
            sub(/^[ \t]+/, "", key_val)
            sub(/[ \t]+$/, "", key_val)
            if (index(key_val, ":") > 0) {
                key = substr(key_val, 1, index(key_val, ":") - 1)
                value = substr(key_val, index(key_val, ":") + 1)
                result[key] = value
            }
        }
    }
    return result
}

# Read C emulator trace (first file)
NR == FNR {
    delete c_mem
    parse_memory($10, c_mem)
    c_vpage[$1] = c_mem["vpage"]
    c_offset[$1] = c_mem["off"]
    c_pc[$1] = $2
    next
}

# Compare with Zig emulator trace (second file)
{
    line_num = $1
    
    delete zig_mem
    parse_memory($10, zig_mem)
    zig_vpage = zig_mem["vpage"]
    zig_offset = zig_mem["off"]
    zig_pc = $2
    
    if (!(line_num in c_vpage)) {
        missing_in_c++
        next
    }
    
    has_diff = 0
    if (c_vpage[line_num] != zig_vpage) {
        print "Line " line_num ": vpage differs - C=" c_vpage[line_num] " Zig=" zig_vpage " (PC=" zig_pc ")"
        vpage_differences++
        has_diff = 1
    }
    if (c_offset[line_num] != zig_offset) {
        print "Line " line_num ": offset differs - C=" c_offset[line_num] " Zig=" zig_offset " (PC=" zig_pc ")"
        offset_differences++
        has_diff = 1
    }
    
    if (has_diff) {
        memory_differences++
    }
    
    zig_line_count++
}

END {
    min_lines = (FNR < (NR - FNR)) ? FNR : (NR - FNR)
    print ""
    print "=== SUMMARY ==="
    print "Total lines compared: " min_lines
    print "Memory context differences: " memory_differences
    print "  vpage differences: " vpage_differences
    print "  offset differences: " offset_differences
    
    if (memory_differences == 0) {
        print ""
        print "✅ Memory context matches perfectly!"
    } else {
        print ""
        print "=== ANALYSIS ==="
        if (vpage_differences > 0) {
            print "⚠️  Virtual page calculation differs - check PC to vpage conversion"
            print "   Formula: vpage = PC_byte_offset / 512"
        }
        if (offset_differences > 0) {
            print "⚠️  Page offset calculation differs - check PC to offset conversion"
            print "   Formula: offset = PC_byte_offset % 512"
        }
    }
}
