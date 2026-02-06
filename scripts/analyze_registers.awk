#!/usr/bin/awk -f
# Register Analysis Script
# Analyzes register values (r1, r2, r3) across traces
# Usage: awk -f analyze_registers.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt

BEGIN {
    FS = "|"
    print "=== REGISTER ANALYSIS ==="
    print ""
}

# Function to parse registers field
function parse_registers(reg_field, regs_arr,    parts, i, key_val, key, value) {
    n = split(reg_field, parts, ",")
    for (i = 1; i <= n; i++) {
        if (match(parts[i], /^[^:]+:/)) {
            key_val = parts[i]
            sub(/^[ \t]+/, "", key_val)
            sub(/[ \t]+$/, "", key_val)
            if (index(key_val, ":") > 0) {
                key = substr(key_val, 1, index(key_val, ":") - 1)
                value = substr(key_val, index(key_val, ":") + 1)
                regs_arr[key] = value
            }
        }
    }
}

# Read C emulator trace (first file)
NR == FNR {
    delete c_regs
    parse_registers($6, c_regs)
    c_r1[$1] = c_regs["r1"]
    c_r2[$1] = c_regs["r2"]
    c_r3[$1] = c_regs["r3"]
    next
}

# Compare with Zig emulator trace (second file)
{
    line_num = $1
    
    delete zig_regs
    parse_registers($6, zig_regs)
    zig_r1 = zig_regs["r1"]
    zig_r2 = zig_regs["r2"]
    zig_r3 = zig_regs["r3"]
    
    if (!(line_num in c_r1)) {
        missing_in_c++
        next
    }
    
    has_diff = 0
    if (c_r1[line_num] != zig_r1) {
        print "Line " line_num ": r1 differs - C=" c_r1[line_num] " Zig=" zig_r1
        r1_differences++
        has_diff = 1
    }
    if (c_r2[line_num] != zig_r2) {
        print "Line " line_num ": r2 differs - C=" c_r2[line_num] " Zig=" zig_r2
        r2_differences++
        has_diff = 1
    }
    if (c_r3[line_num] != zig_r3) {
        print "Line " line_num ": r3 differs - C=" c_r3[line_num] " Zig=" zig_r3
        r3_differences++
        has_diff = 1
    }
    
    if (has_diff) {
        register_differences++
    }
    
    zig_line_count++
}

END {
    min_lines = (FNR < (NR - FNR)) ? FNR : (NR - FNR)
    print ""
    print "=== SUMMARY ==="
    print "Total lines compared: " min_lines
    print "Register differences: " register_differences
    print "  r1 differences: " r1_differences
    print "  r2 differences: " r2_differences
    print "  r3 differences: " r3_differences
    
    if (register_differences == 0) {
        print ""
        print "✅ Register values match perfectly!"
    } else {
        print ""
        print "=== ANALYSIS ==="
        if (r1_differences > 0) {
            print "⚠️  r1 (PC low 16 bits) differs - check PC calculation"
        }
        if (r2_differences > 0) {
            print "⚠️  r2 (TOS low 16 bits) differs - check TOS synchronization"
        }
        if (r3_differences > 0) {
            print "⚠️  r3 (TOS high 8 bits) differs - check TOS synchronization"
        }
    }
}
