#!/usr/bin/awk -f
# TOS (Top of Stack) Analysis Script
# Analyzes TOS values across traces to identify patterns
# Usage: awk -f analyze_tos.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt

BEGIN {
    FS = "|"
    print "=== TOS (Top of Stack) ANALYSIS ==="
    print ""
}

# Function to extract TOS value from STACK field
function extract_tos(stack_field,    parts, i, key_val) {
    n = split(stack_field, parts, ",")
    for (i = 1; i <= n; i++) {
        if (match(parts[i], /^TOS:/)) {
            sub(/^TOS:/, "", parts[i])
            return parts[i]
        }
    }
    return "<not found>"
}

# Read C emulator trace (first file)
NR == FNR {
    c_tos[$1] = extract_tos($9)
    c_line[$1] = $0
    next
}

# Compare with Zig emulator trace (second file)
{
    line_num = $1
    zig_tos = extract_tos($9)
    
    if (!(line_num in c_tos)) {
        missing_in_c++
        next
    }
    
    if (c_tos[line_num] != zig_tos) {
        print "Line " line_num ": TOS differs"
        print "  C:   " c_tos[line_num]
        print "  Zig: " zig_tos
        print ""
        tos_differences++
    }
    
    # Track TOS value distribution
    tos_values_c[c_tos[line_num]]++
    tos_values_zig[zig_tos]++
    
    zig_line_count++
}

END {
    min_lines = (FNR < (NR - FNR)) ? FNR : (NR - FNR)
    print "=== SUMMARY ==="
    print "Total lines compared: " min_lines
    print "TOS differences: " tos_differences
    
    if (tos_differences > 0) {
        print ""
        print "=== TOS VALUE DISTRIBUTION (C) ==="
        for (tos in tos_values_c) {
            if (tos_values_c[tos] > 1) {
                print "  " tos ": " tos_values_c[tos] " occurrences"
            }
        }
        
        print ""
        print "=== TOS VALUE DISTRIBUTION (Zig) ==="
        for (tos in tos_values_zig) {
            if (tos_values_zig[tos] > 1) {
                print "  " tos ": " tos_values_zig[tos] " occurrences"
            }
        }
    }
    
    if (tos_differences == 0) {
        print ""
        print "✅ TOS values match perfectly!"
    }
}
