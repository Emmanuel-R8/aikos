#!/usr/bin/awk -f
# Deep Divergence Analysis Script
# Analyzes trace divergences with sub-field precision to identify patterns and root causes
# Usage: awk -f analyze_trace_divergence.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt

BEGIN {
    FS = "|"
    first_divergence_line = -1
    first_divergence_field = ""
    total_divergence_lines = 0
    # Initialize divergence_count as array
    delete divergence_count
    delete divergence_lines
    print "=== DEEP DIVERGENCE ANALYSIS ==="
    print ""
}

# Function to parse comma-separated sub-fields
function parse_sub_fields(field_str, result_arr,    n, i, parts, key_val, key, value) {
    n = split(field_str, parts, ",")
    for (i = 1; i <= n; i++) {
        if (match(parts[i], /^[^:]+:/)) {
            key_val = parts[i]
            sub(/^[ \t]+/, "", key_val)
            sub(/[ \t]+$/, "", key_val)
            if (index(key_val, ":") > 0) {
                key = substr(key_val, 1, index(key_val, ":") - 1)
                value = substr(key_val, index(key_val, ":") + 1)
                result_arr[key] = value
            }
        }
    }
    return n
}

# Function to compare sub-fields and track divergences
function compare_sub_fields(c_arr, zig_arr, field_name, line_num,    key, diff_count, all_keys) {
    diff_count = 0
    for (key in c_arr) all_keys[key] = 1
    for (key in zig_arr) all_keys[key] = 1
    
    for (key in all_keys) {
        c_val = (key in c_arr) ? c_arr[key] : "<missing>"
        zig_val = (key in zig_arr) ? zig_arr[key] : "<missing>"
        if (c_val != zig_val) {
            subfield_name = field_name "." key
            divergence_count[subfield_name]++
            divergence_lines[subfield_name] = divergence_lines[subfield_name] "," line_num
            if (first_divergence_line < 0 || line_num < first_divergence_line) {
                first_divergence_line = line_num
                first_divergence_field = subfield_name
            }
            diff_count++
        }
    }
    return diff_count
}

# Read C emulator trace (first file)
NR == FNR {
    c_line_count++
    c_line[$1] = $0
    c_pc[$1] = $2
    c_registers[$1] = $6
    c_flags[$1] = $7
    c_sp_fp[$1] = $8
    c_stack[$1] = $9
    c_memory[$1] = $10
    c_mapping[$1] = $11
    next
}

# Compare with Zig emulator trace (second file)
{
    line_num = $1 + 0  # Convert to number for comparison
    
    if (!(line_num in c_line)) {
        missing_in_c++
        next
    }

    # Compare critical fields
    has_diff = 0

    # PC (critical)
    if ($2 != c_pc[line_num]) {
        divergence_count["PC"]++
        divergence_lines["PC"] = divergence_lines["PC"] "," line_num
        if (first_divergence_line < 0 || line_num < first_divergence_line) {
            first_divergence_line = line_num
            first_divergence_field = "PC"
        }
        has_diff = 1
    }

    # REGISTERS (sub-field comparison)
    delete c_regs
    delete zig_regs
    parse_sub_fields(c_registers[line_num], c_regs)
    parse_sub_fields($6, zig_regs)
    if (compare_sub_fields(c_regs, zig_regs, "REGISTERS", line_num) > 0) {
        has_diff = 1
    }

    # FLAGS (sub-field comparison)
    delete c_flags_arr
    delete zig_flags_arr
    parse_sub_fields(c_flags[line_num], c_flags_arr)
    parse_sub_fields($7, zig_flags_arr)
    if (compare_sub_fields(c_flags_arr, zig_flags_arr, "FLAGS", line_num) > 0) {
        has_diff = 1
    }

    # SP_FP (sub-field comparison)
    delete c_spfp
    delete zig_spfp
    parse_sub_fields(c_sp_fp[line_num], c_spfp)
    parse_sub_fields($8, zig_spfp)
    if (compare_sub_fields(c_spfp, zig_spfp, "SP_FP", line_num) > 0) {
        has_diff = 1
    }

    # STACK_SUMMARY (sub-field comparison)
    delete c_stack_arr
    delete zig_stack_arr
    parse_sub_fields(c_stack[line_num], c_stack_arr)
    parse_sub_fields($9, zig_stack_arr)
    if (compare_sub_fields(c_stack_arr, zig_stack_arr, "STACK", line_num) > 0) {
        has_diff = 1
    }

    # MEMORY_CONTEXT (sub-field comparison)
    delete c_mem
    delete zig_mem
    parse_sub_fields(c_memory[line_num], c_mem)
    parse_sub_fields($10, zig_mem)
    if (compare_sub_fields(c_mem, zig_mem, "MEMORY", line_num) > 0) {
        has_diff = 1
    }

    # FP_VP_FO_VA (sub-field comparison)
    delete c_map
    delete zig_map
    parse_sub_fields(c_mapping[line_num], c_map)
    parse_sub_fields($11, zig_map)
    if (compare_sub_fields(c_map, zig_map, "MAPPING", line_num) > 0) {
        has_diff = 1
    }

    if (has_diff) {
        total_divergence_lines++
    }

    zig_line_count++
}

END {
    print "=== FIRST DIVERGENCE ==="
    if (first_divergence_line >= 0) {
        print "Line: " first_divergence_line
        print "Field: " first_divergence_field
        print ""
    } else {
        print "No divergences found!"
        print ""
    }

    min_lines = (c_line_count < zig_line_count) ? c_line_count : zig_line_count
    print "=== DIVERGENCE STATISTICS ==="
    print "Total lines compared: " min_lines
    print "Lines with divergences: " total_divergence_lines
    if (total_divergence_lines > 0) {
        divergence_percentage = (total_divergence_lines / min_lines) * 100
        print "Divergence rate: " sprintf("%.2f", divergence_percentage) "%"
    }
    print ""

    if (length(divergence_count) > 0) {
        print "=== FIELD DIVERGENCE HEATMAP ==="
        print "Field | Count | Percentage | First Occurrence"
        print "------|-------|------------|----------------"
        
        # Calculate percentages and find first occurrences
        for (field in divergence_count) {
            count = divergence_count[field]
            percentage = (count / min(c_line_count, zig_line_count)) * 100
            
            # Extract first line number from divergence_lines
            first_line = ""
            if (field in divergence_lines) {
                lines_str = divergence_lines[field]
                sub(/^,/, "", lines_str)  # Remove leading comma
                split(lines_str, lines_arr, ",")
                if (length(lines_arr) > 0 && lines_arr[1] != "") {
                    first_line = lines_arr[1]
                }
            }
            
            printf "%-30s | %5d | %8.2f%% | %s\n", field, count, percentage, first_line
        }
        print ""
    }

    print "=== PATTERN ANALYSIS ==="
    
    # Check for systematic patterns
    if ("STACK.TOS" in divergence_count) {
        tos_divergence_rate = (divergence_count["STACK.TOS"] / min_lines) * 100
        if (tos_divergence_rate > 50) {
            print "⚠️  WARNING: STACK.TOS diverges in " sprintf("%.1f", tos_divergence_rate) "% of lines"
            print "   Likely root cause: Top-of-stack synchronization issue"
        }
    }
    
    if ("SP_FP.SP" in divergence_count || "SP_FP.FP" in divergence_count) {
        spfp_divergence_rate = ((divergence_count["SP_FP.SP"] + divergence_count["SP_FP.FP"]) / min_lines) * 100
        if (spfp_divergence_rate > 30) {
            print "⚠️  WARNING: SP_FP diverges in " sprintf("%.1f", spfp_divergence_rate) "% of lines"
            print "   Likely root cause: Stack/frame pointer initialization or synchronization issue"
        }
    }
    
    if ("MEMORY.vpage" in divergence_count || "MEMORY.off" in divergence_count) {
        mem_divergence_rate = ((divergence_count["MEMORY.vpage"] + divergence_count["MEMORY.off"]) / min_lines) * 100
        if (mem_divergence_rate > 20) {
            print "⚠️  WARNING: MEMORY context diverges in " sprintf("%.1f", mem_divergence_rate) "% of lines"
            print "   Likely root cause: Memory page mapping or address translation issue"
        }
    }

    if (c_line_count != zig_line_count) {
        print ""
        print "⚠️  WARNING: Different number of trace lines!"
        print "   C: " c_line_count " lines"
        print "   Zig: " zig_line_count " lines"
        if (zig_line_count < c_line_count) {
            print "   → Zig emulator stopped early (possible early exit bug)"
        } else {
            print "   → Zig emulator produced more lines (possible loop or extra execution)"
        }
    }

    print ""
    print "=== RECOMMENDATIONS ==="
    if (first_divergence_line >= 0) {
        print "1. Start debugging at line " first_divergence_line " (field: " first_divergence_field ")"
        print "2. Check C reference implementation for this field"
        print "3. Verify Zig implementation matches C behavior"
    }
    
    if (total_divergence_lines == 0) {
        print "✅ Perfect parity achieved!"
    } else {
        print "4. Fix first divergence and re-run comparison from step 0"
        print "5. Iterate until all divergences are resolved"
    }
    
    print ""
    print "Analysis complete."
}
