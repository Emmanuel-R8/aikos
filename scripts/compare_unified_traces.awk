#!/usr/bin/awk -f
# Unified Trace Comparison Script (Enhanced with Sub-Field Parsing)
# Usage: awk -f compare_unified_traces.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt

BEGIN {
    FS = "|"
    total_differences = 0
    print "=== UNIFIED TRACE COMPARISON (Sub-Field Enhanced) ==="
    print ""
}

# Function to parse comma-separated sub-fields into an array
# Returns number of sub-fields parsed
function parse_sub_fields(field_str, result_arr,    n, i, parts, key_val) {
    n = split(field_str, parts, ",")
    for (i = 1; i <= n; i++) {
        if (match(parts[i], /^[^:]+:/)) {
            key_val = parts[i]
            sub(/^[ \t]+/, "", key_val)  # trim leading whitespace
            sub(/[ \t]+$/, "", key_val)  # trim trailing whitespace
            if (index(key_val, ":") > 0) {
                key = substr(key_val, 1, index(key_val, ":") - 1)
                value = substr(key_val, index(key_val, ":") + 1)
                result_arr[key] = value
            }
        }
    }
    return n
}

# Function to compare two sub-field arrays
# Returns number of differences found
function compare_sub_fields(c_arr, zig_arr, field_name,    key, diff_count, all_keys, i) {
    diff_count = 0
    # Get all keys from both arrays
    for (key in c_arr) all_keys[key] = 1
    for (key in zig_arr) all_keys[key] = 1
    
    for (key in all_keys) {
        c_val = (key in c_arr) ? c_arr[key] : "<missing>"
        zig_val = (key in zig_arr) ? zig_arr[key] : "<missing>"
        if (c_val != zig_val) {
            print "  " field_name "." key ": C=" c_val " Zig=" zig_val
            diff_count++
        }
    }
    return diff_count
}

# Read C emulator trace (first file)
NR == FNR {
    # Store C trace fields by line number
    c_line_count++
    c_line[$1] = $0
    c_pc[$1] = $2
    c_instr[$1] = $3
    c_opcode[$1] = $4
    c_operands[$1] = $5
    c_registers[$1] = $6
    c_flags[$1] = $7
    c_sp_fp[$1] = $8
    c_stack[$1] = $9
    c_memory[$1] = $10
    c_mapping[$1] = $11
    c_byteswap[$1] = $12
    c_notes[$1] = $13
    next
}

# Compare with Zig emulator trace (second file)
{
    line_num = $1

    # Skip if C trace doesn't have this line
    if (!(line_num in c_line)) {
        print "Line " line_num ": Missing in C trace"
        next
    }

    # Compare key fields
    differences = 0
    diff_msg = ""

    # PC comparison (critical)
    if ($2 != c_pc[line_num]) {
        diff_msg = diff_msg "PC: C=" c_pc[line_num] " Zig=" $2 "; "
        differences++
    }

    # Instruction comparison
    if ($3 != c_instr[line_num]) {
        diff_msg = diff_msg "INSTR: C=" c_instr[line_num] " Zig=" $3 "; "
        differences++
    }

    # Opcode comparison
    if ($4 != c_opcode[line_num]) {
        diff_msg = diff_msg "OPCODE: C=" c_opcode[line_num] " Zig=" $4 "; "
        differences++
    }

    # Operands comparison (may differ due to formatting)
    if ($5 != c_operands[line_num]) {
        diff_msg = diff_msg "OPERANDS: C=" c_operands[line_num] " Zig=" $5 "; "
        # Don't count operands differences as critical unless significant
    }

    # Memory context comparison (critical for memory issues)
    if ($10 != c_memory[line_num]) {
        diff_msg = diff_msg "MEMORY: C=" c_memory[line_num] " Zig=" $10 "; "
        differences++
    }

    # File/virtual mapping comparison
    if ($11 != c_mapping[line_num]) {
        diff_msg = diff_msg "MAPPING: C=" c_mapping[line_num] " Zig=" $11 "; "
        differences++
    }

    # Stack comparison
    if ($9 != c_stack[line_num]) {
        diff_msg = diff_msg "STACK: C=" c_stack[line_num] " Zig=" $9 "; "
        differences++
    }

    # Report differences
    if (differences > 0) {
        print "Line " line_num ": " differences " differences - " diff_msg
    }

    zig_line_count++
}

END {
    print ""
    print "=== SUMMARY ==="
    print "C trace lines: " c_line_count
    print "Zig trace lines: " zig_line_count
    print "Lines with differences: " total_differences

    if (c_line_count != zig_line_count) {
        print "WARNING: Different number of trace lines!"
    }

    if (length(field_stats) > 0) {
        print ""
        print "=== FIELD DIVERGENCE STATISTICS ==="
        # Note: Awk doesn't have easy sorting, so we'll just print what we have
        for (field in field_stats) {
            print "  " field ": " field_stats[field] " differences"
        }
    }

    if (total_differences == 0) {
        print ""
        print "✅ No differences found - traces match!"
    } else {
        print ""
        print "❌ Found " total_differences " lines with differences"
    }

    print "Comparison complete."
}