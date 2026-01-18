#!/usr/bin/awk -f
# Unified Trace Comparison Script
# Usage: awk -f compare_unified_traces.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt

BEGIN {
    FS = "|"
    print "=== UNIFIED TRACE COMPARISON ==="
    print ""
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

    if (c_line_count != zig_line_count) {
        print "WARNING: Different number of trace lines!"
    }

    print "Comparison complete."
}