#!/usr/bin/awk -f
# Example Awk script demonstrating parsing of the new comma-separated sub-field format
# Usage: awk -f parse_trace_fields.awk trace_file.txt

BEGIN {
    FS = "|"
    print "=== Trace Field Parser Example ==="
    print ""
}

{
    # Extract major fields (pipe-delimited)
    line_num = $1
    pc = $2
    instruction = $3
    opcode = $4
    
    # Parse REGISTERS field (comma-separated sub-fields)
    split($6, regs, ",")
    for (i in regs) {
        split(regs[i], r, ":")
        if (r[1] == "r1") r1_val = r[2]
        if (r[1] == "r2") r2_val = r[2]
        if (r[1] == "r3") r3_val = r[2]
    }
    
    # Parse FLAGS field (comma-separated sub-fields)
    split($7, flags, ",")
    for (i in flags) {
        split(flags[i], f, ":")
        if (f[1] == "Z") z_flag = f[2]
        if (f[1] == "N") n_flag = f[2]
        if (f[1] == "C") c_flag = f[2]
    }
    
    # Parse SP_FP field (comma-separated sub-fields)
    split($8, spfp, ",")
    split(spfp[1], sp, ":")
    split(spfp[2], fp, ":")
    
    # Parse STACK_SUMMARY field (comma-separated sub-fields)
    split($9, stack, ",")
    split(stack[1], tos, ":")
    
    # Parse MEMORY_CONTEXT field (comma-separated sub-fields)
    split($10, mem, ",")
    split(mem[2], vpage, ":")
    split(mem[3], off, ":")
    
    # Print parsed fields (example: first 3 lines only)
    if (NR <= 3) {
        print "Line " line_num ":"
        print "  PC: " pc
        print "  Instruction: " instruction
        print "  Opcode: " opcode
        print "  Registers: r1=" r1_val ", r2=" r2_val ", r3=" r3_val
        print "  Flags: Z=" z_flag ", N=" n_flag ", C=" c_flag
        print "  SP: " sp[2] ", FP: " fp[2]
        print "  TOS: " tos[2]
        print "  Memory: vpage=" vpage[2] ", offset=" off[2]
        print ""
    }
}

END {
    print "Parsed " NR " trace lines"
}
