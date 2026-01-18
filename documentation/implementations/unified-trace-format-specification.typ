# Unified Single-Line Trace Format Specification

**Date**: 2026-01-17 17:45
**Status**: Design Specification
**Purpose**: Unified column-formatted trace for rapid C/Zig emulator comparison

## Overview

This specification defines a unified single-line trace format that consolidates all essential VM state information into a single, column-formatted line. This enables rapid divergence identification using tools like awk while maintaining comprehensive context for debugging.

## Trace Format

### File Locations
- **C Emulator**: `c_emulator_unified_trace.txt`
- **Zig Emulator**: `zig_emulator_unified_trace.txt`

### Line Format
```
LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
```

### Field Specifications

#### 1. LINE# (Line Number)
- **Format**: `%6d`
- **Description**: Sequential instruction counter
- **Example**: `   123`

#### 2. PC (Program Counter)
- **Format**: `0x%06x`
- **Description**: Byte offset from Lisp_world
- **Example**: `0x307898`

#### 3. INSTRUCTION
- **Format**: `%-16s`
- **Description**: Instruction mnemonic
- **Example**: `CALL` or `JUMP` or `LOAD`

#### 4. OPCODE
- **Format**: `0x%02x`
- **Description**: Byte opcode value
- **Example**: `0x4a`

#### 5. OPERANDS
- **Format**: `%-20s`
- **Description**: Instruction operands (decoded)
- **Example**: `r1, 0x1234` or `offset: +42`

#### 6. REGISTERS
- **Format**: `r1:0x%04x r2:0x%04x r3:0x%04x`
- **Description**: Key register values
- **Example**: `r1:0x1234 r2:0x5678 r3:0x9abc`

#### 7. FLAGS
- **Format**: `Z:%d N:%d C:%d V:%d`
- **Description**: Zero, Negative, Carry, Overflow flags
- **Example**: `Z:0 N:1 C:0 V:0`

#### 8. SP_FP (Stack Pointer / Frame Pointer)
- **Format**: `SP:0x%06x FP:0x%06x`
- **Description**: Stack and frame pointer values
- **Example**: `SP:0x308000 FP:0x307800`

#### 9. STACK_SUMMARY
- **Format**: `D:%d TOS:0x%08x N1:0x%08x N2:0x%08x`
- **Description**: Stack depth, Top of Stack, Next 2 values
- **Example**: `D:42 TOS:0x12345678 N1:0x9abcdef0 N2:0x11223344`

#### 10. MEMORY_CONTEXT
- **Format**: `@mem:0x%p [vpage:%u off:0x%03x]`
- **Description**: Memory address, virtual page, offset
- **Example**: `@mem:0x7ff12345678 [vpage:6204 off:0x0f8]`

#### 11. FP_VP_FO_VA (File/Virtual Mapping)
- **Format**: `FP:%u VP:%u FO:0x%lx VA:0x%lx`
- **Description**: File page, Virtual page, File offset, Virtual address
- **Example**: `FP:5178 VP:6204 FO:0x145a000 VA:0x307800`

#### 12. BS_MEM (Byte Swap + Memory)
- **Format**: `BS:%s MEM:0x%02x%02x%02x%02x`
- **Description**: Byte-swap status, Memory content at PC (4 bytes)
- **Example**: `BS:SWAPPED MEM:0x4a123456`

#### 13. NOTES
- **Format**: `%-30s`
- **Description**: Optional notes (PC_MISMATCH, MEM_ZEROS, etc.)
- **Example**: `PC_MISMATCH_CHECK` or `` (empty)

## Example Lines

### Standard Instruction
```
   123|0x307898|CALL          |0x4a|r1, 0x1234         |r1:0x1234 r2:0x5678 r3:0x9abc|Z:0 N:1 C:0 V:0|SP:0x308000 FP:0x307800|D:42 TOS:0x12345678 N1:0x9abcdef0 N2:0x11223344|@mem:0x7ff12345678 [vpage:6204 off:0x0f8]|FP:5178 VP:6204 FO:0x145a000 VA:0x307800|BS:SWAPPED MEM:0x4a123456|PC_MISMATCH_CHECK
```

### Simple Instruction
```
   124|0x30789a|LOAD          |0x8b|r1, [r2+4]         |r1:0x1234 r2:0x5678 r3:0x9abc|Z:0 N:0 C:0 V:0|SP:0x308000 FP:0x307800|D:42 TOS:0x12345678 N1:0x9abcdef0 N2:0x11223344|@mem:0x7ff1234567c [vpage:6204 off:0x0fc]|FP:5178 VP:6204 FO:0x145a000 VA:0x307800|BS:SWAPPED MEM:0x8b567890|
```

## Implementation Requirements

### C Emulator Implementation
- **Location**: `maiko/src/xc.c` (modify existing execution log)
- **Function**: `unified_trace_log()`
- **Integration**: Replace/add to existing execution log loop

### Zig Emulator Implementation
- **Location**: `zaiko/src/vm/execution_trace.zig`
- **Function**: `unifiedTraceLog()`
- **Integration**: Add to existing execution trace infrastructure

### Conditional Logging
- **Always log**: First 100 instructions
- **Always log**: Known problem locations (PC 0x307898, etc.)
- **Conditional log**: When memory content changes unexpectedly
- **Conditional log**: When file page mapping changes

## Comparison Tools

### Awk Examples
```bash
# Compare PC fields between C and Zig traces
awk -F'|' 'NR==FNR{c[$2]; next} ($2 in c && c[$2]!=$3){print "Line " $1 " PC mismatch: C=" c[$2] " Zig=" $3}' c_unified.txt zig_unified.txt

# Find instructions with different memory context
awk -F'|' 'NR==FNR{c[$10]; next} ($2 in c && c[$10]!=$10){print "Line " $1 " Memory context differs"}' c_unified.txt zig_unified.txt

# Extract all lines with PC_MISMATCH_CHECK
awk -F'|' '/PC_MISMATCH_CHECK/ {print $0}' unified_trace.txt
```

### Python Comparison Script
```python
def compare_traces(c_file, zig_file):
    c_lines = open(c_file).readlines()
    zig_lines = open(zig_file).readlines()

    for i, (c_line, zig_line) in enumerate(zip(c_lines, zig_lines)):
        c_fields = c_line.strip().split('|')
        zig_fields = zig_line.strip().split('|')

        if c_fields[2] != zig_fields[2]:  # Instruction field
            print(f"Line {i+1}: Instruction mismatch - C:{c_fields[2]} Zig:{zig_fields[2]}")
```

## Integration with Existing Enhanced Tracing

### Relationship to Enhanced Tracing
- **Unified traces**: For rapid divergence identification
- **Enhanced traces**: For deep investigation when divergences found
- **Documentation**: Reference for understanding trace context

### Workflow Integration
1. **Generate unified traces** from both emulators
2. **Compare with awk/python** to identify divergences
3. **Trigger enhanced tracing** for deep investigation
4. **Apply systematic debugging** using existing enhanced traces
5. **Fix and verify** using both trace types

## Benefits

1. **Rapid Comparison**: Single-line format enables fast awk-based divergence detection
2. **Comprehensive Context**: All essential VM state in one line
3. **Tool Compatibility**: Works with existing text processing tools
4. **Integration**: Leverages existing enhanced tracing infrastructure
5. **Consistency**: Same format for C and Zig emulators

## Success Criteria

- [ ] C emulator generates unified trace format
- [ ] Zig emulator generates unified trace format
- [ ] Awk comparison tools identify divergences correctly
- [ ] Integration with existing enhanced tracing workflow
- [ ] Documentation updated with new format

## Next Steps

1. **IMMEDIATE**: Implement C emulator unified trace generation
2. **IMMEDIATE**: Implement Zig emulator unified trace generation
3. **IMMEDIATE**: Create comparison scripts
4. Test unified trace generation and comparison
5. Integrate with existing divergence workflow
6. Update documentation

## References

- Enhanced Tracing Proposal: `documentation/implementations/c-emulator-enhanced-tracing-proposal.typ`
- Zig Enhanced Tracing: `documentation/implementations/zig-enhanced-tracing-implementation.typ`
- Critical Debugging Technique: `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`
- Memory Documentation: `documentation/specifications/memory-management.typ`