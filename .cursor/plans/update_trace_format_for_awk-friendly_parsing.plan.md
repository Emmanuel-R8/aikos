---
name: Update trace format for Awk-friendly parsing
overview: Update both C and Zig trace formats to use comma-separated sub-fields within pipe-delimited fields, making the format more programmatically manipulable with Awk while maintaining backward compatibility with existing comparison scripts.
todos:
  - id: update_c_trace_format
    content: Update C trace format in maiko/src/execution_trace.c to use comma-separated sub-fields
    status: completed
  - id: update_zig_trace_format
    content: Update Zig trace format in zaiko/src/vm/execution_trace.zig to use comma-separated sub-fields
    status: completed
  - id: update_documentation
    content: Update documentation/specifications/vm-core/trace-and-logging-formats.typ with new format specification
    status: completed
  - id: test_trace_generation
    content: Test trace generation with both emulators and verify format consistency
    status: completed
  - id: verify_awk_parsing
    content: Verify Awk can parse sub-fields correctly with the new format
    status: completed
---

# Update Trace Format for Awk-Friendly Parsing

## Goal

Make the unified trace format more programmatically manipulable by using consistent comma separators for sub-fields within pipe-delimited fields. This enables easier Awk parsing while maintaining the existing pipe-delimited structure.

## Current Format Issues

- Sub-fields within major fields use spaces (e.g., `r1:0x... r2:0x... r3:0x...`)
- Makes it harder to extract individual sub-fields with Awk
- Inconsistent separators across different field types

## Proposed Format Changes

### Field-by-Field Updates

1. **REGISTERS** (field 6)

   - Current: `r1:0x1234 r2:0x5678 r3:0x9a`
   - New: `r1:0x1234,r2:0x5678,r3:0x9a`

2. **FLAGS** (field 7)

   - Current: `Z:1 N:0 C:0`
   - New: `Z:1,N:0,C:0`

3. **SP_FP** (field 8)

   - Current: `SP:0x012e8a FP:0x012e72`
   - New: `SP:0x012e8a,FP:0x012e72`

4. **STACK_SUMMARY** (field 9)

   - Current: `TOS:0x00000000 N1:0x00000000 N2:0x00000000`
   - New: `TOS:0x00000000,N1:0x00000000,N2:0x00000000`

5. **MEMORY_CONTEXT** (field 10)

   - Current: `@mem:? [vpage:12408 off:0x130]`
   - New: `@mem:?,vpage:12408,off:0x130` (remove brackets, use commas)

6. **FP_VP_FO_VA** (field 11)

   - Current: `FP:0 VP:12408 FO:0x0 VA:0x60f000`
   - New: `FP:0,VP:12408,FO:0x0,VA:0x60f000`

7. **BS_MEM** (field 12)

   - Current: `BS:RAW MEM:????????`
   - New: `BS:RAW,MEM:????????` (or keep as-is if atomic)

## Implementation Files

### C Implementation

- **File**: `maiko/src/execution_trace.c`
- **Function**: `log_execution_trace()` (lines 95-186)
- **Changes**: Update snprintf format strings for fields 6-12 to use commas instead of spaces

### Zig Implementation

- **File**: `zaiko/src/vm/execution_trace.zig`
- **Function**: `unifiedTraceLog()` (lines 47-131)
- **Changes**: Update format strings in the append calls for fields 6-12

### Documentation Updates

- **File**: `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- **Changes**: Update field specifications to reflect comma-separated sub-fields

### Comparison Scripts (if needed)

- **Files**:
  - `scripts/compare_unified_traces.awk`
  - `scripts/compare_unified_traces.py`
- **Note**: These should continue to work since they parse by pipe delimiter, but may need updates if they parse sub-fields

## Example Output

**Before:**

```
     0|0x60f130|POP             |0xbf|                    |r1:0xf130 r2:0x0000 r3:0x00  |Z:1 N:0 C:0|SP:0x012e8a FP:0x012e72|TOS:0x00000000 N1:0x00000000 N2:0x00000000|@mem:? [vpage:12408 off:0x130]|FP:0 VP:12408 FO:0x0 VA:0x60f000|BS:RAW MEM:????????|
```

**After:**

```
     0|0x60f130|POP             |0xbf|                    |r1:0xf130,r2:0x0000,r3:0x00|Z:1,N:0,C:0|SP:0x012e8a,FP:0x012e72|TOS:0x00000000,N1:0x00000000,N2:0x00000000|@mem:?,vpage:12408,off:0x130|FP:0,VP:12408,FO:0x0,VA:0x60f000|BS:RAW,MEM:????????|
```

## Awk Usage Examples

After this change, Awk can easily extract sub-fields:

```awk
# Extract register r1 value
awk -F'|' '{split($6, regs, ","); split(regs[1], r1, ":"); print r1[2]}'

# Extract TOS value
awk -F'|' '{split($9, stack, ","); split(stack[1], tos, ":"); print tos[2]}'

# Extract SP value
awk -F'|' '{split($8, spfp, ","); split(spfp[1], sp, ":"); print sp[2]}'
```

## Testing

1. Run both emulators with `EMULATOR_MAX_STEPS=15`
2. Verify trace files are generated correctly
3. Test Awk parsing with example commands above
4. Run comparison scripts to ensure they still work
5. Verify format consistency between C and Zig outputs
