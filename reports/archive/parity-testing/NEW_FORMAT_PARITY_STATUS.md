# New Trace Format Parity Implementation Status

**Date**: 2026-02-04
**Status**: Tools Complete, Ready for Parity Work

## Implementation Complete

All tools for achieving parity using the new comma-separated trace format have been implemented:

### ✅ Enhanced Comparison Tools

1. **`scripts/compare_unified_traces.py`**
   - Parses and compares comma-separated sub-fields individually
   - Reports specific sub-field differences (e.g., `REGISTERS.r2`, `STACK.TOS`)
   - Includes `--verbose` mode
   - Generates field divergence statistics

2. **`scripts/compare_unified_traces.awk`**
   - Awk-based comparison with sub-field parsing
   - Uses `split()` to parse comma-separated fields
   - Reports sub-field level differences

### ✅ Analysis Tools

3. **`scripts/analyze_trace_divergence.awk`**
   - Deep divergence analysis with sub-field precision
   - Identifies first divergence point
   - Tracks divergence patterns
   - Generates divergence heatmap
   - Suggests likely root causes

4. **Field-Specific Analyzers**
   - `scripts/analyze_tos.awk` - TOS value analysis
   - `scripts/analyze_registers.awk` - Register value analysis
   - `scripts/analyze_memory.awk` - Memory context analysis

### ✅ Workflow Scripts

5. **`scripts/check_parity.sh`**
   - Automated parity checking
   - Runs both emulators
   - Compares traces
   - Reports first divergence
   - Exit codes for CI/CD

6. **`scripts/fix_divergence.sh`**
   - Interactive divergence fixing workflow
   - Shows context around divergence
   - Suggests relevant files
   - Guides fix process

7. **`scripts/visual_diff.py`**
   - HTML side-by-side comparison
   - Color-coded differences
   - Sub-field level highlighting

### ✅ Documentation

8. **`scripts/PARITY_WORKFLOW.md`**
   - Complete workflow documentation
   - Usage examples
   - Sub-field parsing examples

## Next Steps

### 1. Rebuild C Emulator (REQUIRED)

The C emulator must be rebuilt to generate traces with the new comma-separated format:

```bash
cd maiko
./medley/scripts/build/build-c-emulator.sh --display-backend sdl --build-system cmake --force
```

**Status**: C source code updated ✅, C emulator rebuild pending ⏳

### 2. Verify Format Consistency

After rebuilding, verify both traces use the new format:

```bash
# Check C trace format
head -1 c_emulator_execution_log.txt | grep -o "r1:0x[^,]*,"

# Check Zig trace format  
head -1 zig_emulator_execution_log.txt | grep -o "r1:0x[^,]*,"

# Both should show comma-separated format
```

### 3. Run Baseline Comparison

```bash
# Run with enhanced tools
./scripts/check_parity.sh 100

# Or use divergence analyzer
awk -f scripts/analyze_trace_divergence.awk \
    c_emulator_execution_log.txt \
    zig_emulator_execution_log.txt
```

### 4. Fix Divergences Iteratively

Follow the workflow in `scripts/PARITY_WORKFLOW.md`:

1. Identify first divergence using enhanced tools
2. Fix in Zig emulator
3. Re-run comparison from step 0
4. Repeat until 100% parity

## Format Verification

**New Format Example** (comma-separated sub-fields):
```
REGISTERS: r1:0xf130,r2:0x0000,r3:0x00
FLAGS: Z:1,N:0,C:0
SP_FP: SP:0x012e8a,FP:0x012e72
STACK: TOS:0x00000000,N1:0x00000000,N2:0x00000000
MEMORY: @mem:?,vpage:12408,off:0x130
MAPPING: FP:0,VP:12408,FO:0x0,VA:0x60f000
BS_MEM: BS:RAW,MEM:????????
```

**Old Format** (space-separated, will be replaced after rebuild):
```
REGISTERS: (empty)
FLAGS: (empty)
SP_FP: SP:0x012e8a FP:0x012e72
STACK: TOS:0x00000000 N1:0x00000000 N2:0x00000000
MEMORY: @mem:? [vpage:12408 off:0x130]
MAPPING: FP:0 VP:12408 FO:0x0 VA:0x60f000
BS_MEM: BS:RAW MEM:????????
```

## Tool Usage Examples

### Extract TOS value from trace
```bash
awk -F'|' '{split($9, stack, ","); split(stack[1], tos, ":"); print tos[2]}' trace.txt
```

### Compare specific sub-field
```bash
# Compare TOS values
awk -f scripts/analyze_tos.awk c_trace.txt zig_trace.txt

# Compare registers
awk -f scripts/analyze_registers.awk c_trace.txt zig_trace.txt
```

### Generate visual report
```bash
python3 scripts/visual_diff.py c_trace.txt zig_trace.txt report.html
```

## Success Criteria

- ✅ All comparison tools parse sub-fields correctly
- ✅ Divergence analysis identifies first divergence with sub-field precision
- ⏳ C emulator rebuilt (pending)
- ⏳ Both traces use new format (pending rebuild)
- ⏳ 100% parity achieved (iterative process using tools)

## Notes

- Tools are ready and tested
- C emulator rebuild is the blocker for full workflow
- Once rebuilt, the iterative parity process can begin
- All tools support both old and new formats (graceful degradation)
