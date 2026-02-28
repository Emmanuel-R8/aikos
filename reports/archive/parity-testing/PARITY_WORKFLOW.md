# Parity Workflow Using New Trace Format

## Overview

This document describes the workflow for achieving execution parity between C and Zig emulators using the enhanced comma-separated trace format.

## Prerequisites

1. **C Emulator Rebuild Required**: The C emulator must be rebuilt after the trace format update to generate traces with comma-separated sub-fields.

   ```bash
   cd maiko
   ./medley/scripts/build/build-c-emulator.sh --display-backend sdl --build-system cmake --force
   ```

2. **Enhanced Tools**: All comparison and analysis tools have been updated to parse sub-fields:
   - `scripts/compare_unified_traces.py` - Python comparison with sub-field precision
   - `scripts/compare_unified_traces.awk` - Awk comparison with sub-field parsing
   - `scripts/analyze_trace_divergence.awk` - Deep divergence analysis
   - `scripts/check_parity.sh` - Automated parity checking
   - `scripts/fix_divergence.sh` - Interactive fix workflow
   - Field-specific analyzers: `analyze_tos.awk`, `analyze_registers.awk`, `analyze_memory.awk`
   - `scripts/visual_diff.py` - HTML comparison report

## Workflow

### Step 1: Run Baseline Comparison

```bash
# Run automated parity check
./scripts/check_parity.sh 100

# Or manually run comparison
EMULATOR_MAX_STEPS=100 ./scripts/compare_emulator_execution.sh medley/internal/loadups/starter.sysout
python3 scripts/compare_unified_traces.py c_emulator_execution_log.txt zig_emulator_execution_log.txt
```

### Step 2: Analyze Divergences

```bash
# Deep divergence analysis
awk -f scripts/analyze_trace_divergence.awk \
    c_emulator_execution_log.txt \
    zig_emulator_execution_log.txt

# Field-specific analysis
awk -f scripts/analyze_tos.awk c_emulator_execution_log.txt zig_emulator_execution_log.txt
awk -f scripts/analyze_registers.awk c_emulator_execution_log.txt zig_emulator_execution_log.txt
awk -f scripts/analyze_memory.awk c_emulator_execution_log.txt zig_emulator_execution_log.txt
```

### Step 3: Fix First Divergence

```bash
# Interactive fix workflow
./scripts/fix_divergence.sh 100

# This will:
# 1. Identify first divergence line and field
# 2. Show context around divergence
# 3. Suggest relevant files to check
# 4. Guide you through the fix process
```

### Step 4: Verify Fix

```bash
# Re-run comparison from step 0 (mandatory)
./scripts/check_parity.sh 100

# Verify no regressions
./scripts/check_parity.sh 1000
```

### Step 5: Iterate

Repeat steps 2-4 until 100% parity is achieved.

## Sub-Field Parsing Examples

### Extract TOS value

```awk
awk -F'|' '{split($9, stack, ","); split(stack[1], tos, ":"); print tos[2]}' trace.txt
```

### Extract SP value

```awk
awk -F'|' '{split($8, spfp, ","); split(spfp[1], sp, ":"); print sp[2]}' trace.txt
```

### Extract register r1

```awk
awk -F'|' '{split($6, regs, ","); split(regs[1], r1, ":"); print r1[2]}' trace.txt
```

## Visual Comparison

Generate HTML report:

```bash
python3 scripts/visual_diff.py c_emulator_execution_log.txt zig_emulator_execution_log.txt comparison.html
```

Open `comparison.html` in a browser for side-by-side comparison with color-coded differences.

## Notes

- **Always re-run from step 0** after any fix to catch regressions
- **Use sub-field analysis** to pinpoint exact differences
- **Check C reference** implementation when fixing divergences
- **Document findings** in reports/STEP_COMPARISON_STATUS.md
