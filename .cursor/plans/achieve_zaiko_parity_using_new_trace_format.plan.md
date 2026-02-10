---
name: Achieve Zaiko Parity Using New Trace Format
overview: Leverage the new comma-separated trace format to systematically achieve execution parity between C and Zig emulators by enhancing comparison tools and creating a precise divergence detection workflow.
todos:
  - id: enhance_python_comparison
    content: Update scripts/compare_unified_traces.py to parse and compare comma-separated sub-fields individually
    status: completed
  - id: enhance_awk_comparison
    content: Update scripts/compare_unified_traces.awk to parse and compare comma-separated sub-fields using split()
    status: completed
  - id: create_divergence_analyzer
    content: Create scripts/analyze_trace_divergence.awk for deep divergence analysis with sub-field precision
    status: completed
  - id: create_parity_checker
    content: Create scripts/check_parity.sh for automated parity checking workflow
    status: completed
  - id: create_fix_workflow
    content: Create scripts/fix_divergence.sh for interactive divergence fixing workflow
    status: completed
  - id: create_field_analyzers
    content: Create field-specific analysis scripts (analyze_tos.awk, analyze_registers.awk, analyze_memory.awk)
    status: completed
  - id: create_visual_diff
    content: Create scripts/visual_diff.py for HTML side-by-side comparison reports
    status: completed
  - id: run_baseline_comparison
    content: Run initial baseline comparison with EMULATOR_MAX_STEPS=100 using enhanced tools
    status: completed
  - id: fix_first_divergence
    content: Fix first identified divergence using sub-field analysis and verify fix
    status: pending
  - id: achieve_full_parity
    content: Iteratively fix all divergences until 100% parity achieved for EMULATOR_MAX_STEPS=1000
    status: pending
---

# Achieve Zaiko Parity Using New Trace Format

## Goal

Use the newly implemented comma-separated trace format to systematically achieve 100% execution parity between the C and Zig emulators. The improved format enables precise sub-field parsing with Awk, making divergence detection and analysis more efficient.

## Current Status

- **Trace Format**: ✅ Both C and Zig emulators now use comma-separated sub-fields
- **Parity Status**: Steps 0-7 match for PC/SP/FP/opcode; remaining divergences in TOS values and early exit behavior
- **Comparison Tools**: Basic comparison scripts exist but don't leverage sub-field parsing yet

## Plan Overview

### Phase 1: Enhance Comparison Tools

**1.1 Update Python Comparison Script**

- **File**: `scripts/compare_unified_traces.py`
- **Changes**:
  - Add sub-field parsing functions for REGISTERS, FLAGS, SP_FP, STACK_SUMMARY, MEMORY_CONTEXT, FP_VP_FO_VA
  - Compare individual sub-fields (e.g., r1, r2, r3 separately; SP and FP separately; TOS, N1, N2 separately)
  - Generate detailed divergence reports showing which specific sub-field differs
  - Add `--verbose` mode to show all matching fields even when differences exist

**1.2 Update Awk Comparison Script**

- **File**: `scripts/compare_unified_traces.awk`
- **Changes**:
  - Parse comma-separated sub-fields using `split()` function
  - Compare sub-fields individually (e.g., `split($6, regs, ",")` then compare each register)
  - Report specific sub-field differences (e.g., "Line 2: REGISTERS.r2 differs: C=0x5678 Zig=0x1234")
  - Add summary statistics (how many lines differ, which fields differ most often)

**1.3 Create Enhanced Divergence Analysis Script**

- **File**: `scripts/analyze_trace_divergence.awk` (new)
- **Purpose**: Deep analysis of trace divergences using sub-field parsing
- **Features**:
  - Identify first divergence point with sub-field precision
  - Track divergence patterns (e.g., "TOS differs in 80% of lines after line 10")
  - Generate divergence heatmap showing which fields diverge most
  - Suggest likely root causes based on divergence patterns

### Phase 2: Systematic Parity Workflow

**2.1 Create Automated Parity Check Script**

- **File**: `scripts/check_parity.sh` (new)
- **Purpose**: Run both emulators, compare traces, and report parity status
- **Features**:
  - Run both emulators with same `EMULATOR_MAX_STEPS` value
  - Generate unified traces
  - Run comparison scripts
  - Report first divergence line and field
  - Exit with appropriate code for CI/CD integration

**2.2 Create Divergence Fix Workflow**

- **File**: `scripts/fix_divergence.sh` (new)
- **Purpose**: Interactive workflow for fixing divergences
- **Features**:
  - Run parity check
  - Identify first divergence
  - Extract relevant trace lines with context
  - Open relevant source files
  - Guide user through fix process

### Phase 3: Enhanced Trace Analysis

**3.1 Create Field-Specific Analysis Tools**

- **Files**:
  - `scripts/analyze_tos.awk` - Analyze TOS (Top of Stack) values
  - `scripts/analyze_registers.awk` - Analyze register values
  - `scripts/analyze_memory.awk` - Analyze memory context
- **Purpose**: Deep dive into specific fields to identify patterns

**3.2 Create Visual Comparison Tool**

- **File**: `scripts/visual_diff.py` (new)
- **Purpose**: Generate side-by-side comparison of traces
- **Features**:
  - Parse comma-separated sub-fields
  - Generate HTML report with color-coded differences
  - Highlight divergences in red, matches in green
  - Show sub-field level differences

### Phase 4: Parity Achievement Process

**4.1 Initial Baseline Comparison**

- Run both emulators with `EMULATOR_MAX_STEPS=100`
- Generate traces using new format
- Run enhanced comparison tools
- Document all divergences

**4.2 Fix Divergences Systematically**

- Start with first divergence (lowest line number)
- Use sub-field analysis to identify root cause
- Fix in Zig emulator
- Re-run comparison from step 0
- Verify fix doesn't introduce new divergences
- Repeat until 100% parity

**4.3 Validation**

- Run extended comparison (`EMULATOR_MAX_STEPS=1000`)
- Verify 100% parity across all fields
- Document any remaining edge cases
- Update parity status documentation

## Implementation Details

### Sub-Field Parsing Functions

**Python Example**:

```python
def parse_registers(reg_field):
    """Parse REGISTERS field: r1:0x...,r2:0x...,r3:0x..."""
    regs = {}
    for item in reg_field.split(','):
        key, value = item.split(':')
        regs[key] = value
    return regs

def parse_stack(stack_field):
    """Parse STACK_SUMMARY field: TOS:0x...,N1:0x...,N2:0x..."""
    stack = {}
    for item in stack_field.split(','):
        key, value = item.split(':')
        stack[key] = value
    return stack
```

**Awk Example**:

```awk
# Parse REGISTERS field
split($6, regs, ",")
for (i in regs) {
    split(regs[i], r, ":")
    if (r[1] == "r1") r1_val = r[2]
    if (r[1] == "r2") r2_val = r[2]
    if (r[1] == "r3") r3_val = r[2]
}
```

### Comparison Enhancement

**Before**: Compare entire field strings

- `"r1:0x1234,r2:0x5678,r3:0x9a"` vs `"r1:0x1234,r2:0x9999,r3:0x9a"` → "REGISTERS differ"

**After**: Compare individual sub-fields

- r1: ✅ Match (0x1234)
- r2: ❌ Differ (C=0x5678, Zig=0x9999)
- r3: ✅ Match (0x9a)

## Files to Modify

### Existing Files

- `scripts/compare_unified_traces.py` - Add sub-field parsing
- `scripts/compare_unified_traces.awk` - Add sub-field parsing

### New Files

- `scripts/analyze_trace_divergence.awk` - Deep divergence analysis
- `scripts/check_parity.sh` - Automated parity checking
- `scripts/fix_divergence.sh` - Interactive fix workflow
- `scripts/analyze_tos.awk` - TOS-specific analysis
- `scripts/analyze_registers.awk` - Register-specific analysis
- `scripts/analyze_memory.awk` - Memory-specific analysis
- `scripts/visual_diff.py` - HTML comparison report

## Success Criteria

1. ✅ Comparison tools parse and compare sub-fields individually
2. ✅ First divergence identified with sub-field precision
3. ✅ All known divergences fixed (TOS, early exit, etc.)
4. ✅ 100% parity achieved for `EMULATOR_MAX_STEPS=1000`
5. ✅ Automated parity checking integrated into workflow

## Testing

- Run enhanced comparison on existing traces
- Verify sub-field parsing works correctly
- Test divergence detection with known issues
- Validate fixes don't introduce regressions

## Documentation Updates

- Update `documentation/specifications/vm-core/trace-and-logging-formats.typ` with sub-field parsing examples
- Document new comparison workflow in `reports/STEP_COMPARISON_STATUS.md`
- Create guide for using new analysis tools
