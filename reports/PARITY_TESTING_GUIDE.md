# Parity Testing Guide

**Last Updated**: 2026-02-27 08:31
**Purpose**: Comprehensive guide for systematic parity testing between emulator implementations

## Overview

This guide consolidates all parity testing methodology, workflows, and tools for comparing emulator implementations (C, Zig, Common Lisp, TypeScript). The goal is to ensure consistent behavior across all implementations of the Maiko emulator.

## Core Testing Strategy

### Fast Smoke Tests
- Use quick test runner by default (user preference)
- Comprehensive tests kept separate for thorough validation
- Test location: Tests should be next to scripts in `tests/` folders

### Parity Testing Methodology
- **Cross-Reference C Traces**: Compare execution traces instruction-by-instruction
- **Step-by-Step Validation**: Run emulators with `EMULATOR_MAX_STEPS=N` for controlled testing
- **Canonical Comparison Script**: Use `scripts/compare_emulator_execution.sh` for systematic comparison
- **Unified Trace Format**: Ensure consistent logging format across implementations

### Regression Testing
- **Commit-Hook Validation**: Run parity tests before commits to catch regressions
- **Incremental Testing**: Test after each opcode implementation
- **Boundary Testing**: Validate edge cases and error conditions

### Debugging Integration
- **Systematic Debugging**: Follow `documentation/core/critical-debugging-technique.typ` hierarchy
- **Trace Analysis**: Use comparison tools for divergence identification
- **Performance Profiling**: Monitor execution speed during testing

## Canonical Parity Workflow (C vs Zig)

### Primary Script
**`scripts/compare_emulator_execution.sh`** - The canonical script for repeatable execution-trace parity work

**Features**:
- Supports the shared runtime cap knob **`EMULATOR_MAX_STEPS`** (unset/0 → run to completion)
- Generates unified trace format for both emulators
- Provides automated comparison and divergence detection
- Path robustness: Prefers absolute sysout paths (scripts should not depend on cwd)

### Unified Trace Format

**Purpose**: Enable rapid divergence identification between C and Zig emulators

**Format**: Single-line, pipe-delimited columns:
```
LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
```

**Key Benefits**:
- **Rapid comparison** with awk/Python scripts
- **Comprehensive context** in single line
- **Consistent format** across both emulators
- **Memory issue triage** with dedicated fields

**Trace Files**:
- C trace: `c_emulator_unified_trace.txt`
- Zig trace: `zig_emulator_unified_trace.txt`

### Comparison Tools

**`scripts/compare_unified_traces.awk`**
- Fast awk-based comparison
- Rapid divergence identification
- Line-by-line matching

**`scripts/compare_unified_traces.py`**
- Detailed Python analysis
- Statistical reporting
- Advanced filtering options

**`scripts/analyze_execution_divergence.py`**
- Supports LCP (Longest Common Prefix) skip
- `--start-line` resume capability
- Detailed divergence analysis

## Systematic Debugging Workflow

**CRITICAL**: For any implementation divergence or bug, follow this systematic process:

1. **Cross-reference C traces** - Establish baseline with verified C emulator output
2. **Instrument execution** - Add targeted debug prints following technique hierarchy
3. **Validate incrementally** - Test after each code change, not at the end
4. **Document findings** - Update both specifications and implementation docs
5. **Regression test** - Ensure no existing functionality breaks

**Key Principle**: Never implement blindly - always validate against C reference first.

### Performance Considerations
- VM emulators require careful optimization - profile memory access patterns
- Stack operations are performance-critical - minimize redundant calculations
- Memory translation overhead should be amortized across operations
- Consider instruction caching for frequently executed opcodes

## Centralized Memory Management

### Problem Solved
Scattered memory logic causing recurring address translation, endianness, and paging issues

### Solution
Centralized memory management module (`zaiko/src/memory/manager.zig`)

### Components
- **AddressManager**: LispPTR ↔ byte conversions, virtual page calculations
- **FPtoVPManager**: File page ↔ virtual page mapping, page OK flags
- **EndiannessManager**: Byte-swapping logic, XOR addressing
- **MemoryAccessManager**: Safe memory reads, bounds checking

### Integration
Both C and Zig emulators use centralized functions for consistency

## Multi-Implementation Parity

### Supported Implementations
- **C (Maiko)**: Production-ready reference implementation
- **Zig (Zaiko)**: Incomplete (~60-70% actual coverage)
- **Common Lisp (Laiko)**: In progress (early stage)
- **TypeScript (Taiko)**: In progress (early stage)

### Parity Status Tracking
See [`reports/IMPLEMENTATION_STATUS.md`](IMPLEMENTATION_STATUS.md) for detailed implementation status of all Maiko emulator implementations.

### Comparison Scripts
- `scripts/compare_all_implementations.sh` - Compare all implementations
- `scripts/compare_multi_implementation.py` - Python-based multi-impl comparison
- `scripts/check_parity.sh` - C vs Zig parity check
- `scripts/check_parity_laiko.sh` - C vs Common Lisp parity check

## Testing Tools and Scripts

### Trace Generation
- `scripts/generate_debug_logs.sh` - Generate debug logs for comparison
- `scripts/generate_parity_report.py` - Generate parity reports

### Analysis Tools
- `scripts/analyze_execution_divergence.py` - Analyze execution divergences
- `scripts/analyze_memory_divergence.py` - Analyze memory state differences
- `scripts/analyze_opcode_divergence.py` - Analyze opcode-level differences
- `scripts/analyze_pc_divergence.py` - Analyze program counter divergences
- `scripts/analyze_stack_divergence.py` - Analyze stack state differences
- `scripts/analyze_tos.awk` - Analyze top-of-stack values
- `scripts/analyze_registers.awk` - Analyze register state
- `scripts/analyze_memory.awk` - Analyze memory state
- `scripts/analyze_trace_divergence.awk` - Analyze trace divergences

### Specialized Analysis
- `scripts/analyze_execution_window.py` - Analyze execution windows
- `scripts/analyze_fptovp_tables.py` - Compare FPtoVP tables
- `scripts/visual_diff.py` - Visual diff tool

### Continuous Testing
- `scripts/continuous_parity_check.sh` - Continuous parity monitoring
- `scripts/detect_regressions.py` - Regression detection
- `scripts/iterative_parity_workflow.py` - Iterative parity workflow
- `scripts/run_parity_suite.sh` - Run full parity test suite

### Utility Scripts
- `scripts/compare_execution_logs.sh` - Compare execution logs
- `scripts/compare_debug_logs.sh` - Compare debug logs
- `scripts/compare_state.py` - Compare emulator state
- `scripts/fix_divergence.sh` - Fix divergence workflow
- `scripts/parse_trace_fields.awk` - Parse trace fields
- `scripts/update_path_references.py` - Update path references

## Best Practices

### For Parity Testing
1. **Always use C as reference** - C implementation is production-ready and verified
2. **Use unified trace format** - Ensures consistent comparison across implementations
3. **Test incrementally** - Don't wait until everything is done
4. **Document divergences** - Record findings in specifications and implementations
5. **Validate fixes** - Ensure no regressions in existing functionality

### For Debugging
1. **Follow systematic debugging workflow** - See `documentation/core/critical-debugging-technique.typ`
2. **Use EMULATOR_MAX_STEPS** - For controlled step-by-step validation
3. **Cross-reference C traces** - Establish baseline before investigating
4. **Add targeted debug prints** - Follow technique hierarchy
5. **Document findings** - Update both specifications and implementation docs

### For Performance
1. **Profile before optimizing** - Identify bottlenecks first
2. **Focus on hot paths** - Optimize frequently executed code
3. **Minimize redundant calculations** - Especially in stack operations
4. **Consider instruction caching** - For frequently executed opcodes

## Common Issues and Solutions

### Memory Corruption
- **Symptom**: Execution diverges from C traces unexpectedly
- **Cause**: Incorrect memory access, bounds violations, or type mismatches
- **Solution**: Use memory integrity verification techniques from `documentation/core/critical-debugging-technique.typ`

### Stack Corruption
- **Symptom**: Invalid stack pointers or TOPOFSTACK values
- **Cause**: Incorrect stack manipulation, missing synchronization
- **Solution**: Validate stack state after each operation, check CSTKPTRL/TOPOFSTACK sync

### Type Mismatches
- **Symptom**: Compilation errors or runtime crashes
- **Cause**: LispPTR vs native pointer confusion, signed vs unsigned issues
- **Solution**: Always cross-reference C code for correct types and casting

### Performance Issues
- **Symptom**: Execution too slow for practical use
- **Cause**: Inefficient algorithms, excessive memory allocations
- **Solution**: Profile with `EMULATOR_MAX_STEPS`, optimize hot paths

## Related Documentation

- **Critical Debugging Techniques**: `documentation/core/critical-debugging-technique.typ`
- **Implementation Status**: `reports/IMPLEMENTATION_STATUS.md`
- **Memory Management**: `documentation/components/memory-management.typ`
- **VM Core Architecture**: `documentation/components/vm-core.typ`

## Archive Information

This guide consolidates information from the following archived files:
- `scripts/PARITY_WORKFLOW.md`
- `scripts/NEW_FORMAT_PARITY_STATUS.md`
- `reports/parity/multi_implementation_parity_plan.md`
- AGENTS.md sections: "Systematic Testing and Validation", "Parity Workflow (C vs Zig) – Canonical", "Unified Trace Format"

Original files archived to: `reports/archive/parity-testing/`