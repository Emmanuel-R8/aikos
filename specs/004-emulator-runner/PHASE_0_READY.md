# Phase 0: C Emulator Enhanced Tracing - Ready for Implementation

**Date**: 2025-12-23 17:06
**Status**: ✅ Ready to Start

## Overview

Phase 0 adds comprehensive tracing to the C emulator to provide ground truth for debugging the Zig emulator. All tasks are properly documented, specified with exact file paths and line numbers, and ready for implementation.

## Tasks Summary

**Total**: 6 tasks (CT000-CT006)
- **CT000-CT003**: Memory loading trace (4 tasks)
- **CT004-CT005**: Enhanced execution log (2 tasks)  
- **CT006**: Address calculation trace (1 task)

## Implementation Files

### C Emulator Files to Modify

1. **`maiko/src/ldsout.c`** (4 tasks)
   - CT000: Add memory trace log file handle (after line 61)
   - CT001: Trace FPtoVP table loading (after line 306)
   - CT002: Trace page loading (in page loading loop, ~line 450-550)
   - CT003: Trace critical address verification (end of `sysout_loader`)

2. **`maiko/src/xc.c`** (2 tasks)
   - CT004: Add file page lookup function (before `nextopcode`, ~line 520)
   - CT005: Enhance execution log format (after line 620)
   - CT006: Add address calculation trace (after line 540)

## Deliverables

After implementation, the C emulator will generate:

1. **`c_emulator_memory_loading_trace.txt`**
   - FPtoVP table loading details
   - File page → virtual page mappings
   - Byte-swapping operations
   - Critical address verification (PC 0x307898)

2. **Enhanced `c_emulator_execution_log.txt`**
   - New fields: `[FP:%u VP:%u FO:0x%lx VA:0x%lx]`
   - Byte-swap status: `[BS:SWAPPED]` or `[BS:RAW]`
   - Memory content: `[MEM:0x%02x...]`

3. **`c_emulator_address_trace.txt`**
   - Step-by-step address calculations
   - Critical debugging technique applied (value, value/2, value*2)
   - All intermediate values logged

## Documentation

All implementation details are documented in:

1. **Proposal**: `documentation/implementations/c-emulator-enhanced-tracing-proposal.typ`
   - Overview of all 6 tracing enhancements
   - Proposed log formats
   - Benefits and success criteria

2. **Implementation Guide**: `documentation/implementations/c-emulator-tracing-implementation-guide.typ`
   - Concrete code changes with line numbers
   - Code snippets ready to integrate
   - Step-by-step instructions

3. **Tasks**: `specs/004-emulator-runner/execution-debugging-tasks.md`
   - Phase 0 tasks (CT000-CT006)
   - Exact file paths and line numbers
   - Verification criteria

## Task Details

### CT000: Memory Trace Log File Handle
- **File**: `maiko/src/ldsout.c`
- **Location**: After line 61
- **Action**: Add `FILE *memory_trace_log` static variable, initialize in `sysout_loader`
- **Output**: `c_emulator_memory_loading_trace.txt`

### CT001: Trace FPtoVP Table Loading
- **File**: `maiko/src/ldsout.c`
- **Location**: After line 306 (after FPtoVP read)
- **Action**: Log FPtoVP offset, entries count, byte-swap boundary, first 10 entries, entry 5178
- **Output**: FPtoVP table details in memory trace log

### CT002: Trace Page Loading
- **File**: `maiko/src/ldsout.c`
- **Location**: In page loading loop (~line 450-550)
- **Action**: Log file→virtual mapping, file offset, virtual address, GETPAGEOK, raw/swapped bytes
- **Output**: Page loading details in memory trace log

### CT003: Trace Critical Address Verification
- **File**: `maiko/src/ldsout.c`
- **Location**: End of `sysout_loader` function
- **Action**: Log PC 0x307898 analysis with critical debugging technique (value, value/2, value*2)
- **Output**: Critical address verification in memory trace log

### CT004: File Page Lookup Function
- **File**: `maiko/src/xc.c`
- **Location**: Before `nextopcode` label (~line 520)
- **Action**: Create `getFilePageForVirtualPage(vpage)` function
- **Output**: Function available for use in execution log

### CT005: Enhance Execution Log Format
- **File**: `maiko/src/xc.c`
- **Location**: After line 620 (after current debug additions)
- **Action**: Add `[FP:%u VP:%u FO:0x%lx VA:0x%lx] [BS:%s] [MEM:...]` fields
- **Output**: Enhanced execution log with memory diagnostics

### CT006: Address Calculation Trace
- **File**: `maiko/src/xc.c`
- **Location**: After line 540 (after debug_log initialization)
- **Action**: Add address trace log with step-by-step calculations
- **Output**: `c_emulator_address_trace.txt`

## Implementation Order

Recommended sequence:

1. **CT000** - Set up memory trace log file handle
2. **CT001** - Add FPtoVP table tracing
3. **CT002** - Add page loading tracing
4. **CT003** - Add critical address verification
5. **CT004** - Add file page lookup function
6. **CT005** - Enhance execution log format
7. **CT006** - Add address calculation trace

## Verification Steps

After implementing each task:

1. Compile C emulator: `cd maiko && make`
2. Run C emulator: `./lde <sysout_file>`
3. Check trace files created:
   - `c_emulator_memory_loading_trace.txt`
   - `c_emulator_execution_log.txt` (enhanced)
   - `c_emulator_address_trace.txt`
4. Verify trace content matches expected format
5. Verify critical debugging technique applied (value, value/2, value*2)

## Success Criteria

- [ ] All 6 tasks implemented
- [ ] C emulator compiles successfully
- [ ] All trace files generated
- [ ] Trace content matches expected format
- [ ] Critical debugging technique applied throughout
- [ ] Memory loading trace shows file page → virtual page mappings
- [ ] Enhanced execution log shows file page, virtual address, byte-swap status
- [ ] Address calculation trace shows step-by-step calculations

## Next Steps After Phase 0

1. Run C emulator with enhanced tracing
2. Compare C traces with Zig emulator traces
3. Use C traces to systematically fix Zig emulator (Phase 1)
4. Apply critical debugging technique to identify discrepancies
5. Fix memory loading issues using C traces as reference

## Related Documentation

- **Tasks**: `specs/004-emulator-runner/execution-debugging-tasks.md` (Phase 0)
- **Plan**: `specs/004-emulator-runner/execution-debugging-plan.md`
- **Proposal**: `documentation/implementations/c-emulator-enhanced-tracing-proposal.typ`
- **Implementation Guide**: `documentation/implementations/c-emulator-tracing-implementation-guide.typ`
- **Critical Debugging Technique**: `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`

---

**Status**: ✅ All tasks documented, specified, and ready for implementation
**Start Date**: Ready to begin immediately
