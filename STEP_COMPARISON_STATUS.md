# Step-Wise Comparison Status - Multi-Implementation

**Last Updated**: 2026-02-09
**Priority**: Parity testing across all three emulator implementations

---

## Overview

This document tracks step-wise execution parity between:
1. **C (Maiko)** - Reference implementation
2. **Common Lisp (Laiko)** - New implementation under development  
3. **Zig (Zaiko)** - Alternative implementation (15-step parity achieved)

---

## C Implementation (maiko/) - BASELINE

**Status**: ✅ REFERENCE IMPLEMENTATION

**Trace Command**:
```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
EMULATOR_MAX_STEPS=N ./maiko/build/c/linux.x86_64/lde ./medley/internal/loadups/starter.sysout
```

**Trace Format**:
```
LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
```

**Sample Output (Steps 0-2)**:
```
0|0x60f130|RECLAIMCELL|0xbf|-| | |SP:0x012e8a FP:0x012e72|TOS:0x00000000|@mem:[vpage:1231 off:0x130]||
1|0x60f131|UNKNOWN|0x60|...| | |SP:0x012e88 FP:0x012e72|TOS:0x0000000e|...|
2|0x60f136|FN2|0x12|...| | |SP:0x012e86 FP:0x012e72|TOS:0x00140000|...|
```

---

## Laiko Implementation (laiko/) - NEW

**Status**: 🔧 IN DEVELOPMENT - Testing Infrastructure

**Trace Command**:
```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/laiko
sbcl --non-interactive --load load-emulator.lisp --eval "(trace-log ...)"
```

**Trace Format**: Identical to C implementation

**Load Command**:
```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/laiko
./load-emulator.lisp
```

**Current Issues**:
- ⚠️ Opcode handler count shows 0 (BUG - should be ~190+)
- ⚠️ Some undefined function warnings during compilation
- ⚠️ IFPAGE accessor functions not properly exported

**Files for Laiko Parity**:
- `tests/run-parity.lisp` - Parity testing framework
- `src/vm/trace.lisp` - Trace infrastructure
- `src/vm/dispatch.lisp` - Dispatch loop
- `src/main.lisp` - VM initialization

**Next Steps for Laiko**:
1. Fix opcode handler registration
2. Export IFPAGE accessor functions
3. Achieve first successful VM execution
4. Run parity test against C

---

## Zaiko Implementation (zaiko/) - 15-STEP PARITY

**Status**: ⚠️ PARTIAL PARITY - 15 Steps Match, Then Diverges

### ✅ PARITY ACHIEVED: EMULATOR_MAX_STEPS=15

**Comparison Command**:
```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
EMULATOR_MAX_STEPS=15 ./scripts/compare_emulator_execution.sh ./medley/internal/loadups/starter.sysout
```

**Result**: ✅ 14-line traces are byte-for-byte identical

**Matching Fields** (Steps 0-14):
- ✅ PC values match exactly
- ✅ SP (stack pointer) matches
- ✅ FP (frame pointer) matches  
- ✅ TOS (top of stack) matches
- ✅ Opcode names match (via getOpcodeNameForTrace)
- ✅ Memory context fields match

**Format Alignment**:
- ✅ SP_FP: `SP:0x012e8a FP:0x012e72`
- ✅ TOS: `TOS:0x00000000`
- ✅ MEMORY: `@mem:[vpage:N off:0xNNN]`
- ✅ Last line: Truncated to match C format

### ⚠️ CURRENT DIVERGENCE: EMULATOR_MAX_STEPS=100

**Problem**:
- C produces: ~86 trace lines
- Zig produces: ~40 trace lines
- Zig exits early at step ~40

**Root Cause**:
- Zig hits RETURN with non-zero alink
- Bounds check sets stop_requested = true
- C has no such check and continues

**Location**: `zaiko/src/vm/function.zig` (returnFromFunction)

### 📋 ZIG PARITY FIXES (Completed)

| Fix | Date | Status |
|-----|------|--------|
| Stack/Frame Pointer Initialization | 2026-02-03 | ✅ |
| FastRetCALL Validation Logic | 2026-02-03 | ✅ |
| GVAR Value and PC Advance | 2026-02-04 | ✅ |
| Line 0 TOS Sync | 2026-02-04 | ✅ |
| RETURN alink=0 Handling | 2026-02-04 | ✅ |
| Trace Format Identity | 2026-02-05 | ✅ |
| UNBIND Semantics | 2026-02-03 | ✅ |
| SP Trace Logging | 2026-02-04 | ✅ |
| Opcode Name Mapping | 2026-02-06 | ✅ |
| 15-Step Parity | 2026-02-06 | ✅ |

---

## Comparison Infrastructure

### Working Components

| Component | C | Laiko | Zig |
|----------|---|-------|-----|
| Trace Generation | ✅ | 🔧 | ✅ |
| Step Counting | ✅ | 🔧 | ✅ |
| Memory Context | ✅ | 🔧 | ✅ |
| SP/FP Logging | ✅ | 🔧 | ✅ |
| TOS Logging | ✅ | 🔧 | ✅ |
| Format Matching | ✅ | 🔧 | ✅ |
| Comparison Script | N/A | 🔧 | ✅ |

### Comparison Scripts

**Main Script**:
```bash
./scripts/compare_emulator_execution.sh <sysout>
```

**Python Analysis**:
```bash
./scripts/compare_unified_traces.py c_trace.txt z_trace.txt
```

**AWK Quick Compare**:
```bash
./scripts/compare_unified_traces.awk c_trace.txt z_trace.txt
```

---

## Documentation Accuracy Note

**Previous Overstatement**: 
- Task tracking claimed 89.2% Zig completion
- **Actual**: ~60-70% with significant gaps

**Documented Issues**:
- 245 TODO/FIXME markers in Zig (8x more than C)
- Completely stubbed floating point operations
- Missing/incomplete graphics operations (BitBLT, drawing)
- Incomplete I/O subsystems
- Early exit at ~40 steps (not full execution)

**Action Required**: Status documents need updating to reflect reality.

---

## Next Steps

### Laiko (Priority 1)
1. Fix opcode handler registration (currently shows 0)
2. Export IFPAGE accessor functions
3. Run first successful VM execution
4. Generate first trace file
5. Begin step-by-step parity comparison

### Zaiko (Priority 2)
1. Fix early exit at ~40 steps
2. Extend parity beyond 15 steps
3. Implement missing floating point operations
4. Complete graphics operations

### General
1. Rebuild C emulator with unified trace format
2. Update all documentation to reflect actual completion
3. Add Laiko coverage to all status documents

---

## Key Files by Implementation

### C (Reference)
- `maiko/src/xc.c` - Trace generation
- `maiko/src/main.c` - Entry point
- `maiko/inc/ifpage.h` - IFPAGE structure

### Laiko (Common Lisp)
- `laiko/src/vm/trace.lisp` - Trace infrastructure
- `laiko/src/vm/dispatch.lisp` - Dispatch loop
- `laiko/src/main.lisp` - VM initialization
- `laiko/tests/run-parity.lisp` - Parity testing

### Zig (Zaiko)
- `zaiko/src/vm/execution_trace.zig` - Trace generation
- `zaiko/src/vm/function.zig` - RETURN handling
- `zaiko/src/vm/vm_initialization.zig` - VM init
- `zaiko/src/main.zig` - Entry point

---

## Environment Variables

| Variable | Purpose | Example |
|----------|---------|---------|
| `EMULATOR_MAX_STEPS` | Limit execution steps | `EMULATOR_MAX_STEPS=15` |
| `ZIG_GLOBAL_CACHE_DIR` | Zig cache location | `zaiko/.zig-cache` |
| `DISPLAY` | X11 display | `:0` |

---

## Session Continuity

### If C Emulator Issues
1. Rebuild: `./medley/scripts/build/build-c-emulator.sh --display-backend sdl --force`
2. Test: `./maiko/build/c/linux.x86_64/lde ./medley/internal/loadups/starter.sysout`

### If Laiko Issues
1. Clear cache: `find ~/.cache/common-lisp -name "*.fasl" -path "*Interlisp*" -delete`
2. Load: `./load-emulator.lisp`
3. Check handlers: `(hash-table-count maiko-lisp.vm:*opcode-handlers*)`

### If Zig Issues
1. Clear cache: `rm -rf zaiko/.zig-cache`
2. Rebuild: `ZIG_GLOBAL_CACHE_DIR=zaiko/.zig-cache zig build`
3. Test: `EMULATOR_MAX_STEPS=15 ./zaiko/build/zaiko ./medley/internal/loadups/starter.sysout`
