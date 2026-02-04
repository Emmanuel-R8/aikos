# ZIG EMULATOR STEP-WISE COMPARISON STATUS

**Session Date**: 2025-01-17 19:45
**Priority**: STEP-WISE COMPARATIVE EXECUTION (ABSOLUTE PRIORITY)
**Skill**: superpowers:executing-plans

## CRITICAL FINDING - SP TRACE FIX APPLIED (2026-02-04) ✅

**SP/FP TRACE ALIGNMENT**: Resolved

- **C logs**: `sp_offset = (DLword *)CSTKPTRL - (DLword *)Lisp_world` (CSTKPTRL = CurrentStackPTR + 2 DLwords).
- **Zig fix**: getStackPtrOffset() now uses vm.cstkptrl when set (matches C); fallback (stack_ptr offset + 2) for init.
- **Result**: Step 0 SP:0x012e8a and Step 1 SP:0x012e88 now match C in Zig trace.

**RESOLVED (2026-02-04)**: GVAR value and PC advance.
- GVAR: 5-byte format (match C), atom index = getPointerOperand(0) & 0xFFFF, explicit return null in execution_data.zig. Step 1 TOS 0x0e and step 2 PC 0x60f136 now match C.

**RESOLVED (2026-02-03)**: Trace timing, UNBIND TOS, GVAR value cell, full state.
- Trace: Zig logs after execution; TOPOFSTACK synced from memory before log. UNBIND leaves TOS unchanged (match C). Valspace byte offset 0x180000 (C DLword 0xC0000). REGISTERS and FLAGS populated in both traces.

**REMAINING DIVERGENCES**:
1. Zig stops after 8 steps (RETURN at step 7). Mitigated: top-level return sets stop_requested (clean exit).
2. Re-run comparison to confirm step 2 TOS parity after Valspace/UNBIND fixes.

## CURRENT COMPARISON INFRASTRUCTURE STATUS

### ✅ WORKING COMPONENTS

1. **C Emulator Trace Generation**

   - Command: `EMULATOR_MAX_STEPS=N ./maiko/linux.x86_64/ldesdl sysout`
   - Output: `c_emulator_execution_log.txt`
   - Format: C native detailed trace format
   - Working: ✅ Generates 5+ instruction traces successfully

2. **Zig Emulator Trace Generation**

   - Command: `EMULATOR_MAX_STEPS=N zig build run -- sysout`
   - Output: `zaiko/zig_emulator_execution_log.txt`
   - Format: Unified single-line trace format
   - Working: ✅ Generates matching instruction traces

3. **Comparison Capability**
   - Both emulators run for same number of instructions
   - Both generate trace files
   - First instruction comparison completed
   - Divergence identified at instruction 0

### 📋 COMPARISON RESULTS (First 8 Steps – Zig; 86+ C with cap 100)

**INSTRUCTION 0**:

- C: `PC:0x60f130 RECLAIMCELL (0xbf)` with SP:0x012e8a FP:0x012e72
- Zig: `PC:0x60f130 POP (0xbf)` with SP:0x012e8a FP:0x012e72
- **RESULT**: ✅ SP/FP match (opcode name differs: C trace "RECLAIMCELL", Zig "POP" – same byte 0xbf)

**INSTRUCTION 1**:

- C: `PC:0x60f131 UNKNOWN (0x60)` SP:0x012e88
- Zig: `PC:0x60f131 GVAR (0x60)` SP:0x012e88
- **RESULT**: ✅ SP matches; opcode name differs (C trace labels 0x60 UNKNOWN, Zig GVAR)

**INSTRUCTIONS 2–7**: PC and SP/FP match where Zig runs; opcode names may differ (C trace uses different naming for some bytes). Zig stops after step 7 (RETURN).

## IMMEDIATE NEXT STEP REQUIRED

**FIX ZIG STOPPING AT 8 STEPS** (top-level RETURN):

- Zig executes RETURN at step 7; returnFromFunction sets vm.pc = 0 when no previous frame; next decode (pc=1) may fail and dispatch returns; only 8 trace lines written.
- Options: (1) On top-level return, set stop_requested and return normally so step count is consistent; (2) Ensure decode from pc=1 does not fail so Zig continues to step cap.
- File: `zaiko/src/vm/function.zig` (returnFromFunction), `zaiko/src/vm/dispatch/dispatch_loop.zig` (decode at low PC).

## FILES TO INVESTIGATE

### Primary Issue

- `zaiko/src/vm/vm_initialization.zig` - Stack/frame pointer initialization
- Lines 40-60: currentfxp calculation and pointer setting
- Compare with C implementation in `maiko/src/main.c` start_lisp()

### Reference Traces

- `c_emulator_execution_log.txt` - C emulator trace (working)
- `zaiko/zig_emulator_execution_log.txt` - Zig emulator trace (wrong SP/FP)

### Working Commands

```bash
# Run C emulator (baseline)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
EMULATOR_MAX_STEPS=5 ./maiko/linux.x86_64/ldesdl /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/medley/internal/loadups/starter.sysout

# Run Zig emulator (comparison)
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/zaiko
env ZIG_GLOBAL_CACHE_DIR=zaiko/.zig-cache EMULATOR_MAX_STEPS=5 zig build run -- /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/medley/internal/loadups/starter.sysout

# Compare traces
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
head -3 c_emulator_execution_log.txt
head -3 zaiko/zig_emulator_execution_log.txt
```

## PROJECT STATUS CONTEXT

**NOT DOCUMENTATION COMPLETE** - Despite task tracking showing 100%, the emulator has critical runtime bugs
**ACTUAL STATUS**: ~95% complete - Core infrastructure works, stack initialization broken
**PRIORITY**: Fix stack/frame pointer initialization to enable proper comparison

## SESSION CONTINUITY INSTRUCTIONS

1. **IMMEDIATE**: Fix VM initialization stack/frame pointers to match C (SP=0x02e88, FP=0x307864)
2. **VERIFY**: Run step-wise comparison again after fix
3. **CONTINUE**: Fix next divergence if stack/frame pointers resolved
4. **ITERATE**: Continue until both emulators produce identical traces

## CURRENT ACTION: STEP-WISE PROGRESS

**STATUS**: SP trace logging fixed; Zig matches C for steps 0–7 (SP/FP/PC). Zig stops at 8 steps.
**TARGET**: Zig runs to step cap (e.g. 100) like C so full trace comparison is possible.
**FILE**: `zaiko/src/vm/function.zig`, `zaiko/src/vm/execution_trace.zig` (SP fix applied)
**PROGRESS**: SP/FP in trace now match C (CSTKPTRL-based SP). Next: fix top-level RETURN so Zig does not exit early.
**NEXT**: Fix top-level return handling; re-run comparison from 0; continue to next divergence.

## DEBUGGING ENVIRONMENT

- Working directory: `/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp`
- Zig cache: `ZIG_GLOBAL_CACHE_DIR=zaiko/.zig-cache`
- Sysout: `/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/medley/internal/loadups/starter.sysout`
- Step limit: `EMULATOR_MAX_STEPS=5` (for controlled testing)

**KEY INSIGHT**: The comparison infrastructure is working perfectly - we can now systematically fix each divergence step by step.
