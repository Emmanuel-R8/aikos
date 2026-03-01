# Laiko Stack Divergence Analysis

**Date**: 2026-03-01
**Status**: Stack fix verified; new divergence identified

## Executive Summary

The "VM Error: Stack underflow" at GETBASEPTR-N is **RESOLVED**. However, a new critical divergence has been identified where Laiko's Stack Pointer (SP) does not synchronize with virtual memory.

## 1. Stack Fix Verification: ✅ WORKING

- **Previous Error**: "VM Error: Stack underflow" at GETBASEPTR-N opcode
- **Fix Applied**: Consolidated two incompatible stack systems into one unified VM-based system
- **Result**: POP → GVAR → UNBIND → GETBASEPTR-N → COPY now executes without error

## 2. Next Divergence Identified

### Symptom
Laiko stops after 5 instructions because the Stack Pointer (SP) never changes during execution.

### Evidence: SP Comparison

| Instruction | C Emulator SP | Laiko SP |
|-------------|---------------|----------|
| POP | 0x012e88 | 0x025D10 |
| GVAR | 0x012e8a | 0x025D14 |
| UNBIND | 0x012e8a | 0x025D10 |
| GETBASEPTR-N | 0x012e86 | 0x025D10 |
| COPY | 0x012e86 | 0x025D10 (NEVER CHANGES!) |

**C's SP fluctuates properly** as it pushes/pops values.
**Laiko's SP stays constant** at 0x025D10 - indicating stack memory management is broken.

### TOS Comparison

| Instruction | C TOS | Laiko TOS |
|-------------|-------|-----------|
| POP | 0x0 | 0x7403 |
| GVAR | 0x140000 | 0x4c |
| UNBIND | 0x140000 | 0x140000 |
| GETBASEPTR-N | 0x140000 | 0x140000 |
| COPY | 0x4c | 0x0 |

Laiko's TOS values are completely wrong after POP and GVAR operations.

## 3. Root Cause Analysis

### Most Likely Source (1 of 7 analyzed)
**The VM state SP value is not being synchronized with the virtual memory stack.**

The stack push/pop operations work at the TOS (Top Of Stack) level but don't update the actual stack pointer in virtual memory.

### Supporting Evidence
- Laiko trace shows SP constant at 0x025D10 throughout execution
- C trace shows SP fluctuating 0x012e86 → 0x012e88 → 0x012e8a (2 DLwords per operation)
- Laiko appears to manage only TOS, not the underlying stack pointer

### Secondary Possibility
Memory access using wrong addresses for stack operations.

## 4. Investigation Required

Need to examine:
- `laiko/src/vm/stack.lisp` - push-stack and pop-stack functions
- `laiko/src/vm/vm.lisp` - VM state management
- Compare with C implementation in `maiko/src/` for correct SP handling

## 5. Next Steps

1. Fix SP synchronization in Laiko's stack implementation
2. Re-run parity check to verify fix
3. Identify next divergence (likely TJUMP1 opcode implementation)

## Files Referenced

- `laiko/src/vm/stack.lisp` - Stack operations (marked DEPRECATED)
- `laiko/load-emulator.lisp` - Laiko entry point
- `maiko/src/` - C reference implementation
- `scripts/check_parity_laiko.sh` - Parity comparison script