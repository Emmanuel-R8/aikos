# Laiko Parity Divergence Report #001

**Date**: 2026-03-01
**Phase**: Phase 1: Tier 1 Core Opcodes
**Status**: First divergence found

## Summary

Laiko (Common Lisp implementation) diverges from C (Maiko) at the very first instruction. The emulators start with fundamentally different stack state, causing Laiko to crash with "VM Error: Stack underflow" after 4 instructions, while C continues successfully.

## Divergence Point

- **PC**: 0x60F130 (both start at same PC)
- **Opcode**: POP (0xBF) - first instruction
- **Result**: Stack underflow error after instruction 3

## Detailed Comparison

### C Emulator (Maiko) - First 3 Instructions:

| Line | PC       | Opcode | SP        | FP        | TOS         | Status        |
|------|----------|--------|-----------|-----------|-------------|---------------|
| 0    | 0x60f130 | POP    | 0x012e8a  | 0x012e72  | 0x00000000  | OK            |
| 1    | 0x60f131 | GVAR   | 0x012e88  | 0x012e72  | 0x0000000e  | OK            |
| 2    | 0x60f136 | UNBIND | 0x012e8a  | 0x012e72  | 0x00140000  | OK            |

### Laiko - First 3 Instructions:

| Line | PC       | Opcode | SP        | FP        | TOS         | Status           |
|------|----------|--------|-----------|-----------|-------------|------------------|
| 0    | 0x60F130 | POP    | 0x025D10  | 0x000000  | 0x00007403  | Wrong SP/TOS     |
| 1    | 0x60F131 | GVAR   | 0x025D14  | 0x000000  | 0x00A0FFFF  | Stack corrupted  |
| 2    | 0x60F136 | UNBIND | 0x025D10  | 0x000000  | 0x00000000  | Stack underflow |

## Root Cause Analysis

### Issue 1: Stack Pointer Initialization

**Laiko initialization** (from main.lisp):
```lisp
(let* ((nextblock (maiko-lisp.data:fx-nextblock fx))
       (stackspace-offset maiko-lisp.data:+stackspace-byte-offset+)
       (current-stack-ptr (+ stackspace-offset (* nextblock 2) -4)))
  (setf (maiko-lisp.vm:vm-stack-ptr-offset vm) current-stack-ptr))
```

- FX->nextblock: 0x2E8A (11914 decimal)
- stackspace-offset: 0x18000 (98304 decimal) 
- Laiko SP calculation: 0x18000 + (11914 * 2) - 4 = 0x1DD10

**C initialization** (from VM state):
- C SP: 0x012e8a = 4842 decimal

The SP values are completely different:
- Laiko SP: 0x025D10
- C SP: 0x012e8a

### Issue 2: Frame Pointer Initialization

- **Laiko FP**: 0x000000 (not initialized)
- **C FP**: 0x012e72

### Issue 3: Top-of-Stack (TOS) Not Synchronized

Laiko initializes TOS from memory at startup:
```lisp
(setf (maiko-lisp.vm:vm-top-of-stack vm)
      (maiko-lisp.vm:vm-read-lispptr vm current-stack-ptr))
```

But after POP operations, the TOS is not being re-read from memory. The C implementation uses CSTKPTRL/TOPOFSTACK synchronization.

## Critical Differences

1. **SP initialization formula**: Laiko uses different calculation than C
2. **FP is zero in Laiko**: C has valid FP (0x012e72)
3. **TOS not re-read after operations**: Missing CSTKPTRL synchronization

## Recommended Fix Approach

1. **Review stack initialization** in `laiko/src/main.lisp`:
   - Compare with C implementation in `maiko/src/` for correct SP formula
   - The STK_OFFSET constant may need adjustment

2. **Initialize FP properly**:
   - FP should be set from IFPAGE or FX structure

3. **Add TOS synchronization**:
   - After POP, re-read TOS from memory location
   - Follow C's CSTKPTRL/TOPOFSTACK pattern

## Files to Modify

- `laiko/src/main.lisp` - VM initialization (initialize-vm-from-ifpage function)
- `laiko/src/vm/stack.lisp` - Stack operations (add TOS synchronization)

## References

- C reference: `maiko/src/` for correct SP/FP/TOS initialization
- Common pitfalls: See `documentation/core/critical-debugging-technique.typ`
- PC is byte offset, not DLword
- FPtoVP uses 512-byte pages
- CSTKPTRL/TOPOFSTACK must be re-read from memory after restore
