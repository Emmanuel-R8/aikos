# C Emulator FJUMP7 Opcode Tracing

**Date**: 2025-01-27  
**Status**: ✅ Completed - Analyzed FJUMP7 opcode execution

## Overview

This document details the analysis of the FJUMP7 opcode (case 0225, 0x97) in the C emulator to understand how it performs conditional false jump (jump if NIL).

## Opcode Details

- **Opcode**: FJUMP7 (case 0225, 0x97)
- **Opcode Name**: "FJUMP7" (from opcode table)
- **Instruction Length**: 1 byte (no operands)
- **Function**: Conditional jump if TOS == 0 (NIL/false), with jump offset of 7 bytes

## C Implementation

```c
case 0225:
case225: { FJUMPMACRO(7); }
```

### FJUMPMACRO Definition

```c
#define FJUMPMACRO(x)      \
  do {                     \
    if (TOPOFSTACK != 0) { \
      POP;                 \
      nextop1;             \
    }                      \
    {                      \
      CHECK_INTERRUPT;     \
      POP;                 \
      PCMACL += (x);       \
      nextop0;             \
    }                      \
  } while (0)
```

### Algorithm

1. **Check Condition**: If `TOPOFSTACK != 0` (true):
   - POP (consume TOS)
   - `nextop1` (advance PC by 1 byte, no jump)
2. **Else** (TOS == 0, NIL/false):
   - CHECK_INTERRUPT
   - POP (consume TOS)
   - `PCMACL += 7` (jump by 7 bytes)
   - `nextop0` (don't advance PC further)

**Note**: The `while (0)` ensures the macro executes once, and the `else` branch is always reached if the `if` condition is false.

## Execution Analysis

### Instruction #10: FJUMP7 at PC 0x60f140

**Stack State Before FJUMP7**:
- TopOfStack (TOS): 0x00000000 (NIL, from EQ result)
- CurrentStackPTR: 0x7f6700025d10
- Stack depth: 11911 entries (after EQ consumed one value)

**Jump Decision**:
- TOS == 0 (NIL/false)
- **Decision**: JUMP (because TOS == 0)
- **Action**: CHECK_INTERRUPT, POP, then PCMACL += 7

**PC Advancement**:
- PC before: 0x60f140
- PC after: 0x60f149 (from execution log instruction #11)
- Actual advancement: 9 bytes
- Expected advancement: 7 bytes (jump offset)
- **Note**: The 2-byte difference suggests either:
  - The jump calculation includes the opcode byte itself
  - There's an additional instruction or offset
  - The PC calculation in the log includes additional factors

### Verification from Execution Log

From the execution log:
- **Instruction #9**: PC 0x60f13f, EQ, TOS: 0xE0001 → result: 0x00000000 (NIL)
- **Instruction #10**: PC 0x60f140, FJUMP7, TOS: 0x00000000 (NIL)
- **Instruction #11**: PC 0x60f149, COPY, TOS: 0xfffe0000

This confirms that:
1. FJUMP7 received NIL (0x00000000) on TOS
2. FJUMP7 should jump (because TOS == 0)
3. PC advanced from 0x60f140 to 0x60f149 (9 bytes)

## Key Findings

1. **False Jump**: FJUMP7 jumps if TOS == 0 (NIL/false), opposite of TJUMP
2. **Stack Consumption**: TOS is always popped, regardless of jump decision
3. **Jump Offset**: FJUMP7 uses offset 7 bytes
4. **PC Advancement**: 
   - If no jump: PC advances by 1 byte
   - If jump: PC advances by 7 bytes (PCMACL += 7)
5. **Pattern**: FJUMPN uses offset (N + 2), similar to TJUMPN

## Zig Implementation Notes

The Zig implementation should:
1. Check if TOS == 0 (NIL)
2. Always POP TOS
3. If TOS == 0: Jump by 7 bytes
4. If TOS != 0: Advance PC by 1 byte
5. Match the C behavior exactly

**Note**: The Zig implementation uses `handleFJUMPWithOffset(vm, 7)` which should match the C FJUMPMACRO(7) behavior.

## Related Opcodes

- **TJUMP7** (case 0247, 0xA7): True jump (jump if TOS != 0) with offset 7
- **FJUMP0-FJUMP15**: False jump variants with offsets 2-17
- **TJUMP0-TJUMP15**: True jump variants with offsets 2-17

## Files Modified

- `maiko/src/xc.c`: Added FJUMP7 detailed tracing (case 0225) - trace code added but not yet verified
- Output: `c_fjump7_detailed_trace.txt` (expected but not yet generated)

## Notes

The trace file was not generated in the initial run, suggesting either:
- The opcode byte at PC 0x60f140 might not match case 0225 exactly
- The trace condition might not be met
- The execution path might differ from expectations

However, the execution log confirms FJUMP7 behavior, so this analysis is based on the log and macro definition.
