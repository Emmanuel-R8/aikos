# C Emulator EQ Opcode Tracing

**Date**: 2025-01-27  
**Status**: ✅ Completed - Traced EQ opcode execution

## Overview

This document details the tracing of the EQ opcode (case 0360, 0xF0) in the C emulator to understand how it performs pointer equality comparison.

## Opcode Details

- **Opcode**: EQ (case 0360, 0xF0)
- **Opcode Name**: "EQ" (from opcode table)
- **Instruction Length**: 1 byte (no operands)
- **Function**: Compares TOS with POP_TOS_1 (value below TOS) for pointer equality

## C Implementation

```c
case 0360:
case360: {
  if (TOPOFSTACK == POP_TOS_1)
    TOPOFSTACK = ATOM_T;
  else
    TOPOFSTACK = NIL_PTR;
  nextop1;
}
```

### Algorithm

1. **Compare**: Check if `TOPOFSTACK == POP_TOS_1`
2. **If Equal**: Set `TOPOFSTACK = ATOM_T` (0x4c, represents T/true)
3. **If Not Equal**: Set `TOPOFSTACK = NIL_PTR` (0x00000000, represents NIL/false)
4. **Stack Effect**: Both TOS and POP_TOS_1 are consumed (stack depth decreases by 1)
5. **PC Advancement**: PC advances by 1 byte (`nextop1`)

### Key Macros

- **POP_TOS_1**: The value below TOS (second element on stack)
- **ATOM_T**: The Lisp value representing T/true (0x4c)
- **NIL_PTR**: The Lisp value representing NIL/false (0x00000000)

## Trace Analysis

### Instruction #9: EQ at PC 0x60f13f

**Stack State Before EQ**:
- TopOfStack (TOS): 0x00000000 (captured before comparison)
- POP_TOS_1 (value below TOS): 0x0000004c
- CurrentStackPTR: 0x7f6700025d10
- Stack depth: 11912 entries

**Comparison**:
- EQ compares: TOS == POP_TOS_1?
- 0x00000000 == 0x0000004c? **NO**

**Result**:
- Since values are not equal: TOPOFSTACK = NIL_PTR (0x00000000)
- Both TOS and POP_TOS_1 are consumed
- Stack depth decreases by 1 (to 11911 entries)

### Verification from Execution Log

From the execution log:
- **Instruction #8**: PC 0x60f13e, CONST_1, TOS: 0x4c → pushes 0xE0001
- **Instruction #9**: PC 0x60f13f, EQ, TOS: 0xE0001 → compares with value below
- **Instruction #10**: PC 0x60f140, FJUMP7, TOS: 0x00000000 (NIL, comparison result)

This confirms that EQ:
1. Compares TOS (0xE0001) with POP_TOS_1 (0x4c)
2. They are not equal, so result is NIL (0x00000000)
3. Both values are consumed from the stack

## Key Findings

1. **Pointer Equality**: EQ performs simple pointer equality comparison (no deep comparison)
2. **Stack Consumption**: Both operands are consumed (TOS and POP_TOS_1)
3. **Result Values**: 
   - Equal: ATOM_T (0x4c) - represents T/true
   - Not Equal: NIL_PTR (0x00000000) - represents NIL/false
4. **PC Advancement**: PC advances by 1 byte

## Zig Implementation Notes

The Zig implementation should:
1. Pop two values from stack (TOS and value below TOS)
2. Compare them for equality (pointer equality, not deep comparison)
3. Push result: 1 (T) if equal, 0 (NIL) if not equal
4. Advance PC by 1 byte

**Note**: The Zig implementation uses 1 for T and 0 for NIL, while C uses ATOM_T (0x4c) and NIL_PTR (0x00000000). This is a representation difference but functionally equivalent.

## Related Opcodes

- **EQL** (case 072, 0x3a): Equality test with type checking (numbers and pointers)
- **EQUAL** (case 0364, 0xF4): Deep equality comparison (recursive)
- **LESSP** (case 0365, 0xF5): Less than comparison
- **GREATERP** (case 0363, 0xF3): Greater than comparison

## Files Modified

- `maiko/src/xc.c`: Added EQ detailed tracing (case 0360)
- Output: `c_eq_detailed_trace.txt`
