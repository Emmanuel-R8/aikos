= C Emulator UNBIND Opcode Deep Tracing

*Date*: 2025-01-27
*Status*: ✅ Completed - Verified Understanding
*Location*: `maiko/src/xc.c`

== Purpose

This document tracks exhaustive tracing of the C emulator's UNBIND opcode to understand:
- Stack unwinding mechanism
- Marker value format and extraction
- PVAR calculation and variable unbinding
- Stack pointer manipulation

== Tracing Implementation

=== UNBIND Opcode Detailed Trace

*Location*: `maiko/src/xc.c:1262-1342`

Added comprehensive tracing for UNBIND opcode (instruction #3) to understand stack unwinding:

```c
/* AUTO: DETAILED TRACING for UNBIND to understand stack unwinding */
static int unbind_traced = 0;
if (!unbind_traced) {
  // ... detailed tracing ...
}
```

=== Trace Output

The trace captures:
1. **Current State**: PC, CurrentStackPTR pointers and offsets
2. **Stack State**: Top 10 stack entries before UNBIND
3. **Marker Search**: Walking backwards through stack to find marker
4. **Marker Analysis**: Extracting num and offset from marker value
5. **PVAR Calculation**: Computing ppvar pointer
6. **Values to be Unbound**: Showing which variables will be restored to unbound

== Findings from Trace Execution

*Date*: 2025-01-27
*Trace File*: `c_unbind_detailed_trace.txt`

=== Execution Context

- **PC (byte pointer)**: `0x7f71f860f136`
- **PC offset from Lisp_world**: `0x60f136` bytes (`0x30789b` DLwords)
- **CurrentStackPTR**: `0x7f71f8025d10`
- **CurrentStackPTR offset from Lisp_world**: `0x25d10` bytes
- **Stack depth**: 11912 entries

=== Stack State Before UNBIND

Top 10 stack entries (from CurrentStackPTR backwards):
```
[0] = 0x0000000e (14)
[1] = 0x000efffe (983038)
[2] = 0xfffe0002 (-131070) (MARKER - negative) ← Found marker
[3] = 0x0002fffe (196606)
[4] = 0xfffe0000 (-131072) (MARKER - negative)
[5] = 0x0000006d (109)
[6] = 0x006de388 (7201672)
[7] = 0xe3882eb4 (-477614412) (MARKER - negative)
[8] = 0x2eb405a5 (783549861)
[9] = 0x05a50000 (94699520)
```

=== Marker Search Process

**Algorithm**: Walk backwards through stack until finding a negative value (high bit set)

1. **Step 0**: `[0x7f71f8025d0e] = 0x000efffe` (positive, continue)
2. **Found**: `[0x7f71f8025d0c] = 0xfffe0002` (negative, this is the marker)

**Marker Value**: `0xfffe0002`

=== Marker Analysis

**Marker Format**:
- High 16 bits: `~(num)` where `num` is number of variables to unbind
- Low 16 bits: `offset << 1` where `offset` is the PVAR offset

**Extraction**:
- `num = ~(value >> 16) = ~(0xfffe) = 0x0001` (1 variable)
- `offset = GetLoWord(value) = 0x0002` (2 DLwords)

**Interpretation**:
- Will unbind **1 variable**
- Starting from PVAR offset **2 DLwords**

=== PVAR Calculation

**PVAR**: `0x7f71f8025cf8` (pointer to parameter variables in current frame)

**Calculation**:
- `PVAR + 2 DLwords = 0x7f71f8025cfc` (skip 2 DLwords)
- `ppvar = PVAR + 2 + offset = PVAR + 2 + 2 = 0x7f71f8025d00`
- `ppvar offset from Lisp_world`: `0x25d00` bytes

**Formula**: `ppvar = (LispPTR *)((DLword *)PVAR + 2 + offset)`

=== Values to be Unbound

**Will unbind 1 value starting from ppvar[0] backwards**:
- `ppvar[0] = [0x7f71f8025d00] = 0x2eb405a5` → will be set to `0xffffffff`

**Unbinding Process**:
```c
for (i = num; --i >= 0;) {
  *--ppvar = 0xffffffff;
}
```

This restores the variable to the "unbound" state (0xffffffff).

== UNBIND Algorithm Summary

1. **Search backwards** through stack until finding negative value (marker)
2. **Extract** `num` and `offset` from marker value
3. **Calculate** `ppvar = PVAR + 2 + offset`
4. **Unbind** `num` variables by setting them to `0xffffffff`, starting from `ppvar` and working backwards

== Key Understanding

### Marker Value Format

- **High 16 bits**: `~(num)` - inverted count of variables to unbind
- **Low 16 bits**: `offset << 1` - PVAR offset (stored as DLword offset, shifted left by 1)

### Stack Pointer Manipulation

- `CurrentStackPTR` is a `DLword *` (not `LispPTR *`)
- Stack entries are accessed via `GetLongWord()` to read 32-bit values
- Marker search decrements `CurrentStackPTR` until finding negative value

### PVAR Layout

- `PVAR` points to parameter variables in current frame
- `PVAR + 2` skips first 2 DLwords (frame header overhead)
- `offset` is added to get the specific variable location

== Zig Implementation Verification

*Status*: ✅ Completed - UNBIND executes past instruction #3

The Zig implementation in `zaiko/src/vm/opcodes/binding.zig` should match the C behavior:
1. Walk backwards through stack to find marker
2. Extract num and offset from marker
3. Calculate ppvar using DLword pointer arithmetic
4. Unbind num variables by setting to 0xffffffff

**Key Fixes Applied**:
- Removed incorrect `>> 1` shift on offset (C uses GetLoWord directly)
- Fixed ppvar calculation to use DLword pointer arithmetic then cast to LispPTR*
- Fixed unbinding loop to decrement ppvar before each assignment

**Additional Fix (Critical, 2026-01-15)**:
- Match C’s marker search semantics exactly: `for (; (((int)*--CSTKPTRL) >= 0););`
  - Zig UNBIND must *decrement stack pointer first* and read the value from stack memory (not from cached `TopOfStack`)
- Handle potential unaligned access for `ppvar = (LispPTR *)((DLword *)PVAR + 2 + offset)`
  - Zig uses `[*]align(1) LispPTR` for `ppvar` and `current_ppvar` to avoid alignment traps

== Related Documentation

- `c-emulator-address-xor-tracing.typ` - XOR addressing mechanism
- `zig-frame-initialization-analysis.typ` - Frame initialization
- `unified-logging-format.typ` - Unified log format specification
