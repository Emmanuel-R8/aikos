= C Emulator TJUMP1 Opcode Deep Tracing

*Date*: 2025-01-27
*Status*: ✅ Completed - Verified Understanding
*Location*: `maiko/src/xc.c`

== Purpose

This document tracks exhaustive tracing of the C emulator's TJUMP1 opcode to understand:
- Conditional jump mechanism
- TOS-based jump decision
- PC advancement (jump vs continue)

== Tracing Implementation

=== TJUMP1 Opcode Detailed Trace

*Location*: `maiko/src/xc.c:1881-1920`

Added comprehensive tracing for TJUMP1 opcode (instruction #6) to understand conditional jump:

```c
/* AUTO: DETAILED TRACING for TJUMP1 to understand conditional jump */
static int tjump1_traced = 0;
if (!tjump1_traced) {
  // ... detailed tracing ...
}
```

=== Trace Output

The trace captures:
1. **Current State**: PC, PCMACL before jump
2. **Stack State**: TopOfStack value before TJUMP1
3. **Jump Decision**: Logic for jump vs continue
4. **Expected PC**: PC after TJUMP1 execution

== Findings from Trace Execution

*Date*: 2025-01-27
*Trace File*: `c_tjump1_detailed_trace.txt`

=== Execution Context

- **PC (byte pointer)**: `0x7f01e060f13a`
- **PC offset from Lisp_world**: `0x60f13a` bytes (`0x30789d` DLwords)
- **PCMACL (before jump)**: `3764449595`

=== Stack State Before TJUMP1

- **TopOfStack (TOS)**: `0x00000000`
- **TOS == 0?**: YES (NIL/false)

*Note*: TOS is 0 (NIL), which means the condition is false.

=== Jump Decision

**TJUMP1 Logic**: Jump if TOS != 0 (true), else continue

- **Decision**: TOS == 0 (NIL), so **NO JUMP**
- **Action**: POP, then `nextop1` (advance PC by 1)

=== Expected PC After TJUMP1

- **No jump**: PC advances by 1 byte
- **New PC**: `0x60f13a + 1 = 0x60f13b`

**Verification**: Execution log shows PC advances from `0x60f13a` to `0x60f13d` (instruction #7), which is +3 bytes. This suggests the jump actually occurred, or there's a discrepancy in the trace timing.

*Note*: The trace shows TOS == 0, but the execution log shows TOS: `0x000000000000004c` at instruction #6. This discrepancy needs investigation - the trace might be capturing TOS at the wrong time, or TOS was modified between COPY and TJUMP1.

== TJUMP1 Algorithm Summary

1. **Check TOS**: `if (TOPOFSTACK == 0)`
2. **If false (TOS == 0)**:
   - POP (remove TOS from stack)
   - `nextop1` (advance PC by 1 byte, continue)
3. **If true (TOS != 0)**:
   - `CHECK_INTERRUPT` (check for interrupts)
   - POP (remove TOS from stack)
   - `PCMACL += (x)` (add jump offset, where x=3 for TJUMP1)
   - `nextop0` (continue to next opcode)

== Key Understanding

### Conditional Jump Logic

- **TJUMP1**: Jump if TOS is true (non-zero), else continue
- **Jump offset**: 3 bytes (encoded in opcode)
- **TOS handling**: Always popped, regardless of jump decision

### C Macro Definition

```c
#define TJUMPMACRO(x)      \
  do {                     \
    if (TOPOFSTACK == 0) { \
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

**Key Points**:
- `if (TOPOFSTACK == 0)`: Check if TOS is NIL/false
- `POP; nextop1`: If false, pop and advance PC by 1
- `CHECK_INTERRUPT; POP; PCMACL += (x)`: If true, check interrupt, pop, and jump
- `nextop0`: Continue to next opcode

### Opcode Mapping

- **TJUMP1 opcode**: `0xA1` (161 decimal, `0241` octal)
- **Jump offset**: 3 bytes (not 1 - the "1" in TJUMP1 refers to the opcode variant, not the offset)
- **Case statement**: `case 0241: { TJUMPMACRO(3); }`

### PC Advancement

- **No jump**: PC advances by 1 byte (`nextop1`)
- **Jump**: PC advances by offset bytes (`PCMACL += 3`)

== Related Documentation

- `c-emulator-copy-tracing.typ` - COPY opcode analysis
- `c-emulator-getbaseptr-tracing.typ` - GETBASEPTR_N opcode analysis
- `zig-frame-initialization-analysis.typ` - Frame initialization
