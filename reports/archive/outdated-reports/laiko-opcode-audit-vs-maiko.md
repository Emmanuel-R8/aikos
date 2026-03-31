# Laiko vs Maiko Opcode Audit

**Date**: 2026-02-26  
**Reference**: `maiko/inc/opcodes.h` (canonical enum)

## Summary

- **Fixed**: NIL, T, CONST_0 (opc_0), CONST_1 (opc_1) in Laiko and in documentation to match C.
- **Remaining**: Other files may have further mismatches; see below.

## Fixes Applied

### 1. `laiko/src/vm/op-stack.lisp`

| Opcode | Was (Laiko) | C (opcodes.h) | Fixed to |
|--------|-------------|---------------|----------|
| nil    | #x69        | opc_NIL = 104 = 0x68 | #x68 |
| t      | #x6A        | opc_T = 105 = 0x69   | #x69 |
| const-0| #x6B        | opc_0 = 106 = 0x6A   | #x6A |
| const-1| #x6C        | opc_1 = 107 = 0x6B   | #x6B |

C uses `case 0150:` (octal) for NIL in `maiko/src/xc.c`; 0150 octal = 104 decimal = 0x68 hex. The comment there saying "0xA8" is incorrect; the canonical value is from `opcodes.h`.

### 2. `documentation/specifications/instruction-set/opcodes.typ`

- Constants line: NIL (0xA8) → NIL (0x68). T, CONST_0, CONST_1 were already correct (0x69, 0x6A, 0x6B).

## Remaining Discrepancies (for follow-up)

These were not changed in this pass; Laiko currently uses different byte values than C for these opcodes.

### Comparison range 0x3A–0x3F

From `opcodes.h`: EQL=58(0x3A), DRAWLINE=59(0x3B), STORE_N=60(0x3C), COPY_N=61(0x3D), RAID=62(0x3E), SLRETURN=63(0x3F).

- Laiko `eq #x3A` matches C EQL (0x3A).
- Laiko `eql #x3B`: C uses 0x3B for DRAWLINE (already in op-graphics.lisp). So 0x3B is duplicated (eql vs drawline).
- Laiko `lessp #x3C`, `greaterp #x3D`, `leq #x3E`, `geq #x3F`: C uses 0x3C–0x3F for STORE_N, COPY_N, RAID, SLRETURN.
- Laiko `equal` and `numequal` both at #x3D: C has EQUAL=244(0xF4), GREATERP=243(0xF3). No NUMEQUAL in C enum.
- Laiko `igreaterp #xF1` matches C IGREATERP=241(0xF1).

So comparison opcodes 0x3B–0x3F and equal/greaterp need to be aligned with C (and with actual bytecode usage) in a later pass.

### Logic / shift range 0xE0–0xE7

From `opcodes.h`: LLSH1=224(0xE0), LLSH8=225(0xE1), LRSH1=226(0xE2), LRSH8=227(0xE3), LOGOR2=228(0xE4), LOGAND2=229(0xE5), LOGXOR2=230(0xE6), LSH=231(0xE7). Float ops: FPLUS2=232(0xE8), FDIFFERENCE=233(0xE9), FTIMES2=234(0xEA), FQUOTIENT=235(0xEB).

- Laiko uses 0xE0–0xE3 for logand, logior, logxor, lognot; C uses 0xE0–0xE3 for LLSH1, LLSH8, LRSH1, LRSH8.
- Laiko uses 0xE4–0xE7 for llsh1, llsh8, lrsh1, lrsh8; C uses 0xE4–0xE7 for LOGOR2, LOGAND2, LOGXOR2, LSH.
- Laiko uses 0xE8–0xEA for logor2, logand2, logxor2; C uses 0xE8–0xEB for FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT (and Laiko op-misc.lisp already has fplus2 #xE8, fdifference #xE9, ftimes2 #xEA, fquotient #xEB).
- Laiko `lsh #xEC`: C has LSH=231(0xE7). So lsh should be 0xE7, not 0xEC.
- Laiko hiloc #xD2, loloc #xD3 match C HILOC=210(0xD2), LOLOC=211(0xD3).

So op-logic.lisp 0xE0–0xE7 and lsh 0xEC need to be aligned with C (and float ops in op-misc left as-is).

## Verification

After the constants fix, load through `op-stack.lisp` succeeds and "OP-STACK LOADED OK" is printed. No change to other op-*.lisp files in this pass beyond the stack constants.
