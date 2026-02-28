# Laiko Opcode Implementation Priorities (Instruction-by-Instruction Focus)

**Date**: 2026-02-26 08:35
**Scope**: Laiko (Common Lisp) implementation only

## Coverage and context

- Laiko currently has ~190 opcode handlers registered (out of 256), per `*opcode-metadata*` and prior audits.
- Some categories are still stubbed or incomplete:
  - Floating point ops (`FPLUS2`, `FDIFFERENCE`, `FTIMES2`, `FQUOTIENT`) – use `DECODE-FLOAT-POINTER` / `ENCODE-FLOAT-POINTER`, not yet implemented.
  - Graphics and BitBLT (`PILOTBITBLT`, `DRAWLINE`, subroutines, display I/O).
  - Various VM/data helpers (Valspace, atom cells, memory access) are present but not yet wired into full execution.
- The C implementation in `maiko/src/` remains the canonical reference for semantics and control flow.

For instruction-by-instruction work, the priority is driven by **where the first divergence occurs** when running starter.sysout with a small `EMULATOR_MAX_STEPS`, rather than by raw opcode number alone.

## Tier 1 – Early-executed core opcodes (highest priority)

These opcodes are executed very early in starter.sysout traces and are essential for basic execution:

- **Stack and constants**: `POP`, `COPY`, `NIL`, `T`, `CONST_0`, `CONST_1`, `ACONST`, `SIC`, `SNIC`, `SICX`, `GCONST`.
- **Variable access**:
  - `IVAR0–IVAR6`, `PVAR0–PVAR6`, `FVAR0–FVAR6`, `GVAR`, `ARG0`, `MYARGCOUNT`, `PVARSETPOP0–PVARSETPOP6`.
- **Control flow**:
  - `RETURN`, `FN0–FN4`, `FNX`, `APPLYFN`.
  - `JUMP0–JUMP15`, `JUMPX`, `FJUMP0–FJUMP15`, `FJUMPX`, `TJUMP0–TJUMP15`, `TJUMPX`.
  - `BIND`, `UNBIND`, `UNWIND`.
- **Base/memory access appearing early in traces**:
  - `GETBASEPTR-N`, `GETBASE-N`, `PUTBASE-N`, `PUTBASEPTR-N`, `GETBASEBYTE`, `PUTBASEBYTE`.

**Plan**:

1. Use `scripts/compare_emulator_execution.sh --with-laiko` with a small `EMULATOR_MAX_STEPS` (e.g., 32–64) to find the _first_ divergence `(PC, opcode)`.
2. If the opcode is in the above list, focus on:
   - The Laiko `DEFOP` handler (`laiko/src/vm/op-*.lisp`).
   - Any supporting VM accessors (`stack.lisp`, `data/*.lisp`).
   - Matching C behavior at that PC using C traces and `maiko/src/...`.
3. Add a minimal Laiko test under `laiko/tests/` that constructs a small VM state and executes just that opcode, verifying the same stack/memory effect as C.
4. Repeat until the first divergence moves beyond this Tier 1 set.

## Tier 2 – Data and memory structure opcodes

Once Tier 1 opcodes behave correctly for the initial trace window, the next group to prioritize:

- **List operations**: `CAR`, `CDR`, `CONS`, `RPLACA`, `RPLACD`, `CREATECELL`, `RPLCONS`, `NTH`, `NTHCDR`, `LAST`, `LISTLENGTH`, `APPEND`, `REVERSE`, `ASSOC`, `FMEMB`, `LISTGET`.
- **Memory and arrays**: `AREF1`, `AREF2`, `ASET1`, `ASET2`, `GETAEL1`, `GETAEL2`, `SETAEL1`, `SETAEL2`.
- **General arithmetic and comparison**:
  - `IPLUS2`, `IDIFFERENCE`, `ITIMES2`, `IQUO`, `IREM`, `IMINUS`, `IDIVIDE`, `IMOD`.
  - `PLUS2`, `DIFFERENCE`, `TIMES2`, `QUOTIENT`.
  - `EQ`, `EQL`, `EQUAL`, `LESSP`, `GREATERP`, `LEQ`, `GEQ`, `IGREATERP`, `NUMEQUAL`, `CL_EQUAL`.
- **Bitwise and shifts**: `LOGAND`, `LOGIOR`, `LOGXOR`, `LOGNOT`, `LOGOR2`, `LOGAND2`, `LOGXOR2`, `LSH`, `LLSH1`, `LLSH8`, `LRSH1`, `LRSH8`.

**Plan**:

- Continue stepping C vs Laiko with increasing `EMULATOR_MAX_STEPS`, recording the first divergence at each stage.
- When the divergence opcode is in this Tier 2 set:
  - Implement or fix the Laiko handler semantics using C as reference.
  - Add focused tests (e.g., list length, aref/aset round-trips, arithmetic edge cases).

## Tier 3 – Floating point, graphics, and advanced I/O

These are important for completeness but can wait until the instruction-by-instruction baseline is stable:

- **Floating point arithmetic**:
  - `FPLUS2`, `FDIFFERENCE`, `FTIMES2`, `FQUOTIENT` – depend on `DECODE-FLOAT-POINTER` / `ENCODE-FLOAT-POINTER` and the float representation decisions in Laiko.
- **Graphics and display**:
  - `PILOTBITBLT`, `DRAWLINE`, `SUBRCALL`, and BitBLT subroutines.
  - Display pixel operations, BitBLT variants, and SDL3 integration.
- **I/O subroutines**:
  - Stream open/close, character I/O subrs used by graphics and console paths.

**Plan**:

- Defer these until the core instruction stream (Tiers 1–2) can be run for a substantial `EMULATOR_MAX_STEPS` without divergence.
- When you begin this tier, ensure:
  - The float representation is documented clearly in `documentation/implementations/lisp-implementation.typ`.
  - The graphics and I/O behavior matches the C implementation’s expectations for BitBLT, window size, and basic input events.

## How to use this document

- For each instruction-by-instruction debugging session:
  1. Run the unified trace comparison for a chosen `EMULATOR_MAX_STEPS`.
  2. Identify the first differing opcode.
  3. Locate it in the tiers above and treat the divergence as a single focused task.
  4. Update test coverage and (if needed) documentation for any new behavior you uncover.
- Re-run coverage (`maiko-lisp.vm:report-opcode-coverage`) periodically to ensure that newly implemented handlers are correctly registered in `*opcode-metadata*`.
