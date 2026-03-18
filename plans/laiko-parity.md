# Implementation Plan: Laiko Parity & Full Execution

**Date**: 2026-02-27
**Status**: In Progress

## Problem Statement
The Laiko emulator (Common Lisp) has reached a state where it can load `starter.sysout` and execute the initialization sequence. Opcode collisions have been resolved, and the stack architecture has been modernized to use virtual memory, matching the C implementation. The immediate goal is to achieve instruction-level parity with the C reference (Maiko) and enable interactive execution (REPL).

## Goal
Achieve verified instruction-level parity with Maiko for the `starter.sysout` initialization sequence, and implement necessary I/O opcodes to reach an interactive Lisp prompt.

## Strategy
1.  **Parity Verification**: Systematically compare Laiko's execution trace against Maiko's trace to identify and fix divergence.
2.  **Core Opcode Implementation**: Implement missing opcodes required for I/O and control flow (SUBRCALL, basic I/O).
3.  **Graphics & Interaction**: Implement SDL3 display backend and input handling.

## Tasks

### Phase 1: Parity Verification
- [ ] **Generate Reference Trace**: Run Maiko with `starter.sysout` to generate a golden trace (`c_emulator_unified_trace.txt`).
- [ ] **Generate Laiko Trace**: Run Laiko with tracing enabled (`laiko_unified_trace.txt`).
- [ ] **Compare Traces**: Use `scripts/compare_unified_traces.awk` or `scripts/analyze_execution_divergence.py` to find the first divergence.
- [ ] **Fix Divergence**: Debug and fix the specific opcode or logic causing the divergence.
- [ ] **Repeat**: Iteratively fix divergences until initialization sequence matches.

### Phase 2: Core Functionality (I/O & Control)
- [ ] **Implement SUBRCALL (0x10)**: This is crucial for I/O.
    - [ ] Map subr numbers to Lisp helper functions.
    - [ ] Implement `subr-read-char` connected to stdin.
    - [ ] Implement `subr-write-char` connected to stdout.
- [ ] **Investigate REPL Loop**: Determine why execution exits. Does `starter.sysout` expect a specific return value to continue?
- [ ] **Implement Missing Misc Opcodes**: Resolve stubs in `laiko/src/vm/op-misc.lisp` (e.g., `RAID`, `RETURN`).

### Phase 3: Graphics & Input
- [ ] **Implement BitBLT**: Complete `pilotbitblt` opcode with full raster op support.
- [ ] **Implement SDL3 Backend**: Connect `op-graphics.lisp` to `cl-sdl3` (or CFFI bindings).
- [ ] **Implement Input**: Keyboard and mouse event handling.

### Phase 4: Advanced Features
- [ ] **Floating Point**: Implement floating point opcodes (currently stubs in Zaiko too).
- [ ] **GC Integration**: Ensure Lisp objects are properly pinned/managed.

## Notes
- **Trace Format**: Ensure Laiko's trace format matches Maiko's exactly (PC, opcode, operands, TOS, SP, FP).
- **Subroutines**: Maiko uses specific subroutines for I/O. These need to be emulated faithfully.
- **Reference**: Consult `maiko/src/` for exact semantics of opcodes and subroutines.
