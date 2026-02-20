// GVAR Introspection Timing Documentation
// ========================================
//
// Date: 2026-02-20
// Purpose: Document the timing of introspection events for GVAR opcode
//
// == Key Finding: TOS Recording Convention ==
//
// The introspection module records TOS (Top of Stack) values AFTER the
// PREVIOUS instruction has executed, BEFORE the current instruction executes.
//
// This means:
// - Event N shows TOS value AFTER instruction N-1 executed
// - Event N shows TOS value BEFORE instruction N executes
//
// == Example: Atom 522 GVAR Execution ==
//
// Consider the following execution sequence:
//
// | Event ID | PC       | Opcode | Name | TOS      | Interpretation                    |
// |----------|----------|--------|------|----------|-----------------------------------|
// | 6        | 0x60F130 | 191    | POP  | 0x0      | TOS after previous instruction    |
// | 7        | 0x60F131 | 96     | GVAR | 0xE      | TOS after POP, BEFORE GVAR        |
// | 8        | 0x60F136 | 18     | UNBIND | 0x140000 | TOS after GVAR, BEFORE UNBIND   |
//
// The key insight:
// - Event 7 (GVAR) shows TOS=0xE, but this is NOT the value GVAR pushed
// - Event 8 (UNBIND) shows TOS=0x140000, which IS the value GVAR pushed
// - The gvar_executions table confirms: atom 522 value_read = 0x140000
//
// == Why This Matters ==
//
// When debugging atom value issues, do NOT assume the TOS value shown for
// a GVAR event is the value GVAR pushed. Instead:
//
// 1. Check the NEXT event's TOS value to see what GVAR actually pushed
// 2. Use the gvar_executions table for the actual value read
// 3. Remember: TOS is recorded BEFORE the current instruction executes
//
// == Implementation Details ==
//
// In xc.c, the introspection call happens at the start of the dispatch loop:
//
// ```c
// nextopcode:
//   // ... tracing code ...
//   log_global_execution_trace(opcode, pc_byte_offset, TOPOFSTACK, ...);
//   // ... then the opcode executes ...
// ```
//
// This means TOPOFSTACK reflects the state AFTER the previous instruction.
//
// == Cross-References ==
//
// - C implementation: maiko/src/xc.c (dispatch loop)
// - GVAR macro: maiko/inc/inlineC.h (lines 419-480)
// - Introspection schema: maiko/src/introspect/schema.sql
// - Debugging technique: documentation/core/critical-debugging-technique.typ
