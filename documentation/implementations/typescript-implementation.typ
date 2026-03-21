= TypeScript Implementation: Taiko Emulator

*Navigation*: Implementations README | Main README

*Date*: 2026-03-21 20:00 *Status*: ACTIVE - Bun-based parity work in progress *Location*: `taiko/` *Build
System*: TypeScript/Bun *Target Platform*: Browser/WebGL plus Bun-based CLI and tests

== Overview

The TypeScript implementation (Taiko) provides a browser-based emulator implementation using WebGL
for display rendering. The implementation also has a Bun-based CLI and Bun test suite used for
parity work. Current work is focused on making Taiko follow Maiko's address and frame semantics
instead of relying on an overly flat byte-address model.

== Current Status

=== ✅ Confirmed

- ✅ TypeScript source code exists in `taiko/src/`
- ✅ Bun is the execution and test runtime (`bun test`, `bun run`)
- ✅ Unified trace format module exists (`taiko/src/vm/trace.ts`)
- ✅ VM core structure exists
- ✅ Opcode handlers defined
- ✅ WebGL renderer implemented
- ✅ Sysout loading module exists
- ✅ Memory/address helper layer now distinguishes Lisp-world and stack-space conversions
- ✅ `CURRENTFX` bookkeeping is now tracked explicitly in VM state for frame-relative calculations
- ✅ Bun test suite currently passes for the edited memory/frame slice

=== 🔍 Needs Verification

- Trace generation capability
- Execution parity with C reference
- Opcode implementation completeness
- Browser environment requirements
- Sysout startup parity against the real C entry path

== Implementation Structure

=== Core Modules

- `taiko/src/vm/vm.ts` - VM core
- `taiko/src/vm/memory/address.ts` - Shared LispPTR and stack-space address conversions
- `taiko/src/vm/memory/frame.ts` - `CURRENTFX`, `PVAR`, and `IVAR` helpers
- `taiko/src/vm/execution.ts` - Execution engine
- `taiko/src/vm/trace.ts` - Unified trace format
- `taiko/src/vm/dispatch/` - Opcode dispatch
- `taiko/src/vm/opcodes/` - Opcode handlers
- `taiko/src/io/sysout.ts` - Sysout loading
- `taiko/src/display/webgl.ts` - WebGL renderer

== Trace Format

The TypeScript implementation supports the unified trace format as defined in
`documentation/specifications/vm-core/trace-and-logging-formats.typ`.

Format:
`LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES`

== Testing Status

*Status*: Active Bun-based validation

- Current Bun status after the startup/sysout slice: `59 pass, 1 skip, 0 fail`
- The newly stabilized area is frame-relative address calculation:
  - shared stack offset <-> byte helpers
  - explicit `CURRENTFX` tracking in VM state
  - `PVAR` derived from frame base plus `FRAMESIZE`
  - `IVAR` derived from BF / `nextblock` stack offsets

== Recent TypeScript-Specific Findings

=== 2026-03-21 19:49 - Memory and Frame Addressing Slice

The Taiko implementation benefited from separating three categories that had started to blur together:

- LispPTR values: DLword offsets from `Lisp_world`
- Stack offsets: DLword offsets from `Stackspace`
- Cached VM runtime locations: byte offsets after translation

For TypeScript specifically, the safest pattern is:

1. Keep explicit helper functions for stack offset <-> byte conversion.
2. Track `CURRENTFX` directly in VM state once the current frame is known.
3. Derive `PVAR` and `IVAR` from frame data instead of inferring the frame from `PVAR` alone.

This reduces the risk of factor-of-two and wrong-base bugs when working across sysout loading,
initialization, function entry, return handling, and tracing.

=== 2026-03-21 19:57 - Function Entry / Return Test Slice

The next Taiko slice used the explicit frame bookkeeping to stabilize function-call tests under Bun.

TypeScript-specific outcomes:

- `RETURN` now prefers the explicitly tracked current frame offset when available.
- `PVAR` restoration is derived from the frame base rather than decoding an ad hoc marker word.
- The cached return value is preserved during return-path restoration instead of being immediately overwritten by a premature memory re-sync.
- The Bun test suite now exercises `FN0` and `RETURN` directly as instruction handlers, which is useful while byte-level decoder parity and sysout startup parity are still being tightened.

=== 2026-03-21 20:00 - Sysout Fixture and Loader Validation Slice

The next Taiko slice focused on turning skipped sysout tests into real Bun coverage without widening scope unnecessarily.

TypeScript-specific outcomes:

- synthetic sysout fixtures are now written in big-endian form to match the file format that `loadSysout()` actually parses
- FPtoVP test fixtures were repositioned so table data does not overlap synthetic file pages
- sysout assertions now validate loader-visible bytes and computed offsets instead of assuming host-endian fixture writes
- this slice improved coverage without requiring a production loader rewrite

=== 2026-03-21 20:15 - Real Sysout Startup Restoration Slice

The next Taiko slice tightened startup parity against Maiko's `start_lisp()` / `FastRetCALL` path for `starter.sysout`.

TypeScript-specific outcomes:

- `initializeVM()` now restores `PC` using the saved frame `pc` field directly relative to `FuncObj`, instead of re-adding `startpc`
- sparse-stack initialization now writes a valid free-stack-block header with a 16-bit bounded size field
- entry-point discovery now reads TypeScript function-header candidates using the same field order as Maiko (`stkmin`, `na`, `pv`, `startpc`)
- the real-sysout Bun test now validates the actual runtime startup path (`loadSysout()` plus `initializeVM()`), not just the provisional loader hint values
- `starter.sysout` now initializes to a non-zero `PC` under the Bun test environment

== Related Documentation

- Trace Format: `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Parity Divergences: `documentation/implementations/parity-divergences.typ`
- Opcode Coverage: `documentation/implementations/opcode-coverage-matrix.typ`

== Next Steps

1. Continue opcode and runtime parity work under Bun now that real sysout startup no longer blocks initialization
2. Re-run parity comparisons against the C reference as each slice lands
3. Reduce startup-path heuristics by replacing entry-point guessing with more direct sysout/runtime evidence where available
4. Extend real-sysout validation beyond initialization into the first executed instruction stream

*Last Updated*: 2026-03-21 20:15
