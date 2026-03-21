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

=== 2026-03-21 20:35 - First Starter Instruction Slice

The next Taiko slice closed the gap between "startup initializes" and "startup lands on a real first instruction".

TypeScript-specific outcomes:

- synthetic startup entry-point discovery now seeds `CURRENTFX->pc` with `startpc + 1`, matching Maiko's ordinary `FNx` entry convention
- the real `starter.sysout` startup path now lands on a decodable opcode instead of on function-header data
- the real-sysout Bun test now checks that `decodeInstructionFromMemory(vm, vm.pc)` succeeds after `initializeVM()`
- this gives Taiko a stable first instruction boundary for the next parity slice

=== 2026-03-21 20:48 - Sysout File-Order Parsing Slice

The next Taiko slice corrected a deeper sysout-loading mismatch: Taiko was still mixing file-order parsing with host-side BYTESWAP assumptions.

TypeScript-specific outcomes:

- IFPAGE parsing now follows the logical on-disk field order for direct big-endian file reads, which restores meaningful early startup fields such as `currentfxp`
- FPtoVP loading no longer applies an extra 32-bit byte swap after `DataView` has already decoded big-endian entries
- the focused sysout tests now cover file-order IFPAGE parsing explicitly
- this moved Taiko from a partially synthetic startup path to loading the real saved stack frame and real page map from `starter.sysout`
- the next startup blocker is now narrower and better scoped: saved-frame / free-stack-block interpretation during `initializeVM()`

=== 2026-03-21 20:52 - Saved Startup Frame Restoration Slice

The next Taiko slice stopped treating the real startup frame as if it were the older split-pointer layout.

TypeScript-specific outcomes:

- `initializeVM()` now reads the saved startup frame's function-header pointer as a full 32-bit LispPTR
- free-stack-block validation now reads the loaded on-stack header as two big-endian DLwords, matching the real saved bytes
- Taiko now restores the same startup `PC` as the C emulator from `starter.sysout` (`0x60f130`)
- the remaining blocker is no longer startup-frame reconstruction; it is the bytecode fetch / decode mismatch at that now-correct restored PC

=== 2026-03-21 21:17 - Startup Decode Alignment Slice

The next Taiko slice resolved the byte-orientation mismatch between loaded sysout pages and the decoder.

TypeScript-specific outcomes:

- `loadVirtualMemory()` now stores each sysout 32-bit word in host-order memory, matching Maiko's post-`word_swap_page()` layout instead of preserving raw file byte order
- startup-frame and free-stack-block DLword fields are now read with the same `address ^ 2` rule that Maiko uses under `GETWORD`
- the real `starter.sysout` path now restores the C startup frame, lands at `PC = 0x60f130`, and decodes the same first opcode as C (`POP`, `0xBF`)
- the real-sysout Bun regression is now hard rather than advisory: it requires successful initialization, the expected startup `PC`, and a decodable first instruction
- this narrows the next parity slice from "can Taiko decode startup?" to "does Taiko execute the first startup instruction sequence like C?"

=== 2026-03-21 22:50 - Startup Execution Progress Slice

The next Taiko slice advanced from "first opcode decodes" to "the first startup instructions execute in the same order as C."

TypeScript-specific outcomes:

- `GVAR` now follows the current `BIGATOMS`/`BIGVM` operand-width rule on the real startup path, so the restored `starter.sysout` stream advances from `0x60f131` to `0x60f136` instead of desynchronizing after one byte
- the real-sysout regression now checks the first executed startup slice, not just the first decoded opcode: `POP` at `0x60f130`, `GVAR`, then `UNBIND` at `0x60f136`, then `GETBASEPTR_N` at `0x60f137`
- `UNBIND` now walks backward from `CSTKPTRL` through the restored evaluation stack, uses `PVAR` as the restoration base, and preserves cached `TOPOFSTACK` instead of clobbering it eagerly
- Taiko now survives the old startup `UNBIND` frontier and reaches a later control-flow divergence after 8 startup steps (`PC = 0xfffe`)
- the next parity slice is therefore no longer about sysout decoding or binding markers; it is about the later startup execution semantics that drive Taiko off the real control-flow path

== Related Documentation

- Trace Format: `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Parity Divergences: `documentation/implementations/parity-divergences.typ`
- Opcode Coverage: `documentation/implementations/opcode-coverage-matrix.typ`

== Next Steps

1. Compare Taiko's later startup control-flow steps against the C reference trace
2. Re-run parity comparisons against the C reference as each slice lands
3. Reduce startup-path heuristics by replacing entry-point guessing with more direct sysout/runtime evidence where available
4. Extend real-sysout validation from "first matched execution steps" to a longer matched startup prefix

*Last Updated*: 2026-03-21 22:50
