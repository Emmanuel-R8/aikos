# Interlisp

Multi-implementation repository for the **Maiko** virtual machine: the bytecode interpreter and runtime that runs [Medley Interlisp](https://interlisp.org/medley/). This repo contains the canonical C implementation (Maiko), reimplementations in Zig (Zaiko), Common Lisp (Laiko), and TypeScript (Taiko), plus shared documentation, specs, and parity tooling.

## Overview

- **Maiko (C)** — Production reference implementation; ~95% opcode coverage, SDL2/SDL3 display, full execution with Medley sysouts.
- **Zaiko (Zig)** — Alternative implementation in progress (~60–70% actual coverage); parity testing and trace comparison supported.
- **Laiko (Common Lisp)** — SBCL implementation in progress; VM core, trace, and parity harness aligned with C.
- **Taiko (TypeScript)** — Browser-based emulator (WebGL); targets full parity with C.

All implementations aim to execute the same bytecode and sysout format; the C codebase in `maiko/` is the reference for behavior and for cross-implementation parity work.

## Repository structure

| Path | Description |
|------|-------------|
| `maiko/` | C implementation (production). Build with CMake or via `medley/scripts/build/`. |
| `maiko_untouched/` | Historical C baseline (submodule, read-only). |
| `zaiko/` | Zig implementation. `zig build`; run with optional `EMULATOR_MAX_STEPS`. |
| `laiko/` | Common Lisp implementation (SBCL, ASDF). `./run.sh <sysout>`. |
| `taiko/` | TypeScript/browser implementation. `bun run dev` / `bun run build`. |
| `medley/` | Medley Interlisp system (submodule); provides sysouts and build scripts. |
| `documentation/` | Typst specs and implementation notes. `documentation/README.md` for layout. |
| `specs/` | Task/spec artifacts (e.g. multi-impl parity, build system). |
| `reports/` | Status reports, parity analyses, audits (markdown). |
| `scripts/` | Parity and comparison scripts (e.g. `compare_emulator_execution.sh`). |

## Quick start

### C (Maiko) — reference

```bash
# Build (from repo root)
./medley/scripts/build/build-c-emulator.sh --display-backend sdl --build-system cmake --force

# Run
./maiko/build-c-linux.x86_64/lde ./medley/internal/loadups/starter.sysout
```

Or from `maiko/`: `cmake . && make` (produces `ldesdl`).

### Zig (Zaiko)

```bash
cd zaiko
zig build
# Optional: limit steps for parity testing
EMULATOR_MAX_STEPS=100 ./zig-out/bin/zaiko ../medley/internal/loadups/starter.sysout
```

### Common Lisp (Laiko)

```bash
cd laiko
./run.sh ../medley/internal/loadups/starter.sysout
# Or: sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)" -- -sysout path/to/sysout
```

### TypeScript (Taiko)

```bash
cd taiko
bun install && bun run dev
# Open browser; load a sysout via the UI.
```

## Parity testing

Execution traces can be compared across implementations to find divergences from the C reference.

- **Canonical script**: `scripts/compare_emulator_execution.sh`  
  - Runs C and Zig (and optionally Laiko with `--with-laiko`) for a fixed step count and compares unified traces.
- **Step cap**: Set `EMULATOR_MAX_STEPS=N` (or unset/0 to run to completion).
- **Sysout**: Default is `medley/internal/loadups/starter.sysout`; pass another path as the last argument.
- **Unified trace format**: Single-line, pipe-delimited; see `documentation/specifications/vm-core/trace-and-logging-formats.typ` and `scripts/compare_unified_traces.py` / `scripts/compare_unified_traces.awk`.

Example:

```bash
EMULATOR_MAX_STEPS=500 ./scripts/compare_emulator_execution.sh --with-laiko
```

## Documentation

- **Layout and PDF**: `documentation/README.md` — structure, `typst compile`, index generation.
- **Specifications**: `documentation/specifications/` — emulator-independent specs (instruction set, VM core, memory, trace format, etc.).
- **Implementations**: `documentation/implementations/` — language-specific notes (C, Zig, Lisp, TypeScript).
- **Policy and debugging**:  
  - `documentation/core/critical-memory.typ` — documentation rules, commit checklist.  
  - `documentation/core/critical-debugging-technique.typ` — systematic parity/debug workflow.
- **Project rules for contributors and AI**: `AGENTS.md` — must-read for development, submodules, and reporting.

## Implementation status (summary)

| Implementation | Status | Notes |
|----------------|--------|--------|
| **C (maiko/)** | Production | ~95% opcode coverage; primary reference. |
| **Zig (zaiko/)** | In progress | ~60–70% coverage; many TODOs; parity harness in use. |
| **Lisp (laiko/)** | In progress | VM core, trace, GVAR/atom work; parity tests and comparison script. |
| **TypeScript (taiko/)** | In progress | Browser; targeting C parity. |

Detailed status: `reports/CURRENT_STATUS.md` and implementation-specific READMEs under `maiko/`, `zaiko/`, `laiko/`, `taiko/`.

## Submodules

- `maiko_untouched/` — Historical C baseline; read-only.
- `medley/` — Medley Interlisp (sysouts, build scripts). See [Interlisp/medley](https://github.com/Interlisp/medley).

Clone with submodules:

```bash
git clone --recurse-submodules <repo-url>
# or
git submodule update --init --recursive
```

## License

Maiko (C) is under the license in `maiko/LICENSE`. Other components follow the same terms unless stated otherwise; see per-directory READMEs and license files.
