= Documentation Improvement Proposal

*Date*: 2026-01-21 09:11
*Status*: Proposal for review
*Purpose*: Restructure `documentation/` into a coherent, buildable Typst corpus with clear spec vs implementation separation and minimal duplication.

== 1. Executive Summary

The `documentation/` tree has grown from a Markdown-to-Typst conversion into a mixed, partly redundant structure. This proposal addresses:

1. *Scattered notes*: many files are ad-hoc notes or session logs rather than structured chapters.
2. *Misplaced content*: implementation-independent specs live under `implementations/`; the obsolete `rewrite-spec/` path is referenced but does not exist (the real tree is `specifications/`).
3. *Incomplete main build*: `main.typ` omits core policy docs, debugging technique, trace-format specs, several data-structure and vm-core files, and the implementations README. Some included paths may be broken (e.g. `sysout-format.typ` vs `sysout-format-overview.typ`).
4. *Duplication and drift*: `core/` vs `reference/` (glossary, index); `CRITICAL_DEBUGGING_TECHNIQUE.typ` at root vs `core/critical-debugging-technique.typ`; `specifications/vm-core/execution-trace.typ` vs `implementations/unified-trace-format-specification.typ` and `unified-logging-format.typ`.
5. *Mixed formats and build cruft*: `.md`, `.typ`, `.pdf` and Python scripts live next to narrative content; `test-code.typ`, `test-diagram.typ` are scratch.

Recommended direction: (a) align all references with `specifications/` and fix broken includes; (b) move implementation-independent specs out of `implementations/` into `specifications/`; (c) consolidate glossaries, indices, and critical policy into a single authoritative location and wire them into `main.typ`; (d) group implementation session notes under `implementations/` subdirs and exclude them from the main book; (e) isolate build/scratch assets and mixed formats.

== 2. Current State

=== 2.1 Layout (abbreviated)

#codeblock(lang: "text", [
documentation/
├── main.typ                      # Single entrypoint for PDF
├── README.md, CONVERSION_STATUS.md
├── CRITICAL_DEBUGGING_TECHNIQUE.typ   # Full methodology (not in main)
├── core/
│   ├── introduction.typ, architecture.typ, build-system.typ   # in main
│   ├── critical-memory.typ, critical-debugging-technique.typ  # not in main
│   ├── contributing.typ, glossary.typ, index.typ, readme.typ  # not in main; overlap with reference/
├── components/                  # vm-core, memory-management, display, io — in main
├── specifications/              # Emulator-independent specs (in main, partially)
│   ├── data-structures/         # cons-cells, arrays, function-headers, number-types,
│   │   # sysout-*, atom-table, sysout-format.typ, README — main includes subset
│   ├── vm-core/                 # execution-model, stack-management, function-calls,
│   │   # interrupt-handling, execution-trace, type-checking — main omits execution-trace, type-checking
│   ├── instruction-set/, memory/, display/, io/, platform-abstraction/, validation/
│   ├── README.typ, quickstart.typ, INDEX.typ
│   ├── COMPLETENESS.typ, CONTRIBUTING.typ, DOCUMENTATION_REVIEW.typ
│   └── SOURCE_CODE_MAPPING*.typ
├── implementations/             # 50+ files
│   ├── zig-implementation.typ, lisp-implementation.typ   # in main
│   ├── README.typ               # not in main
│   ├── unified-trace-format-specification.typ, unified-logging-format.typ   # spec-level, in impl
│   ├── centralized-memory-management-design.typ         # design with Zig examples; portable idea
│   ├── c-emulator-*.typ (22), zig-*.typ (18+)           # session/tracing notes, not in main
│   ├── *.md, *.pdf                                       # mixed formats
├── medley/                      # architecture, components, interface, platform — in main
├── reference/
│   ├── glossary.typ, api.typ, index.typ   # in main
│   └── overview.typ             # not in main
└── *.py, test-code.typ, test-diagram.typ  # build/scratch
])

=== 2.2 Broken or Obsolete References

- *`rewrite-spec/`*: Referenced in `core/critical-memory.typ` and 17 other files. The directory does not exist; the actual emulator-independent tree is `specifications/`.
- *`.md in specs*: Several specs (e.g. `DOCUMENTATION_REVIEW.typ`, `SOURCE_CODE_MAPPING*.typ`, `specifications/INDEX.typ`, `quickstart.typ`, `vm-core/function-calls.typ`) point at `data-structures/cons-cells.md`, `sysout-format.md`, `rewrite-spec/data-structures/...`. The real files are `.typ` under `specifications/data-structures/`.
- *`CRITICAL_DEBUGGING_TECHNIQUE.md`*: `core/critical-memory.typ` links to `.md`; the file is `CRITICAL_DEBUGGING_TECHNIQUE.typ` at `documentation/` root.
- *`core/index.typ`*: Describes an old layout (INDEX.md, rewrite-spec/, .md) and is stale; `main.typ` uses `reference/index.typ`.

=== 2.3 Implementation-Independent Content in `implementations/`

- *`unified-trace-format-specification.typ`*: Defines a single-line, pipe-delimited trace format for C/Zig comparison. It is a specification of a debugging/tooling format, not Zig- or C-specific.
- *`unified-logging-format.typ`*: Similar; logging format for emulator comparison.
- *`centralized-memory-management-design.typ`*: Describes a shared design for address translation, FPtoVP, endianness; uses Zig snippets but the *design* is implementation-agnostic.
- *`specifications/vm-core/execution-trace.typ`*: Another trace-format description (column-based). Overlaps in purpose with the two above; the formats differ.

=== 2.4 Duplication

- *Glossary*: `core/glossary.typ` and `reference/glossary.typ` are largely the same; `main.typ` includes only `reference/glossary.typ`.
- *Index*: `core/index.typ` is legacy; `reference/index.typ` is the generated, in-use index.
- *Critical debugging*: `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ` (full Part I–III) vs `core/critical-debugging-technique.typ` (focused “value analysis” technique). AGENTS.md points at the root file. The core one reads like an extract or summary.

=== 2.5 Not in `main.typ`

- `core/critical-memory.typ`, `core/critical-debugging-technique.typ`, `core/contributing.typ`, `core/readme.typ`, `core/glossary.typ`, `core/index.typ`
- `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`
- `specifications/quickstart.typ`, `specifications/INDEX.typ`, `specifications/COMPLETENESS.typ`, `specifications/CONTRIBUTING.typ`, `specifications/DOCUMENTATION_REVIEW.typ`, `specifications/SOURCE_CODE_MAPPING*.typ`
- `specifications/data-structures/atom-table.typ`, `specifications/data-structures/README.typ`, `specifications/data-structures/sysout-format.typ` (distinct from sysout-format-overview, -fptovp, -loading)
- `specifications/vm-core/execution-trace.typ`, `specifications/vm-core/type-checking.typ`
- `reference/overview.typ`
- `implementations/README.typ`
- All `implementations/` session and tracing notes (by design, but they need a clear home and optional “Implementation Notes” appendix or similar).

== 3. Proposed Changes

=== 3.1 Reference and Path Fixes (High Priority)

1. *Replace `rewrite-spec/` with `specifications/`* everywhere (e.g. `core/critical-memory.typ`, `SOURCE_CODE_MAPPING*.typ`, `medley/interface/file-formats.typ`, `medley/components/sysout.typ`, `implementations/*.typ`, `reference/index.typ`, `specifications/quickstart.typ`).
2. *Update `.md` → `.typ`* in internal links (e.g. `data-structures/cons-cells.md` → `data-structures/cons-cells.typ`, `sysout-format.md` → the appropriate `sysout-format*.typ` or `data-structures/sysout-format.typ`).
3. *Fix `CRITICAL_DEBUGGING_TECHNIQUE`*: In `core/critical-memory.typ`, change `CRITICAL_DEBUGGING_TECHNIQUE.md` to `../CRITICAL_DEBUGGING_TECHNIQUE.typ` (or the chosen canonical location after consolidation).

=== 3.2 Move Implementation-Independent Specs to `specifications/`

1. *Trace and logging formats*
   - Create or reuse `specifications/vm-core/` or `specifications/debugging/` (new) for a single *Trace and Logging Format* spec.
   - Merge or clearly relate: `specifications/vm-core/execution-trace.typ`, `implementations/unified-trace-format-specification.typ`, `implementations/unified-logging-format.typ`. Prefer one canonical format (e.g. the unified pipe-delimited one) and relegate the column-based and C-specific variants to “Legacy / alternate” or move to implementation notes.
   - After merge, remove or redirect the two implementation files.

2. *Centralized memory design*
   - Move `implementations/centralized-memory-management-design.typ` to `specifications/memory/centralized-memory-design.typ` (or `design/` under `memory/`). Optionally replace Zig snippets with pseudocode; if kept, label as “Reference implementation in Zig” so the design stays spec-level.

=== 3.3 Consolidate Glossary, Index, and Critical Policy

1. *Glossary*: Keep `reference/glossary.typ` as the single glossary. Remove or redirect `core/glossary.typ` (e.g. `#include "../reference/glossary.typ"` if something still includes it, or delete).
2. *Index*: Keep `reference/index.typ` as the generated index. Archive or delete `core/index.typ`; if useful, extract any unique high-level structure into `documentation/README.typ` or `core/readme.typ` and drop the rest.
3. *Critical policy and debugging*
   - Keep `core/critical-memory.typ` as the policy (date format, code docs, emulator-independent vs implementation-specific, commit checklist). Ensure it references `specifications/` and the canonical debugging doc.
   - Choose one canonical *Critical Debugging Technique*:
     - *Option A*: Keep `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ` as canonical; treat `core/critical-debugging-technique.typ` as an excerpt — either `#include` it from the main doc as a “Value analysis” section or merge that section in and remove `core/critical-debugging-technique.typ`.
     - *Option B*: Move the root file into `core/critical-debugging-technique.typ` and delete the root copy; update AGENTS.md and `critical-memory.typ` accordingly.
   - Add both `core/critical-memory.typ` and the chosen critical-debugging doc to `main.typ` (e.g. under a “Project and documentation policy” or “Debugging and validation” section near the start or before Implementations).

=== 3.4 Wire `main.typ` to Existing and Moved Specs

1. *Data structures*: Include `specifications/data-structures/atom-table.typ` and `specifications/data-structures/README.typ` if they add content not already in the other sysout/atom docs. Decide whether `sysout-format.typ` is redundant with `sysout-format-overview.typ` (and -fptovp, -loading); if not, add the appropriate one(s) to `main.typ`.
2. *VM core*: Include `specifications/vm-core/execution-trace.typ` (or the merged trace spec) and `specifications/vm-core/type-checking.typ` in the VM Core Specifications section.
3. *Reference*: Include `reference/overview.typ` in the Reference section (or merge into `reference/api.typ` and remove `overview.typ`).
4. *Implementations*: Include `implementations/README.typ` as an “Implementations overview” before or as part of the Zig/Lisp sections.
5. *Specifications meta*: Add `specifications/quickstart.typ` to the Specifications section. Decide whether `COMPLETENESS`, `DOCUMENTATION_REVIEW`, `CONTRIBUTING`, `SOURCE_CODE_MAPPING*` belong in the main book or in a “For contributors” / “Meta” appendix; if yes, add them in an ordered way.

=== 3.5 Implementations: Organize Session Notes and Mixed Formats

1. *Subdirs under `implementations/`*
   - `implementations/c/` — C emulator analysis and tracing notes: move `c-emulator-*.typ` here.
   - `implementations/zig/` — Zig-specific notes: move `zig-*.typ`, `zig-*.md` here (except `zig-implementation.typ`, which stays as the head).
   - Keep `zig-implementation.typ`, `lisp-implementation.typ`, and `implementations/README.typ` at `implementations/` top level.
2. *Mixed formats*
   - Convert or remove `*.md` under `implementations/` (e.g. `critical-runtime-issue-stack-initialization.md`, `execution-comparison-analysis.md`, `zig-jumpx-divergence.md`, `zig-mylink-overflow-fix.md`, `zig-parity-debugging-success.md`, `zig-vs-c-execution-comparison.md`). Prefer `.typ` for narrative; if they are obsolete, move to `documentation/archive/` or delete.
   - Treat `*.pdf` as generated: do not commit; add to `.gitignore` if not already. Keep only the `.typ` (or source) that generates them.
3. *main.typ*: Do not include the `c/` and `zig/` session notes. The main book should reference only the implementation “head” docs and the README; the README can link to the subdirs for “further notes”.

=== 3.6 Build and Scratch Hygiene

1. *Scripts and generated*: Move `batch_update_typst.py`, `convert_md_to_typst.py`, `convert_mermaid_to_typst.py`, `fix_diagrams.py`, `generate_index.py`, `move_to_deprecated.py`, `update_typst_from_md.py` into a `documentation/scripts/` (or `tools/`) directory. Adjust `generate_index.py` and any wiring so it still updates `reference/index.typ` from the desired roots.
2. *Scratch*: Move `test-code.typ`, `test-diagram.typ` to `documentation/scratch/` or delete if unused.
3. *Status and README*: `CONVERSION_STATUS.md` is meta; move to `documentation/scripts/` or `documentation/meta/`. `README.md` stays at `documentation/` root; refresh it to match the new layout and `main.typ` (and remove references to `rewrite-spec`).

=== 3.7 Optional: Medley and specifications links

- In `medley/interface/file-formats.typ` and `medley/components/sysout.typ`, replace `../rewrite-spec/data-structures/sysout-format.md` (and similar) with `../specifications/data-structures/sysout-format-overview.typ` or the chosen canonical sysout spec.

== 4. Resulting `main.typ` Shape (Target)

Suggested order (conceptually):

#codeblock(lang: "text", [
1. Introduction
2. Architecture Overview
3. Build System
4. Components (VM Core, Memory, Display, I/O)
5. *Project and documentation policy* (NEW)
   - core/critical-memory.typ
   - CRITICAL_DEBUGGING_TECHNIQUE.typ (or core/critical-debugging-technique.typ if consolidated there)
6. Specifications
   - README, quickstart
   - Instruction set, VM core (incl. execution-trace, type-checking), Memory (incl. centralized-memory-design if moved)
   - Data structures (incl. atom-table, README; resolve sysout-format*)
   - Display, I/O, platform-abstraction, validation
   - (Optional) Meta: COMPLETENESS, DOCUMENTATION_REVIEW, CONTRIBUTING, SOURCE_CODE_MAPPING* in appendix
7. Implementations
   - implementations/README.typ
   - zig-implementation.typ, lisp-implementation.typ
8. Medley Interlisp (unchanged)
9. Reference
   - overview.typ (or merged into api)
   - glossary.typ, api.typ, index.typ
])

== 5. File and Reference Checklist

- [ ] Replace `rewrite-spec` → `specifications` in all 18 files.
- [ ] Fix `.md` → `.typ` in `DOCUMENTATION_REVIEW.typ`, `SOURCE_CODE_MAPPING*.typ`, `quickstart.typ`, `INDEX.typ`, `vm-core/function-calls.typ`, `medley/` and `implementations/` as needed.
- [ ] Fix `CRITICAL_DEBUGGING_TECHNIQUE.md` → `CRITICAL_DEBUGGING_TECHNIQUE.typ` in `core/critical-memory.typ`.
- [ ] Move `unified-trace-format-specification.typ`, `unified-logging-format.typ` into `specifications/`; merge with `execution-trace.typ` or clearly supersede; remove duplicates.
- [ ] Move `centralized-memory-management-design.typ` → `specifications/memory/centralized-memory-design.typ`.
- [ ] Retire `core/glossary.typ`, `core/index.typ`; keep `reference/glossary.typ`, `reference/index.typ`.
- [ ] Consolidate `CRITICAL_DEBUGGING_TECHNIQUE.typ` and `core/critical-debugging-technique.typ`; update AGENTS.md and critical-memory.
- [ ] Add to `main.typ`: critical-memory, critical-debugging, implementations/README, quickstart, execution-trace, type-checking, atom-table, data-structures/README, reference/overview (or merge), and optionally COMPLETENESS, DOCUMENTATION_REVIEW, CONTRIBUTING, SOURCE_CODE_MAPPING*.
- [ ] Create `implementations/c/`, `implementations/zig/`; move session/tracing and zig-specific notes; fix README.typ links.
- [ ] Convert or archive `implementations/*.md`; stop tracking `*.pdf` in git.
- [ ] Move `*.py` to `documentation/scripts/`; move or delete `test-code.typ`, `test-diagram.typ`; move `CONVERSION_STATUS.md`; refresh `README.md`.

== 6. Risks and Minimization

- *Broken includes*: After moving files, `#include` and any `@`-links in Typst must be updated. Run a full `typst compile main.typ` and fix missing modules.
- *generate_index.py*: Must still see the new `specifications/` and `implementations/` layout so `reference/index.typ` stays accurate.
- *AGENTS.md*: Update paths for `CRITICAL_DEBUGGING_TECHNIQUE.typ` and `critical_memory.typ` if they move.

== 7. Suggested Phasing

- *Phase 1*: Path and reference fixes (3.1), plus add critical-memory and critical-debugging to `main.typ` (3.3). Low risk, high clarity.
- *Phase 2*: Move implementation-independent specs (3.2), consolidate glossary/index (3.3), and extend `main.typ` (3.4). Enables a single, consistent spec tree.
- *Phase 3*: Implementations reorg (3.5), build/scratch hygiene (3.6), and medley/spec link updates (3.7). Cleans up without changing the main narrative.

— *End of proposal*
