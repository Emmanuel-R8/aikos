# Maiko Documentation - Typst Format

This directory contains the complete Maiko Virtual Machine documentation in Typst format for PDF generation.

## Structure

- `main.typ` — Main document including all sections
- `core/` — Introduction, architecture, build system, documentation policy (`critical-memory.typ`)
- `components/` — VM core, memory, display, I/O
- `specifications/` — Emulator-independent specifications (instruction set, vm-core, memory, data-structures, display, io, platform-abstraction, validation)
- `implementations/` — Language-specific notes: `zig-implementation.typ`, `lisp-implementation.typ`, `README.typ`; detailed C and Zig notes in `c/` and `zig/`
- `reviews/` — Coverage reports and source-to-doc mapping (implementation-dependent)
- `medley/` — Medley Interlisp (architecture, components, interface, platform)
- `reference/` — Glossary, API overview, auto-generated index
- `scripts/` — Python and helper scripts (e.g. `generate_index.py`, `CONVERSION_STATUS.md`)
- `scratch/` — Scratch/test Typst files (not part of the main build)

## Generating PDF

```bash
cd documentation
typst compile main.typ maiko-documentation.pdf
```

Or with Nix: `nix-shell -p typst --run "typst compile main.typ maiko-documentation.pdf"`

## Index

The index is auto-generated. From the *project root*:

```bash
python3 documentation/scripts/generate_index.py
```

Output: `documentation/reference/index.typ`.

## Policy and Debugging

- `core/critical-memory.typ` — Documentation rules (date format, spec vs implementation, commit checklist)
- `CRITICAL_DEBUGGING_TECHNIQUE.typ` — Systematic debugging for parity and VM issues

## Notes

- Keep Typst files under 500 lines where possible
- Emulator-independent content belongs in `specifications/`; language- or project-specific notes in `implementations/` (and `implementations/c/`, `implementations/zig/`)
- Generated PDFs under `documentation/` are ignored by `.gitignore`; commit only Typst (and other source) files
