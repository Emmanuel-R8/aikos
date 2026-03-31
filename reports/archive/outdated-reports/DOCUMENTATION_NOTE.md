# CRITICAL: Documentation Location Change

**Date**: 2025-12-22 09:28

## Important Notice

**`.ai_assistant_db/` is DEPRECATED and should NOT be used.**

All documentation has been migrated to **`documentation/`** directory using **Typst format** (`.typ` files).

## Documentation Structure

- **`documentation/specifications/`** - Emulator-independent specifications (Typst format)
- **`documentation/implementations/`** - Language-specific implementation notes (Typst format)
- **`documentation/medley/`** - Medley Interlisp documentation (Typst format)
- **`documentation/reference/`** - Reference materials (Typst format)

## For AI Assistants

**NEVER update `.ai_assistant_db/`** - it is deprecated.

**ALWAYS update `documentation/`** - this is the current, authoritative documentation location.

When making documentation updates:
1. Find the corresponding `.typ` file in `documentation/`
2. Update the Typst-formatted file
3. Follow Typst syntax (not Markdown)

## Generating PDF

To generate the PDF documentation:

```bash
cd documentation
typst compile main.typ maiko-documentation.pdf
```

## Migration Status

✅ All documentation has been migrated from `.ai_assistant_db/` to `documentation/`
✅ Documentation is now in Typst format for PDF generation
✅ Cross-references and indexing system in plac