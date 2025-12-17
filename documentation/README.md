# Maiko Documentation - Typst Format

This directory contains the complete Maiko Virtual Machine documentation converted to Typst format for PDF generation.

## Structure

- `main.typ` - Main document that includes all sections
- `core/` - Core documentation (introduction, architecture, build system)
- `components/` - Component documentation (VM core, memory, display, I/O)
- `specifications/` - Emulator-independent specifications
- `implementations/` - Language-specific implementation notes
- `medley/` - Medley Interlisp documentation
- `reference/` - Reference materials (glossary, API, index)

## Generating PDF

To generate the PDF document:

```bash
cd documentation
typst compile main.typ maiko-documentation.pdf
```

## File Statistics

- **Total Files**: 101 Typst files
- **Total Lines**: ~43,000 lines
- **Largest File**: 475 lines (all files under 500 line limit)
- **Index**: Auto-generated from all content

## Conversion Status

✅ **Completed**:
- All 94 markdown files converted to Typst
- File splitting completed (sysout-format split into 3 files)
- Index generation system created
- Main document structure created

⚠️ **Needs Manual Review**:
- Mermaid diagrams (marked with TODO comments) - need conversion to Typst diagram syntax
- Tables (marked with TODO comments) - need conversion to Typst table syntax
- Some complex formatting may need adjustment

## Mermaid Diagrams

Files containing Mermaid diagrams that need manual conversion:
- `core/architecture.typ` - System architecture diagrams
- `components/vm-core.typ` - VM core diagrams
- `components/memory-management.typ` - Memory diagrams
- `components/display.typ` - Display diagrams
- `components/io.typ` - I/O diagrams
- `specifications/vm-core/execution-model.typ` - Execution diagrams
- `specifications/vm-core/stack-management.typ` - Stack diagrams
- `specifications/memory/memory-layout.typ` - Memory layout diagrams
- `specifications/memory/garbage-collection.typ` - GC diagrams
- And several others in medley/ and specifications/

## Index

The index is auto-generated and includes:
- Terms (from glossary)
- Functions (from code blocks)
- Opcodes (OP_* patterns)
- Concepts (key terms and types)

Run `python3 generate_index.py` to regenerate the index after making changes.

## Notes

- All files are kept under 500 lines as required
- Cross-references use Typst's native linking system
- Code blocks use Typst's `#codeblock` syntax
- Navigation sections from markdown have been removed (PDF has its own navigation)
