#!/usr/bin/env python3
"""
Move markdown files to deprecated/ directory with deprecation notices.
"""

import shutil
from pathlib import Path
from datetime import datetime

DEPRECATION_NOTICE = f"""---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: {datetime.now().strftime('%Y-%m-%d')}
**Replacement**: See `documentation/` directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in `documentation/`.
---

"""

def add_deprecation_notice(file_path: Path):
    """Add deprecation notice to the beginning of a file."""
    content = file_path.read_text()
    if not content.startswith('---'):
        file_path.write_text(DEPRECATION_NOTICE + content)

def main():
    deprecated_dir = Path('deprecated/.ai_assistant_db')
    deprecated_dir.mkdir(parents=True, exist_ok=True)

    source_dir = Path('.ai_assistant_db')

    # Copy entire directory structure
    for md_file in source_dir.rglob('*.md'):
        # Get relative path
        rel_path = md_file.relative_to(source_dir)
        dest_path = deprecated_dir / rel_path

        # Create parent directories
        dest_path.parent.mkdir(parents=True, exist_ok=True)

        # Copy file
        shutil.copy2(md_file, dest_path)

        # Add deprecation notice
        add_deprecation_notice(dest_path)

        print(f"Moved {rel_path} -> deprecated/documentation/{rel_path}")

    # Create README in deprecated
    readme_content = f"""# Deprecated Documentation

This directory contains deprecated markdown documentation files that have been replaced by Typst format documentation.

**Date Deprecated**: {datetime.now().strftime('%Y-%m-%d')}

## Replacement

All documentation has been converted to Typst format and is located in the `documentation/` directory.

## Purpose

These files are kept for:
- Historical reference
- Migration verification
- Cross-reference checking

## Status

**DO NOT UPDATE THESE FILES** - All updates should be made to Typst files in `documentation/`.

"""

    (deprecated_dir.parent / 'README.md').write_text(readme_content)
    print(f"\nCreated deprecated/README.md")

if __name__ == '__main__':
    main()
