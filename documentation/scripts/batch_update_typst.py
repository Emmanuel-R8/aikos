#!/usr/bin/env python3
"""
Batch update all Typst files from markdown sources.
"""

import subprocess
import sys
from pathlib import Path

def get_all_md_files():
    """Get all markdown files from documentation."""
    md_files = []
    for md_path in Path('documentation').rglob('*.md'):
        md_files.append(md_path)
    return sorted(md_files)

def main():
    script_path = Path('documentation/update_typst_from_md.py')
    md_files = get_all_md_files()
    
    print(f"Found {len(md_files)} markdown files to update")
    
    updated = 0
    errors = []
    
    for md_file in md_files:
        try:
            result = subprocess.run(
                [sys.executable, str(script_path), str(md_file)],
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                updated += 1
                if updated % 10 == 0:
                    print(f"Updated {updated}/{len(md_files)} files...")
            else:
                errors.append((md_file, result.stderr))
        except Exception as e:
            errors.append((md_file, str(e)))
    
    print(f"\nCompleted: {updated}/{len(md_files)} files updated")
    if errors:
        print(f"\nErrors ({len(errors)}):")
        for md_file, error in errors[:10]:
            print(f"  {md_file}: {error}")

if __name__ == '__main__':
    main()
