#!/usr/bin/env python3
"""
Update all path references from old paths to new paths.
"""

import re
from pathlib import Path

def update_file(file_path, replacements):
    """Update a file with path replacements."""
    try:
        content = file_path.read_text()
        original = content
        
        for old_path, new_path in replacements:
            content = content.replace(old_path, new_path)
        
        if content != original:
            file_path.write_text(content)
            return True
    except Exception as e:
        print(f"  Error updating {file_path}: {e}")
    return False

def main():
    repo_root = Path('.')
    
    # Define replacements
    replacements = [
        ('zaiko', 'zaiko'),
        ('laiko', 'laiko'),
        ('alternatives/zig', 'zaiko'),  # Relative paths
        ('alternatives/lisp', 'laiko'),  # Relative paths
    ]
    
    # Files to update (exclude certain directories)
    exclude_dirs = {'.git', 'maiko', 'deprecated', 'zaiko', 'laiko', '__pycache__', 'documentation'}
    exclude_extensions = {'.pdf', '.png', '.jpg', '.jpeg', '.gif', '.zip', '.tar', '.gz'}
    
    updated_count = 0
    file_types = {
        '.typ': 0,
        '.md': 0,
        '.zig': 0,
        '.lisp': 0,
        '.sh': 0,
        '.nix': 0,
        'other': 0
    }
    
    print("Searching for files to update...")
    
    for file_path in repo_root.rglob('*'):
        # Skip excluded directories
        if any(part in exclude_dirs for part in file_path.parts):
            continue
        
        # Skip excluded extensions
        if file_path.suffix in exclude_extensions:
            continue
        
        # Skip directories
        if file_path.is_dir():
            continue
        
        # Update file
        if update_file(file_path, replacements):
            updated_count += 1
            ext = file_path.suffix
            if ext in file_types:
                file_types[ext] += 1
            else:
                file_types['other'] += 1
            if updated_count % 10 == 0:
                print(f"  Updated {updated_count} files...")
    
    print(f"\n✓ Updated {updated_count} files:")
    for ext, count in file_types.items():
        if count > 0:
            print(f"  {ext}: {count}")

if __name__ == '__main__':
    main()
