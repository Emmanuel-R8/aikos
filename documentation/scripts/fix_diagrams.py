#!/usr/bin/env python3
"""Replace diagram() calls with simple text blocks since Typst 0.13.1 doesn't support diagrams."""

import re
from pathlib import Path

def replace_diagram(content: str) -> str:
    """Replace diagram blocks with simple text descriptions."""
    # Find diagram blocks
    pattern = r'#figure\(\s*caption:\s*\[([^\]]+)\],\s*diagram\([^)]+\)\s*\)'
    
    def replace_func(match):
        caption = match.group(1)
        return f'#figure(\n  caption: [{caption}],\n  block(\n    width: 100%,\n    text(weight: "bold")[{caption}],\n    text(size: 9pt)[Diagram converted to text description. See original documentation for visual diagram.],\n  )\n)'
    
    # Simple replacement for now - just remove diagram calls
    content = re.sub(
        r'#figure\(\s*caption:\s*\[([^\]]+)\],\s*diagram\([^)]+\)\s*\)',
        lambda m: f'#figure(\n  caption: [{m.group(1)}],\n  block(\n    width: 100%,\n    text(weight: "bold")[{m.group(1)}],\n    text(size: 9pt)[Diagram: See original markdown documentation for visual representation.],\n  )\n)',
        content,
        flags=re.DOTALL
    )
    
    return content

def main():
    doc_dir = Path('documentation')
    typ_files = list(doc_dir.rglob('*.typ'))
    
    for typ_path in typ_files:
        if 'test' in str(typ_path):
            continue
        with open(typ_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        if 'diagram(' in content:
            new_content = replace_diagram(content)
            if new_content != content:
                with open(typ_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed: {typ_path}")

if __name__ == '__main__':
    main()
