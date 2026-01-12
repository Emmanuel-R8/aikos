#!/usr/bin/env python3
"""
Batch conversion script to convert markdown files to Typst format.
Handles basic conversions; complex elements (diagrams, tables) need manual review.
"""

import re
import sys
from pathlib import Path
from typing import List, Tuple

def convert_header(line: str) -> str:
    """Convert markdown headers to Typst headers."""
    line = line.rstrip()
    if line.startswith('# '):
        return f"= {line[2:].strip()}\n"
    elif line.startswith('## '):
        return f"== {line[3:].strip()}\n"
    elif line.startswith('### '):
        return f"=== {line[4:].strip()}\n"
    elif line.startswith('#### '):
        return f"==== {line[5:].strip()}\n"
    elif line.startswith('##### '):
        return f"===== {line[6:].strip()}\n"
    elif line.startswith('###### '):
        return f"====== {line[7:].strip()}\n"
    return line + '\n' if line else '\n'

def convert_bold(line: str) -> str:
    """Convert **bold** to *bold*."""
    # Handle **text** but not ***text*** (which might be bold+italic)
    line = re.sub(r'\*\*([^*]+?)\*\*', r'*\1*', line)
    return line

def convert_italic(line: str) -> str:
    """Convert *italic* (but not **bold**)."""
    # Only convert single * that aren't part of **
    # This is tricky, so we'll be conservative
    pass  # Typst uses _ for italic, but we'll keep * for now
    return line

def convert_link(line: str) -> str:
    """Convert [text](url) to text (remove links for PDF)."""
    # Remove markdown links, keep text
    line = re.sub(r'\[([^\]]+)\]\([^\)]+\)', r'\1', line)
    return line

def convert_inline_code(line: str) -> str:
    """Convert `code` - Typst uses same syntax."""
    return line

def convert_list_item(line: str) -> str:
    """Ensure list items are properly formatted."""
    stripped = line.lstrip()
    if stripped.startswith('- ') or stripped.startswith('* '):
        indent = len(line) - len(stripped)
        return ' ' * indent + stripped.replace('* ', '- ', 1) if stripped.startswith('* ') else line
    return line

def convert_table(lines: List[str], i: int) -> Tuple[str, int]:
    """Detect and mark tables for manual conversion."""
    if '|' in lines[i] and lines[i].strip().startswith('|'):
        result = ['// TODO: Convert table to Typst table syntax\n']
        result.append('// Original markdown table:\n')
        while i < len(lines) and ('|' in lines[i] or lines[i].strip() == ''):
            result.append('// ' + lines[i])
            i += 1
        result.append('\n')
        return ''.join(result), i
    return None, i

def convert_code_block(lines: List[str], i: int) -> Tuple[str, int]:
    """Convert code blocks to Typst codeblock."""
    if lines[i].strip().startswith('```'):
        lang_match = re.match(r'```(\w+)', lines[i].strip())
        lang = lang_match.group(1) if lang_match else 'text'
        result = [f'#codeblock(lang: "{lang}", [\n']
        i += 1
        code_lines = []
        while i < len(lines) and not lines[i].strip().startswith('```'):
            code_lines.append(lines[i])
            i += 1
        # Remove trailing newline from last line if present
        if code_lines and code_lines[-1].endswith('\n'):
            code_lines[-1] = code_lines[-1].rstrip('\n')
        result.append(''.join(code_lines))
        result.append('\n])\n')
        return ''.join(result), i + 1
    return None, i

def convert_mermaid_block(lines: List[str], i: int) -> Tuple[str, int]:
    """Detect mermaid blocks - mark for manual conversion."""
    if '```mermaid' in lines[i] or '```' in lines[i] and 'mermaid' in lines[i].lower():
        result = ['// TODO: Convert Mermaid diagram to Typst diagram syntax\n']
        result.append('// Original Mermaid diagram:\n')
        i += 1
        while i < len(lines) and not lines[i].strip().startswith('```'):
            result.append('// ' + lines[i])
            i += 1
        result.append('// End Mermaid diagram\n\n')
        return ''.join(result), i + 1
    return None, i

def should_skip_line(line: str) -> bool:
    """Determine if line should be skipped."""
    stripped = line.strip()
    # Skip navigation sections
    if '**Navigation**:' in stripped or stripped.startswith('**Navigation**'):
        return True
    # Skip empty navigation links
    if stripped.startswith('**Related Documentation**:') and not stripped[30:].strip():
        return True
    return False

def convert_file(md_path: Path, typ_path: Path) -> None:
    """Convert a markdown file to Typst format."""
    with open(md_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    output: List[str] = []
    i = 0
    skip_next = False
    
    while i < len(lines):
        line = lines[i]
        
        # Skip navigation lines
        if should_skip_line(line):
            i += 1
            continue
        
        # Handle code blocks
        code_result, new_i = convert_code_block(lines, i)
        if code_result is not None:
            output.append(code_result)
            i = new_i
            continue
        
        # Handle mermaid blocks
        mermaid_result, new_i = convert_mermaid_block(lines, i)
        if mermaid_result is not None:
            output.append(mermaid_result)
            i = new_i
            continue
        
        # Handle tables
        table_result, new_i = convert_table(lines, i)
        if table_result is not None:
            output.append(table_result)
            i = new_i
            continue
        
        # Convert line
        converted = convert_header(line)
        converted = convert_bold(converted)
        converted = convert_link(converted)
        converted = convert_list_item(converted)
        
        output.append(converted)
        i += 1
    
    # Write output
    typ_path.parent.mkdir(parents=True, exist_ok=True)
    with open(typ_path, 'w', encoding='utf-8') as f:
        f.writelines(output)
    
    print(f"Converted: {md_path} -> {typ_path}")

def get_output_path(md_path: Path, base_dir: Path) -> Path:
    """Determine output path based on source path."""
    # Map documentation structure to documentation structure
    rel_path = md_path.relative_to(base_dir)
    
    # Remove .md extension
    stem = rel_path.stem
    
    # Map directories
    if rel_path.parts[0] == 'rewrite-spec':
        if len(rel_path.parts) > 1:
            return base_dir.parent / 'documentation' / 'specifications' / Path(*rel_path.parts[1:]).with_suffix('.typ')
        else:
            return base_dir.parent / 'documentation' / 'specifications' / f'{stem}.typ'
    elif rel_path.parts[0] == 'implementations':
        if len(rel_path.parts) > 1:
            return base_dir.parent / 'documentation' / 'implementations' / Path(*rel_path.parts[1:]).with_suffix('.typ')
        else:
            return base_dir.parent / 'documentation' / 'implementations' / f'{stem}.typ'
    elif rel_path.parts[0] == 'medley':
        if len(rel_path.parts) > 1:
            return base_dir.parent / 'documentation' / 'medley' / Path(*rel_path.parts[1:]).with_suffix('.typ')
        else:
            return base_dir.parent / 'documentation' / 'medley' / f'{stem}.typ'
    elif rel_path.parts[0] == 'components':
        return base_dir.parent / 'documentation' / 'components' / f'{stem}.typ'
    elif rel_path.parts[0] == 'api':
        return base_dir.parent / 'documentation' / 'reference' / f'{stem}.typ'
    else:
        # Root level files
        if stem in ['README', 'INDEX', 'CRITICAL_MEMORY']:
            return base_dir.parent / 'documentation' / 'core' / f'{stem.lower().replace("_", "-")}.typ'
        elif stem in ['architecture', 'glossary', 'build-system', 'CONTRIBUTING']:
            return base_dir.parent / 'documentation' / 'core' / f'{stem.lower().replace("_", "-")}.typ'
        else:
            return base_dir.parent / 'documentation' / 'core' / f'{stem.lower()}.typ'

def batch_convert(source_dir: Path) -> None:
    """Batch convert all markdown files in source directory."""
    md_files = list(source_dir.rglob('*.md'))
    print(f"Found {len(md_files)} markdown files to convert")
    
    for md_path in md_files:
        # Skip if already converted manually
        typ_path = get_output_path(md_path, source_dir)
        
        # Check if file exists and was manually edited (has content beyond TODO)
        if typ_path.exists():
            with open(typ_path, 'r') as f:
                content = f.read()
                # If file has substantial content (not just TODOs), skip
                if len(content) > 500 and 'TODO' not in content[:200]:
                    print(f"Skipping (already converted): {md_path}")
                    continue
        
        try:
            convert_file(md_path, typ_path)
        except Exception as e:
            print(f"Error converting {md_path}: {e}")

def main():
    """Main conversion function."""
    if len(sys.argv) > 1:
        source_dir = Path(sys.argv[1])
    else:
        source_dir = Path(__file__).parent.parent / 'documentation'
    
    if not source_dir.exists():
        print(f"Error: {source_dir} does not exist")
        sys.exit(1)
    
    batch_convert(source_dir)
    print("\nConversion complete!")
    print("Note: Files with Mermaid diagrams and tables need manual review.")

if __name__ == '__main__':
    main()
