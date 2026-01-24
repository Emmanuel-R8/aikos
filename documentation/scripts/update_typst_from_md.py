#!/usr/bin/env python3
"""
Update Typst files from markdown source files.
Converts markdown to Typst format following existing patterns.
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

def convert_link(line: str) -> str:
    """Convert [text](url) to text (remove links for PDF)."""
    # Remove markdown links, keep text
    line = re.sub(r'\[([^\]]+)\]\([^\)]+\)', r'\1', line)
    return line

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
    if '```mermaid' in lines[i] or ('```' in lines[i] and 'mermaid' in lines[i].lower()):
        result = ['// TODO: Convert Mermaid diagram to Typst diagram syntax\n']
        result.append('// Original Mermaid diagram:\n')
        i += 1
        while i < len(lines) and not lines[i].strip().startswith('```'):
            result.append('// ' + lines[i])
            i += 1
        result.append('// End Mermaid diagram\n\n')
        return ''.join(result), i + 1
    return None, i

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

def convert_markdown_to_typst(md_content: str) -> str:
    """Convert markdown content to Typst format."""
    lines = md_content.splitlines(keepends=True)
    result = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Check for code blocks first
        code_result, new_i = convert_code_block(lines, i)
        if code_result:
            result.append(code_result)
            i = new_i
            continue
        
        # Check for mermaid blocks
        mermaid_result, new_i = convert_mermaid_block(lines, i)
        if mermaid_result:
            result.append(mermaid_result)
            i = new_i
            continue
        
        # Check for tables
        table_result, new_i = convert_table(lines, i)
        if table_result:
            result.append(table_result)
            i = new_i
            continue
        
        # Convert headers
        if line.strip().startswith('#'):
            result.append(convert_header(line))
        else:
            # Convert bold and links
            converted = convert_bold(line)
            converted = convert_link(converted)
            result.append(converted)
        
        i += 1
    
    return ''.join(result)

def get_typst_path(md_path: Path) -> Path:
    """Convert markdown path to Typst path."""
    md_str = str(md_path)
    
    # Handle special cases
    if 'rewrite-spec' in md_str:
        typ_str = md_str.replace('documentation/rewrite-spec', 'documentation/specifications')
    elif 'api' in md_str:
        typ_str = md_str.replace('documentation/api', 'documentation/reference')
    elif 'components' in md_str:
        typ_str = md_str.replace('documentation/components', 'documentation/components')
    elif 'medley' in md_str:
        typ_str = md_str.replace('documentation/medley', 'documentation/medley')
    elif 'implementations' in md_str:
        typ_str = md_str.replace('documentation/implementations', 'documentation/implementations')
    elif md_str == 'documentation/architecture.md':
        typ_str = 'documentation/core/architecture.typ'
    elif md_str == 'documentation/build-system.md':
        typ_str = 'documentation/core/build-system.typ'
    elif md_str == 'documentation/glossary.md':
        typ_str = 'documentation/reference/glossary.typ'
    elif md_str == 'documentation/INDEX.md':
        typ_str = 'documentation/reference/index.typ'
    elif md_str == 'documentation/README.md':
        typ_str = 'documentation/core/readme.typ'
    elif md_str == 'documentation/CONTRIBUTING.md':
        typ_str = 'documentation/core/contributing.typ'
    elif md_str == 'documentation/CRITICAL_MEMORY.md':
        typ_str = 'documentation/core/critical-memory.typ'
    elif md_str == 'documentation/CRITICAL_DEBUGGING_TECHNIQUE.md':
        typ_str = 'documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ'
    else:
        typ_str = md_str.replace('documentation', 'documentation')
    
    return Path(typ_str.replace('.md', '.typ'))

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python3 update_typst_from_md.py <md_file>")
        sys.exit(1)
    
    md_path = Path(sys.argv[1])
    if not md_path.exists():
        print(f"Error: {md_path} does not exist")
        sys.exit(1)
    
    typ_path = get_typst_path(md_path)
    
    # Read markdown
    md_content = md_path.read_text()
    
    # Convert to Typst
    typ_content = convert_markdown_to_typst(md_content)
    
    # Write Typst file
    typ_path.parent.mkdir(parents=True, exist_ok=True)
    typ_path.write_text(typ_content)
    
    print(f"Converted {md_path} -> {typ_path}")
