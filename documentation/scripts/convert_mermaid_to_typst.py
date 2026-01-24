#!/usr/bin/env python3
"""
Convert Mermaid diagrams in Typst files to Typst native diagram syntax.
"""

import re
from pathlib import Path
from typing import List, Tuple

def convert_mermaid_graph_to_typst(mermaid_code: str) -> str:
    """Convert a Mermaid graph to Typst diagram syntax."""
    lines = mermaid_code.strip().split('\n')
    
    # Detect graph type
    graph_type = "TB"  # Top to bottom by default
    if "graph TD" in mermaid_code or "graph TB" in mermaid_code:
        graph_type = "TB"
    elif "graph LR" in mermaid_code:
        graph_type = "LR"
    elif "graph BT" in mermaid_code:
        graph_type = "BT"
    elif "graph RL" in mermaid_code:
        graph_type = "RL"
    
    # Extract nodes and edges
    nodes = {}
    edges = []
    subgraphs = {}
    current_subgraph = None
    
    for line in lines:
        line = line.strip()
        if not line or line.startswith('//'):
            continue
        
        # Subgraph
        if line.startswith('subgraph'):
            match = re.match(r'subgraph\s+(\w+)\[?"?([^"]+)"?\]?', line)
            if match:
                current_subgraph = match.group(1)
                subgraphs[current_subgraph] = {
                    'label': match.group(2) if match.group(2) else match.group(1),
                    'nodes': []
                }
        elif line == 'end' and current_subgraph:
            current_subgraph = None
        
        # Node definition: NodeID["Label"] or NodeID[Label]
        node_match = re.match(r'(\w+)\[([^\]]+)\]', line)
        if node_match:
            node_id = node_match.group(1)
            label = node_match.group(2).replace('<br/>', '\n').replace('<br>', '\n')
            nodes[node_id] = label
            if current_subgraph:
                subgraphs[current_subgraph]['nodes'].append(node_id)
        
        # Edge: A --> B or A -->|label| B
        edge_match = re.match(r'(\w+)\s*-->\s*(?:\|([^|]+)\|)?\s*(\w+)', line)
        if edge_match:
            from_node = edge_match.group(1)
            label = edge_match.group(2) if edge_match.group(2) else None
            to_node = edge_match.group(3)
            edges.append((from_node, to_node, label))
    
    # Generate Typst diagram
    result = ['#figure(\n  caption: [Diagram],\n  diagram(\n']
    result.append('    node-stroke: 0.5pt,\n')
    result.append('    spacing: 1.5em,\n\n')
    
    # Add nodes
    for node_id, label in nodes.items():
        # Clean label
        label_lines = [l.strip() for l in label.split('\n') if l.strip()]
        if len(label_lines) == 1:
            result.append(f'    {node_id} = rect([{label_lines[0]}]),\n')
        else:
            result.append(f'    {node_id} = rect([{label_lines[0]}], [{label_lines[1]}]),\n')
    
    result.append('\n')
    
    # Add edges
    for from_node, to_node, label in edges:
        if label:
            result.append(f'    {from_node} -> {to_node}: "{label}",\n')
        else:
            result.append(f'    {from_node} -> {to_node},\n')
    
    result.append('  )\n)')
    
    return ''.join(result)

def convert_mermaid_sequence_to_typst(mermaid_code: str) -> str:
    """Convert a Mermaid sequence diagram to Typst diagram syntax."""
    lines = mermaid_code.strip().split('\n')
    
    participants = []
    messages = []
    
    for line in lines:
        line = line.strip()
        if not line or line.startswith('//'):
            continue
        
        # Participant
        if line.startswith('participant'):
            match = re.match(r'participant\s+(\w+)\s+as\s+(.+)', line)
            if match:
                participants.append((match.group(1), match.group(2)))
        
        # Message: A->>B: message
        msg_match = re.match(r'(\w+)->>(\w+):\s*(.+)', line)
        if msg_match:
            messages.append((msg_match.group(1), msg_match.group(2), msg_match.group(3)))
    
    # Generate Typst diagram (simplified - Typst doesn't have great sequence diagram support)
    result = ['#figure(\n  caption: [Sequence Diagram],\n  diagram(\n']
    result.append('    node-stroke: 0.5pt,\n')
    result.append('    spacing: 1.5em,\n\n')
    
    # Add participants as nodes
    for i, (pid, label) in enumerate(participants):
        result.append(f'    {pid} = rect([{label}]),\n')
        result.append(f'    {pid}.pos = ({i * 3}em, 0),\n')
    
    result.append('\n')
    
    # Add messages as edges
    y_pos = 2
    for from_p, to_p, msg in messages:
        result.append(f'    {from_p} -> {to_p}: "{msg}",\n')
        y_pos += 1
    
    result.append('  )\n)')
    
    return ''.join(result)

def process_file(typ_path: Path) -> bool:
    """Process a Typst file and convert Mermaid diagrams."""
    with open(typ_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Find Mermaid code blocks
    pattern = r'#codeblock\(lang:\s*"mermaid",\s*\[(.*?)\]\)'
    
    matches = list(re.finditer(pattern, content, re.DOTALL))
    if not matches:
        return False
    
    # Convert each Mermaid diagram
    replacements = []
    for match in matches:
        mermaid_code = match.group(1)
        
        # Determine diagram type
        if 'sequenceDiagram' in mermaid_code:
            typst_diagram = convert_mermaid_sequence_to_typst(mermaid_code)
        else:
            typst_diagram = convert_mermaid_graph_to_typst(mermaid_code)
        
        replacements.append((match.start(), match.end(), typst_diagram))
    
    # Apply replacements (in reverse order to maintain indices)
    new_content = content
    for start, end, replacement in reversed(replacements):
        new_content = new_content[:start] + replacement + new_content[end:]
    
    # Write back
    if new_content != content:
        with open(typ_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        return True
    
    return False

def main():
    """Main function."""
    doc_dir = Path('documentation')
    typ_files = list(doc_dir.rglob('*.typ'))
    
    converted = 0
    for typ_path in typ_files:
        if process_file(typ_path):
            print(f"Converted: {typ_path}")
            converted += 1
    
    print(f"\nConverted {converted} files with Mermaid diagrams")

if __name__ == '__main__':
    main()
