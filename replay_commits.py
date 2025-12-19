#!/usr/bin/env python3
"""
Replay git commits from maiko submodule to main repo with path transformations.
"""

import subprocess
import sys
import tempfile
import shutil
from pathlib import Path
from datetime import datetime

def run_git(cmd, cwd=None):
    """Run git command and return output."""
    result = subprocess.run(
        ['git'] + cmd,
        cwd=cwd,
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print(f"Warning: git {' '.join(cmd)} failed: {result.stderr}")
    return result.stdout.strip(), result.returncode

def get_commits(submodule_path, path_filter):
    """Get all commits affecting a path in submodule."""
    commits = []
    output, code = run_git(
        ['log', '--reverse', '--format=%H|%an|%ae|%ad|%s', '--date=iso', '--', path_filter],
        cwd=submodule_path
    )
    if code != 0:
        return commits
    
    for line in output.split('\n'):
        if '|' in line:
            parts = line.split('|', 4)
            if len(parts) == 5:
                commits.append({
                    'hash': parts[0],
                    'author': parts[1],
                    'email': parts[2],
                    'date': parts[3],
                    'message': parts[4]
                })
    return commits

def get_changed_files(submodule_path, commit_hash, path_filter):
    """Get list of files changed in a commit."""
    output, code = run_git(
        ['diff-tree', '--no-commit-id', '--name-only', '-r', commit_hash, '--', path_filter],
        cwd=submodule_path
    )
    if code != 0:
        return []
    return [f for f in output.split('\n') if f]

def extract_files_from_commit(submodule_path, commit_hash, path_filter, temp_dir):
    """Extract files from a commit to temp directory."""
    # Get list of files in this commit
    output, code = run_git(
        ['ls-tree', '-r', '--name-only', commit_hash, '--', path_filter],
        cwd=submodule_path
    )
    if code != 0:
        return []
    
    files = [f for f in output.split('\n') if f]
    
    # Checkout files to temp directory
    for file_path in files:
        full_path = Path(temp_dir) / file_path
        full_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Use git show to get file content
        content, code = run_git(
            ['show', f'{commit_hash}:{file_path}'],
            cwd=submodule_path
        )
        if code == 0:
            full_path.write_text(content)
    
    return files

def transform_path(file_path, old_prefix, new_prefix):
    """Transform file path from old prefix to new prefix."""
    if file_path.startswith(old_prefix):
        return file_path.replace(old_prefix, new_prefix, 1)
    return file_path

def main():
    repo_root = Path('.')
    submodule_path = repo_root / 'maiko'
    
    # Replay zig commits
    print("Extracting zig commits...")
    zig_commits = get_commits(submodule_path, 'alternatives/zig')
    print(f"Found {len(zig_commits)} commits for zig")
    
    # Replay lisp commits  
    print("Extracting lisp commits...")
    lisp_commits = get_commits(submodule_path, 'alternatives/lisp')
    print(f"Found {len(lisp_commits)} commits for lisp")
    
    # For now, just create initial commits with current state
    # Full history replay would require more complex git operations
    print("\nCreating initial commits...")
    
    # Stage zaiko
    run_git(['add', 'zaiko/'], cwd=repo_root)
    if zig_commits:
        first_commit = zig_commits[0]
        run_git([
            'commit', '-m', f"Initial zaiko commit (from maiko submodule)\n\nOriginal commit: {first_commit['hash']}\nMessage: {first_commit['message']}",
            '--author', f"{first_commit['author']} <{first_commit['email']}>",
            '--date', first_commit['date']
        ], cwd=repo_root)
        print("Created initial zaiko commit")
    
    # Stage laiko
    run_git(['add', 'laiko/'], cwd=repo_root)
    if lisp_commits:
        first_commit = lisp_commits[0]
        run_git([
            'commit', '-m', f"Initial laiko commit (from maiko submodule)\n\nOriginal commit: {first_commit['hash']}\nMessage: {first_commit['message']}",
            '--author', f"{first_commit['author']} <{first_commit['email']}>",
            '--date', first_commit['date']
        ], cwd=repo_root)
        print("Created initial laiko commit")
    
    print(f"\nNote: Full commit history replay would require {len(zig_commits) + len(lisp_commits)} commits.")
    print("This script created initial commits. Full history replay can be done with git filter-branch or similar tools.")

if __name__ == '__main__':
    main()
