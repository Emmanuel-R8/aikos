#!/usr/bin/env python3
"""
Replay git commits from maiko submodule for laiko implementation.
"""

import subprocess
from pathlib import Path

def run_git(cmd, cwd=None, check=True):
    """Run git command and return output."""
    result = subprocess.run(
        ['git'] + cmd,
        cwd=cwd,
        capture_output=True,
        text=True,
        check=check
    )
    return result.stdout.strip()

def get_commits(submodule_path, path_filter):
    """Get all commits affecting a path in submodule."""
    commits = []
    output = run_git(
        ['log', '--reverse', '--format=%H|%an|%ae|%ad|%s', '--date=iso-strict', '--', path_filter],
        cwd=submodule_path
    )
    
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
    try:
        output = run_git(
            ['diff-tree', '--no-commit-id', '--name-only', '-r', commit_hash, '--', path_filter],
            cwd=submodule_path
        )
        return [f for f in output.split('\n') if f]
    except:
        output = run_git(
            ['ls-tree', '-r', '--name-only', commit_hash, '--', path_filter],
            cwd=submodule_path
        )
        return [f for f in output.split('\n') if f]

def extract_file_content(submodule_path, commit_hash, file_path):
    """Extract file content from a specific commit."""
    try:
        return run_git(['show', f'{commit_hash}:{file_path}'], cwd=submodule_path)
    except:
        return None

def transform_path(file_path, old_prefix='alternatives/lisp/', new_prefix='laiko/'):
    """Transform file path from old prefix to new prefix."""
    if file_path.startswith(old_prefix):
        return file_path.replace(old_prefix, new_prefix, 1)
    return file_path

def main():
    repo_root = Path('.')
    submodule_path = repo_root / 'maiko'
    laiko_path = repo_root / 'laiko'
    
    print("Extracting lisp commits from maiko submodule...")
    commits = get_commits(submodule_path, 'alternatives/lisp')
    print(f"Found {len(commits)} commits to replay\n")
    
    if not commits:
        print("No commits found. Exiting.")
        return
    
    for i, commit in enumerate(commits, 1):
        print(f"[{i}/{len(commits)}] Processing commit {commit['hash'][:8]}: {commit['message'][:60]}...")
        
        changed_files = get_changed_files(submodule_path, commit['hash'], 'alternatives/lisp')
        
        if not changed_files:
            print(f"  No files changed, skipping...")
            continue
        
        files_to_commit = []
        for file_path in changed_files:
            content = extract_file_content(submodule_path, commit['hash'], file_path)
            if content is None:
                new_path = laiko_path / transform_path(file_path)
                if new_path.exists():
                    run_git(['rm', str(new_path.relative_to(repo_root))], cwd=repo_root, check=False)
                continue
            
            new_path = laiko_path / transform_path(file_path)
            new_path.parent.mkdir(parents=True, exist_ok=True)
            new_path.write_text(content)
            files_to_commit.append(str(new_path.relative_to(repo_root)))
        
        if files_to_commit:
            for file_path in files_to_commit:
                run_git(['add', file_path], cwd=repo_root, check=False)
            
            commit_msg = f"{commit['message']}\n\n(Original commit: {commit['hash']})"
            run_git([
                'commit',
                '-m', commit_msg,
                '--author', f"{commit['author']} <{commit['email']}>",
                '--date', commit['date'],
                '--allow-empty'
            ], cwd=repo_root, check=False)
            
            print(f"  ✓ Committed {len(files_to_commit)} files")
    
    print(f"\n✓ Completed replaying {len(commits)} commits")

if __name__ == '__main__':
    main()
