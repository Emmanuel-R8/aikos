# Consolidate All Branches and Worktrees into Single Branch

## Overview

Create a unified branch that includes:

- All commits from all local branches
- All uncommitted work from all worktrees
- All stashed changes
- Preserve merge history (not squash)
- Stop for manual conflict resolution

## Current State Analysis

### Common Ancestor

- **Base commit**: `41cd524` ("stack/frame pointer initialization bug")
- All branches diverge from this point

### Branches to Consolidate

1. **5168103** (current HEAD, detached)

- "Laiko: Fix Common Lisp emulator loading issues"
- Has staged taiko recovery (48 files)

1. **745dfb4** (`convert-maiko-submodule`)

- "Beginning on Taiko"
- Contains original taiko commit

1. **16673d1** (`execution-parity-2026-01-26`)

- "feat: add .worktrees/ to .gitignore for worktree isolation"
- Has active worktree with uncommitted changes

1. **4e0eac7** (`003-unified-build-system`)

- "chore: Update medley submodule - unified build system complete"

1. **4348c5a** (`004-emulator-runner`, `backup-before-maiko-conversion-20260128-145840`)

- "Snapshot"
- Multiple branches point to same commit

1. **e88c1f3** (`backup-before-maiko-conversion-20260128-150059`)

- "Fix maiko submodule to correct commit..."

### Worktrees with Uncommitted Changes

1. **Main worktree** (`/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp`)

- Staged: 48 taiko files + other changes
- Unstaged: `.worktrees/execution-parity-2026-01-26`, `TAIKO_RECOVERY_INSTRUCTIONS.md`, submodules

1. **execution-parity-2026-01-26** (`.worktrees/execution-parity-2026-01-26`)

- Modified: `.cursor/rules/specify-rules.mdc`, `AGENTS.md`, `reports/CURRENT_STATUS.md`, `reports/WORK_STATE.md`

1. **Other worktrees** (fix, mif, nlj, nuh, qox, uxn, vdj, vrv, wtw)

- Various uncommitted changes (mostly deletions and config changes)

### Stashes to Apply

1. **stash@{0}**: "WIP on convert-maiko-submodule: 745dfb4 Beginning on Taiko"
2. **stash@{1}**: "WIP on convert-maiko-submodule: 3b8c654 docs: Add comprehensive comments..."
3. **stash@{2}**: "WIP on 001-medley-documentation: e42fc02 Fix IVAR opcodes..."
4. **stash@{3}**: "WIP on 004-emulator-runner: 4348c5a Snapshot"

## Consolidation Strategy

### Phase 0: Preserve Current Work (CRITICAL - Must Do First)

**Step 0.1**: Commit all current changes before switching branches

Git will not allow checkout when there are uncommitted changes that would be overwritten. We must commit everything first.

```bash
# Check current status
git status

# Stage all changes (both staged and unstaged)
git add -A

# Commit all current work to current HEAD (5168103)
git commit -m "WIP: Preserve all current work before consolidation

- Taiko recovery (48 files)
- Laiko changes (load-emulator.lisp, storage.lisp, package.lisp, opcodes.lisp, etc.)
- Maiko changes (execution_trace.h, atom.c, hardrtn.c, etc.)
- Documentation updates
- Recovery scripts and instructions
- Other uncommitted changes"

# Verify commit was successful
git log --oneline -1
```

**Note**: This creates a commit on the current detached HEAD. We'll merge this commit into the consolidated branch later.

### Phase 1: Prepare Base Branch

**Step 1.1**: Create new consolidation branch from common ancestor

Now that all changes are committed, we can safely checkout:

```bash
git checkout -b consolidated-master 41cd524
```

**Step 1.2**: Merge the commit we just made (contains all current work)

```bash
# Merge the commit from detached HEAD (5168103 + our new commit)
# First, get the commit hash of the commit we just made
git log --oneline -1  # Note the commit hash

# Merge that commit into consolidated-master
git merge 5168103 --no-ff -m "Merge current HEAD: Laiko fixes and Taiko recovery"
```

**Alternative approach** (if the above doesn't work cleanly):

```bash
# After committing on detached HEAD, note the commit hash
CURRENT_COMMIT=$(git rev-parse HEAD)

# Create consolidated branch from ancestor
git checkout -b consolidated-master 41cd524

# Cherry-pick the commit with all current work
git cherry-pick $CURRENT_COMMIT
```

### Phase 2: Merge All Branches (Preserve History)

**Step 2.1**: Merge branches in dependency order

```bash
# Merge convert-maiko-submodule (has taiko commit)
git merge convert-maiko-submodule --no-ff -m "Merge convert-maiko-submodule: Taiko TypeScript implementation"

# If conflicts: STOP, resolve manually, then continue
# git add <resolved-files>
# git commit (don't use --continue, commit completes merge)
```

**Step 2.2**: Merge execution-parity branch

```bash
git merge execution-parity-2026-01-26 --no-ff -m "Merge execution-parity-2026-01-26: Worktree isolation and parity work"
```

**Step 2.3**: Merge unified-build-system

```bash
git merge 003-unified-build-system --no-ff -m "Merge 003-unified-build-system: Unified build system"
```

**Step 2.4**: Merge emulator-runner

```bash
git merge 004-emulator-runner --no-ff -m "Merge 004-emulator-runner: Emulator runner snapshot"
```

**Step 2.5**: Merge backup branches (if they have unique commits)

```bash
# Check if backup-before-maiko-conversion-20260128-150059 has unique commits
git log 4348c5a..e88c1f3 --oneline

# If yes, merge it
git merge backup-before-maiko-conversion-20260128-150059 --no-ff -m "Merge backup: maiko submodule fix"
```

**Step 2.6**: Skip - Already merged in Phase 1

The current HEAD (5168103) and all its uncommitted changes were already merged in Phase 1, Step 1.2.

**Note**: After each merge, if conflicts occur:

- Git will stop automatically
- Resolve conflicts manually
- `git add <resolved-files>`
- `git commit` (completes merge)

### Phase 3: Collect Uncommitted Work from Worktrees

**Step 3.1**: Commit uncommitted changes from execution-parity worktree

```bash
# Switch to that worktree
cd .worktrees/execution-parity-2026-01-26

# Check what's modified
git status

# Commit changes
git add -A
git commit -m "WIP: execution-parity worktree changes"

# Switch back to main worktree
cd ../..

# Merge the commit from execution-parity branch
git merge execution-parity-2026-01-26 --no-ff -m "Merge execution-parity worktree uncommitted changes"
```

**Step 3.2**: Handle other worktrees

For each worktree (fix, mif, nlj, nuh, qox, uxn, vdj, vrv, wtw):

```bash
# Check if worktree has valuable uncommitted changes
cd <worktree-path>
git status

# If changes are valuable:
# Option A: Commit in worktree, then merge
git add -A
git commit -m "WIP: <worktree-name> changes"
cd ../..
git merge <worktree-commit> --no-ff -m "Merge <worktree-name> changes"

# Option B: Stash changes, apply to consolidated branch
git stash push -m "Worktree <name> changes"
cd ../..
git stash apply stash@{N}  # Apply the stash
git add -A
git commit -m "WIP: <worktree-name> changes"
```

**Step 3.3**: Commit remaining uncommitted changes in main worktree

```bash
# Back on consolidated-master branch
git add -A
git commit -m "WIP: Remaining uncommitted changes from main worktree"
```

### Phase 4: Apply Stashes

**Step 4.1**: Apply stashes in order (oldest first, newest last)

```bash
# Apply stash@{3} (oldest)
git stash apply stash@{3}
# Resolve conflicts if any
git add -A
git commit -m "Apply stash@{3}: WIP on 004-emulator-runner"

# Apply stash@{2}
git stash apply stash@{2}
# Resolve conflicts if any
git add -A
git commit -m "Apply stash@{2}: WIP on 001-medley-documentation"

# Apply stash@{1}
git stash apply stash@{1}
# Resolve conflicts if any
git add -A
git commit -m "Apply stash@{1}: WIP on convert-maiko-submodule docs"

# Apply stash@{0} (newest)
git stash apply stash@{0}
# Resolve conflicts if any
git add -A
git commit -m "Apply stash@{0}: WIP on convert-maiko-submodule Taiko"
```

**Note**: After applying all stashes, they can be dropped:

```bash
git stash drop stash@{0}
git stash drop stash@{1}
git stash drop stash@{2}
git stash drop stash@{3}
```

### Phase 5: Verification and Cleanup

**Step 5.1**: Verify consolidation

```bash
# Check that all branches are merged
git log --oneline --graph --all --decorate | head -50

# Verify all expected commits are present
git log --oneline --all | grep -E "(Taiko|Laiko|execution-parity|unified-build|emulator-runner)"

# Check for any uncommitted changes
git status
```

**Step 5.2**: Clean up old branches (optional, after verification)

```bash
# List branches that are now merged
git branch --merged consolidated-master

# Delete merged branches (if desired)
git branch -d convert-maiko-submodule
git branch -d execution-parity-2026-01-26
git branch -d 003-unified-build-system
git branch -d 004-emulator-runner
# etc.
```

**Step 5.3**: Clean up worktrees (after consolidation verified)

```bash
# Remove worktrees that are no longer needed
git worktree remove <path>
```

## Conflict Resolution Strategy

When conflicts occur during merges:

1. **Git will stop automatically** (no auto-resolution)
2. **Identify conflicted files**: `git status`
3. **Examine conflicts**: `git diff <file>`
4. **Resolve manually**: Edit files, choose versions
5. **Mark as resolved**: `git add <file>`
6. **Complete merge**: `git commit` (no `-m` needed, uses merge message)

## Alternative: Safer Incremental Approach

If you prefer a more cautious approach:

### Option A: Test Merge Strategy First

```bash
# Create test branch
git checkout -b test-consolidation 41cd524

# Try merging one branch at a time
git merge convert-maiko-submodule --no-ff

# If successful, continue to next branch
# If issues, abort: git merge --abort
```

### Option B: Backup Before Consolidation

```bash
# Create backup branch of current state
git branch backup-before-consolidation-$(date +%Y%m%d) HEAD

# Create backup of all branches
for branch in $(git branch | grep -v '\*'); do
  git branch backup-$branch $branch
done
```

## Expected Result

After consolidation:

- **Single branch**: `consolidated-master`
- **All commits preserved**: Full history with merge commits
- **All changes included**: Committed work + uncommitted work + stashes
- **Clean state**: No uncommitted changes, all stashes applied
- **Old branches**: Can be safely deleted after verification

## Rollback Plan

If consolidation fails or issues are discovered:

```bash
# Abort current merge
git merge --abort

# Or reset to before consolidation
git reset --hard <commit-before-consolidation>

# Or checkout original branch
git checkout <original-branch>
```

## Notes

- **Preserve history**: Using `--no-ff` ensures merge commits preserve branch structure
- **Manual conflict resolution**: Git will stop for each conflict
- **Worktree changes**: Must be committed in worktree before merging
- **Stash order**: Apply oldest first to maintain chronological order
- **Verification**: Always verify before deleting old branches/worktrees
