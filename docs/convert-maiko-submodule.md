# Converting maiko from Submodule to Regular Directory

**Date**: 2026-01-28
**Purpose**: Convert `maiko` from a git submodule to a regular directory within the main repository

## Overview

This process will:
1. Remove the submodule reference
2. Add maiko's files directly to the repository
3. Optionally preserve maiko's git history (recommended)

## Prerequisites

- Ensure all changes in `maiko/` are committed in the submodule
- Ensure the main repository is on the correct branch (`001-medley-documentation`)
- Create a backup branch before starting

## Step-by-Step Process

### Step 1: Create Backup Branch

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
git checkout -b backup-before-maiko-conversion
git push origin backup-before-maiko-conversion
git checkout 001-medley-documentation
```

### Step 2: Remove Submodule Reference

```bash
# Remove the submodule entry from .gitmodules
git rm --cached maiko

# Remove the submodule entry from .git/config (if present)
# This is usually done automatically, but verify:
git config --file=.git/config --remove-section submodule.maiko || true

# Remove .gitmodules entry (or edit manually)
# If maiko is the only submodule, delete .gitmodules
# Otherwise, edit .gitmodules to remove the [submodule "maiko"] section
```

### Step 3: Remove Submodule Directory (Temporarily)

```bash
# Remove the submodule directory
rm -rf .git/modules/maiko
rm -rf maiko
```

### Step 4: Add maiko Files Directly

**Option A: Simple Copy (No History Preservation)**

```bash
# Clone maiko separately to get the files
git clone https://github.com/Interlisp/maiko.git maiko-temp
cd maiko-temp
git checkout 50efed4f9bd56a76eb24b1e12455a2dc3e04b990
cd ..

# Copy files (excluding .git directory)
cp -r maiko-temp/* maiko/
rm -rf maiko-temp

# Add to repository
git add maiko/
git commit -m "Convert maiko from submodule to regular directory"
```

**Option B: Preserve Git History (Recommended)**

This preserves the full git history from maiko:

```bash
# Clone maiko separately
git clone https://github.com/Interlisp/maiko.git maiko-temp
cd maiko-temp
git checkout 50efed4f9bd56a1e12455a2dc3e04b990

# Use git subtree or git filter-repo to merge history
# Method 1: Using git subtree (if available)
cd ..
git subtree add --prefix=maiko maiko-temp 50efed4f9bd56a76eb24b1e12455a2dc3e04b990 --squash

# OR Method 2: Using git filter-repo (more control)
cd maiko-temp
git filter-repo --to-subdirectory-filter maiko
cd ..
git remote add maiko-remote maiko-temp
git fetch maiko-remote
git merge --allow-unrelated-histories maiko-remote/main
git remote remove maiko-remote
rm -rf maiko-temp
```

**Option C: Manual History Merge (Most Control)**

```bash
# Clone maiko
git clone https://github.com/Interlisp/maiko.git maiko-temp
cd maiko-temp
git checkout 50efed4f9bd56a76eb24b1e12455a2dc3e04b990

# Rewrite history to add maiko/ prefix
git filter-branch --prune-empty --tree-filter '
    if [ ! -e maiko ]; then
        mkdir -p maiko
        git ls-tree --name-only $GIT_COMMIT | xargs -I files mv files maiko/
    fi
' -- --all

# Add as remote and merge
cd ..
git remote add maiko-remote maiko-temp
git fetch maiko-remote
git merge --allow-unrelated-histories maiko-remote/main -m "Merge maiko history into main repository"
git remote remove maiko-remote
rm -rf maiko-temp
```

### Step 5: Update .gitignore (if needed)

```bash
# Check if maiko-specific ignores are needed
# Usually not needed since maiko is now part of the repo
```

### Step 6: Verify and Test

```bash
# Verify maiko files are tracked
git ls-files maiko/ | head -20

# Verify no submodule references remain
git ls-files --stage | grep "^160000" | grep maiko
# Should return nothing

# Test build (if applicable)
cd maiko
make clean && make  # or appropriate build command
```

### Step 7: Update Documentation

Update any documentation that references maiko as a submodule:

- `AGENTS.md` - Update submodule instructions
- `README.md` - Remove submodule setup instructions
- Any build scripts that reference submodule initialization

### Step 8: Commit Final Changes

```bash
git add .
git commit -m "Complete maiko submodule to directory conversion"
```

## Recommended Approach

For this project, I recommend **Option B (Preserve Git History)** using `git subtree` if available, as it:
- Preserves maiko's commit history
- Makes it easy to track changes
- Allows future updates if needed
- Is simpler than manual history rewriting

## Alternative: Using git-subtree (if installed)

```bash
# Install git-subtree if not available
# On NixOS: nix-env -iA nixpkgs.git-subtree

# Remove submodule first (Steps 1-3)
# Then:
git clone https://github.com/Interlisp/maiko.git maiko-temp
cd maiko-temp
git checkout 50efed4f9bd56a76eb24b1e12455a2dc3e04b990
cd ..

# Add as subtree
git subtree add --prefix=maiko maiko-temp 50efed4f9bd56a76eb24b1e12455a2dc3e04b990

# Cleanup
rm -rf maiko-temp
```

## Important Notes

1. **Backup First**: Always create a backup branch before starting
2. **Submodule State**: Ensure the submodule is at the correct commit
3. **History Preservation**: Option B/C preserves history but creates a larger repository
4. **Future Updates**: If maiko needs updates, you'll need to either:
   - Manually copy files
   - Use `git subtree pull` (if using subtree method)
   - Re-add as submodule (not recommended after conversion)

## Rollback Plan

If something goes wrong:

```bash
git checkout backup-before-maiko-conversion
# Or manually restore:
git rm -rf maiko
git submodule add https://github.com/Interlisp/maiko.git maiko
git submodule update --init maiko
```

## Verification Checklist

- [ ] Backup branch created
- [ ] Submodule reference removed from `.gitmodules`
- [ ] Submodule entry removed from `.git/config`
- [ ] `maiko/` directory contains files (not a submodule)
- [ ] Files are tracked by git (`git ls-files maiko/` shows files)
- [ ] No submodule mode (160000) entries for maiko
- [ ] Build/test passes
- [ ] Documentation updated
