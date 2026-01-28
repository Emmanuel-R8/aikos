# Converting maiko from Submodule to Regular Directory

**Date**: 2026-01-28  
**Purpose**: Convert `maiko` from a git submodule to a regular directory within the main repository

## Quick Start

Use the automated script:

```bash
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
./scripts/convert-maiko-submodule.sh
```

This will:
- Create a backup branch
- Remove submodule references
- Add maiko files with history preservation (using git subtree)
- Verify the conversion

## Manual Process

If you prefer to do it manually, follow these steps:

### Step 1: Create Backup Branch

```bash
git checkout -b backup-before-maiko-conversion
git push origin backup-before-maiko-conversion
git checkout 001-medley-documentation
```

### Step 2: Get Submodule Commit

```bash
SUBMODULE_COMMIT=$(git ls-files --stage maiko | awk '{print $2}')
echo "Submodule commit: $SUBMODULE_COMMIT"
```

### Step 3: Remove Submodule Reference

```bash
# Remove from git index
git rm --cached maiko

# Remove from .git/config
git config --file=.git/config --remove-section submodule.maiko

# Remove from .gitmodules (edit manually or use sed)
sed -i '/\[submodule "maiko"\]/,/^$/d' .gitmodules
# If .gitmodules is now empty, remove it
[ ! -s .gitmodules ] && rm .gitmodules
git add .gitmodules
```

### Step 4: Remove Submodule Directory

```bash
rm -rf .git/modules/maiko
rm -rf maiko
```

### Step 5: Add maiko Files (with History)

```bash
# Clone maiko to temp location
TEMP_DIR=$(mktemp -d)
git clone https://github.com/Interlisp/maiko.git "$TEMP_DIR/maiko"
cd "$TEMP_DIR/maiko"
git checkout $SUBMODULE_COMMIT
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp

# Add as subtree (preserves history)
git subtree add --prefix=maiko "$TEMP_DIR/maiko" "$SUBMODULE_COMMIT" -m "Convert maiko from submodule to directory"

# Cleanup
rm -rf "$TEMP_DIR"
```

### Step 6: Verify

```bash
# Check no submodule references remain
git ls-files --stage | grep "^160000.*maiko"
# Should return nothing

# Check files are tracked
git ls-files maiko/ | head -10

# Verify directory exists
ls -la maiko/
```

## Current Submodule State

- **Submodule URL**: https://github.com/Interlisp/maiko.git
- **Current Commit**: `50efed4f9bd56a76eb24b1e12455a2dc3e04b990`
- **Path**: `maiko/`

## Options

### With History Preservation (Recommended)

Uses `git subtree` to preserve maiko's full commit history:

```bash
./scripts/convert-maiko-submodule.sh
# or
./scripts/convert-maiko-submodule.sh --preserve-history
```

**Benefits**:
- Full history preserved
- Can track changes over time
- Easier to understand file evolution

### Without History Preservation

Simple copy of files at current commit:

```bash
./scripts/convert-maiko-submodule.sh --no-history
```

**Benefits**:
- Simpler process
- Smaller repository size
- Faster conversion

## After Conversion

1. **Update Documentation**:
   - Remove submodule setup instructions from README
   - Update AGENTS.md if it references submodules
   - Update any build scripts

2. **Test Build**:
   ```bash
   cd maiko
   make clean && make  # or appropriate build command
   ```

3. **Commit and Push**:
   ```bash
   git push origin 001-medley-documentation
   ```

## Rollback

If something goes wrong:

```bash
git checkout backup-before-maiko-conversion
```

Or manually restore:

```bash
git rm -rf maiko
git submodule add https://github.com/Interlisp/maiko.git maiko
git submodule update --init maiko
```

## Notes

- **git subtree** is available (git 2.52.0+)
- The script creates a backup branch automatically
- All changes are committed automatically
- Verification steps ensure conversion succeeded

## Related Files

- `.gitmodules` - Submodule configuration (will be updated)
- `.git/config` - Local git config (submodule section removed)
- `maiko/` - Will become a regular directory
