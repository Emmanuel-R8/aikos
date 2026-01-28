#!/usr/bin/env bash
# Convert maiko from submodule to regular directory
# Usage: ./scripts/convert-maiko-submodule.sh [--preserve-history|--no-history]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

PRESERVE_HISTORY=true
if [[ "${1:-}" == "--no-history" ]]; then
    PRESERVE_HISTORY=false
fi

echo "=========================================="
echo "Converting maiko from submodule to directory"
echo "Preserve history: $PRESERVE_HISTORY"
echo "=========================================="
echo ""

# Step 1: Verify we're on the right branch
CURRENT_BRANCH=$(git branch --show-current)
echo "Current branch: $CURRENT_BRANCH"
read -p "Continue? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 1
fi

# Step 2: Create backup branch
echo ""
echo "Step 1: Creating backup branch..."
BACKUP_BRANCH="backup-before-maiko-conversion-$(date +%Y%m%d-%H%M%S)"
git checkout -b "$BACKUP_BRANCH"
git checkout "$CURRENT_BRANCH"
echo "Backup branch created: $BACKUP_BRANCH"
echo ""

# Step 3: Get current submodule commit
echo "Step 2: Getting current submodule commit..."
SUBMODULE_COMMIT=$(git ls-files --stage maiko | awk '{print $2}')
echo "Submodule commit: $SUBMODULE_COMMIT"
echo ""

# Step 4: Remove submodule reference
echo "Step 3: Removing submodule reference..."
git rm --cached maiko 2>/dev/null || true

# Remove from .git/config
git config --file=.git/config --remove-section submodule.maiko 2>/dev/null || true

# Update .gitmodules
if [ -f .gitmodules ]; then
    # Remove maiko section from .gitmodules
    if grep -q '\[submodule "maiko"\]' .gitmodules; then
        # Use sed to remove the maiko submodule section
        sed -i '/\[submodule "maiko"\]/,/^$/d' .gitmodules
        
        # If .gitmodules is now empty or only has whitespace, remove it
        if [ ! -s .gitmodules ] || [ -z "$(cat .gitmodules | tr -d '[:space:]')" ]; then
            rm .gitmodules
        fi
    fi
    git add .gitmodules 2>/dev/null || true
fi
echo "Submodule reference removed"
echo ""

# Step 5: Remove submodule directory
echo "Step 4: Removing submodule directory..."
rm -rf .git/modules/maiko
rm -rf maiko
echo "Submodule directory removed"
echo ""

# Step 6: Add maiko files
echo "Step 5: Adding maiko files..."

if [ "$PRESERVE_HISTORY" = true ]; then
    echo "  Using git subtree (history preservation)..."
    # Clone maiko to temp directory
    TEMP_DIR=$(mktemp -d)
    git clone https://github.com/Interlisp/maiko.git "$TEMP_DIR/maiko"
    cd "$TEMP_DIR/maiko"
    git checkout "$SUBMODULE_COMMIT"
    cd "$REPO_ROOT"
    
    # Add as subtree
    git subtree add --prefix=maiko "$TEMP_DIR/maiko" "$SUBMODULE_COMMIT" -m "Convert maiko from submodule to directory (preserving history)"
    
    # Cleanup
    rm -rf "$TEMP_DIR"
else
    echo "  Using simple copy (no history preservation)..."
    # Clone maiko to temp directory
    TEMP_DIR=$(mktemp -d)
    git clone https://github.com/Interlisp/maiko.git "$TEMP_DIR/maiko"
    cd "$TEMP_DIR/maiko"
    git checkout "$SUBMODULE_COMMIT"
    cd "$REPO_ROOT"
    
    # Copy files (excluding .git)
    cp -r "$TEMP_DIR/maiko" .
    rm -rf maiko/.git
    
    # Add to repository
    git add maiko/
    git commit -m "Convert maiko from submodule to regular directory"
    
    # Cleanup
    rm -rf "$TEMP_DIR"
fi

echo "Maiko files added"
echo ""

# Step 7: Verify
echo "Step 6: Verifying conversion..."
if git ls-files --stage | grep "^160000.*maiko" > /dev/null; then
    echo "ERROR: Submodule reference still exists!"
    exit 1
fi

if [ ! -d "maiko" ] || [ -z "$(ls -A maiko 2>/dev/null)" ]; then
    echo "ERROR: maiko directory is empty or missing!"
    exit 1
fi

MAIKO_FILES=$(git ls-files maiko/ | wc -l)
if [ "$MAIKO_FILES" -eq 0 ]; then
    echo "ERROR: No maiko files are tracked!"
    exit 1
fi

echo "Verification passed:"
echo "  - Submodule reference removed"
echo "  - maiko directory exists with files"
echo "  - $MAIKO_FILES files tracked in maiko/"
echo ""

echo "=========================================="
echo "Conversion complete!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "1. Review the changes: git log --oneline -5"
echo "2. Test the build if applicable"
echo "3. Update documentation that references maiko as a submodule"
echo "4. If everything looks good, push: git push origin $CURRENT_BRANCH"
echo ""
echo "Backup branch: $BACKUP_BRANCH"
echo "To rollback: git checkout $BACKUP_BRANCH"
