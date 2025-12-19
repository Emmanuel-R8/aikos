# Implementation Relocation Summary

**Date**: 2025-01-27  
**Status**: Initial commits created, full history replay pending

## Overview

Relocating implementations from `zaiko` and `laiko` to standalone projects `zaiko/` and `laiko/` respectively, with full git history preservation.

## Current Status

### ✅ Completed

1. **Initial Relocation**
   - Created `zaiko/` directory with current state from `zaiko`
   - Created `laiko/` directory with current state from `laiko`
   - Created initial commit: `e0a427e` - "Initial zaiko commit: Relocate zaiko to zaiko/"

2. **Git History Analysis**
   - Found **29 commits** for `alternatives/zig` in maiko submodule
   - Found **1 commit** for `alternatives/lisp` in maiko submodule
   - First zig commit: `0fe44f92605f91d03c9b46e470d9998465f28e14` (2025-12-05)
   - First lisp commit: `c4c5b64efd3f1ec0e645d4410f63d6db400267d5` (2025-12-06)

## Remaining Tasks

### 1. Git History Replay (HIGH PRIORITY)

**Goal**: Replay all 29 commits from maiko submodule for zaiko, preserving commit messages, authors, and dates.

**Approach Options**:

#### Option A: Manual Commit Replay (Recommended for accuracy)
- For each of the 29 commits:
  1. Extract changed files from commit hash
  2. Transform paths: `alternatives/zig/` → `zaiko/`
  3. Apply changes to zaiko/ directory
  4. Create commit with:
     - Original commit message
     - Original author and email
     - Original date
     - Reference to original commit hash

**Script Required**: `replay_zig_history.py`
```python
# Pseudocode:
for commit in zig_commits:
    changed_files = get_changed_files(commit.hash, 'alternatives/zig')
    for file in changed_files:
        new_path = file.replace('alternatives/zig', 'zaiko')
        content = extract_file_content(commit.hash, file)
        write_file(new_path, content)
    git.commit(message=commit.message, author=commit.author, date=commit.date)
```

#### Option B: Git Filter-Branch (Faster but more complex)
- Use `git filter-branch` or `git filter-repo` to rewrite history
- Requires working in maiko submodule
- More complex path transformations

**Recommended**: Option A (manual replay) for better control and accuracy.

### 2. Path Reference Updates (HIGH PRIORITY)

**Goal**: Update all references from old paths to new paths throughout the codebase.

#### Files to Update:

**Build Files**:
- [ ] `zaiko/build.zig` - Update any internal path references
- [ ] `shell.nix` - Update paths if referenced
- [ ] Any CI/CD configs (`.github/workflows/`) - Update paths

**Documentation**:
- [ ] `documentation/implementations/zig-implementation.typ` - Update location references
- [ ] `documentation/implementations/lisp-implementation.typ` - Update location references
- [ ] `specs/005-zig-completion/plan.md` - Update paths
- [ ] `specs/005-zig-completion/tasks.md` - Update paths
- [ ] `specs/005-zig-completion/spec.md` - Update paths
- [ ] `AGENTS.md` - Update paths
- [ ] All Typst files referencing `zaiko` or `laiko`

**Code References**:
- [ ] Search for `zaiko` in all files → replace with `zaiko`
- [ ] Search for `laiko` in all files → replace with `laiko`
- [ ] Update import paths if any cross-references exist
- [ ] Update README files in zaiko/ and laiko/

**Script Required**: `update_path_references.py`
```bash
# Find all references
grep -r "zaiko" . --exclude-dir=.git --exclude-dir=maiko --exclude-dir=deprecated
grep -r "laiko" . --exclude-dir=.git --exclude-dir=maiko --exclude-dir=deprecated
```

### 3. Project Name Updates (MEDIUM PRIORITY)

**Goal**: Update project names from "zaiko"/"laiko" to "zaiko"/"laiko".

**Files to Update**:
- [ ] `zaiko/README.md` - Update project name
- [ ] `zaiko/build.zig` - Update project name if hardcoded
- [ ] `laiko/README.md` - Update project name
- [ ] `laiko/laiko.asd` - Consider renaming to `laiko.asd`
- [ ] Documentation references to project names
- [ ] Build output names (binaries, artifacts)

### 4. Deprecation Notices (MEDIUM PRIORITY)

**Goal**: Add deprecation notices to old locations.

**Files to Create/Update**:
- [ ] `zaiko/README.md` - Add deprecation notice
- [ ] `laiko/README.md` - Add deprecation notice

**Deprecation Notice Template**:
```markdown
# DEPRECATED

This directory has been relocated.

**New Location**: `../../zaiko/` (for zig) or `../../laiko/` (for lisp)

**Date Deprecated**: 2025-01-27

**Status**: This directory is kept for reference only. All new development should use the new locations.

**Migration**: See `RELOCATION_SUMMARY.md` in repository root for details.
```

### 5. Build System Updates (LOW PRIORITY)

**Goal**: Ensure build systems work with new paths.

**Tasks**:
- [ ] Verify `zaiko/build.zig` works from new location
- [ ] Verify `laiko/build.sh` works from new location
- [ ] Update any build scripts that reference old paths
- [ ] Test compilation from repository root

### 6. Documentation Updates (LOW PRIORITY)

**Goal**: Update all documentation to reflect new structure.

**Tasks**:
- [ ] Update `AGENTS.md` with new paths
- [ ] Update `specs/005-zig-completion/quickstart.md` if it references paths
- [ ] Update any other documentation referencing old paths
- [ ] Update Typst documentation index if needed

## Implementation Plan

### Phase 1: Git History Replay (Estimated: 2-3 hours)

1. Create `replay_zig_history.py` script
2. Extract all 29 commits with metadata
3. For each commit:
   - Get list of changed files
   - Extract file contents at that commit
   - Transform paths
   - Apply to zaiko/ directory
   - Create commit with original metadata
4. Verify commit history matches original

### Phase 2: Path Reference Updates (Estimated: 1-2 hours)

1. Create `update_path_references.py` script
2. Find all references to old paths
3. Replace systematically:
   - `zaiko` → `zaiko`
   - `laiko` → `laiko`
4. Verify no broken references remain

### Phase 3: Project Name Updates (Estimated: 30 minutes)

1. Update README files
2. Update build configuration files
3. Update documentation

### Phase 4: Deprecation Notices (Estimated: 15 minutes)

1. Add deprecation notices to old locations
2. Create migration guide if needed

### Phase 5: Verification (Estimated: 30 minutes)

1. Test builds from new locations
2. Verify all references updated
3. Check git history integrity
4. Verify documentation accuracy

## Git History Details

### Zig Implementation Commits (29 total)

**First Commit**: `0fe44f92605f91d03c9b46e470d9998465f28e14`
- Date: 2025-12-05 08:33:21 +0800
- Author: Emmanuel_R8 <emmanuel.rialland@gmail.com>
- Message: "feat(zig): Implement Maiko VM core in Zig with comprehensive test coverage"

**Latest Commit**: `f5cb35d` (most recent)
- Message: "Refactor: Split large files to stay under 500 lines"

**Key Commits** (from git log):
- SDL2 integration commits (T075-T091)
- BIGVM format support
- Frame structure fixes
- Opcode implementations
- Test suite additions

### Lisp Implementation Commits (1 total)

**Single Commit**: `c4c5b64efd3f1ec0e645d4410f63d6db400267d5`
- Date: 2025-12-06 15:45:47 +0800
- Author: Emmanuel_R8 <emmanuel.rialland@gmail.com>
- Message: "feat: Complete Common Lisp implementation of Maiko emulator (002-lisp-implementation)"

## Files Requiring Updates

### High Priority (Path References)

1. **Documentation Files** (Typst):
   - `documentation/implementations/zig-implementation.typ` - Line 7: `zaiko/`
   - `documentation/implementations/lisp-implementation.typ` - Similar references
   - All other Typst files referencing old paths

2. **Specification Files**:
   - `specs/005-zig-completion/plan.md` - Multiple path references
   - `specs/005-zig-completion/tasks.md` - Multiple path references
   - `specs/005-zig-completion/spec.md` - Path references

3. **Project Documentation**:
   - `AGENTS.md` - Path references to implementations
   - Any README files

### Medium Priority (Project Names)

1. `zaiko/README.md` - Update project name
2. `laiko/README.md` - Update project name
3. `laiko/laiko.asd` - Consider renaming

### Low Priority (Build System)

1. `shell.nix` - If it references old paths
2. CI/CD workflows - If they reference old paths
3. Any build helper scripts

## Verification Checklist

After completion, verify:

- [ ] All 29 zig commits replayed successfully
- [ ] All 1 lisp commit replayed successfully
- [ ] Git log shows correct commit history
- [ ] No references to `zaiko` remain (except in deprecated/)
- [ ] No references to `laiko` remain (except in deprecated/)
- [ ] `zaiko/build.zig` compiles successfully
- [ ] `laiko/build.sh` works correctly
- [ ] Documentation references updated
- [ ] Deprecation notices in place
- [ ] Project names updated consistently

## Notes

- The maiko submodule will remain unchanged - only the main repository is being updated
- Old locations (`zaiko` and `laiko`) will remain with deprecation notices
- Full git history replay ensures traceability and preserves development context
- Path transformations must be consistent across all file types (code, docs, configs)

## Next Steps

1. **Immediate**: Create `replay_zig_history.py` script to replay all 29 commits
2. **Then**: Create `update_path_references.py` to systematically update all references
3. **Finally**: Add deprecation notices and verify everything works
