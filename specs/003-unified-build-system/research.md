# Research: Unified Build System for Medley with Multiple Maiko Emulators

**Date**: 2025-01-27
**Feature**: Unified Build System
**Phase**: Phase 0 - Outline & Research

## Research Questions

### 1. Build Output Locations and Executable Names

**Question**: Where do each emulator's build systems place their outputs, and what are the executable names?

**Research Findings**:

#### C Emulator
- **Build Systems**: CMake or Make
- **Output Location (Make)**: `maiko/<os>.<arch>/` (e.g., `maiko/linux.x86_64/`)
- **Output Location (CMake)**: Build directory specified during cmake configuration
- **Executables**:
  - `lde` - Lisp Development Environment (init version)
  - `ldeinit` - Initialization executable
  - `ldex` - Execution version with X11 display
  - `ldesdl` - Execution version with SDL display
- **Platform Detection**: Uses `maiko/bin/osversion` and `maiko/bin/machinetype`

#### Zig Emulator
- **Build System**: Zig build (`zig build`)
- **Output Location**: `maiko/alternatives/zig/zig-out/bin/maiko-zig`
- **Executable**: `maiko-zig` (single executable)
- **Platform Detection**: Zig build system handles target platform via `b.standardTargetOptions`
- **Note**: Currently outputs to `zig-out/bin/` relative to source directory

#### Lisp Emulator
- **Build System**: ASDF (via SBCL)
- **Output Location**: ASDF compiles to FASL files, but no standalone executable is created
- **Execution**: Requires SBCL to load and run: `sbcl --load maiko-lisp.asd --eval "(asdf:load-system :maiko-lisp)"`
- **Note**: Lisp implementation may need a wrapper script or executable builder to match other emulators

**Decision**:
- C emulator outputs to `maiko/<os>.<arch>/` with multiple executables
- Zig emulator outputs to `zig-out/bin/maiko-zig` relative to source
- Lisp emulator requires SBCL runtime, may need executable wrapper
- Unified system will copy/move executables to `maiko/build/<emulator>/<os>.<arch>/`

**Rationale**:
- Unified location structure provides consistent interface for Medley scripts
- Moving/copying allows preserving original build outputs
- Lisp emulator may need special handling to create executable wrapper

**Alternatives Considered**:
- Symlinking instead of copying: Rejected due to potential issues with relative paths and portability
- Modifying build systems to output directly: Rejected per requirement to not modify existing build systems

---

### 2. Build Completion Detection

**Question**: How can the system detect if an emulator build is complete, incomplete, or needs rebuilding?

**Research Findings**:

#### C Emulator
- **Completion Indicators**:
  - Presence of `lde` and `ldex` (or `ldesdl`) executables in output directory
  - Executables must be executable (`-x` test)
  - Timestamp comparison with source files for incremental builds
- **Incomplete Build Detection**: Missing required executables, non-executable files, or corrupted binaries

#### Zig Emulator
- **Completion Indicators**:
  - Presence of `zig-out/bin/maiko-zig` executable
  - Executable must be executable
  - Zig build system tracks dependencies internally
- **Incomplete Build Detection**: Missing executable or build errors

#### Lisp Emulator
- **Completion Indicators**:
  - Compiled FASL files in ASDF cache
  - System successfully loads via ASDF
  - Wrapper script or executable exists (if created)
- **Incomplete Build Detection**: ASDF load failures, missing dependencies

**Decision**:
- Check for required executables in expected locations
- Verify executables are actually executable (`test -x`)
- For incremental builds: compare executable timestamps with source file modification times
- Use build system exit codes to detect build failures
- Store build metadata (timestamp, platform, options) for validation

**Rationale**:
- Simple file existence and permissions checks are reliable
- Timestamp comparison enables incremental builds without complex dependency tracking
- Build system exit codes provide immediate failure detection

**Alternatives Considered**:
- Complex dependency graph tracking: Rejected as too complex for shell scripts
- Build artifact checksums: Rejected as unnecessary overhead for initial implementation

---

### 3. Integration with Medley Loadup Scripts

**Question**: How can the unified build system integrate with existing Medley loadup scripts without breaking compatibility?

**Research Findings**:

#### Current Loadup Script Behavior
- **Location Resolution Order**:
  1. Check PATH for `lde`/`ldeinit`
  2. Check `$MAIKODIR/<os>.<arch>/` if MAIKODIR is set
  3. Check `$MEDLEYDIR/../maiko/<os>.<arch>/`
  4. Check `$MEDLEYDIR/maiko/<os>.<arch>/`
- **Required Functions**:
  - `check_if_maiko_dir()`: Validates Maiko directory structure
  - `check_for_maiko_exe()`: Finds executable in platform subdirectory
- **Executable Names**: Expects `lde`, `ldeinit`, `ldex`, `ldesdl`

#### Integration Points
- **Option 1**: Modify location resolution to include `maiko/build/<emulator>/<os>.<arch>/`
- **Option 2**: Create symlinks from existing locations to unified build directory
- **Option 3**: Set `MAIKODIR` environment variable to point to unified build directory
- **Option 4**: Modify loadup scripts to accept `--emulator` argument and adjust location resolution

**Decision**:
- **Hybrid Approach**:
  - Add `maiko/build/<emulator>/<os>.<arch>/` to location resolution order (after existing locations)
  - Add `--emulator` argument to loadup scripts to specify which emulator to use
  - Support `MEDLEY_EMULATOR` environment variable for default selection
  - Preserve all existing location resolution for backward compatibility
  - Automatically build emulator if not found and `--auto-build` flag is set (or default behavior)

**Rationale**:
- Adding to resolution order maintains backward compatibility
- Command-line and environment variable selection provides flexibility
- Automatic building integrates emulator builds into loadup workflow
- Preserving existing behavior ensures no breaking changes

**Alternatives Considered**:
- Replacing existing location resolution: Rejected as breaking change
- Only supporting unified location: Rejected as too restrictive

---

### 4. Build Option Passing

**Question**: How should build options (display backend, release version, etc.) be passed to different build systems?

**Research Findings**:

#### C Emulator Options
- **CMake**: `-DMAIKO_DISPLAY_X11=ON`, `-DMAIKO_DISPLAY_SDL=2`, `-DMAIKO_RELEASE=351`, `-DMAIKO_NETWORK_TYPE=NONE`
- **Make**: Environment variables and makefile variables, `./makeright x` or `./makeright sdl`

#### Zig Emulator Options
- **Zig Build**: Currently minimal options, uses `b.standardTargetOptions` and `b.standardOptimizeOption`
- **Display Backend**: SDL2 linking (currently commented out for NixOS compatibility)
- **Future**: May need build options for display backend selection

#### Lisp Emulator Options
- **ASDF**: System definition in `maiko-lisp.asd`, dependencies specified there
- **Display Backend**: SDL3 (specified in system definition)
- **Runtime Options**: Passed via SBCL command-line arguments

**Decision**:
- **Unified Option Interface**:
  - `--display-backend <x11|sdl2|sdl3>`: Maps to appropriate build system option
  - `--release <version>`: For C emulator only (351 default)
  - `--network-type <type>`: For C emulator only (NONE default)
  - `--optimize <debug|release>`: For all emulators where supported
- **Build System Mapping**:
  - C/CMake: `-DMAIKO_DISPLAY_X11=ON/OFF`, `-DMAIKO_DISPLAY_SDL=2/3`, `-DMAIKO_RELEASE=<version>`
  - C/Make: `./makeright x` or `./makeright sdl`, environment variables for release
  - Zig: Pass via `zig build` arguments (may need build.zig modifications, but out of scope)
  - Lisp: Options are in system definition (out of scope to modify)

**Rationale**:
- Unified interface simplifies user experience
- Mapping layer translates options to build-system-specific formats
- Defaults match existing behavior
- Some options only apply to C emulator (release version, network type)

**Alternatives Considered**:
- Build-system-specific options: Rejected as too complex for users
- Configuration files: Considered but deferred to future enhancement

---

### 5. Incremental Build Detection

**Question**: How can the system detect when an emulator needs rebuilding vs. when existing build is still valid?

**Research Findings**:

#### Source Change Detection
- **C Emulator**: Many source files in `maiko/src/`, header files in `maiko/inc/`
- **Zig Emulator**: Source files in `maiko/alternatives/zig/src/`
- **Lisp Emulator**: Source files in `maiko/alternatives/lisp/src/`

#### Build System Capabilities
- **CMake**: Tracks dependencies, only rebuilds changed files
- **Make**: Dependency-based, only rebuilds changed targets
- **Zig Build**: Tracks dependencies internally
- **ASDF**: Tracks file dependencies, recompiles only changed files

**Decision**:
- **Simple Timestamp Comparison** (Initial Implementation):
  - Compare executable modification time with source directory modification time
  - If sources newer than executable, trigger rebuild
  - Use `find` with `-newer` or compare `stat` timestamps
- **Build Metadata File** (Future Enhancement):
  - Store build configuration, platform, options in metadata file
  - Compare current configuration with stored metadata
  - Rebuild if configuration changed even if sources unchanged

**Rationale**:
- Timestamp comparison is simple and reliable for shell scripts
- Build systems handle fine-grained dependency tracking internally
- Metadata file enables detecting configuration changes
- Can be enhanced later without breaking existing functionality

**Alternatives Considered**:
- Full dependency graph tracking: Rejected as too complex for shell scripts
- Always rebuild: Rejected as too slow for developer workflow
- Rely solely on build systems: Rejected as doesn't detect configuration changes

---

### 6. Prerequisite Detection

**Question**: How should the system detect and report missing build prerequisites?

**Research Findings**:

#### Required Prerequisites
- **C Emulator**: C compiler (clang/gcc), CMake or Make, X11 or SDL2 libraries
- **Zig Emulator**: Zig compiler 0.11+, SDL2 libraries
- **Lisp Emulator**: SBCL, ASDF (included with SBCL), SDL3 libraries

#### Detection Methods
- **Command Availability**: `command -v <tool>` or `which <tool>`
- **Version Checking**: Run tool with `--version` and parse output
- **Library Detection**: `pkg-config` or check library paths

**Decision**:
- **Prerequisite Checking Function**:
  - Check for required commands: `clang`, `gcc`, `cmake`, `make`, `zig`, `sbcl`
  - Check for version requirements where applicable (Zig 0.11+)
  - Check for libraries via `pkg-config` or common library paths
  - Report missing prerequisites with clear error messages
  - Continue building other emulators if one has missing prerequisites
  - Provide installation hints in error messages

**Rationale**:
- Early detection prevents confusing build failures
- Clear error messages help developers resolve issues quickly
- Continuing with available emulators improves user experience
- Installation hints reduce support burden

**Alternatives Considered**:
- Auto-install prerequisites: Rejected as out of scope and platform-specific
- Fail fast on any missing prerequisite: Rejected as too restrictive

---

## Summary

All research questions have been resolved. Key decisions:

1. **Build Outputs**: Copy/move executables to unified `maiko/build/<emulator>/<os>.<arch>/` structure
2. **Build Detection**: File existence, permissions, and timestamp comparison
3. **Integration**: Add unified location to resolution order, add `--emulator` argument, preserve backward compatibility
4. **Build Options**: Unified interface with mapping to build-system-specific options
5. **Incremental Builds**: Timestamp comparison with optional metadata file for configuration tracking
6. **Prerequisites**: Command and library detection with clear error messages

No remaining `NEEDS CLARIFICATION` markers. Ready to proceed to Phase 1: Design & Contracts.
