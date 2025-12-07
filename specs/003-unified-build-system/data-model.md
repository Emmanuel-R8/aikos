# Data Model: Unified Build System for Medley

**Date**: 2025-01-27
**Feature**: Unified Build System
**Phase**: Phase 1 - Design & Contracts

## Entities

### EmulatorBuildConfig

Represents the configuration for building a specific emulator.

**Attributes**:
- `emulator_type` (string, required): One of `"c"`, `"zig"`, or `"lisp"`
- `source_directory` (path, required): Path to emulator source code
- `output_directory` (path, required): Path where built executables will be placed (`maiko/build/<emulator>/<os>.<arch>/`)
- `build_tool` (string, required): Native build tool (`"cmake"`, `"make"`, `"zig"`, `"asdf"`)
- `platform` (string, required): Platform identifier (`"<os>.<arch>"`, e.g., `"linux.x86_64"`)
- `display_backend` (string, optional): Display backend (`"x11"`, `"sdl2"`, `"sdl3"`, default depends on emulator)
- `release_version` (string, optional): Release version for C emulator (default: `"351"`)
- `network_type` (string, optional): Network type for C emulator (default: `"NONE"`)
- `optimize_level` (string, optional): Optimization level (`"debug"`, `"release"`, default: `"release"`)

**Validation Rules**:
- `emulator_type` must be one of the valid values
- `source_directory` must exist and be a directory
- `output_directory` must be a valid path (will be created if doesn't exist)
- `platform` must match pattern `<os>.<arch>`
- `display_backend` must be valid for the emulator type
- `release_version` only applies to C emulator

**State Transitions**:
- **Initial**: Config created with default values
- **Configured**: All required attributes set, optional attributes may have defaults
- **Validated**: Config validated against source directory and prerequisites

---

### BuildArtifact

Represents a built executable or artifact from an emulator build.

**Attributes**:
- `emulator_type` (string, required): Emulator that produced this artifact
- `platform` (string, required): Platform identifier
- `executable_name` (string, required): Name of the executable (e.g., `"lde"`, `"maiko-zig"`)
- `executable_path` (path, required): Full path to the executable
- `build_timestamp` (datetime, required): When the artifact was built
- `build_options` (object, optional): Build options used (display backend, release version, etc.)
- `is_executable` (boolean, computed): Whether file exists and is executable

**Validation Rules**:
- `executable_path` must exist
- File at `executable_path` must be executable (`test -x`)
- `build_timestamp` must be valid datetime

**State Transitions**:
- **Built**: Executable exists and is executable
- **Stale**: Source files newer than executable (needs rebuild)
- **Missing**: Executable doesn't exist (needs build)
- **Invalid**: Executable exists but not executable (needs rebuild)

---

### PlatformInfo

Represents detected platform information.

**Attributes**:
- `os` (string, required): Operating system (`"linux"`, `"darwin"`, `"freebsd"`, `"sunos"`, `"windows"`)
- `arch` (string, required): Architecture (`"x86_64"`, `"i386"`, `"arm64"`, `"arm7l"`, `"sparc"`)
- `platform_string` (string, computed): Combined platform identifier (`"<os>.<arch>"`)
- `detection_method` (string, required): How platform was detected (`"osversion"`, `"machinetype"`, `"uname"`, `"manual"`)

**Validation Rules**:
- `os` must be one of supported operating systems
- `arch` must be one of supported architectures
- `platform_string` must match pattern `<os>.<arch>`

**State Transitions**:
- **Detected**: Platform information successfully detected
- **Manual**: Platform information provided manually (override)

---

### PrerequisiteCheck

Represents the result of checking build prerequisites.

**Attributes**:
- `emulator_type` (string, required): Emulator being checked
- `tool_name` (string, required): Name of required tool (e.g., `"zig"`, `"sbcl"`)
- `is_available` (boolean, required): Whether tool is available
- `version` (string, optional): Version of tool if available
- `version_requirement` (string, optional): Required version (e.g., `"0.11+"`)
- `meets_requirement` (boolean, computed): Whether version meets requirement
- `library_name` (string, optional): Required library name (e.g., `"SDL2"`)
- `library_available` (boolean, optional): Whether library is available

**Validation Rules**:
- `is_available` must be true for build to proceed
- If `version_requirement` specified, `meets_requirement` must be true
- If `library_name` specified, `library_available` must be true

**State Transitions**:
- **Checking**: Prerequisites being checked
- **Available**: All prerequisites available
- **Missing**: One or more prerequisites missing
- **VersionMismatch**: Tool available but version doesn't meet requirement

---

### BuildResult

Represents the result of a build operation.

**Attributes**:
- `emulator_type` (string, required): Emulator that was built
- `platform` (string, required): Platform identifier
- `success` (boolean, required): Whether build succeeded
- `exit_code` (integer, optional): Exit code from build command
- `error_message` (string, optional): Error message if build failed
- `artifacts` (array of BuildArtifact, optional): Built artifacts if successful
- `build_duration` (duration, optional): How long build took
- `build_command` (string, optional): Command that was executed

**Validation Rules**:
- If `success` is true, `artifacts` must be non-empty
- If `success` is false, `error_message` should be present
- `exit_code` should be 0 if `success` is true

**State Transitions**:
- **Building**: Build in progress
- **Success**: Build completed successfully, artifacts created
- **Failed**: Build failed, error information available

---

## Relationships

- **EmulatorBuildConfig** → **PlatformInfo**: Each config has one platform
- **EmulatorBuildConfig** → **BuildArtifact**: Config produces multiple artifacts
- **BuildArtifact** → **PlatformInfo**: Each artifact is for a specific platform
- **EmulatorBuildConfig** → **PrerequisiteCheck**: Config requires prerequisite checks
- **EmulatorBuildConfig** → **BuildResult**: Config produces build result

---

## Data Flow

1. **Configuration**: Create `EmulatorBuildConfig` with platform detection (`PlatformInfo`)
2. **Prerequisite Check**: Check prerequisites (`PrerequisiteCheck`) for the emulator
3. **Build Execution**: Execute build using config, producing `BuildResult`
4. **Artifact Creation**: `BuildResult` contains `BuildArtifact` objects
5. **Validation**: Verify artifacts exist and are executable

---

## Storage

All entities are transient (in-memory during script execution). No persistent storage required. Build metadata (timestamps, options) may be stored in files for incremental build detection, but this is optional.

---

## Notes

- Entities are conceptual - actual implementation will use shell script variables and functions
- No database or persistent storage needed
- File system serves as the "database" (executables, build directories)
- Timestamps and file existence serve as state indicators
