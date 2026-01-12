# Feature Specification: Unified Build System for Medley with Multiple Maiko Emulators

**Feature Branch**: `003-unified-build-system`
**Created**: 2025-01-27
**Status**: Draft
**Input**: User description: "We are going to create a new set of build files for the Medley environment. The build files should be able to accommodate the 3 different Maiko emulators in @maiko and @maiko/alternatives. Keep in mind that you have created a full set of information in @documentation. If at any point that documentation feels incomplete, the C version of the emulator and the original Medley are your reference to improve your notes."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Build All Maiko Emulators (Priority: P1)

A developer wants to build all three Maiko emulator implementations (C, Zig, and Lisp) from a single command, with each emulator built using its native build tool, and all executables placed in a unified location structure.

**Why this priority**: This is the foundational capability that enables all other build workflows. Without the ability to build all emulators, the unified system cannot function.

**Independent Test**: Can be fully tested by running a build command that produces executables for all three emulators in the expected locations, without requiring Medley loadup scripts to run.

**Acceptance Scenarios**:

1. **Given** a clean repository with all three emulator implementations, **When** a developer runs the unified build command, **Then** all three emulators are built successfully using their native build tools (CMake/Make for C, Zig build for Zig, ASDF for Lisp)
2. **Given** a repository with only one emulator implementation available, **When** a developer runs the unified build command, **Then** only the available emulator is built, with clear messaging about which emulators were skipped
3. **Given** a repository with build prerequisites missing for one emulator, **When** a developer runs the unified build command, **Then** the build continues for other emulators and reports which emulator failed and why

---

### User Story 2 - Integrated Medley Loadup with Emulator Selection (Priority: P1)

A developer wants to build Medley sysout files using any of the three Maiko emulators, with the build system automatically building the required emulator if it's not already built, and integrating the emulator build into the loadup workflow.

**Why this priority**: This directly addresses the user's requirement to fully integrate Maiko builds into the Medley loadup process. It's essential for the unified build experience.

**Independent Test**: Can be fully tested by running a Medley loadup command that specifies an emulator, verifying that the emulator is built if needed, and that the loadup completes successfully using that emulator.

**Acceptance Scenarios**:

1. **Given** a clean repository with no built emulators, **When** a developer runs a Medley loadup command specifying the C emulator, **Then** the C emulator is built first, then the loadup proceeds using the C emulator
2. **Given** a repository with all emulators already built, **When** a developer runs a Medley loadup command specifying the Zig emulator, **Then** the loadup proceeds immediately using the pre-built Zig emulator without rebuilding
3. **Given** a repository with a partially built emulator, **When** a developer runs a Medley loadup command, **Then** the system detects the incomplete build and rebuilds the emulator before proceeding

---

### User Story 3 - Select Emulator for Loadup (Priority: P2)

A developer wants to choose which Maiko emulator to use for building Medley sysout files, either through command-line arguments, environment variables, or configuration files, with sensible defaults.

**Why this priority**: While essential for flexibility, this can be implemented after the core build and integration functionality. Developers need the ability to test different emulators.

**Independent Test**: Can be fully tested by running loadup commands with different emulator selections and verifying that the correct emulator is used for each loadup.

**Acceptance Scenarios**:

1. **Given** all three emulators are built, **When** a developer specifies `--emulator zig` on a loadup command, **Then** the loadup uses the Zig emulator
2. **Given** a developer has set `MEDLEY_EMULATOR=lisp` in their environment, **When** they run a loadup command without specifying an emulator, **Then** the loadup uses the Lisp emulator
3. **Given** no emulator is specified and no environment variable is set, **When** a developer runs a loadup command, **Then** the system uses the C emulator as the default

---

### User Story 4 - Build Individual Emulators (Priority: P2)

A developer wants to build a specific emulator without building all three, either for faster iteration during development or because they only need one emulator.

**Why this priority**: Important for developer workflow efficiency, but not critical for the core functionality. Allows developers to work on one emulator without the overhead of building others.

**Independent Test**: Can be fully tested by running a build command targeting a specific emulator and verifying that only that emulator is built.

**Acceptance Scenarios**:

1. **Given** a repository with all three emulator sources, **When** a developer runs `build-emulator --emulator c`, **Then** only the C emulator is built
2. **Given** a developer is working on the Zig emulator, **When** they run the build command for Zig only, **Then** the build completes faster than building all three emulators

---

### User Story 5 - Cross-Platform Build Support (Priority: P3)

A developer wants to build emulators on different operating systems and architectures, with the build system detecting the platform and building appropriately for each emulator's capabilities.

**Why this priority**: Important for broad compatibility but not essential for initial functionality. The existing emulators already have platform detection, so this is primarily about ensuring the unified system respects those mechanisms.

**Independent Test**: Can be fully tested by running builds on different platforms and verifying that platform-specific builds are created correctly.

**Acceptance Scenarios**:

1. **Given** a Linux x86_64 system, **When** a developer runs the unified build, **Then** emulators are built for linux.x86_64 platform
2. **Given** a macOS ARM64 system, **When** a developer runs the unified build, **Then** emulators are built for darwin.aarch64 platform
3. **Given** a developer specifies a target platform different from their current system, **When** they run a cross-compilation build, **Then** the build system attempts cross-compilation where supported by the native build tools

---

### Edge Cases

- What happens when a developer specifies an emulator that doesn't exist in the repository?
- How does the system handle build failures in one emulator when building all three?
- What happens if an emulator's native build tool is not installed (e.g., Zig compiler missing)?
- How does the system handle partial builds that were interrupted?
- What happens when the build directory structure exists but contains incomplete or corrupted builds?
- How does the system handle version mismatches between emulator implementations?
- What happens when loadup scripts are run with an emulator that hasn't been built yet?
- How does the system handle concurrent builds of the same emulator?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST build all three Maiko emulator implementations (C, Zig, Lisp) using their native build tools
- **FR-002**: System MUST place all built emulator executables in unified location structure: `maiko/build/<emulator>/<os>.<arch>/` where `<emulator>` is one of `c`, `zig`, or `lisp`
- **FR-003**: System MUST integrate emulator builds into Medley loadup workflow, building emulators automatically when needed
- **FR-004**: System MUST allow developers to select which emulator to use for loadup operations via command-line arguments, environment variables, or configuration
- **FR-005**: System MUST use C emulator as default when no emulator is specified
- **FR-006**: System MUST detect platform (OS and architecture) and build emulators appropriately for the detected platform
- **FR-007**: System MUST preserve existing Medley loadup script functionality and compatibility
- **FR-008**: System MUST support building individual emulators without building all three
- **FR-009**: System MUST provide clear error messages when emulator builds fail, including which emulator failed and why
- **FR-010**: System MUST skip building emulators that are not available in the repository, with informative messages
- **FR-011**: System MUST detect and handle missing build prerequisites for each emulator type
- **FR-012**: System MUST support incremental builds, only rebuilding emulators when source changes are detected
- **FR-013**: System MUST maintain executable naming conventions expected by Medley scripts (`lde`, `ldeinit`, `ldex`, `ldesdl`)
- **FR-014**: System MUST work with existing Maiko location resolution logic in Medley scripts (PATH, MAIKODIR, MEDLEYDIR/../maiko, MEDLEYDIR/maiko)
- **FR-015**: System MUST allow developers to specify build options (display backend, release version, etc.) that are passed to native build tools

### Key Entities

- **Emulator Build Configuration**: Represents the settings and options for building a specific emulator, including build tool type, source location, output location, and build options
- **Unified Build Orchestrator**: Coordinates building multiple emulators, manages build order and dependencies, and provides unified interface for build operations
- **Platform Detection**: Identifies operating system and architecture for determining build targets and output locations
- **Build Artifact Location**: Represents the unified location structure where built executables are placed, following pattern `maiko/build/<emulator>/<os>.<arch>/`

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can build all three emulators from a single command in under 10 minutes on a standard development machine
- **SC-002**: Developers can successfully run Medley loadup using any of the three emulators with 100% success rate when prerequisites are met
- **SC-003**: Build system correctly detects and builds for the current platform in 95% of standard development environments (Linux, macOS, FreeBSD)
- **SC-004**: Developers can build a single emulator in under 3 minutes on a standard development machine
- **SC-005**: Build system provides clear, actionable error messages that allow developers to resolve build issues without consulting documentation in 90% of common failure scenarios
- **SC-006**: Existing Medley loadup workflows continue to function without modification when using pre-built emulators
- **SC-007**: Build system successfully integrates emulator builds into loadup process, with automatic emulator building adding less than 5 minutes to total loadup time when emulators need to be built

## Assumptions

- Each emulator implementation will continue to use its native build tool (CMake/Make for C, Zig build for Zig, ASDF for Lisp)
- The C emulator will remain the default choice for compatibility with existing workflows
- Platform detection mechanisms from existing build systems will be reused where possible
- The unified build system will be a wrapper/orchestrator rather than replacing individual build systems
- Build prerequisites (compilers, libraries) are the responsibility of the developer to install
- The build system will be implemented primarily as shell scripts, consistent with existing Medley build infrastructure
- Executable naming conventions will remain compatible with existing Medley scripts
- The build directory structure `maiko/build/` will be separate from existing build outputs to avoid conflicts
- Emulator version identifiers will use simple names: `c`, `zig`, `lisp` (not version numbers)

## Dependencies

- Access to all three emulator source trees (C in `maiko/src/`, Zig in `zaiko/`, Lisp in `laiko/`)
- Native build tools for each emulator (CMake/Make, Zig compiler, SBCL with ASDF)
- Platform detection utilities (`osversion`, `machinetype` from Maiko)
- Existing Medley loadup scripts and their expected interfaces
- Documentation in `documentation/` for reference and validation

## Out of Scope

- Modifying the internal build systems of individual emulators (CMakeLists.txt, build.zig, laiko.asd)
- Cross-compilation support beyond what native build tools provide
- Automated installation of build prerequisites
- Build caching or artifact management beyond basic incremental detection
- Support for emulator implementations beyond the three specified (C, Zig, Lisp)
- Modifying the core functionality of Medley loadup scripts beyond integration points
- Creating a new unified build tool to replace CMake, Zig build, or ASDF
