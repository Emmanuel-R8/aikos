# Feature Specification: Medley Documentation Project

**Feature Branch**: `001-medley-documentation`
**Created**: 2025-01-27
**Status**: Draft
**Input**: User description: "We are starting a documentation project in a new branch. We will document the contents of @medley/docs/man-page/medley.1 The contents of the documentation will be stored in @.ai_assistant_db which already contains the documentation of maiko. The documentation you will create should contain everything that a new implementor of medley should know. You should also thoroughly document the interface between medley and maiko."

## User Scenarios & Testing

### User Story 1 - New Implementor Understands Medley Architecture (Priority: P1)

A developer who wants to implement a new Medley system from scratch needs comprehensive documentation covering all Medley components, their structure, and how they work together.

**Why this priority**: This is the primary goal - enabling new implementors to understand and build Medley systems. Without this, the documentation project fails its core purpose.

**Independent Test**: A developer unfamiliar with Medley can read the documentation and understand the complete system architecture, component relationships, and implementation requirements without needing to examine source code directly.

**Acceptance Scenarios**:

1. **Given** a developer wants to understand Medley's overall architecture, **When** they read the architecture documentation, **Then** they understand how scripts, Lisp code, sysout files, and Maiko interact
2. **Given** a developer needs to understand a specific Medley component, **When** they navigate to the component documentation, **Then** they find detailed information about that component's purpose, structure, and dependencies
3. **Given** a developer wants to understand Medley's directory structure, **When** they read the documentation, **Then** they understand what each major directory contains and its purpose

---

### User Story 2 - Developer Understands Medley-Maiko Interface (Priority: P1)

A developer needs to understand exactly how Medley communicates with and controls the Maiko emulator, including all command-line arguments, environment variables, file formats, and protocols.

**Why this priority**: The Medley-Maiko interface is critical for both implementors and maintainers. Understanding this interface is essential for debugging, extending functionality, and ensuring compatibility.

**Independent Test**: A developer can read the interface documentation and understand all mechanisms by which Medley passes information to Maiko, including command-line arguments, environment variables, file formats, and runtime protocols.

**Acceptance Scenarios**:

1. **Given** a developer wants to understand how Medley starts Maiko, **When** they read the interface documentation, **Then** they understand all command-line arguments passed from Medley scripts to Maiko
2. **Given** a developer needs to understand sysout file format, **When** they read the documentation, **Then** they understand the complete sysout file structure and how Maiko loads it
3. **Given** a developer wants to understand virtual memory files, **When** they read the documentation, **Then** they understand the vmem file format and how Medley and Maiko coordinate its use
4. **Given** a developer needs to understand environment variable communication, **When** they read the documentation, **Then** they understand all environment variables used for Medley-Maiko communication

---

### User Story 3 - Maintainer Finds Implementation Details (Priority: P2)

A developer maintaining or extending Medley needs detailed documentation of implementation specifics, including script behavior, Lisp code organization, and configuration file formats.

**Why this priority**: While less critical than architecture understanding, detailed implementation documentation significantly improves maintainability and reduces time to understand code changes.

**Independent Test**: A maintainer can find specific implementation details about any Medley component, script, or configuration mechanism without needing to reverse-engineer from source code.

**Acceptance Scenarios**:

1. **Given** a maintainer needs to understand how the medley script works, **When** they read the script documentation, **Then** they understand argument parsing, Maiko invocation, and all script behaviors
2. **Given** a maintainer wants to modify configuration file handling, **When** they read the documentation, **Then** they understand the config file format, precedence rules, and parsing logic
3. **Given** a maintainer needs to understand greet file execution, **When** they read the documentation, **Then** they understand greet file format, execution order, and integration with Maiko startup

---

### Edge Cases

- What happens when multiple Medley instances try to use the same ID or vmem file?
- How does Medley handle missing or corrupted sysout files?
- What happens when environment variables conflict between config file and command line?
- How does Medley handle platform-specific differences (Windows/Cygwin, WSL, macOS, Linux)?
- What happens when Maiko executable is not found or fails to start?
- How does Medley handle invalid command-line argument combinations?
- What happens when greet files or REM.CM files contain errors?
- How does Medley handle network configuration when Nethub is unavailable?

## Requirements

### Functional Requirements

- **FR-001**: Documentation MUST cover all Medley components including scripts, Lisp source code, configuration files, greet files, sysout files, and directory structure
- **FR-002**: Documentation MUST comprehensively document the Medley-Maiko interface including all command-line arguments, environment variables, file formats, and communication protocols
- **FR-003**: Documentation MUST explain Medley's architecture and how all components interact
- **FR-004**: Documentation MUST be organized in `.ai_assistant_db` directory following a structure similar to existing Maiko documentation
- **FR-005**: Documentation MUST include detailed information about sysout file format, structure, and loading process
- **FR-006**: Documentation MUST explain virtual memory (vmem) file format, usage, and coordination between Medley and Maiko
- **FR-007**: Documentation MUST document all command-line flags and options from the medley.1 man page with implementation context
- **FR-008**: Documentation MUST explain environment variable usage including MEDLEYDIR, LOGINDIR, LDESOURCESYSOUT, LDEINIT, LDEREMCM, and LDEDESTSYSOUT
- **FR-009**: Documentation MUST document configuration file format, precedence rules, and parsing behavior
- **FR-010**: Documentation MUST explain greet file format, execution order, and purpose
- **FR-011**: Documentation MUST document REM.CM file usage and execution context
- **FR-012**: Documentation MUST explain how Medley scripts invoke Maiko, including argument transformation and environment setup
- **FR-013**: Documentation MUST cover platform-specific behaviors (Windows/Cygwin, WSL, macOS, Linux, X11, SDL, VNC)
- **FR-014**: Documentation MUST explain network configuration (Nethub) and how it integrates with Maiko
- **FR-015**: Documentation MUST document the loadup workflow and how sysout files are created
- **FR-016**: Documentation MUST include cross-references to relevant Maiko documentation where interfaces are discussed

### Key Entities

- **Medley Script**: The main entry point script that parses arguments, sets up environment, and invokes Maiko
- **Sysout File**: A binary file containing a complete Lisp system state that Maiko can load and execute
- **Virtual Memory File (vmem)**: A file storing the persistent state of a Medley session, allowing continuation across restarts
- **Configuration File**: A text file containing default command-line arguments for Medley
- **Greet File**: A Lisp file executed during Medley startup to initialize the environment
- **REM.CM File**: A Lisp file executed after greet files, typically used for loadup operations
- **LOGINDIR**: The directory where Medley stores user-specific files (vmem, INIT files)
- **MEDLEYDIR**: The top-level directory of the Medley installation
- **Maiko Executable**: The emulator binary that Medley invokes to run the Lisp system

## Success Criteria

### Measurable Outcomes

- **SC-001**: A developer new to Medley can understand the complete system architecture by reading the documentation without needing to examine source code
- **SC-002**: Documentation covers 100% of command-line options documented in medley.1 man page with implementation context
- **SC-003**: Documentation provides complete interface specification for all Medley-Maiko communication mechanisms (command-line, environment variables, file formats)
- **SC-004**: Documentation is organized and cross-referenced such that developers can find specific information within 3 navigation steps
- **SC-005**: Documentation includes sufficient detail that a developer can implement a Medley-compatible system by following the documentation
- **SC-006**: All major Medley components (scripts, directories, file types) are documented with their purpose and relationships
- **SC-007**: Documentation structure mirrors Maiko documentation organization for consistency and ease of navigation
- **SC-008**: Documentation includes diagrams and examples illustrating Medley-Maiko interaction flows

## Assumptions

- Documentation will be written in Markdown format consistent with existing `.ai_assistant_db` documentation
- Documentation will include Mermaid diagrams where helpful for illustrating architecture and flows
- Existing Maiko documentation structure provides a good template for Medley documentation organization
- Source code analysis will be required to document implementation details not covered in the man page
- Documentation should be suitable for both implementors building new systems and maintainers extending existing code

## Out of Scope

- Detailed documentation of Lisp language features (covered by Lisp documentation)
- User guide for using Medley (covered by user documentation)
- Maiko implementation details (covered by existing Maiko documentation)
- Historical context or design rationale unless directly relevant to implementation

## Dependencies

- Access to Medley source code for implementation details
- Existing Maiko documentation in `.ai_assistant_db` for interface context
- medley.1 man page as primary reference for command-line interface
