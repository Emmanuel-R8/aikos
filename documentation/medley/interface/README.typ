= Medley-Maiko Interface Documentation

*Navigation*: Medley README | Medley Index | Architecture

== Overview

This directory contains comprehensive documentation of the interface between Medley and Maiko. The Medley-Maiko interface is the complete specification of how Medley scripts communicate with and control the Maiko emulator.

== Interface Mechanisms

Medley and Maiko communicate through four primary mechanisms:

=== 1. Command-Line Arguments

Medley scripts parse user arguments and transform them into Maiko command-line flags. The complete mapping of Medley flags to Maiko flags is documented in command-line.md.

*Key Topics*:

- Argument transformation logic
- Flag mapping (Medley → Maiko)
- Pass-through arguments (after `--`)
- All options from medley.1 man page

*See*: Command-Line Interface

=== 2. Environment Variables

Medley scripts set environment variables that Maiko reads during initialization and execution. These variables communicate file paths, configuration, and runtime state.

*Key Variables*:

- `MEDLEYDIR`: Top-level Medley installation directory
- `LOGINDIR`: User-specific Medley directory
- `LDESOURCESYSOUT`: Source sysout file path
- `LDEINIT`: Greet file path
- `LDEREMCM`: REM.CM file path
- `LDEDESTSYSOUT`: Destination vmem file path

*See*: Environment Variables

=== 3. File Formats

Medley and Maiko coordinate through standardized file formats. These formats enable sysout loading, session persistence, configuration, and initialization.

*File Types*:

- *Sysout Files*: Binary format containing Lisp system state
- *Vmem Files*: Binary format for session persistence
- *Config Files*: Text format for default arguments
- *Greet Files*: Lisp source code format

*See*: File Formats

=== 4. Runtime Protocols

Medley scripts invoke Maiko and coordinate the startup sequence, error handling, and session management through runtime protocols.

*Protocols*:

- Script invocation patterns
- Error handling and exit codes
- Maiko startup sequence
- Session management

*See*: Protocols

== Documentation Structure

#codeblock(lang: "text", [
interface/
├── README.md           # This file - interface overview
├── command-line.md     # Command-line argument mapping
├── environment.md      # Environment variable specifications
├── file-formats.md     # File format specifications
└── protocols.md        # Runtime communication protocols
])

== Quick Reference

=== Understanding How Medley Starts Maiko

1. *Command-Line Arguments*: command-line.md - How Medley flags map to Maiko flags
2. *Environment Setup*: environment.md - Environment variables set by Medley
3. *File Resolution*: file-formats.md - How files are located and passed to Maiko
4. *Invocation*: protocols.md - How scripts invoke Maiko

=== Understanding File Formats

1. *Sysout Format*: file-formats.md - Sysout file structure
2. *Vmem Format*: file-formats.md - Vmem file structure
3. *Config Format*: file-formats.md - Config file format
4. *Greet Format*: file-formats.md - Greet file format

=== Understanding Communication Flow

1. *Startup Sequence*: protocols.md - Complete startup flow
2. *Error Handling*: protocols.md - Error handling patterns
3. *Session Management*: protocols.md - Run ID and vmem coordination

== Integration with Component Documentation

Interface documentation complements component documentation:

- *Scripts Component*: ../components/scripts.md - How scripts use the interface
- *Sysout Component*: ../components/sysout.md - Sysout files in context
- *Vmem Component*: ../components/vmem.md - Vmem files in context
- *Configuration Component*: ../components/configuration.md - Config files in context
- *Greet Files Component*: ../components/greetfiles.md - Greet files in context

== Related Maiko Documentation

For understanding the Maiko side of the interface:

- *Maiko Architecture*: `../../architecture.md` - Maiko system architecture
- *Maiko Components*: `../../components/` - Maiko component documentation
- *Maiko API*: `../../api/` - Maiko API reference

== Interface Specification Completeness

This interface documentation provides:

- ✅ Complete command-line argument mapping (all medley.1 options)
- ✅ Complete environment variable specifications
- ✅ Complete file format specifications
- ✅ Complete runtime protocol specifications
- ✅ Platform-specific interface variations
- ✅ Cross-references to Maiko documentation

== Documentation Files

=== [Command-Line Interface](command-line.md)

Complete documentation of command-line argument mapping from Medley flags to Maiko flags.

*Contents*:

- Complete flag mapping table
- Argument transformation logic
- Maiko invocation pattern
- All options from medley.1 man page
- Examples and usage patterns

=== [Environment Variables](environment.md)

Complete specification of all environment variables used for Medley-Maiko communication.

*Contents*:

- MEDLEYDIR, LOGINDIR specifications
- LDESOURCESYSOUT, LDEDESTSYSOUT specifications
- LDEINIT, LDEREMCM specifications
- Variable lifecycle and resolution
- Platform considerations

=== [File Formats](file-formats.md)

Complete specifications of all file formats used in Medley-Maiko communication.

*Contents*:

- Sysout file format specification
- Vmem file format specification
- Config file format specification
- Greet file format specification
- REM.CM file format specification

=== [Protocols](protocols.md)

Complete documentation of runtime communication protocols.

*Contents*:

- Script invocation patterns
- Startup sequence
- Session continuation protocol
- Error handling
- Exit codes
- Session management
- Repeat protocol
- VNC protocol (WSL)

== For Implementors

If you are implementing a Medley-compatible system or a Maiko-compatible emulator:

1. *Read All Interface Docs*: Start with this overview, then read each interface mechanism document
2. *Reference Maiko Docs*: Understand Maiko's expectations for each interface mechanism
3. *Test Compatibility*: Verify your implementation matches the specifications
4. *Check Platform Docs*: Review platform-specific variations in ../platform/

== For Maintainers

If you are maintaining or extending Medley or Maiko:

1. *Update Interface Docs*: When interface mechanisms change, update the relevant documentation
2. *Maintain Consistency*: Ensure interface docs match implementation
3. *Cross-Reference*: Keep cross-references to component and Maiko docs current
4. *Document Edge Cases*: Add edge cases and error conditions to protocol documentation
