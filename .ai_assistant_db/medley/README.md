# Medley Interlisp Documentation

**Navigation**: [Index](INDEX.md) | [Architecture](architecture.md) | [Components](components/) | [Interface](interface/) | [Platform](platform/) | [Glossary](glossary.md)

This directory contains comprehensive documentation of the Medley Interlisp system. Medley is the Lisp machine content that runs on the Maiko emulator, providing a complete Lisp development environment.

## Quick Start

1. **New to Medley?** → Start with [Architecture Overview](architecture.md)
2. **Understanding Medley-Maiko Interface?** → See [Interface Documentation](interface/)
3. **Understanding a Component?** → See [Component Documentation](components/)
4. **Looking for a Term?** → Check [Glossary](glossary.md)
5. **Need Quick Reference?** → Use [Index](INDEX.md)

## Documentation Structure

### [Architecture Overview](architecture.md)

High-level Medley system architecture, component relationships, and how Medley interacts with Maiko. Includes Mermaid diagrams showing system structure, data flow, and component interactions.

**Key Topics**:

- [System Architecture](architecture.md#high-level-architecture) - Overall Medley system design
- [Core Components](architecture.md#core-components) - Major Medley subsystems
- [Medley-Maiko Integration](architecture.md#medley-maiko-integration) - How Medley and Maiko work together
- [Data Flow](architecture.md#data-flow) - Information flow through the system

### [Component Documentation](components/)

Detailed documentation organized by Medley component:

- **[Scripts](components/scripts.md)** - Medley script system and argument parsing
  - Script types and platform variations
  - Argument parsing and transformation
  - Maiko invocation logic

- **[Sysout Files](components/sysout.md)** - Sysout file format and loading
  - Sysout file structure and types
  - Loading process and initialization
  - Relationship to Lisp system state

- **[Virtual Memory Files](components/vmem.md)** - Virtual memory file format and usage
  - Vmem file format and structure
  - Session persistence and continuation
  - Coordination with Maiko

- **[Configuration Files](components/configuration.md)** - Configuration file format and precedence
  - Config file format and parsing
  - Precedence rules and defaults
  - Platform-specific locations

- **[Greet Files](components/greetfiles.md)** - Greet file system and execution
  - Greet file format and purpose
  - Execution order and integration
  - Startup initialization

- **[Loadup Workflow](components/loadup.md)** - Sysout creation and build process
  - Loadup workflow and scripts
  - Sysout file creation
  - Build system integration

- **[Directory Structure](components/directory-structure.md)** - Medley directory organization
  - Directory layout and purpose
  - File types and organization
  - Source code structure

### [Interface Documentation](interface/)

Comprehensive documentation of the Medley-Maiko interface:

- **[Interface Overview](interface/README.md)** - Interface documentation navigation and overview
- **[Command-Line Interface](interface/command-line.md)** - Command-line arguments passed to Maiko
- **[Environment Variables](interface/environment.md)** - Environment variable communication
- **[File Formats](interface/file-formats.md)** - File format specifications (sysout, vmem, config, greet)
- **[Protocols](interface/protocols.md)** - Runtime communication protocols and patterns

### [Platform Documentation](platform/)

Platform-specific behaviors and variations:

- **[Linux](platform/linux.md)** - Linux-specific behaviors and script differences
- **[macOS](platform/macos.md)** - macOS-specific behaviors and medley.command script
- **[Windows](platform/windows.md)** - Windows/Cygwin-specific behaviors and medley.ps1 script
- **[WSL](platform/wsl.md)** - WSL-specific behaviors, VNC usage, and automation

### [Glossary](glossary.md)

Terminology, concepts, and abbreviations used throughout Medley documentation.

**Categories**:

- [Core Concepts](glossary.md#core-concepts) - MEDLEYDIR, LOGINDIR, sysout, vmem, etc.
- [File Types](glossary.md#file-types) - Sysout files, vmem files, greet files, config files
- [Script Terms](glossary.md#script-terms) - Medley scripts, argument parsing, Maiko invocation
- [Interface Terms](glossary.md#interface-terms) - Command-line arguments, environment variables, protocols

## Quick Navigation

### By Component Type

- **[Scripts](components/scripts.md)** - Medley script system
- **[Sysout Files](components/sysout.md)** - Sysout file format and loading
- **[Virtual Memory](components/vmem.md)** - Vmem files and session persistence
- **[Configuration](components/configuration.md)** - Configuration files and precedence
- **[Greet Files](components/greetfiles.md)** - Greet file system
- **[Loadup](components/loadup.md)** - Loadup workflow

### By Interface Type

- **[Command-Line](interface/command-line.md)** - Command-line argument mapping
- **[Environment Variables](interface/environment.md)** - Environment variable usage
- **[File Formats](interface/file-formats.md)** - File format specifications
- **[Protocols](interface/protocols.md)** - Runtime communication

### By Platform

- **[Linux](platform/linux.md)** - Linux-specific documentation
- **[macOS](platform/macos.md)** - macOS-specific documentation
- **[Windows](platform/windows.md)** - Windows/Cygwin-specific documentation
- **[WSL](platform/wsl.md)** - WSL-specific documentation

## Project Context

Medley is part of the Medley Interlisp system, which provides:

- A complete Lisp development environment (Interlisp and Common Lisp)
- Bytecode-based virtual machine execution via Maiko
- Cross-platform support (macOS, Linux, FreeBSD, Solaris, Windows)
- Multiple architecture support (x86_64, ARM64, SPARC, etc.)
- Applications including Notecards, Rooms, and CLOS

See [Architecture Overview](architecture.md) for more details on the system design.

## Documentation Conventions

- **File References**: `medley/path/to/file` refers to files in the Medley source tree
- **Script References**: `medley/scripts/medley/medley_run.sh` refers to Medley scripts
- **Maiko References**: Links to Maiko documentation use paths like `../README.md` (Maiko docs are in parent directory)
- **Links**: All documentation files are cross-linked for easy navigation
- **Diagrams**: Mermaid diagrams illustrate system architecture and data flow

## Related Resources

- **Medley Source Code**: `medley/` directory in repository root
- **Maiko Documentation**: `../README.md` - Maiko emulator documentation
- **Maiko Source Code**: `maiko/src/` directory - Emulator implementation
- **Project README**: Repository root `README.md` - Project overview

## Integration with Maiko

Medley runs on the Maiko emulator. For complete understanding:

- **Medley Documentation** (this directory): Medley system, scripts, files, and organization
- **Maiko Documentation** (`../`): Emulator implementation, VM core, memory management, I/O

The [Interface Documentation](interface/) provides the complete specification of how Medley and Maiko communicate.

## Last Updated

Documentation created: 2025-01-27

