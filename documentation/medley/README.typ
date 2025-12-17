= Medley Interlisp Documentation


This directory contains comprehensive documentation of the Medley Interlisp system. Medley is the Lisp machine content that runs on the Maiko emulator, providing a complete Lisp development environment.

== Quick Start

1. *New to* Medley?* → Start with Architecture Overview
2. *Understanding Medley-Maiko* Interface?* → See Interface Documentation
3. *Understanding a* Component?* → See Component Documentation
4. *Looking for a* Term?* → Check Glossary
5. *Need Quick* Reference?* → Use Index

== Documentation Structure

=== Architecture Overview

High-level Medley system architecture, component relationships, and how Medley interacts with Maiko. Includes Mermaid diagrams showing system structure, data flow, and component interactions.

*Key Topics*:

- System Architecture - Overall Medley system design
- Core Components - Major Medley subsystems
- Medley-Maiko Integration - How Medley and Maiko work together - Data Flow - Information flow through the system

=== Component Documentation

Detailed documentation organized by Medley component:- *Scripts - Medley script system and argument parsing
  - Script types and platform variations
- Argument parsing and transformation* - Maiko invocation logic- *Sysout Files - Sysout file format and loading
  - Sysout file structure and types
- Loading process and initialization* - Relationship to Lisp system state- *Virtual Memory Files - Virtual memory file format and usage
  - Vmem file format and structure
- Session persistence and continuation* - Coordination with Maiko- *Configuration Files - Configuration file format and precedence
  - Config file format and parsing
- Precedence rules and defaults* - Platform-specific locations- *Greet Files - Greet file system and execution
  - Greet file format and purpose
- Execution order and integration* - Startup initialization- *Loadup Workflow - Sysout creation and build process
  - Loadup workflow and scripts
- Sysout file creation* - Build system integration- *Directory Structure - Medley directory organization
  - Directory layout and purpose
- File types and organization* - Source code structure

=== Interface Documentation

Comprehensive documentation of the Medley-Maiko interface:- *Interface Overview* - Interface documentation navigation and overview- *Command-Line Interface* - Command-line arguments passed to Maiko- *Environment Variables* - Environment variable communication- *File Formats* - File format specifications (sysout, vmem, config, greet)- *Protocols* - Runtime communication protocols and patterns

=== Platform Documentation

Platform-specific behaviors and variations:- *Linux* - Linux-specific behaviors and script differences- *macOS* - macOS-specific behaviors and medley.command script- *Windows - Windows/Cygwin-specific behaviors and medley.ps1 script
- *WSL - WSL-specific behaviors, VNC usage, and automation

=== Glossary

Terminology, concepts, and abbreviations used throughout Medley documentation.

*Categories*:

- Core Concepts - MEDLEYDIR, LOGINDIR, sysout, vmem, etc.
- File Types - Sysout files, vmem files, greet files, config files
- Script Terms - Medley scripts, argument parsing, Maiko invocation - Interface Terms - Command-line arguments, environment variables, protocols

== Quick Navigation

=== By Component Type- *Scripts* - Medley script system- *Sysout Files* - Sysout file format and loading- *Virtual Memory* - Vmem files and session persistence- *Configuration* - Configuration files and precedence- *Greet Files* - Greet file system- *Loadup* - Loadup workflow

=== By Interface Type- *Command-Line* - Command-line argument mapping- *Environment Variables* - Environment variable usage- *File Formats* - File format specifications- *Protocols* - Runtime communication

=== By Platform- *Linux* - Linux-specific documentation- *macOS* - macOS-specific documentation- *Windows* - Windows/Cygwin-specific documentation- *WSL - WSL-specific documentation

== Project Context

Medley is part of the Medley Interlisp system, which provides:

- A complete Lisp development environment (Interlisp and Common Lisp)
- Bytecode-based virtual machine execution via Maiko
- Cross-platform support (macOS, Linux, FreeBSD, Solaris, Windows)
- Multiple architecture support (x86_64, ARM64, SPARC, etc.)
- Applications including Notecards, Rooms, and CLOS

See Architecture Overview for more details on the system design.

== Documentation Conventions
- *File References*: `medley/path/to/file` refers to files in the Medley source tree
- *Script References*: `medley/scripts/medley/medley_run.sh` refers to Medley scripts
- *Maiko References*: Links to Maiko documentation use paths like `../README.md` (Maiko docs are in parent directory)
- *Links*: All documentation files are cross-linked for easy navigation
- *Diagrams*: Mermaid diagrams illustrate system architecture and data flow

== Related Resources
- *Medley Source Code*: `medley/` directory in repository root
- *Maiko Documentation*: `../README.md` - Maiko emulator documentation
- *Maiko Source Code*: `maiko/src/` directory - Emulator implementation
- *Project README*: Repository root `README.md` - Project overview

== Integration with Maiko

Medley runs on the Maiko emulator. For complete understanding: - *Medley Documentation* (this directory): Medley system, scripts, files, and organization - *Maiko Documentation* (`../`): Emulator implementation, VM core, memory management, I/O

The Interface Documentation provides the complete specification of how Medley and Maiko communicate.

== Last Updated

Documentation created: 2025-01-27

