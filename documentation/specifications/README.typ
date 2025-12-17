= Maiko Emulator Rewrite Specification

This directory contains comprehensive, language-agnostic specifications sufficient for rewriting the Maiko emulator in any programming language. These specifications describe complete algorithms, data structures, protocols, and interfaces without relying on C implementation details.

== Purpose

This documentation enables developers to:

- Implement a complete Maiko-compatible emulator in any language
- Understand execution semantics, memory management, and I/O protocols
- Maintain compatibility with existing Medley Interlisp sysout files
- Implement platform-specific backends while preserving VM compatibility

== Documentation Structure

=== Instruction Set

Complete bytecode instruction specifications:

- Opcodes - All 256 opcodes with execution semantics
- Instruction Format - Bytecode encoding
- Execution Semantics - Instruction execution behavior

=== VM Core

Execution engine specifications:

- Execution Model - Dispatch loop and instruction execution
- Stack Management - Stack frames and operations
- Function Calls - Call/return mechanisms
- Interrupt Handling - Interrupt processing

=== Memory Management

Memory and garbage collection specifications:

- Virtual Memory - Address spaces and page mapping
- Garbage Collection - Reference counting GC algorithm
- Memory Layout - Memory regions and organization
- Address Translation - LispPTR to native address conversion

=== Data Structures

VM data structure formats:

- Cons Cells - Cons cell format and CDR coding
- Arrays - Array formats
- Function Headers - Function metadata
- Sysout Format - Sysout file structure

=== Display

Display interface specifications:

- Interface Abstraction - Display interface contract
- Graphics Operations - BitBLT and rendering
- Event Protocols - Keyboard/mouse event handling

=== I/O

Input/output specifications:

- Keyboard Protocol - Key event translation
- Mouse Protocol - Mouse event handling
- File System - File I/O and pathname handling
- Network Protocol - Network communication

=== Platform Abstraction

Platform requirements:

- Required Behaviors - Must-match behaviors
- Implementation Choices - May-differ choices

=== Validation

Test cases and compatibility:

- Reference Behaviors - Reference test cases
- Compatibility Criteria - Compatibility requirements

== Quick Start

1. *New to rewriting emulators?* → Start with Quickstart Guide
2. *Ready to implement?* → Follow the Implementation Path
3. *Need specific details?* → Browse by subsystem using the structure above
4. *Contributing?* → Read Contributing Guidelines

== Key Principles

1. *Language-Agnostic*: All specifications use pseudocode, diagrams, and formal descriptions
2. *Completeness*: 100% opcode coverage, all subsystems specified
3. *Compatibility*: Specifications ensure sysout file compatibility
4. *Incremental*: Documentation organized for incremental implementation
5. *Validation*: Reference test cases enable correctness verification

== Success Criteria

A successful rewrite implementation:

- ✅ Executes bytecode correctly (matches Maiko behavior)
- ✅ Loads and runs existing sysout files
- ✅ Handles I/O and display correctly
- ✅ Passes validation test cases - ✅ Maintains compatibility with Medley Interlisp
