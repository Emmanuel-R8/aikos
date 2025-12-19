---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Maiko Emulator Rewrite Specification

**Navigation**: [Index](INDEX.md) | [Quickstart](quickstart.md) | [Contributing](CONTRIBUTING.md)

This directory contains comprehensive, language-agnostic specifications sufficient for rewriting the Maiko emulator in any programming language. These specifications describe complete algorithms, data structures, protocols, and interfaces without relying on C implementation details.

## Purpose

This documentation enables developers to:

- Implement a complete Maiko-compatible emulator in any language
- Understand execution semantics, memory management, and I/O protocols
- Maintain compatibility with existing Medley Interlisp sysout files
- Implement platform-specific backends while preserving VM compatibility

## Documentation Structure

### [Instruction Set](instruction-set/)

Complete bytecode instruction specifications:

- [Opcodes](instruction-set/opcodes.md) - All 256 opcodes with execution semantics
- [Instruction Format](instruction-set/instruction-format.md) - Bytecode encoding
- [Execution Semantics](instruction-set/execution-semantics.md) - Instruction execution behavior

### [VM Core](vm-core/)

Execution engine specifications:

- [Execution Model](vm-core/execution-model.md) - Dispatch loop and instruction execution
- [Stack Management](vm-core/stack-management.md) - Stack frames and operations
- [Function Calls](vm-core/function-calls.md) - Call/return mechanisms
- [Interrupt Handling](vm-core/interrupt-handling.md) - Interrupt processing

### [Memory Management](memory/)

Memory and garbage collection specifications:

- [Virtual Memory](memory/virtual-memory.md) - Address spaces and page mapping
- [Garbage Collection](memory/garbage-collection.md) - Reference counting GC algorithm
- [Memory Layout](memory/memory-layout.md) - Memory regions and organization
- [Address Translation](memory/address-translation.md) - LispPTR to native address conversion

### [Data Structures](data-structures/)

VM data structure formats:

- [Cons Cells](data-structures/cons-cells.md) - Cons cell format and CDR coding
- [Arrays](data-structures/arrays.md) - Array formats
- [Function Headers](data-structures/function-headers.md) - Function metadata
- [Sysout Format](data-structures/sysout-format.md) - Sysout file structure

### [Display](display/)

Display interface specifications:

- [Interface Abstraction](display/interface-abstraction.md) - Display interface contract
- [Graphics Operations](display/graphics-operations.md) - BitBLT and rendering
- [Event Protocols](display/event-protocols.md) - Keyboard/mouse event handling

### [I/O](io/)

Input/output specifications:

- [Keyboard Protocol](io/keyboard-protocol.md) - Key event translation
- [Mouse Protocol](io/mouse-protocol.md) - Mouse event handling
- [File System](io/file-system.md) - File I/O and pathname handling
- [Network Protocol](io/network-protocol.md) - Network communication

### [Platform Abstraction](platform-abstraction/)

Platform requirements:

- [Required Behaviors](platform-abstraction/required-behaviors.md) - Must-match behaviors
- [Implementation Choices](platform-abstraction/implementation-choices.md) - May-differ choices

### [Validation](validation/)

Test cases and compatibility:

- [Reference Behaviors](validation/reference-behaviors.md) - Reference test cases
- [Compatibility Criteria](validation/compatibility-criteria.md) - Compatibility requirements

## Quick Start

1. **New to rewriting emulators?** → Start with [Quickstart Guide](quickstart.md)
2. **Ready to implement?** → Follow the [Implementation Path](quickstart.md#implementation-path)
3. **Need specific details?** → Browse by subsystem using the structure above
4. **Contributing?** → Read [Contributing Guidelines](CONTRIBUTING.md)

## Key Principles

1. **Language-Agnostic**: All specifications use pseudocode, diagrams, and formal descriptions
2. **Completeness**: 100% opcode coverage, all subsystems specified
3. **Compatibility**: Specifications ensure sysout file compatibility
4. **Incremental**: Documentation organized for incremental implementation
5. **Validation**: Reference test cases enable correctness verification

## Related Documentation

- [Existing Maiko Documentation](../README.md) - Source code documentation
- [Architecture Overview](../architecture.md) - System architecture
- [Component Documentation](../components/) - Detailed component docs

## Success Criteria

A successful rewrite implementation:

- ✅ Executes bytecode correctly (matches Maiko behavior)
- ✅ Loads and runs existing sysout files
- ✅ Handles I/O and display correctly
- ✅ Passes validation test cases
- ✅ Maintains compatibility with Medley Interlisp

## Last Updated

Documentation created: 2025-12-04
