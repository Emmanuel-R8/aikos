#import "../prelude.typ": codeblock

= Quickstart: Using the Rewrite Documentation

*Navigation*: README | Index

*Date*: 2025-12-04
*Feature*: Complete Emulator Rewrite Documentation

== Overview

This guide helps developers use the rewrite documentation to implement a Maiko-compatible emulator in any programming language.

== Getting Started

=== Prerequisites

- Understanding of virtual machines and bytecode interpreters
- Familiarity with garbage collection concepts
- Knowledge of systems programming (memory management, I/O)
- Target language and platform chosen

=== Documentation Structure

The rewrite documentation is organized in `documentation/specifications/`:

#codeblock(lang: "text", [#raw("specifications/\n├── README.typ                   # Start here - overview and navigation\n├── INDEX.typ                    # Quick reference index\n├── CONTRIBUTING.typ             # Documentation standards\n├── COMPLETENESS.typ             # Completeness checklist\n├── quickstart.typ               # This file\n├── instruction-set/             # Bytecode specifications\n│   ├── README.typ\n│   ├── opcodes.typ              # All 256 opcodes\n│   ├── instruction-format.typ   # Encoding\n│   └── execution-semantics.typ  # Execution rules\n├── vm-core/                     # Execution engine specifications\n│   ├── README.typ\n│   ├── execution-model.typ      # Dispatch loop\n│   ├── stack-management.typ     # Stack frames\n│   ├── function-calls.typ       # Call/return\n│   └── interrupt-handling.typ   # Interrupts\n├── memory/                      # Memory management specifications\n│   ├── README.typ\n│   ├── virtual-memory.typ       # Address spaces\n│   ├── address-translation.typ  # LispPTR translation\n│   ├── garbage-collection.typ   # GC algorithm\n│   └── memory-layout.typ        # Memory regions\n├── data-structures/             # Data structure formats\n│   ├── README.typ\n│   ├── cons-cells.typ           # Cons cells and CDR coding\n│   ├── arrays.typ               # Array formats\n│   ├── function-headers.typ     # Function metadata\n│   └── sysout-format-overview.typ # Sysout file format\n├── display/                     # Display interface specifications\n│   ├── README.typ\n│   ├── interface-abstraction.typ # Display contract\n│   ├── graphics-operations.typ  # BitBLT and rendering\n│   └── event-protocols.typ      # Event handling\n├── io/                          # I/O interface specifications\n│   ├── README.typ\n│   ├── keyboard-protocol.typ    # Keycode translation\n│   ├── mouse-protocol.typ       # Mouse events\n│   ├── file-system.typ          # File I/O\n│   └── network-protocol.typ     # Network communication\n├── platform-abstraction/        # Platform requirements\n│   ├── README.typ\n│   ├── required-behaviors.typ   # Must-match behaviors\n│   └── implementation-choices.typ # May-differ choices\n└── validation/                   # Test cases and validation\n    ├── README.typ\n    ├── reference-behaviors.typ   # Test cases\n    └── compatibility-criteria.typ # Compatibility requirements")])

== Implementation Path

=== Phase 1: VM Core (Foundation)

*Goal*: Implement basic bytecode execution

1. *Read*: Instruction Set Overview - Understand instruction organization
2. *Read*: Instruction Format - Understand bytecode encoding
3. *Read*: Execution Model - Understand dispatch loop
4. *Read*: Stack Management - Understand stack frames
5. *Read*: Opcodes Reference - Start with 10-20 simple opcodes
6. *Implement*: Basic dispatch loop with selected opcodes
7. *Validate*: Use Reference Behaviors test cases

*Deliverable*: Working bytecode interpreter executing simple arithmetic

*Key Documents*:

- Instruction Set - All instruction specifications
- VM Core - Execution engine specifications
- Execution Semantics - Execution rules

=== Phase 2: Memory Management

*Goal*: Implement GC and memory allocation

1. *Read*: Memory Management Overview - Understand memory system
2. *Read*: Virtual Memory - Understand address spaces
3. *Read*: Address Translation - Understand LispPTR translation
4. *Read*: Garbage Collection - Understand GC algorithm
5. *Read*: Memory Layout - Understand memory organization
6. *Read*: Cons Cells - Understand data formats
7. *Implement*: Basic memory allocation and GC
8. *Validate*: Load simple sysout file

*Deliverable*: Memory system compatible with sysout files

*Key Documents*:

- Memory Management - Complete memory specifications
- Data Structures - Data format specifications
- Sysout Format - File format

=== Phase 3: Complete Instruction Set

*Goal*: Implement all 256 opcodes

1. *Read*: Complete Opcodes Reference - All 256 opcodes
2. *Read*: Execution Semantics - Execution rules
3. *Implement*: Remaining opcodes incrementally
4. *Validate*: Execute complex Lisp programs
5. *Test*: Use Reference Behaviors

*Deliverable*: Complete instruction set implementation

*Key Documents*:

- Opcodes Reference - Complete opcode list
- Function Calls - Function invocation
- Interrupt Handling - Interrupt processing

=== Phase 4: I/O and Display

*Goal*: Implement I/O and display subsystems

1. *Read*: I/O Overview - Understand I/O system
2. *Read*: Keyboard Protocol - Understand keycode translation
3. *Read*: Mouse Protocol - Understand mouse events
4. *Read*: File System - Understand file I/O
5. *Read*: Display Interface - Understand display contract
6. *Read*: Graphics Operations - Understand BitBLT
7. *Read*: Platform Abstraction - Understand requirements
8. *Implement*: Platform-specific I/O and display backends
9. *Validate*: Interactive Lisp session

*Deliverable*: Fully functional emulator with graphics and I/O

*Key Documents*:

- Display Subsystem - Display specifications
- I/O Subsystem - I/O specifications
- Platform Abstraction - Platform requirements
- Required Behaviors - Must-match behaviors
- Implementation Choices - May-differ choices

== Reading the Documentation

=== Understanding Specifications

- *Algorithms*: Pseudocode describes step-by-step procedures
- *Data Structures*: Diagrams and field layouts show exact formats
- *Protocols*: Message formats specify byte-level structures
- *Interfaces*: Contracts specify required operations

=== Key Concepts

1. *Language-Agnostic*: Specifications use pseudocode, not C code
2. *Compatibility*: Some behaviors must match exactly (see Required Behaviors)
3. *Incremental*: Implement subsystems in dependency order
4. *Validation*: Use test cases to verify correctness

=== Common Patterns

- *Address Translation*: Always use FPtoVP mapping, never direct pointers (see Address Translation)
- *Stack Frames*: Follow exact layout for compatibility (see Stack Management)
- *GC References*: Use hash table for reference tracking (see Garbage Collection)
- *Event Handling*: Translate OS events to Lisp events (see Event Protocols)

== Validation

=== Reference Test Cases

See Reference Behaviors for:

- Opcode execution tests
- Memory allocation tests
- GC behavior tests
- Compatibility tests

=== Compatibility Criteria

See Compatibility Criteria for:

- What must match exactly
- What may differ
- How to verify compatibility

=== Testing Your Implementation

1. *Unit Tests*: Test individual opcodes and operations
2. *Integration Tests*: Test subsystem interactions
3. *Compatibility Tests*: Load and execute existing sysout files
4. *Performance Tests*: Verify acceptable performance

== Troubleshooting

=== Common Issues

*Problem*: Opcode produces different result than Maiko

- *Solution*: Check execution semantics in Opcodes Reference
- *Check*: Operand decoding, stack effects, side effects
- *See*: Execution Semantics

*Problem*: Sysout file won't load

- *Solution*: Verify memory layout matches specification
- *Check*: Memory Layout and Sysout Format
- *See*: Data Structures for format details

*Problem*: GC causes crashes

- *Solution*: Verify reference counting algorithm
- *Check*: Garbage Collection hash table structure
- *See*: Reference Behaviors for test cases

*Problem*: Display/IO doesn't work

- *Solution*: Verify interface contract implementation
- *Check*: Display Interface and I/O Protocols
- *See*: Required Behaviors for must-match behaviors

=== Getting Help

1. *Check Documentation*: Most issues are covered in specifications
2. *Review Test Cases*: Reference Behaviors show expected results
3. *Compare with Maiko*: Use Maiko as reference implementation (if available)
4. *Platform Notes*: Check Platform Abstraction for platform-specific details

== Documentation Navigation

=== Quick Reference

- *Index*: Quick reference guide
- *README*: Overview and navigation
- *Completeness Checklist*: Documentation status

=== By Topic

- *Bytecode*: Instruction Set
- *Execution*: VM Core
- *Memory*: Memory Management
- *Data*: Data Structures
- *Graphics*: Display
- *Input/Output*: I/O
- *Platform*: Platform Abstraction
- *Testing*: Validation

== Next Steps

After completing basic implementation:

1. *Optimize*: Improve performance while maintaining compatibility
2. *Extend*: Add platform-specific features (if desired)
3. *Test*: Comprehensive testing with real Lisp programs
4. *Document*: Document any platform-specific choices made

== Success Criteria

Your implementation is successful when:

- ✅ Executes bytecode correctly (matches Maiko behavior)
- ✅ Loads and runs existing sysout files
- ✅ Handles I/O and display correctly
- ✅ Passes validation test cases (see Reference Behaviors)
- ✅ Maintains compatibility with Medley Interlisp (see Compatibility Criteria)

Good luck with your rewrite!
