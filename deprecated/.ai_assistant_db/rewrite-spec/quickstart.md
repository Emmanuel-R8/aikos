---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Quickstart: Using the Rewrite Documentation

**Navigation**: [README](README.md) | [Index](INDEX.md)

**Date**: 2025-12-04
**Feature**: Complete Emulator Rewrite Documentation

## Overview

This guide helps developers use the rewrite documentation to implement a Maiko-compatible emulator in any programming language.

## Getting Started

### Prerequisites

- Understanding of virtual machines and bytecode interpreters
- Familiarity with garbage collection concepts
- Knowledge of systems programming (memory management, I/O)
- Target language and platform chosen

### Documentation Structure

The rewrite documentation is organized in `documentation/rewrite-spec/`:

```
rewrite-spec/
├── README.md                    # Start here - overview and navigation
├── INDEX.md                     # Quick reference index
├── CONTRIBUTING.md              # Documentation standards
├── COMPLETENESS.md              # Completeness checklist
├── quickstart.md                # This file
├── instruction-set/             # Bytecode specifications
│   ├── README.md
│   ├── opcodes.md              # All 256 opcodes
│   ├── instruction-format.md   # Encoding
│   └── execution-semantics.md   # Execution rules
├── vm-core/                     # Execution engine specifications
│   ├── README.md
│   ├── execution-model.md       # Dispatch loop
│   ├── stack-management.md     # Stack frames
│   ├── function-calls.md       # Call/return
│   └── interrupt-handling.md   # Interrupts
├── memory/                      # Memory management specifications
│   ├── README.md
│   ├── virtual-memory.md       # Address spaces
│   ├── address-translation.md  # LispPTR translation
│   ├── garbage-collection.md   # GC algorithm
│   └── memory-layout.md        # Memory regions
├── data-structures/             # Data structure formats
│   ├── README.md
│   ├── cons-cells.md           # Cons cells and CDR coding
│   ├── arrays.md               # Array formats
│   ├── function-headers.md      # Function metadata
│   └── sysout-format.md        # Sysout file format
├── display/                     # Display interface specifications
│   ├── README.md
│   ├── interface-abstraction.md # Display contract
│   ├── graphics-operations.md  # BitBLT and rendering
│   └── event-protocols.md      # Event handling
├── io/                          # I/O interface specifications
│   ├── README.md
│   ├── keyboard-protocol.md    # Keycode translation
│   ├── mouse-protocol.md       # Mouse events
│   ├── file-system.md          # File I/O
│   └── network-protocol.md     # Network communication
├── platform-abstraction/        # Platform requirements
│   ├── README.md
│   ├── required-behaviors.md   # Must-match behaviors
│   └── implementation-choices.md # May-differ choices
└── validation/                   # Test cases and validation
    ├── README.md
    ├── reference-behaviors.md   # Test cases
    └── compatibility-criteria.md # Compatibility requirements
```

## Implementation Path

### Phase 1: VM Core (Foundation)

**Goal**: Implement basic bytecode execution

1. **Read**: [Instruction Set Overview](instruction-set/README.md) - Understand instruction organization
2. **Read**: [Instruction Format](instruction-set/instruction-format.md) - Understand bytecode encoding
3. **Read**: [Execution Model](vm-core/execution-model.md) - Understand dispatch loop
4. **Read**: [Stack Management](vm-core/stack-management.md) - Understand stack frames
5. **Read**: [Opcodes Reference](instruction-set/opcodes.md) - Start with 10-20 simple opcodes
6. **Implement**: Basic dispatch loop with selected opcodes
7. **Validate**: Use [Reference Behaviors](validation/reference-behaviors.md) test cases

**Deliverable**: Working bytecode interpreter executing simple arithmetic

**Key Documents**:

- [Instruction Set](instruction-set/) - All instruction specifications
- [VM Core](vm-core/) - Execution engine specifications
- [Execution Semantics](instruction-set/execution-semantics.md) - Execution rules

### Phase 2: Memory Management

**Goal**: Implement GC and memory allocation

1. **Read**: [Memory Management Overview](memory/README.md) - Understand memory system
2. **Read**: [Virtual Memory](memory/virtual-memory.md) - Understand address spaces
3. **Read**: [Address Translation](memory/address-translation.md) - Understand LispPTR translation
4. **Read**: [Garbage Collection](memory/garbage-collection.md) - Understand GC algorithm
5. **Read**: [Memory Layout](memory/memory-layout.md) - Understand memory organization
6. **Read**: [Cons Cells](data-structures/cons-cells.md) - Understand data formats
7. **Implement**: Basic memory allocation and GC
8. **Validate**: Load simple sysout file

**Deliverable**: Memory system compatible with sysout files

**Key Documents**:

- [Memory Management](memory/) - Complete memory specifications
- [Data Structures](data-structures/) - Data format specifications
- [Sysout Format](data-structures/sysout-format.md) - File format

### Phase 3: Complete Instruction Set

**Goal**: Implement all 256 opcodes

1. **Read**: [Complete Opcodes Reference](instruction-set/opcodes.md) - All 256 opcodes
2. **Read**: [Execution Semantics](instruction-set/execution-semantics.md) - Execution rules
3. **Implement**: Remaining opcodes incrementally
4. **Validate**: Execute complex Lisp programs
5. **Test**: Use [Reference Behaviors](validation/reference-behaviors.md)

**Deliverable**: Complete instruction set implementation

**Key Documents**:

- [Opcodes Reference](instruction-set/opcodes.md) - Complete opcode list
- [Function Calls](vm-core/function-calls.md) - Function invocation
- [Interrupt Handling](vm-core/interrupt-handling.md) - Interrupt processing

### Phase 4: I/O and Display

**Goal**: Implement I/O and display subsystems

1. **Read**: [I/O Overview](io/README.md) - Understand I/O system
2. **Read**: [Keyboard Protocol](io/keyboard-protocol.md) - Understand keycode translation
3. **Read**: [Mouse Protocol](io/mouse-protocol.md) - Understand mouse events
4. **Read**: [File System](io/file-system.md) - Understand file I/O
5. **Read**: [Display Interface](display/interface-abstraction.md) - Understand display contract
6. **Read**: [Graphics Operations](display/graphics-operations.md) - Understand BitBLT
7. **Read**: [Platform Abstraction](platform-abstraction/) - Understand requirements
8. **Implement**: Platform-specific I/O and display backends
9. **Validate**: Interactive Lisp session

**Deliverable**: Fully functional emulator with graphics and I/O

**Key Documents**:

- [Display Subsystem](display/) - Display specifications
- [I/O Subsystem](io/) - I/O specifications
- [Platform Abstraction](platform-abstraction/) - Platform requirements
- [Required Behaviors](platform-abstraction/required-behaviors.md) - Must-match behaviors
- [Implementation Choices](platform-abstraction/implementation-choices.md) - May-differ choices

## Reading the Documentation

### Understanding Specifications

- **Algorithms**: Pseudocode describes step-by-step procedures
- **Data Structures**: Diagrams and field layouts show exact formats
- **Protocols**: Message formats specify byte-level structures
- **Interfaces**: Contracts specify required operations

### Key Concepts

1. **Language-Agnostic**: Specifications use pseudocode, not C code
2. **Compatibility**: Some behaviors must match exactly (see [Required Behaviors](platform-abstraction/required-behaviors.md))
3. **Incremental**: Implement subsystems in dependency order
4. **Validation**: Use test cases to verify correctness

### Common Patterns

- **Address Translation**: Always use FPtoVP mapping, never direct pointers (see [Address Translation](memory/address-translation.md))
- **Stack Frames**: Follow exact layout for compatibility (see [Stack Management](vm-core/stack-management.md))
- **GC References**: Use hash table for reference tracking (see [Garbage Collection](memory/garbage-collection.md))
- **Event Handling**: Translate OS events to Lisp events (see [Event Protocols](display/event-protocols.md))

## Validation

### Reference Test Cases

See [Reference Behaviors](validation/reference-behaviors.md) for:

- Opcode execution tests
- Memory allocation tests
- GC behavior tests
- Compatibility tests

### Compatibility Criteria

See [Compatibility Criteria](validation/compatibility-criteria.md) for:

- What must match exactly
- What may differ
- How to verify compatibility

### Testing Your Implementation

1. **Unit Tests**: Test individual opcodes and operations
2. **Integration Tests**: Test subsystem interactions
3. **Compatibility Tests**: Load and execute existing sysout files
4. **Performance Tests**: Verify acceptable performance

## Troubleshooting

### Common Issues

**Problem**: Opcode produces different result than Maiko

- **Solution**: Check execution semantics in [Opcodes Reference](instruction-set/opcodes.md)
- **Check**: Operand decoding, stack effects, side effects
- **See**: [Execution Semantics](instruction-set/execution-semantics.md)

**Problem**: Sysout file won't load

- **Solution**: Verify memory layout matches specification
- **Check**: [Memory Layout](memory/memory-layout.md) and [Sysout Format](data-structures/sysout-format.md)
- **See**: [Data Structures](data-structures/) for format details

**Problem**: GC causes crashes

- **Solution**: Verify reference counting algorithm
- **Check**: [Garbage Collection](memory/garbage-collection.md) hash table structure
- **See**: [Reference Behaviors](validation/reference-behaviors.md) for test cases

**Problem**: Display/IO doesn't work

- **Solution**: Verify interface contract implementation
- **Check**: [Display Interface](display/interface-abstraction.md) and [I/O Protocols](io/)
- **See**: [Required Behaviors](platform-abstraction/required-behaviors.md) for must-match behaviors

### Getting Help

1. **Check Documentation**: Most issues are covered in specifications
2. **Review Test Cases**: [Reference Behaviors](validation/reference-behaviors.md) show expected results
3. **Compare with Maiko**: Use Maiko as reference implementation (if available)
4. **Platform Notes**: Check [Platform Abstraction](platform-abstraction/) for platform-specific details

## Documentation Navigation

### Quick Reference

- **[Index](INDEX.md)**: Quick reference guide
- **[README](README.md)**: Overview and navigation
- **[Completeness Checklist](COMPLETENESS.md)**: Documentation status

### By Topic

- **Bytecode**: [Instruction Set](instruction-set/)
- **Execution**: [VM Core](vm-core/)
- **Memory**: [Memory Management](memory/)
- **Data**: [Data Structures](data-structures/)
- **Graphics**: [Display](display/)
- **Input/Output**: [I/O](io/)
- **Platform**: [Platform Abstraction](platform-abstraction/)
- **Testing**: [Validation](validation/)

## Next Steps

After completing basic implementation:

1. **Optimize**: Improve performance while maintaining compatibility
2. **Extend**: Add platform-specific features (if desired)
3. **Test**: Comprehensive testing with real Lisp programs
4. **Document**: Document any platform-specific choices made

## Success Criteria

Your implementation is successful when:

- ✅ Executes bytecode correctly (matches Maiko behavior)
- ✅ Loads and runs existing sysout files
- ✅ Handles I/O and display correctly
- ✅ Passes validation test cases (see [Reference Behaviors](validation/reference-behaviors.md))
- ✅ Maintains compatibility with Medley Interlisp (see [Compatibility Criteria](validation/compatibility-criteria.md))

Good luck with your rewrite!
