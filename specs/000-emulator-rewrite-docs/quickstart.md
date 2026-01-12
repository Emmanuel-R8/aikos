# Quickstart: Using the Rewrite Documentation

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
├── instruction-set/             # Bytecode specifications
├── vm-core/                     # Execution engine specifications
├── memory/                      # Memory management specifications
├── data-structures/             # Data structure formats
├── display/                     # Display interface specifications
├── io/                          # I/O interface specifications
├── platform-abstraction/        # Platform requirements
└── validation/                  # Test cases and validation
```

## Implementation Path

### Phase 1: VM Core (Foundation)

**Goal**: Implement basic bytecode execution

1. **Read**: `instruction-set/opcodes.md` - Understand instruction format
2. **Read**: `vm-core/execution-model.md` - Understand dispatch loop
3. **Read**: `vm-core/stack-management.md` - Understand stack frames
4. **Implement**: Basic dispatch loop with 10-20 opcodes
5. **Validate**: Use `validation/reference-behaviors.md` test cases

**Deliverable**: Working bytecode interpreter executing simple arithmetic

### Phase 2: Memory Management

**Goal**: Implement GC and memory allocation

1. **Read**: `memory/virtual-memory.md` - Understand address spaces
2. **Read**: `memory/garbage-collection.md` - Understand GC algorithm
3. **Read**: `data-structures/cons-cells.md` - Understand data formats
4. **Implement**: Basic memory allocation and GC
5. **Validate**: Load simple sysout file

**Deliverable**: Memory system compatible with sysout files

### Phase 3: Complete Instruction Set

**Goal**: Implement all 256 opcodes

1. **Read**: `instruction-set/opcodes.md` - Complete opcode reference
2. **Implement**: Remaining opcodes incrementally
3. **Validate**: Execute complex Lisp programs

**Deliverable**: Complete instruction set implementation

### Phase 4: I/O and Display

**Goal**: Implement I/O and display subsystems

1. **Read**: `io/keyboard-protocol.md` - Understand keycode translation
2. **Read**: `display/interface-abstraction.md` - Understand display contract
3. **Read**: `contracts/display-interface.md` - Display interface specification
4. **Read**: `contracts/io-interface.md` - I/O interface specification
5. **Implement**: Platform-specific I/O and display backends
6. **Validate**: Interactive Lisp session

**Deliverable**: Fully functional emulator with graphics and I/O

## Reading the Documentation

### Understanding Specifications

- **Algorithms**: Pseudocode describes step-by-step procedures
- **Data Structures**: Diagrams and field layouts show exact formats
- **Protocols**: Message formats specify byte-level structures
- **Interfaces**: Contracts specify required operations

### Key Concepts

1. **Language-Agnostic**: Specifications use pseudocode, not C code
2. **Compatibility**: Some behaviors must match exactly (see platform-abstraction/)
3. **Incremental**: Implement subsystems in dependency order
4. **Validation**: Use test cases to verify correctness

### Common Patterns

- **Address Translation**: Always use FPtoVP mapping, never direct pointers
- **Stack Frames**: Follow exact layout for compatibility
- **GC References**: Use hash table for reference tracking
- **Event Handling**: Translate OS events to Lisp events

## Validation

### Reference Test Cases

See `validation/reference-behaviors.md` for:

- Opcode execution tests
- Memory allocation tests
- GC behavior tests
- Compatibility tests

### Compatibility Criteria

See `validation/compatibility-criteria.md` for:

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

- **Solution**: Check execution semantics in `instruction-set/opcodes.md`
- **Check**: Operand decoding, stack effects, side effects

**Problem**: Sysout file won't load

- **Solution**: Verify memory layout matches specification
- **Check**: Data structure formats in `data-structures/`

**Problem**: GC causes crashes

- **Solution**: Verify reference counting algorithm
- **Check**: Hash table structure and reference operations

**Problem**: Display/IO doesn't work

- **Solution**: Verify interface contract implementation
- **Check**: Event translation and protocol compliance

### Getting Help

1. **Check Documentation**: Most issues are covered in specifications
2. **Review Test Cases**: Reference behaviors show expected results
3. **Compare with Maiko**: Use Maiko as reference implementation (if available)
4. **Platform Notes**: Check platform-abstraction/ for platform-specific details

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
- ✅ Passes validation test cases
- ✅ Maintains compatibility with Medley Interlisp

Good luck with your rewrite!
