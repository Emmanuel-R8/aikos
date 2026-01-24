= Rewrite Specification Index

*Navigation*: README | Quickstart | Contributing

Quick reference guide to all rewrite specification documents.

== Quick Reference

=== By Implementation Phase

==== Phase 1: VM Core (Foundation)

- Instruction Set Overview
- Opcodes Reference - All 256 opcodes
- Execution Model - Dispatch loop
- Stack Management - Stack frames

==== Phase 2: Memory Management

- Memory Overview
- Garbage Collection - GC algorithm
- Virtual Memory - Address spaces
- Data Structures - Cons cells, arrays, etc.

==== Phase 3: I/O and Display

- Display Interface
- I/O Protocols - Keyboard, mouse, file system, network
- Platform Abstraction - Required vs optional

=== By Topic

==== Bytecode Execution

- *Instruction Format*: instruction-set/instruction-format.typ
- *Opcode Semantics*: instruction-set/opcodes.typ
- *Execution Model*: vm-core/execution-model.typ
- *Dispatch Loop*: vm-core/execution-model.typ#dispatch-loop

==== Memory Management

- *GC Algorithm*: memory/garbage-collection.typ
- *Address Translation*: memory/address-translation.typ
- *Memory Layout*: memory/memory-layout.typ
- *Virtual Memory*: memory/virtual-memory.typ

==== Data Structures

- *Cons Cells*: data-structures/cons-cells.typ
- *Arrays*: data-structures/arrays.typ
- *Function Headers*: data-structures/function-headers.typ
- *Sysout Format*: data-structures/sysout-format.typ

==== Display and Graphics

- *Display Interface*: display/interface-abstraction.typ
- *Graphics Operations*: display/graphics-operations.typ
- *Event Protocols*: display/event-protocols.typ

==== I/O Systems

- *Keyboard*: io/keyboard-protocol.typ
- *Mouse*: io/mouse-protocol.typ
- *File System*: io/file-system.typ
- *Network*: io/network-protocol.typ

==== Platform Requirements

- *Required Behaviors*: platform-abstraction/required-behaviors.typ
- *Implementation Choices*: platform-abstraction/implementation-choices.typ

==== Validation

- *Reference Behaviors*: validation/reference-behaviors.typ
- *Compatibility Criteria*: validation/compatibility-criteria.typ

== File-to-Specification Mapping

=== Source Files → Specifications

==== VM Core (`src/xc.c`, `src/main.c`, etc.)

- Dispatch loop → vm-core/execution-model.typ
- Stack operations → vm-core/stack-management.typ
- Function calls → vm-core/function-calls.typ
- Interrupts → vm-core/interrupt-handling.typ

==== Instruction Handlers (`src/arithops.c`, `src/car-cdr.c`, etc.)

- Opcode implementations → instruction-set/opcodes.typ
- Execution semantics → instruction-set/execution-semantics.typ

==== Memory Management (`src/gc*.c`, `src/storage.c`)

- GC algorithm → memory/garbage-collection.typ
- Memory allocation → memory/memory-layout.typ
- Address translation → memory/address-translation.typ

==== Display (`src/xinit.c`, `src/sdl.c`, `src/xbbt.c`)

- Display interface → display/interface-abstraction.typ
- Graphics operations → display/graphics-operations.typ
- Events → display/event-protocols.typ

==== I/O (`src/kbdif.c`, `src/mouseif.c`, `src/dir.c`)

- Keyboard → io/keyboard-protocol.typ
- Mouse → io/mouse-protocol.typ
- File system → io/file-system.typ

== Common Tasks

=== Implementing a New Opcode

1. Check instruction-set/opcodes.typ for opcode specification
2. Review instruction-set/execution-semantics.typ for execution rules
3. Verify stack effects and side effects
4. Test against validation/reference-behaviors.typ

=== Implementing GC

1. Read memory/garbage-collection.typ for algorithm
2. Understand memory/memory-layout.typ for data structures
3. Implement reference counting per specification
4. Validate with validation/compatibility-criteria.typ

=== Implementing Display Backend

1. Review display/interface-abstraction.typ for contract
2. Implement required operations
3. Follow display/graphics-operations.typ for rendering
4. Handle events per display/event-protocols.typ

== Documentation Status

- ✅ Instruction Set: Complete
- ✅ VM Core: Complete
- ✅ Memory Management: Complete
- ✅ Data Structures: Complete
- ✅ Display: Complete
- ✅ I/O: Complete
- ✅ Platform Abstraction: Complete
- ✅ Validation: Complete

== Related Resources

- Maiko Source Documentation - Original source code documentation
- Architecture Overview - System architecture
- Component Documentation - Component details
