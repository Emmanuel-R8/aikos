= Rewrite Specification Index


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

- *Instruction Format*: instruction-set/instruction-format.md
- *Opcode Semantics*: instruction-set/opcodes.md
- *Execution Model*: vm-core/execution-model.md
- *Dispatch Loop*: vm-core/execution-model.md#dispatch-loop

==== Memory Management
- *GC Algorithm*: memory/garbage-collection.md
- *Address Translation*: memory/address-translation.md
- *Memory Layout*: memory/memory-layout.md
- *Virtual Memory*: memory/virtual-memory.md

==== Data Structures
- *Cons Cells*: data-structures/cons-cells.md
- *Arrays*: data-structures/arrays.md
- *Function Headers*: data-structures/function-headers.md
- *Sysout Format*: data-structures/sysout-format.md

==== Display and Graphics
- *Display Interface*: display/interface-abstraction.md
- *Graphics Operations*: display/graphics-operations.md
- *Event Protocols*: display/event-protocols.md

==== I/O Systems
- *Keyboard*: io/keyboard-protocol.md
- *Mouse*: io/mouse-protocol.md
- *File System*: io/file-system.md
- *Network*: io/network-protocol.md

==== Platform Requirements
- *Required Behaviors*: platform-abstraction/required-behaviors.md - *Implementation Choices*: platform-abstraction/implementation-choices.md

==== Validation
- *Reference Behaviors*: validation/reference-behaviors.md - *Compatibility Criteria*: validation/compatibility-criteria.md

== File-to-Specification Mapping

=== Source Files → Specifications

==== VM Core (`src/xc.c`, `src/main.c`, etc.)

- Dispatch loop → vm-core/execution-model.md
- Stack operations → vm-core/stack-management.md
- Function calls → vm-core/function-calls.md
- Interrupts → vm-core/interrupt-handling.md

==== Instruction Handlers (`src/arithops.c`, `src/car-cdr.c`, etc.)

- Opcode implementations → instruction-set/opcodes.md
- Execution semantics → instruction-set/execution-semantics.md

==== Memory Management (`src/gc pointer.c`, `src/storage.c`)

- GC algorithm → memory/garbage-collection.md
- Memory allocation → memory/memory-layout.md
- Address translation → memory/address-translation.md

==== Display (`src/xinit.c`, `src/sdl.c`, `src/xbbt.c`)

- Display interface → display/interface-abstraction.md
- Graphics operations → display/graphics-operations.md
- Events → display/event-protocols.md

==== I/O* (`src/kbdif.c`, `src/mouseif.c`, `src/dir.c`)

- Keyboard → io/keyboard-protocol.md
- Mouse → io/mouse-protocol.md
- File system → io/file-system.md

== Common Tasks

=== Implementing a New Opcode

1. Check instruction-set/opcodes.md for opcode specification
2. Review instruction-set/execution-semantics.md for execution rules
3. Verify stack effects and side effects
4. Test against validation/reference-behaviors.md

=== Implementing GC

1. Read memory/garbage-collection.md for algorithm
2. Understand memory/memory-layout.md for data structures
3. Implement reference counting per specification
4. Validate with validation/compatibility-criteria.md

=== Implementing Display Backend

1. Review display/interface-abstraction.md for contract
2. Implement required operations
3. Follow display/graphics-operations.md for rendering
4. Handle events per display/event-protocols.md

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
- Component Documentation* - Component details
