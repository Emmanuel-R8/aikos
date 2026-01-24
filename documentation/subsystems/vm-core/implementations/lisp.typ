= Common Lisp VM Core Implementation

*Navigation*: Spec | C Implementation | Zig Implementation

This document describes the Common Lisp implementation of the VM Core subsystem, consolidated from `implementations/lisp-implementation.typ`.

== Overview

The Common Lisp VM Core implementation provides a complete bytecode interpreter using Lisp's dynamic features.

== Key Components

=== Dispatch Loop

- Located in `src/vm/dispatch.lisp` (462 lines)
- Implements variable-length instruction handling
- Manages operand fetching and execution

=== Stack Management

- Stack operations in `src/vm/stack.lisp`
- Frame-based stack with proper allocation/deallocation
- Lisp list-based stack implementation

=== Opcode Implementation

- Opcodes in `src/vm/opcodes.lisp`
- Lisp macro-based opcode definitions
- Dynamic dispatch for flexibility

== Key Implementation Decisions

- *Dispatch Loop*: Variable-length instruction handling with operand fetching
- *Stack Management*: Frame-based stack with proper allocation/deallocation
- *Type System*: Leverages Common Lisp's type system for Maiko types

== Status

- ⏳ Implementation in progress
- Basic structure established
- Targeting SBCL as primary implementation

See `implementations/lisp-implementation.typ` for detailed implementation notes.