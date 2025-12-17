#set page(margin: (top: 2.5cm, bottom: 2.5cm, left: 2.5cm, right: 2.5cm))
#set text(size: 11pt)
#set heading(numbering: "1.")
#set par(justify: true, leading: 0.65em)

#align(center)[
  #[Maiko Virtual Machine Documentation]
  
  #[Complete Reference Guide]
  
  #[Interlisp Project]
]

#pagebreak()

#outline(
  title: none,
  depth: 3,
)

#pagebreak()

= Introduction
#include "core/introduction.typ"

#pagebreak()

= Architecture Overview
#include "core/architecture.typ"

#pagebreak()

= Build System
#include "core/build-system.typ"

#pagebreak()

= Components

== VM Core
#include "components/vm-core.typ"

#pagebreak()

== Memory Management
#include "components/memory-management.typ"

#pagebreak()

== Display
#include "components/display.typ"

#pagebreak()

== I/O Systems
#include "components/io.typ"

#pagebreak()

= Specifications

== Overview
#include "specifications/README.typ"

#pagebreak()

== Instruction Set

=== Overview
#include "specifications/instruction-set/README.typ"

=== Opcodes
#include "specifications/instruction-set/opcodes.typ"

=== Instruction Format
#include "specifications/instruction-set/instruction-format.typ"

=== Execution Semantics
#include "specifications/instruction-set/execution-semantics.typ"

=== Opcode Reference
#include "specifications/instruction-set/opcodes-reference.typ"

=== Arithmetic Opcodes
#include "specifications/instruction-set/opcodes-arithmetic.typ"

=== Data Opcodes
#include "specifications/instruction-set/opcodes-data.typ"

=== Control and Memory Opcodes
#include "specifications/instruction-set/opcodes-control-memory.typ"

#pagebreak()

== VM Core Specifications

=== Execution Model
#include "specifications/vm-core/execution-model.typ"

=== Stack Management
#include "specifications/vm-core/stack-management.typ"

=== Function Calls
#include "specifications/vm-core/function-calls.typ"

=== Interrupt Handling
#include "specifications/vm-core/interrupt-handling.typ"

#pagebreak()

== Memory Specifications

=== Virtual Memory
#include "specifications/memory/virtual-memory.typ"

=== Garbage Collection
#include "specifications/memory/garbage-collection.typ"

=== Memory Layout
#include "specifications/memory/memory-layout.typ"

=== Address Translation
#include "specifications/memory/address-translation.typ"

#pagebreak()

== Data Structures

=== Cons Cells
#include "specifications/data-structures/cons-cells.typ"

=== Arrays
#include "specifications/data-structures/arrays.typ"

=== Function Headers
#include "specifications/data-structures/function-headers.typ"

=== Number Types
#include "specifications/data-structures/number-types.typ"

=== Sysout Format Overview
#include "specifications/data-structures/sysout-format-overview.typ"

=== Sysout Format FPtoVP
#include "specifications/data-structures/sysout-format-fptovp.typ"

=== Sysout Format Loading
#include "specifications/data-structures/sysout-format-loading.typ"

=== Sysout Byte Swapping
#include "specifications/data-structures/sysout-byte-swapping.typ"

=== Sysout Saving
#include "specifications/data-structures/sysout-saving.typ"

#pagebreak()

== Display Specifications

=== Interface Abstraction
#include "specifications/display/interface-abstraction.typ"

=== Graphics Operations
#include "specifications/display/graphics-operations.typ"

=== Event Protocols
#include "specifications/display/event-protocols.typ"

#pagebreak()

== I/O Specifications

=== Keyboard Protocol
#include "specifications/io/keyboard-protocol.typ"

=== Mouse Protocol
#include "specifications/io/mouse-protocol.typ"

=== File System
#include "specifications/io/file-system.typ"

=== Network Protocol
#include "specifications/io/network-protocol.typ"

#pagebreak()

== Platform Abstraction

=== Required Behaviors
#include "specifications/platform-abstraction/required-behaviors.typ"

=== Implementation Choices
#include "specifications/platform-abstraction/implementation-choices.typ"

#pagebreak()

== Validation

=== Reference Behaviors
#include "specifications/validation/reference-behaviors.typ"

=== Compatibility Criteria
#include "specifications/validation/compatibility-criteria.typ"

#pagebreak()

= Implementations

== Zig Implementation
#include "implementations/zig-implementation.typ"

== Lisp Implementation
#include "implementations/lisp-implementation.typ"

#pagebreak()

= Medley Interlisp

== Architecture
#include "medley/architecture.typ"

== Components

=== Sysout
#include "medley/components/sysout.typ"

=== Loadup
#include "medley/components/loadup.typ"

=== Scripts
#include "medley/components/scripts.typ"

=== Directory Structure
#include "medley/components/directory-structure.typ"

=== Configuration
#include "medley/components/configuration.typ"

=== VMem
#include "medley/components/vmem.typ"

=== Greetfiles
#include "medley/components/greetfiles.typ"

#pagebreak()

== Interface

=== Command Line
#include "medley/interface/command-line.typ"

=== Environment
#include "medley/interface/environment.typ"

=== File Formats
#include "medley/interface/file-formats.typ"

=== Protocols
#include "medley/interface/protocols.typ"

#pagebreak()

== Platform

=== Linux
#include "medley/platform/linux.typ"

=== macOS
#include "medley/platform/macos.typ"

=== Windows
#include "medley/platform/windows.typ"

=== WSL
#include "medley/platform/wsl.typ"

#pagebreak()

= Reference

== Glossary
#include "reference/glossary.typ"

#pagebreak()

== API Reference
#include "reference/api.typ"

#pagebreak()

== Index
#include "reference/index.typ"
