# VM Core Specification

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [Instruction Set](../instruction-set/)

Complete specification of the VM core execution engine, including dispatch loop, stack management, function calls, and interrupt handling.

## Overview

The VM Core is the execution engine that interprets Lisp bytecode. It manages the execution state, stack frames, function calls, and coordinates with memory management and I/O subsystems.

## Documentation Structure

- **[Execution Model](execution-model.md)** - Dispatch loop and instruction execution
- **[Stack Management](stack-management.md)** - Stack frames and operations
- **[Function Calls](function-calls.md)** - Call/return mechanisms
- **[Interrupt Handling](interrupt-handling.md)** - Interrupt processing

## Core Components

### Execution Engine

The dispatch loop (`dispatch()`) is the heart of the VM:

- Fetches bytecode instructions
- Decodes opcodes and operands
- Executes opcode handlers
- Manages program counter
- Handles interrupts

See [Execution Model](execution-model.md) for complete specification.

### Stack System

The stack manages function activation:

- Stack frames (FX) for each function call
- Activation links between frames
- Local variable storage
- Parameter passing

See [Stack Management](stack-management.md) for complete specification.

### Function Call Mechanism

Function calls involve:

- Frame allocation
- Argument setup
- Activation link management
- Return value handling

See [Function Calls](function-calls.md) for complete specification.

### Interrupt System

Interrupts are processed between instructions:

- I/O interrupts (keyboard, mouse, network)
- Timer interrupts
- System interrupts

See [Interrupt Handling](interrupt-handling.md) for complete specification.

## Execution State

The VM maintains execution state:

- **Program Counter (PC)**: Current instruction pointer
- **Stack Pointer**: Current stack position
- **Current Frame**: Active function frame
- **Function Object**: Current function metadata
- **Top of Stack**: Current TOS value

## Related Documentation

- [Instruction Set](../instruction-set/) - Bytecode specifications
- [Memory Management](../memory/) - Memory and GC used by VM
- [I/O Systems](../io/) - I/O interrupts processed by VM
