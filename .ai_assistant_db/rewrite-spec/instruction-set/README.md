# Instruction Set Specification

**Navigation**: [README](../README.md) | [Index](../INDEX.md) | [VM Core](../vm-core/)

Complete specification of the Maiko bytecode instruction set. This section provides all information needed to implement bytecode execution in any language.

## Overview

The Maiko VM uses a bytecode instruction set with 256 possible opcode values (0-255). Instructions are variable-length, with the opcode byte followed by zero or more operand bytes.

## Documentation Structure

- **[Instruction Format](instruction-format.md)** - How bytecode instructions are encoded
- **[Opcodes Reference](opcodes.md)** - Complete reference for all 256 opcodes
- **[Execution Semantics](execution-semantics.md)** - Execution behavior and rules

## Instruction Categories

### Control Flow (0x00-0x3F)

- Function calls (FN0-FNX, APPLYFN, CHECKAPPLY)
- Returns (RETURN, SLRETURN)
- Jumps (JUMP0-JUMP15, JUMPX, FJUMP, TJUMP)
- Unwinding (UNWIND)

### Memory Operations (0x40-0x7F)

- Variable access (IVAR, PVAR, FVAR, GVAR)
- Stack operations (POP, POP_N)
- Binding (BIND, UNBIND, DUNBIND)

### Data Operations (0x80-0xBF)

- Cons operations (CAR, CDR, CONS, RPLACA, RPLACD)
- Array operations (AREF, ASET)
- Type operations (TYPEP, NTYPX, DTEST)

### Arithmetic (0xC0-0xFF)

- Integer arithmetic (IPLUS2, IDIFFERENCE, ITIMES2, IQUOTIENT)
- Floating-point arithmetic (FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT)
- Comparisons (EQ, EQUAL, GREATERP, IGREATERP, FGREATERP)
- Bitwise operations (LOGOR2, LOGAND2, LOGXOR2, LSH)

## Opcode Organization

Opcodes are organized by function:

- **Sequential opcodes**: Related opcodes grouped together (e.g., IVAR0-IVAR6, JUMP0-JUMP15)
- **Unused opcodes**: Some values are unused (marked as `opc_unused_N`)
- **UFN handling**: Undefined function names handled specially

## Instruction Length

Instructions have variable length:

- **1 byte**: Opcode only (no operands)
- **2 bytes**: Opcode + 1 operand byte
- **3 bytes**: Opcode + 2 operand bytes
- **4 bytes**: Opcode + 3 operand bytes (for BIGATOMS)
- **Up to 9 bytes**: Multi-byte opcodes for complex operations

See [Instruction Format](instruction-format.md) for encoding details.

## Execution Model

Instructions are executed by:

1. **Fetch**: Read opcode byte from program counter
2. **Decode**: Determine opcode and operand count
3. **Execute**: Call opcode handler with operands
4. **Update**: Advance program counter

See [VM Core Execution Model](../vm-core/execution-model.md) for dispatch loop details.

## Related Documentation

- [VM Core](../vm-core/) - Execution engine that executes these instructions
- [Data Structures](../data-structures/) - Data types used by instructions
- [Memory Management](../memory/) - Memory operations performed by instructions
