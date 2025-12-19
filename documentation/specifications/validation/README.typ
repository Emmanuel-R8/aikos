= Validation and Testing

*Navigation*: README | Index

This section provides reference test cases and compatibility criteria for validating emulator rewrite implementations.

== Overview

Validation ensures that a rewrite implementation:

- Executes bytecode correctly
- Maintains sysout file compatibility
- Handles edge cases properly
- Produces identical results to Maiko

== Test Case Format

Reference test cases specify:

- *Input*: Bytecode sequence or operation
- *Expected Output*: Expected result or behavior
- *State Changes*: How VM state should change
- *Validation Method*: How to verify correctness

== Test Categories

=== Opcode Tests

Test individual opcode execution:

- Arithmetic operations
- Memory operations
- Control flow
- Function calls

=== Integration Tests

Test subsystem interactions:

- GC during execution
- Interrupt handling
- Stack management
- Memory allocation

=== Compatibility Tests

Test sysout compatibility:

- Load existing sysout files
- Execute saved programs
- Verify state restoration

=== Edge Case Tests

Test boundary conditions:

- Stack overflow
- Memory exhaustion
- Invalid opcodes
- Error recovery

== Using Test Cases

1. Implement opcode or feature
2. Run corresponding test case
3. Verify output matches expected result
4. Check state changes match specification
5. Document any deviations (if acceptable)

== Reference Behaviors

See Reference Behaviors for specific test cases.

== Compatibility Criteria

See Compatibility Criteria for what must match exactly.

== Related Documentation

- Instruction Set - Opcode specifications
- VM Core - Execution model
- Memory Management - GC and memory
- Platform Abstraction - Required vs optional behaviors
