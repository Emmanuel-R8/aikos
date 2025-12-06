# Platform Abstraction

**Navigation**: [README](../README.md) | [Index](../INDEX.md)

This section documents platform abstraction requirements for emulator rewrites, distinguishing behaviors that must match exactly from implementation choices that may differ.

## Overview

Maiko runs on multiple platforms (macOS, Linux, FreeBSD, Solaris, Windows) and architectures (x86_64, ARM64, SPARC, etc.). A rewrite must maintain compatibility while allowing platform-specific optimizations.

## Key Concepts

### Required Behaviors

Behaviors that MUST match exactly for compatibility:

- Bytecode execution semantics
- Memory layout and data structure formats
- Sysout file format compatibility
- Interface protocol compliance

See [Required Behaviors](required-behaviors.md) for detailed specifications.

### Implementation Choices

Aspects that MAY differ across platforms:

- Specific graphics library (X11, SDL, DirectX, etc.)
- OS-specific system call interfaces
- Event delivery mechanisms (polling vs callbacks)
- Memory allocation strategies (as long as layout matches)

See [Implementation Choices](implementation-choices.md) for guidance.

## Platform-Independent Specifications

All core VM specifications are platform-independent:

- [Instruction Set](../instruction-set/) - Bytecode semantics
- [VM Core](../vm-core/) - Execution model
- [Memory Management](../memory/) - GC and memory layout
- [Data Structures](../data-structures/) - Format specifications

## Platform-Specific Notes

Platform-specific implementation guidance:

- Display backends may use different graphics libraries
- I/O may use different OS APIs
- Build systems may differ
- Performance optimizations may be platform-specific

## Compatibility Requirements

A rewrite maintains compatibility when:

- ✅ Executes bytecode identically to Maiko
- ✅ Loads and runs existing sysout files
- ✅ Produces identical results for same inputs
- ✅ Handles errors and edge cases identically

Platform-specific optimizations are acceptable as long as:

- Core semantics remain unchanged
- Compatibility is maintained
- Required behaviors match exactly

## Related Documentation

- [Required Behaviors](required-behaviors.md) - Must-match specifications
- [Implementation Choices](implementation-choices.md) - May-differ guidance
- [Validation](../validation/compatibility-criteria.md) - Compatibility testing
