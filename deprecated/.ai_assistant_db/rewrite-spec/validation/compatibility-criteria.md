---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Compatibility Criteria

**Navigation**: [README](README.md) | [Reference Behaviors](reference-behaviors.md)

Complete specification of compatibility criteria defining what must match exactly for emulator compatibility.

## Overview

Compatibility criteria define the requirements for an implementation to be considered compatible with Maiko. These criteria ensure that implementations can run existing Lisp programs and sysout files.

## Core Compatibility Requirements

### Bytecode Execution Compatibility

**MUST MATCH**:

- All 256 opcode execution semantics
- Stack effects of each opcode
- Error conditions and error handling
- Program counter advancement

**Validation**: Execute reference test cases, compare results with Maiko

### Memory Layout Compatibility

**MUST MATCH**:

- Memory region offsets
- Data structure formats
- Field layouts and sizes
- Alignment requirements

**Validation**: Load sysout files, verify data structure access

### Sysout File Compatibility

**MUST MATCH**:

- Sysout file format
- IFPAGE structure
- FPtoVP table format
- Page loading algorithm

**Validation**: Load existing sysout files successfully

## Functional Compatibility

### Instruction Set Compatibility

**Requirement**: All 256 opcodes must execute correctly

**Validation**:

- Execute opcode test cases
- Compare results with Maiko
- Verify error handling

### Memory Management Compatibility

**Requirement**: GC must behave identically

**Validation**:

- Reference counting matches
- Reclamation behavior matches
- Memory layout matches

### I/O Compatibility

**Requirement**: I/O operations must behave identically

**Validation**:

- File operations work correctly
- Pathname translation correct
- Network protocols compatible

## Behavioral Compatibility

### Execution Behavior

**MUST MATCH**:

- Instruction execution order
- Stack frame behavior
- Function call/return behavior
- Interrupt handling

### Memory Behavior

**MUST MATCH**:

- Address translation
- Memory allocation
- GC behavior
- Data structure access

### I/O Behavior

**MUST MATCH**:

- Keycode translation
- Mouse event handling
- File I/O semantics
- Network protocol behavior

## Compatibility Levels

### Level 1: Basic Compatibility

**Requirements**:

- Executes bytecode correctly
- Loads sysout files
- Basic memory management works

**Use Case**: Running simple Lisp programs

### Level 2: Full Compatibility

**Requirements**:

- All opcodes implemented correctly
- Complete GC implementation
- Full I/O support

**Use Case**: Running complex Lisp programs

### Level 3: Production Compatibility

**Requirements**:

- Performance acceptable
- All edge cases handled
- Full platform support

**Use Case**: Production use

## Validation Methods

### Automated Testing

- Unit tests for individual opcodes
- Integration tests for subsystems
- Compatibility tests with sysout files

### Manual Testing

- Run existing Lisp programs
- Compare execution results
- Verify visual output matches

### Reference Implementation

- Compare with Maiko behavior
- Use Maiko as reference
- Verify identical results

## Compatibility Checklist

### VM Core

- [ ] All 256 opcodes implemented
- [ ] Dispatch loop works correctly
- [ ] Stack management correct
- [ ] Function calls work correctly
- [ ] Interrupts handled correctly

### Memory Management

- [ ] Address translation correct
- [ ] GC algorithm matches
- [ ] Memory layout matches
- [ ] Data structures correct

### I/O and Display

- [ ] Keycode translation correct
- [ ] Mouse events handled correctly
- [ ] File I/O works correctly
- [ ] Display rendering correct

### Sysout Compatibility

- [ ] Can load sysout files
- [ ] Can execute sysout programs
- [ ] Results match Maiko

## Related Documentation

- [Reference Behaviors](reference-behaviors.md) - Test cases
- [Required Behaviors](../platform-abstraction/required-behaviors.md) - Must-match behaviors
- [Implementation Choices](../platform-abstraction/implementation-choices.md) - May-differ choices
