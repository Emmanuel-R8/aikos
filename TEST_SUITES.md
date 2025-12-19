# Test Suites for Maiko Emulators and Medley

This document summarizes the test suites available for the C, Zig, and Lisp implementations of the Maiko emulator, as well as Medley.

## C Emulator (Maiko)

### Test Tools
- **Location**: `maiko/src/testtool.c`
- **Purpose**: Debugging and testing utilities for the C emulator
- **Type**: Debugging aids, not a formal automated test suite
- **Functions**: Includes utilities for:
  - `dump_check_atoms()` - Atom table inspection
  - `print_atomname(index)` - Print atom names
  - `dump_dtd()` - Data type descriptor dumping
  - `check_type_68k(type,ptr)` - Type checking
  - `dump_conspage(base, linking)` - Cons page inspection
  - `dump_fnobj(index)` - Function object inspection
  - `all_stack_dump(start,end)` - Stack inspection
  - And many more debugging utilities

### Test Binary
- **Location**: `maiko/bin/test.vm`
- **Type**: Binary test file (format unknown)

### Running Tests
The C emulator does not appear to have a formal automated test suite. Testing is done through:
1. Manual testing with `testtool.c` debugging utilities
2. Running Medley with various sysout files
3. Integration testing through Medley loadup process

## Zig Emulator

### Test Suite
- **Location**: `zaiko/tests/`
- **Type**: Zig unit tests using `std.testing`
- **Test Files**: 40+ test files covering:
  - `integration.zig` - Integration tests for VM execution cycle
  - `compatibility.zig` - Compatibility tests with C implementation
  - `opcodes.zig` - Opcode execution tests
  - `memory.zig` - Memory management tests
  - `gc.zig` - Garbage collection tests
  - `stack.zig` - Stack management tests
  - `dispatch.zig` - Dispatch loop tests
  - `sysout.zig` - Sysout file loading tests
  - `filesystem.zig` - File system I/O tests
  - `display.zig` - Display backend tests
  - `keyboard.zig` - Keyboard input tests
  - `mouse.zig` - Mouse input tests
  - And many more specialized test files

### Running Tests
```bash
cd zaiko
zig build test
```

### Test Coverage
The Zig test suite is comprehensive and includes:
- Unit tests for individual opcodes
- Integration tests for VM execution
- Compatibility tests comparing with C implementation
- Edge case tests for memory management
- I/O subsystem tests

## Lisp Emulator

### Test Suite
- **Location**: `laiko/tests/`
- **Type**: Common Lisp tests using FiveAM testing framework
- **Test Files**: 11 test files:
  - `compatibility.lisp` - Compatibility tests with C implementation
  - `dispatch.lisp` - Dispatch loop tests
  - `display.lisp` - Display backend tests
  - `filesystem.lisp` - File system I/O tests
  - `gc.lisp` - Garbage collection tests
  - `keyboard.lisp` - Keyboard input tests
  - `memory.lisp` - Memory management tests
  - `mouse.lisp` - Mouse input tests
  - `opcodes.lisp` - Opcode execution tests
  - `stack.lisp` - Stack management tests
  - `sysout.lisp` - Sysout file loading tests

### Running Tests
```bash
cd laiko
sbcl --load laiko.asd --eval "(asdf:test-system :laiko)"
```

Or using the build script:
```bash
cd laiko
./build.sh test
```

### Test Framework
- **Framework**: FiveAM (FiveAM is a Common Lisp testing framework)
- **System Definition**: Tests are defined in `laiko.asd` under the `:laiko/tests` system

## Medley Test Suites

### Loadup Tests
- **Location**: `medley/scripts/loadups/`
- **Purpose**: Integration tests through the loadup process
- **Type**: Build and integration tests
- **Process**: The loadup scripts create sysout files and verify they work correctly

### Manual Testing
Medley testing is primarily done through:
1. Running the loadup process to create sysout files
2. Running Medley with various sysout files
3. Manual verification of functionality

## Recommendations

1. **For C Emulator**: Consider creating a formal test suite based on the Zig and Lisp test suites
2. **For Zig Emulator**: The test suite is comprehensive; continue expanding it as features are implemented
3. **For Lisp Emulator**: The test suite structure is good; continue implementing test cases
4. **For Medley**: Consider creating automated integration tests that verify:
   - Script functionality
   - Emulator selection and execution
   - Loadup process correctness
   - Cross-emulator compatibility

## Test Compatibility

All three emulator implementations should produce identical results when:
- Executing the same bytecode
- Loading the same sysout files
- Processing the same input

The compatibility test suites in Zig and Lisp are designed to verify this compatibility with the C implementation.

