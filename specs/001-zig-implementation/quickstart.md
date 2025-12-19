# Quickstart: Zig Implementation

**Date**: 2025-12-04
**Feature**: Maiko Emulator Implementation in Zig

## Overview

This guide helps developers get started with the Zig implementation of Maiko emulator.

## Prerequisites

- Zig compiler 0.15.1 installed
- SDL3 development libraries
- Access to rewrite documentation in `.ai_assistant_db/rewrite-spec/`
- Test sysout files for validation

## Installation

### Install Zig

**Linux/macOS**:

```bash
# Download from https://ziglang.org/download/
# Or use package manager
```

**macOS** (Homebrew):

```bash
brew install zig
```

**Linux** (package manager):

```bash
# Ubuntu/Debian
sudo apt install zig

# Or download from ziglang.org
```

### Install SDL3

**Linux**:

```bash
# SDL3 may require building from source or using a package manager that supports it
# Check https://github.com/libsdl-org/SDL for installation instructions
```

**macOS**:

```bash
brew install sdl3
# Or build from source if not available via Homebrew
```

## Project Structure

```
alternatives/zig/
├── build.zig          # Zig build configuration
├── src/               # Source code
│   ├── vm/           # VM core
│   ├── memory/       # Memory management
│   ├── data/         # Data structures
│   ├── display/      # SDL display backend
│   └── io/           # I/O subsystems
└── tests/            # Test files
```

## Building

```bash
cd alternatives/zig
zig build
```

This will:

- Compile all source files
- Link SDL3 library
- Create executable `zig-out/bin/zaiko`

## Running

```bash
# Run with sysout file
zig-out/bin/zaiko path/to/sysout.sysout
```

## Implementation Path

### Phase 1: VM Core (MVP)

1. **Read**: `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md`
2. **Read**: `.ai_assistant_db/rewrite-spec/vm-core/execution-model.md`
3. **Implement**: Basic dispatch loop with 10-20 opcodes
4. **Test**: Execute simple arithmetic operations

**Deliverable**: Working bytecode interpreter

### Phase 2: Memory Management

1. **Read**: `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md`
2. **Read**: `.ai_assistant_db/rewrite-spec/memory/address-translation.md`
3. **Implement**: GC and memory allocation
4. **Test**: Load simple sysout file

**Deliverable**: Memory system compatible with sysout files

### Phase 3: Complete Instruction Set

1. **Read**: `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md` (all opcodes)
2. **Implement**: Remaining opcodes incrementally
3. **Test**: Execute complex Lisp programs

**Deliverable**: Complete instruction set implementation

### Phase 4: I/O and Display

1. **Read**: `.ai_assistant_db/rewrite-spec/io/keyboard-protocol.md`
2. **Read**: `.ai_assistant_db/rewrite-spec/display/interface-abstraction.md`
3. **Implement**: SDL display backend, keyboard/mouse handling
4. **Test**: Interactive Lisp session

**Deliverable**: Fully functional emulator with graphics and I/O

## Key Concepts

### Memory Safety

Zig provides memory safety through:

- Explicit memory management
- Bounds checking
- No undefined behavior
- Compile-time checks

### Compatibility

- Exact memory layout matching C implementation
- Same bytecode execution semantics
- Sysout file format compatibility

### Testing

Use Zig test framework:

```zig
test "opcode execution" {
    // Test opcode behavior
}
```

## Troubleshooting

**Problem**: SDL not found

- **Solution**: Install SDL3 development libraries
- **Check**: `pkg-config --modversion sdl2`

**Problem**: Sysout file won't load

- **Solution**: Verify memory layout matches specification
- **Check**: `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md`

**Problem**: Opcode produces wrong result

- **Solution**: Check execution semantics in rewrite documentation
- **Check**: `.ai_assistant_db/rewrite-spec/instruction-set/opcodes.md`

## Next Steps

After MVP:

1. Implement remaining opcodes
2. Add I/O and display subsystems
3. Comprehensive testing
4. Performance optimization

## Related Documentation

- [Rewrite Documentation](../../.ai_assistant_db/rewrite-spec/) - Complete specifications
- [Source Code Mapping](../../.ai_assistant_db/rewrite-spec/SOURCE_CODE_MAPPING.md) - Code to docs mapping
- [Quickstart Guide](../../.ai_assistant_db/rewrite-spec/quickstart.md) - General implementation guide
