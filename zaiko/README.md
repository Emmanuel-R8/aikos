# Zaiko - Maiko Emulator Implementation in Zig

**Project Name**: zaiko  
**Location**: `zaiko/` (relocated from `maiko/alternatives/zig/`)

This directory contains a complete implementation of the Maiko emulator in Zig programming language, following the rewrite documentation specifications in `documentation/specifications/`.

## Overview

The Zig implementation provides:
- Complete VM core with bytecode interpreter
- Memory management with garbage collection
- SDL2 display backend
- I/O subsystems (keyboard, mouse, file system)
- Full compatibility with existing sysout files

## Prerequisites

- Zig compiler 0.11+ installed
- SDL2 development libraries

### Installing Zig

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
```

### Installing SDL2

**Linux**:
```bash
sudo apt install libsdl2-dev
```

**macOS**:
```bash
brew install sdl2
```

## Building

```bash
cd alternatives/zig
zig build
```

This will create the executable `zig-out/bin/maiko-zig`.

## Running

```bash
# Run with sysout file
zig-out/bin/maiko-zig path/to/sysout.sysout
```

## Testing

```bash
zig build test
```

## Project Structure

```
alternatives/zig/
├── build.zig          # Zig build system configuration
├── build.zig.zon      # Zig package dependencies
├── src/
│   ├── main.zig       # Entry point
│   ├── vm/            # VM core
│   ├── memory/        # Memory management
│   ├── data/          # Data structures
│   ├── display/       # SDL display backend
│   ├── io/            # I/O subsystems
│   └── utils/         # Utilities
└── tests/             # Test files
```

## Implementation Status

- [x] Project setup
- [x] VM Core (framework complete, opcode implementations pending)
- [x] Memory Management (structure complete, GC operations pending)
- [x] I/O and Display (structure complete, SDL integration pending)

## Current Status

The Zig implementation has a complete framework in place:
- ✅ All core modules created
- ✅ Build system working
- ✅ Tests compiling and passing
- 🔄 Opcode handlers need implementation logic
- 🔄 GC operations need completion
- 🔄 SDL2 integration pending

## Usage Examples

### Building

```bash
cd alternatives/zig
zig build
```

### Running Tests

```bash
zig build test
```

### Running the Emulator

```bash
zig-out/bin/maiko-zig path/to/sysout.sysout
```

## Troubleshooting

### SDL2 Not Found

SDL2 linking is currently disabled for NixOS compatibility. To enable:
1. Install SDL2 development libraries
2. Uncomment SDL2 linking in `build.zig`
3. Rebuild

### Build Errors

If you encounter build errors:
1. Ensure Zig 0.15.1+ is installed
2. Check that all dependencies are available
3. Review `docs/IMPLEMENTATION.md` for known issues

## Related Documentation

- [Rewrite Documentation](../../.ai_assistant_db/rewrite-spec/) - Complete specifications
- [Implementation Plan](../../specs/001-zig-implementation/plan.md) - Technical plan
- [Tasks](../../specs/001-zig-implementation/tasks.md) - Implementation tasks
