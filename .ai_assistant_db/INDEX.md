# Maiko Documentation Index

**Navigation**: [README](README.md) | [Architecture](architecture.md) | [Components](components/) | [API](api/) | [Glossary](glossary.md) | [Build System](build-system.md)

Quick reference guide to all documentation files.

**Note**: All documentation files now include Mermaid diagrams and extensive cross-linking for easy navigation.

## Documentation Structure

```
.ai_assistant_db/
├── README.md                    # Overview and navigation
├── INDEX.md                      # This file - quick reference
├── architecture.md               # System architecture overview
├── build-system.md              # Build system documentation
├── glossary.md                   # Terminology and concepts
├── components/                   # Component documentation
│   ├── vm-core.md              # VM core and execution
│   ├── memory-management.md    # GC and memory
│   ├── display.md              # Display subsystems
│   └── io.md                   # I/O systems
├── api/                         # API reference
│   └── overview.md             # API overview
├── implementations/             # Alternative implementations
│   └── lisp-implementation.md  # Common Lisp implementation
└── rewrite-spec/                # Language-agnostic specifications
    ├── instruction-set/        # Bytecode specifications
    ├── vm-core/                # Execution engine specs
    ├── memory/                 # Memory management specs
    ├── data-structures/        # Data structure formats
    ├── display/                # Display subsystem specs
    ├── io/                     # I/O subsystem specs
    └── platform-abstraction/   # Platform requirements
```

## Quick Reference

### Getting Started

1. **New to Maiko?** → Start with [README.md](README.md)
2. **Understanding Architecture?** → Read [architecture.md](architecture.md)
3. **Building the Project?** → See [build-system.md](build-system.md)
4. **Looking for Terms?** → Check [glossary.md](glossary.md)

### By Topic

#### VM Execution

- **How the VM works**: [architecture.md](architecture.md) → VM Core section
- **Dispatch loop**: [components/vm-core.md](components/vm-core.md) → Dispatch Loop
- **Stack management**: [components/vm-core.md](components/vm-core.md) → Stack Management
- **Instruction execution**: [components/vm-core.md](components/vm-core.md) → Instruction Handlers

#### Memory Management

- **Garbage collection**: [components/memory-management.md](components/memory-management.md)
- **Virtual memory**: [components/memory-management.md](components/memory-management.md) → Virtual Memory
- **Storage allocation**: [components/memory-management.md](components/memory-management.md) → Storage Allocation

#### Display Systems

- **X11 display**: [components/display.md](components/display.md) → X11 Implementation
- **SDL display**: [components/display.md](components/display.md) → SDL Implementation
- **BitBLT operations**: [components/display.md](components/display.md) → BitBLT Operations

#### I/O Systems

- **Keyboard input**: [components/io.md](components/io.md) → Keyboard System
- **Mouse input**: [components/io.md](components/io.md) → Mouse System
- **File system**: [components/io.md](components/io.md) → File System
- **Networking**: [components/io.md](components/io.md) → Network Communication

#### Building

- **CMake build**: [build-system.md](build-system.md) → CMake Build System
- **Make build**: [build-system.md](build-system.md) → Make Build System
- **Platform support**: [build-system.md](build-system.md) → Platform-Specific Considerations

## File-to-Component Mapping

### Source Files → Documentation

#### Core VM

- `maiko/src/main.c` → [components/vm-core.md](components/vm-core.md)
- `maiko/src/xc.c` → [components/vm-core.md](components/vm-core.md)
- `maiko/src/hardrtn.c` → [components/vm-core.md](components/vm-core.md)
- `maiko/src/return.c` → [components/vm-core.md](components/vm-core.md)
- `maiko/src/llstk.c` → [components/vm-core.md](components/vm-core.md)

#### Memory Management

- `maiko/src/gc*.c` → [components/memory-management.md](components/memory-management.md)
- `maiko/src/storage.c` → [components/memory-management.md](components/memory-management.md)
- `maiko/src/conspage.c` → [components/memory-management.md](components/memory-management.md)
- `maiko/src/vmemsave.c` → [components/memory-management.md](components/memory-management.md)

#### Display

- `maiko/src/xinit.c` → [components/display.md](components/display.md)
- `maiko/src/sdl.c` → [components/display.md](components/display.md)
- `maiko/src/xbbt.c` → [components/display.md](components/display.md)
- `maiko/src/bitblt.c` → [components/display.md](components/display.md)

#### I/O

- `maiko/src/kbdif.c` → [components/io.md](components/io.md)
- `maiko/src/mouseif.c` → [components/io.md](components/io.md)
- `maiko/src/dir.c` → [components/io.md](components/io.md)
- `maiko/src/rs232c.c` → [components/io.md](components/io.md)
- `maiko/src/inet.c` → [components/io.md](components/io.md)

## Key Concepts Quick Lookup

### Address Types

- **LispPTR**: Virtual address → [glossary.md](glossary.md)
- **DLword**: 16-bit word → [glossary.md](glossary.md)
- **Address Translation**: [components/vm-core.md](components/vm-core.md) → Address Translation

### Execution

- **Dispatch Loop**: [components/vm-core.md](components/vm-core.md) → Dispatch Loop Structure
- **Stack Frame**: [components/vm-core.md](components/vm-core.md) → Stack Frame Structure
- **Opcode**: [glossary.md](glossary.md)

### Memory

- **Cons Cell**: [glossary.md](glossary.md)
- **GC**: [components/memory-management.md](components/memory-management.md) → Garbage Collection Algorithm
- **Virtual Memory**: [components/memory-management.md](components/memory-management.md) → Virtual Memory

### Display

- **BitBLT**: [glossary.md](glossary.md)
- **Display Region**: [components/display.md](components/display.md) → Display Region
- **DspInterface**: [components/display.md](components/display.md) → Display Interface Structure

## Common Tasks

### Understanding Code Flow

1. Entry point: `main()` in `maiko/src/main.c` → [components/vm-core.md](components/vm-core.md)
2. VM startup: `start_lisp()` → [components/vm-core.md](components/vm-core.md)
3. Execution: `dispatch()` → [components/vm-core.md](components/vm-core.md)

### Adding a New Feature

1. Understand architecture: [architecture.md](architecture.md)
2. Find relevant component: [components/](components/)
3. Check API: [api/overview.md](api/overview.md)
4. Understand build: [build-system.md](build-system.md)

### Debugging

1. Understand execution model: [components/vm-core.md](components/vm-core.md)
2. Check memory management: [components/memory-management.md](components/memory-management.md)
3. Review error handling: [glossary.md](glossary.md) → Error Terms

### Porting to New Platform

1. Build system: [build-system.md](build-system.md) → Platform-Specific Considerations
2. Architecture: [architecture.md](architecture.md) → Platform Abstraction
3. Display: [components/display.md](components/display.md) → Display Architecture

## Documentation Statistics

- **Total Files**: 8 markdown files
- **Components Documented**: 4 major components
- **API Overview**: 1 overview document
- **Build Systems**: 2 build systems documented

## Maintenance

This documentation should be updated when:

- New components are added
- APIs change
- Build system changes
- Architecture evolves

## Alternative Implementations

- **[Common Lisp Implementation](implementations/lisp-implementation.md)** - Complete SBCL implementation (77/78 tasks, 98.7% complete)
  - 24 source files, 11 test files
  - 189 of 256 opcodes implemented
  - ASDF build system, SDL3 display backend
  - Located in `alternatives/lisp/`

## Related Resources

- **Source Code**: `maiko/src/` directory
- **Header Files**: `maiko/inc/` directory
- **Build Files**: `maiko/CMakeLists.txt`, `maiko/bin/makeright`
- **Project README**: `/README.md`
- **Lisp Implementation**: `/alternatives/lisp/` - Common Lisp implementation

## Feedback

For questions or improvements to this documentation, refer to the main project repository.
