# Alternative Implementations

**Navigation**: [Main README](../README.md) | [Index](../INDEX.md)

This directory contains documentation for alternative implementations of the Maiko emulator.

## Implementations

### [Common Lisp Implementation](lisp-implementation.md)

Complete implementation of the Maiko emulator in Common Lisp (SBCL).

- **Status**: ✅ Complete (77/78 tasks, 98.7%)
- **Location**: `alternatives/lisp/`
- **Build System**: ASDF
- **Display Backend**: SDL3
- **Opcodes**: 189 of 256 implemented (73.8%)
- **Source Files**: 24 Lisp files
- **Test Files**: 11 test files

**Key Features**:
- Complete VM core with dispatch loop
- Memory management (storage, GC, virtual memory)
- Display subsystem (SDL3 backend)
- I/O subsystem (keyboard, mouse, filesystem)
- Sysout file loading with endianness handling
- Comprehensive error handling
- Platform-specific support (endianness, pathnames)

## Implementation Status

| Implementation | Language | Status | Opcodes | Tasks | Location |
|----------------|----------|--------|---------|-------|----------|
| Common Lisp | SBCL | ✅ Complete | 189/256 | 77/78 | `alternatives/lisp/` |

## Related Documentation

- [Rewrite Specifications](../rewrite-spec/) - Language-agnostic specifications
- [Component Documentation](../components/) - System architecture
- [API Reference](../api/) - Function signatures
