# AI Agent Guidelines for Interlisp Project

**Date**: 2025-12-10 12:45
**Purpose**: Guidelines and best practices for AI agents working on this project

## Project Overview

This repository contains the **Interlisp** project, which includes:

- **Maiko**: Virtual machine emulator for Medley Interlisp bytecode
- **C Implementation**: Reference implementation in C (fully functional)
- **Zig Implementation**: Alternative implementation of Maiko in Zig (in progress)
- **Common Lisp Implementation**: Alternative implementation of Maiko in Common Lisp (targetting Sbcl) (in progress)

## Critical Documentation

### Must-Read Files

1. **`documentation/core/critical_memory.typ`** - **CRITICAL**: Rules for documentation updates

   - All documentation improvements MUST be emulator-independent in `specifications/`
   - Language-specific details go in `implementations/`
   - **ALWAYS** write documentation using the Typst document format
   - **ALWAYS** update both before committing
   - **ALWAYS** use the command `date`to date entries (when necessary )i as YYYY-mm-dd HH:MM

2. **`documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`** - **CRITICAL**: Essential debugging techniques and practices

3. **`documentation/README.md`** - Overview of documentation structure
4. **`specs/004-emulator-runner/plan.md`** - Implementation plan for emulator runner
5. **`specs/004-emulator-runner/tasks.md`** - Task list for emulator runner
6. **`specs/005-zig-completion/plan.md`** - Implementation plan for Zig emulator
7. **`specs/005-zig-completion/tasks.md`** - Detailed task list (94/108 complete, 87.0%)

## Project Structure

```
Interlisp/
├── documentation/             # Knowledge base (emulator-independent specs + implementations)
│   ├── specifications/        # Emulator-independent specifications
│   ├── implementations/       # Language-specific implementation notes
│   └── core/
│       └── critical_memory.typ # ⚠️ MUST READ AND ABIDE BY - Documentation rules
├── maiko/                     # Maiko VM source code
│   ├── src/                   # C implementation (reference)
│   └── alternatives/zig/      # Zig implementation (in progress)
│   └── alternatives/lisp/     # Common Lisp (sbcl) implementation (in progress - will be developed after Zig)
├── specs/                     # Feature specifications
│   └── 005-zig-completion/   # Zig emulator completion spec
└── medley/                    # Medley Interlisp system
```

## Current Status (2025-01-27)

### Zig Emulator Completion: 87.0% (94/108 tasks)

**Completed**:

- ✅ Phase 1: Sysout Loading (22/22 tasks)
- ✅ Phase 2: Basic Execution (12/12 tasks)
- ✅ Phase 3: Essential Opcodes (25/25 tasks)
- ✅ Phase 4: GC Operations (15/15 tasks)
- ✅ Phase 5: SDL2 Display Integration (22/22 tasks - implementation complete)

**Remaining**:

- ⏳ SDL2 Test Cases (5 tasks: T092-T096)
- ⏳ Polish Tasks (5 tasks: T103-T108)

**Known Issues**:

- ⚠️ Minor compilation fixes needed (SDL2 type mismatches, optional unwrapping)
- ⚠️ Test cases for SDL2 not yet implemented

## Working Guidelines

### 1. Code Organization

- **File Size Limit**: Keep files under 500 lines (user preference)
- **Modular Structure**: Split large files into logical modules
- **Naming**: Follow existing conventions (see `zaiko/src/`)

### 2. Documentation Updates

**CRITICAL**: Before ANY git commit, follow `documentation/core/critical_memory.typ`:

1. ✅ Review discoveries and learnings
2. ✅ Update general documentation (`specifications/`) with emulator-independent findings
3. ✅ Update language-specific documentation (`implementations/`) with Zig-specific details
4. ✅ Make comprehensive commit with documentation updates

**Example Commit Format**:

```
Update documentation: [Brief description]

CRITICAL: Document [general findings] and [language-specific findings]

General Documentation Updates:
- Updated [file]: [general finding 1]
- Updated [file]: [general finding 2]

Language-Specific Documentation Updates:
- Updated [file]: [language-specific detail 1]
- Updated [file]: [language-specific detail 2]
```

### 3. C Implementation as Reference

**CRITICAL RULE**: The C implementation (`maiko/src/`) is the ultimate reference.

- If comments/documentation conflict with C code, **trust the C code**
- If documentation is incorrect, **update the documentation**
- When debugging, use the C emulator with debug statements to understand behavior

### 4. Testing Approach

- **Fast Smoke Tests**: Use quick test runner by default (user preference)
- **Comprehensive Tests**: Keep separate for thorough validation
- **Test Location**: Tests should be next to scripts in `tests/` folders

### 5. Build System

- **Zig**: Uses Zig build system (`build.zig`)
- **C**: Uses traditional Makefile/CMake
- **Nix**: Project uses Nix for managing dependencies (see `shell.nix`)

### 6. Code Style

- **Indentation**: 2 spaces (R conventions)
- **Python Version**: 3.12
- **Zig Version**: 0.15.2+

## Common Tasks

### Adding New Opcodes

1. Add opcode to `zaiko/src/vm/dispatch/opcode.zig`
2. Add handler to appropriate module in `zaiko/src/vm/opcodes/`
3. Add to dispatch switch in `zaiko/src/vm/dispatch/execution.zig`
4. Update documentation in `documentation/specifications/instruction-set/opcodes.typ`
5. Add test cases if applicable

### Debugging Issues

1. **Check C Implementation**: Always compare with `maiko/src/` first
2. **Add Debug Statements**: Use `std.debug.print` in Zig, `printf` in C
3. **Use C Emulator**: Run C emulator with debug statements to understand behavior
4. **Document Findings**: Update `documentation` with discoveries

### File Splitting

When a file exceeds 500 lines:

1. Identify logical groupings
2. Create new modules in appropriate subdirectory
3. Update re-export file to import and re-export from new modules
4. Ensure all imports are updated
5. Test compilation

## Key Concepts

### Memory Management

- **LispPTR**: 32-bit virtual address (DLword offset from Lisp_world, multiply by 2 for bytes)
- **DLword**: 16-bit unsigned integer
- **Virtual Memory**: Complete Lisp address space, allocated based on `process_size`
- **FPtoVP Table**: Maps file page numbers to virtual page numbers
- **Stack**: Part of virtual memory, grows DOWN

### VM Execution

- **PC (Program Counter)**: Byte offset in virtual memory
- **Stack Frame**: 10 DLwords (20 bytes), contains return address, saved registers, local variables
- **Function Header**: Contains `startpc` (byte offset), `na` (arg count), `pv` (param var count)
- **Dispatch Loop**: Fetches, decodes, and executes bytecode instructions

### SDL2 Integration

- **Display Region**: DLword array where each bit represents a pixel
- **BitBLT**: Converts bit array to pixels (foreground/background colors)
- **Event Translation**: SDL keycodes → Lisp keycodes via keymap (74 entries)
- **Coordinate Translation**: Window coordinates → Display coordinates (divide by pixel_scale)

## Resources

### Documentation

- **Main README**: `documentation/README.md`
- **Index**: `documentation/reference/index.typ`
- **Architecture**: `documentation/components/vm-core.typ`
- **Glossary**: `documentation/reference/glossary.typ`

### Specifications

- **Zig Completion Spec**: `specs/005-zig-completion/spec.md`
- **Implementation Plan**: `specs/005-zig-completion/plan.md`
- **Tasks**: `specs/005-zig-completion/tasks.md`
- **Current State**: `specs/005-zig-completion/current-state-analysis.md`

### Implementation Notes

- **Zig Implementation**: `documentation/implementations/zig-implementation.typ`
- **C Implementation**: Reference in `maiko/src/`

## Quick Reference

### Important Paths

- Zig source: `zaiko/src/`
- Zig tests: `zaiko/tests/`
- C reference: `maiko/src/`
- Documentation: `documentation/`
- Specs: `specs/005-zig-completion/`

### Important Constants

- `IFPAGE_KEYVAL`: `0x15e3` (CRITICAL: Must match C implementation)
- `IFPAGE_ADDRESS`: `512` bytes from start of sysout file
- `BYTESPER_PAGE`: `512` bytes (256 DLwords)
- `STK_OFFSET`: `0x00010000` (DLword offset for stack area)
- `FRAMESIZE`: `10` DLwords (20 bytes)

### Build Commands

```bash
# Build Zig emulator
cd zaiko
zig build

# Run Zig emulator
zig build run -- medley/internal/loadups/starter.sysout

# Run tests
zig build test

# Build C emulator (if needed)
cd ../../..
medley/scripts/build/build-c-emulator.sh
```

## Best Practices

1. **Always check C implementation first** when implementing new features
2. **Update documentation** before committing (follow `documentation/core/critical_memory.typ`)
3. **Keep files under 500 lines** for better maintainability
4. **Test incrementally** - don't wait until everything is done
5. **Document findings** in `documentation` for future reference
6. **Use descriptive commit messages** with task IDs when applicable

## Contact & Support

- **Project Documentation**: See `documentation/README.md`
- **Implementation Status**: See `specs/005-zig-completion/current-state-analysis.md`
- **Task Tracking**: See `specs/005-zig-completion/tasks.md`

---

**Last Updated**: 2025-01-27
**Status**: Zig emulator 87.0% complete, SDL2 integration implemented, minor fixes pending
