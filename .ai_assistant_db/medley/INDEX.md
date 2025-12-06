# Medley Documentation Index

**Navigation**: [README](README.md) | [Architecture](architecture.md) | [Components](components/) | [Interface](interface/) | [Platform](platform/) | [Glossary](glossary.md)

Quick reference guide to all Medley documentation files.

**Note**: All documentation files include Mermaid diagrams and extensive cross-linking for easy navigation.

## Documentation Structure

```
.ai_assistant_db/medley/
├── README.md                    # Overview and navigation
├── INDEX.md                      # This file - quick reference
├── architecture.md               # System architecture overview
├── glossary.md                   # Terminology and concepts
├── components/                   # Component documentation
│   ├── scripts.md              # Medley script system
│   ├── sysout.md               # Sysout file format
│   ├── vmem.md                 # Virtual memory files
│   ├── configuration.md        # Configuration files
│   ├── greetfiles.md           # Greet file system
│   ├── loadup.md               # Loadup workflow
│   └── directory-structure.md  # Directory organization
├── interface/                   # Medley-Maiko interface
│   ├── README.md               # Interface overview
│   ├── command-line.md         # Command-line arguments
│   ├── environment.md          # Environment variables
│   ├── file-formats.md         # File format specifications
│   └── protocols.md             # Runtime protocols
└── platform/                    # Platform-specific documentation
    ├── README.md               # Platform overview
    ├── linux.md                # Linux-specific
    ├── macos.md                # macOS-specific
    ├── windows.md              # Windows/Cygwin-specific
    └── wsl.md                  # WSL-specific
```

## Quick Reference

### Getting Started

1. **New to Medley?** → Start with [README.md](README.md)
2. **Understanding Architecture?** → Read [architecture.md](architecture.md)
3. **Understanding Medley-Maiko Interface?** → Read [interface/README.md](interface/README.md)
4. **Looking for Terms?** → Check [glossary.md](glossary.md)

### By Topic

#### Medley Components

- **Scripts**: [components/scripts.md](components/scripts.md) - Medley script system and argument parsing
- **Sysout Files**: [components/sysout.md](components/sysout.md) - Sysout file format and loading
- **Virtual Memory**: [components/vmem.md](components/vmem.md) - Vmem files and session persistence
- **Configuration**: [components/configuration.md](components/configuration.md) - Config files and precedence
- **Greet Files**: [components/greetfiles.md](components/greetfiles.md) - Greet file system
- **Loadup**: [components/loadup.md](components/loadup.md) - Loadup workflow and sysout creation
- **Directory Structure**: [components/directory-structure.md](components/directory-structure.md) - Medley directory organization

#### Medley-Maiko Interface

- **Command-Line Arguments**: [interface/command-line.md](interface/command-line.md) - Complete argument mapping
- **Environment Variables**: [interface/environment.md](interface/environment.md) - Environment variable usage
- **File Formats**: [interface/file-formats.md](interface/file-formats.md) - File format specifications
- **Protocols**: [interface/protocols.md](interface/protocols.md) - Runtime communication protocols

#### Platform-Specific

- **Linux**: [platform/linux.md](platform/linux.md) - Linux-specific behaviors
- **macOS**: [platform/macos.md](platform/macos.md) - macOS-specific behaviors
- **Windows**: [platform/windows.md](platform/windows.md) - Windows/Cygwin-specific behaviors
- **WSL**: [platform/wsl.md](platform/wsl.md) - WSL-specific behaviors

## Common Tasks

### Understanding How Medley Starts

1. **Script System**: [components/scripts.md](components/scripts.md) - How scripts parse arguments
2. **Maiko Invocation**: [interface/command-line.md](interface/command-line.md) - How scripts call Maiko
3. **Environment Setup**: [interface/environment.md](interface/environment.md) - Environment variables
4. **Startup Sequence**: [interface/protocols.md](interface/protocols.md) - Runtime protocols

### Understanding Sysout Files

1. **Sysout Format**: [components/sysout.md](components/sysout.md) - Sysout file structure
2. **File Format Spec**: [interface/file-formats.md](interface/file-formats.md) - Detailed format specification
3. **Loading Process**: [components/sysout.md](components/sysout.md#loading-process) - How sysouts are loaded
4. **Creating Sysouts**: [components/loadup.md](components/loadup.md) - Loadup workflow

### Understanding Virtual Memory Files

1. **Vmem Purpose**: [components/vmem.md](components/vmem.md) - Vmem file purpose and usage
2. **Vmem Format**: [interface/file-formats.md](interface/file-formats.md#vmem-file-format) - Format specification
3. **Session Continuation**: [components/vmem.md](components/vmem.md#session-continuation) - How vmem enables persistence

### Understanding Configuration

1. **Config Files**: [components/configuration.md](components/configuration.md) - Config file format
2. **Precedence Rules**: [components/configuration.md](components/configuration.md#precedence-rules) - How configs are processed
3. **Default Locations**: [components/configuration.md](components/configuration.md#default-locations) - Where configs are found

### Understanding Platform Differences

1. **Platform Overview**: [platform/README.md](platform/README.md) - Platform documentation overview
2. **Linux**: [platform/linux.md](platform/linux.md) - Linux-specific details
3. **macOS**: [platform/macos.md](platform/macos.md) - macOS-specific details
4. **Windows**: [platform/windows.md](platform/windows.md) - Windows-specific details
5. **WSL**: [platform/wsl.md](platform/wsl.md) - WSL-specific details

## Key Concepts Quick Lookup

### Core Concepts

- **MEDLEYDIR**: Top-level Medley installation directory → [glossary.md](glossary.md#medleydir)
- **LOGINDIR**: User-specific Medley directory → [glossary.md](glossary.md#logindir)
- **Sysout**: Binary Lisp system state file → [glossary.md](glossary.md#sysout)
- **Vmem**: Virtual memory file for session persistence → [glossary.md](glossary.md#vmem)

### File Types

- **Sysout Files**: [components/sysout.md](components/sysout.md)
- **Vmem Files**: [components/vmem.md](components/vmem.md)
- **Config Files**: [components/configuration.md](components/configuration.md)
- **Greet Files**: [components/greetfiles.md](components/greetfiles.md)

### Script Terms

- **Medley Scripts**: [components/scripts.md](components/scripts.md)
- **Argument Parsing**: [components/scripts.md](components/scripts.md#argument-parsing)
- **Maiko Invocation**: [interface/command-line.md](interface/command-line.md)

### Interface Terms

- **Command-Line Arguments**: [interface/command-line.md](interface/command-line.md)
- **Environment Variables**: [interface/environment.md](interface/environment.md)
- **File Formats**: [interface/file-formats.md](interface/file-formats.md)
- **Protocols**: [interface/protocols.md](interface/protocols.md)

## Related Documentation

### Maiko Documentation

- **Maiko Overview**: `../README.md` - Maiko emulator documentation
- **Maiko Architecture**: `../architecture.md` - Maiko system architecture
- **Maiko Components**: `../components/` - Maiko component documentation

### Source Code References

- **Medley Scripts**: `medley/scripts/medley/` - Medley script source
- **Medley Source**: `medley/sources/` - Lisp source code
- **Maiko Source**: `maiko/src/` - Maiko emulator source

## Documentation Statistics

- **Total Files**: ~20 documentation files
- **Components Documented**: 7 major components
- **Interface Mechanisms**: 4 interface types documented
- **Platforms Documented**: 4 platforms (Linux, macOS, Windows, WSL)

## Maintenance

This documentation should be updated when:

- New Medley components are added
- Interface mechanisms change
- Platform-specific behaviors change
- Script behavior changes
- File formats evolve

## Feedback

For questions or improvements to this documentation, refer to the main project repository.
