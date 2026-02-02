# Quickstart Guide: Medley Documentation Project

**Feature**: Medley Documentation Project
**Created**: 2025-01-27

## Overview

This guide helps developers understand how to navigate and use the Medley documentation to implement or maintain Medley systems.

## Documentation Location

All Medley documentation is located in `documentation/medley/` directory, organized as follows:

```
documentation/medley/
├── README.md              # Start here - overview and navigation
├── INDEX.md               # Quick reference index
├── architecture.md        # System architecture
├── components/            # Component documentation
├── interface/            # Medley-Maiko interface
├── platform/             # Platform-specific docs
└── glossary.md           # Terminology
```

## Getting Started

### For New Implementors

1. **Start with Architecture**: Read `architecture.md` to understand overall system design
2. **Understand Interface**: Read `interface/README.md` to understand Medley-Maiko communication
3. **Study Components**: Explore `components/` to understand individual parts
4. **Reference Maiko Docs**: Use Maiko documentation in `documentation/` for emulator details

### For Maintainers

1. **Find Component**: Use `INDEX.md` to locate documentation for specific component
2. **Check Interface**: Review `interface/` docs when modifying Medley-Maiko interaction
3. **Platform Considerations**: Check `platform/` docs for platform-specific behaviors
4. **Source References**: Follow source code references in documentation for implementation details

## Key Documentation Files

### Architecture and Overview

- **README.md**: Overview, navigation, and getting started
- **INDEX.md**: Quick reference to all documentation
- **architecture.md**: System architecture and component relationships

### Component Documentation

- **components/scripts.md**: Medley script system and argument parsing
- **components/sysout.md**: Sysout file format and loading
- **components/vmem.md**: Virtual memory file format and usage
- **components/configuration.md**: Configuration file format and precedence
- **components/greetfiles.md**: Greet file system and execution
- **components/loadup.md**: Loadup workflow and sysout creation
- **components/directory-structure.md**: Medley directory organization

### Interface Documentation

- **interface/README.md**: Interface overview and navigation
- **interface/command-line.md**: Command-line arguments passed to Maiko
- **interface/environment.md**: Environment variables used for communication
- **interface/file-formats.md**: File format specifications (sysout, vmem, config)
- **interface/protocols.md**: Runtime communication protocols

### Platform Documentation

- **platform/README.md**: Platform documentation overview
- **platform/linux.md**: Linux-specific behaviors
- **platform/macos.md**: macOS-specific behaviors
- **platform/windows.md**: Windows/Cygwin-specific behaviors
- **platform/wsl.md**: WSL-specific behaviors

## Common Tasks

### Understanding How Medley Starts Maiko

1. Read `components/scripts.md` for script structure
2. Read `interface/command-line.md` for argument mapping
3. Read `interface/environment.md` for environment setup
4. Reference Maiko documentation for Maiko-side behavior

### Understanding Sysout Files

1. Read `components/sysout.md` for sysout format and purpose
2. Read `interface/file-formats.md` for detailed format specification
3. Read `components/loadup.md` for how sysouts are created
4. Reference Maiko documentation for sysout loading in emulator

### Understanding Virtual Memory Files

1. Read `components/vmem.md` for vmem purpose and usage
2. Read `interface/file-formats.md` for vmem format specification
3. Read `components/scripts.md` for vmem file selection logic

### Understanding Platform Differences

1. Read `platform/README.md` for platform overview
2. Read specific platform file (linux.md, macos.md, windows.md, wsl.md)
3. Check `interface/` docs for platform-specific interface variations

## Navigation Tips

### Using INDEX.md

INDEX.md provides quick reference to all documentation organized by topic:
- Component documentation
- Interface documentation
- Platform documentation
- Cross-references to Maiko documentation

### Following Cross-References

- Internal links use relative paths within Medley documentation
- External links to Maiko docs use paths to `documentation/` Maiko documentation
- Source code references link to repository files

### Finding Implementation Details

1. Component documentation includes source code references
2. Follow source references to see actual implementation
3. Interface documentation references Maiko source for complete picture

## Integration with Maiko Documentation

Medley documentation frequently references Maiko documentation for:
- Maiko command-line interface details
- Maiko file format handling
- Maiko environment variable usage
- Maiko runtime behavior

When implementing Medley functionality, consult both Medley and Maiko documentation for complete understanding.

## Validation

To validate documentation completeness:

1. Check that all medley.1 man page options are documented in `interface/command-line.md`
2. Verify all environment variables are documented in `interface/environment.md`
3. Confirm all file formats are specified in `interface/file-formats.md`
4. Ensure all components are documented in `components/`
5. Validate all cross-references are valid links

## Next Steps

After reading this quickstart:

1. **For Implementors**: Start with `architecture.md`, then `interface/README.md`
2. **For Maintainers**: Use `INDEX.md` to find specific component documentation
3. **For All**: Bookmark `INDEX.md` for quick reference

## Getting Help

- Check `glossary.md` for terminology definitions
- Use `INDEX.md` for topic-based navigation
- Reference source code locations provided in documentation
- Consult Maiko documentation for emulator-side details
