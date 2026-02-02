# Research: Medley Documentation Project

**Feature**: Medley Documentation Project
**Created**: 2025-01-27
**Status**: Complete

## Research Questions

### 1. Documentation Structure and Organization

**Question**: How should Medley documentation be organized to match Maiko documentation while being comprehensive for implementors?

**Research Findings**:
- Existing Maiko documentation uses: README.md, INDEX.md, architecture.md, components/, api/, glossary.md
- Maiko documentation is organized by functional area (vm-core, memory-management, display, io)
- Medley documentation should follow similar structure but organized by Medley-specific concerns
- Interface documentation should be prominent given the critical Medley-Maiko relationship

**Decision**: Organize Medley documentation in `documentation/medley/` with:
- Top-level overview files (README.md, INDEX.md, architecture.md)
- `components/` directory for Medley components (scripts, sysout, vmem, etc.)
- `interface/` directory specifically for Medley-Maiko interface documentation
- `platform/` directory for platform-specific behaviors
- Mirror Maiko structure for consistency

**Rationale**: Consistency with Maiko documentation enables easier navigation. Separate interface directory emphasizes the critical relationship. Component organization matches how developers think about the system.

**Alternatives Considered**:
- Single flat structure: Rejected - too difficult to navigate for large documentation set
- Organize by user type (implementor vs maintainer): Rejected - too much overlap, would create duplication

---

### 2. Medley-Maiko Interface Documentation Scope

**Question**: What aspects of the Medley-Maiko interface need documentation?

**Research Findings**:
- Command-line arguments: Medley scripts transform user arguments and pass specific flags to Maiko
- Environment variables: MEDLEYDIR, LOGINDIR, LDESOURCESYSOUT, LDEINIT, LDEREMCM, LDEDESTSYSOUT
- File formats: Sysout files, vmem files, configuration files
- Runtime protocols: How Medley scripts invoke Maiko, error handling, exit codes
- Platform-specific behaviors: Windows/Cygwin, WSL, macOS, Linux differences

**Decision**: Document all interface mechanisms comprehensively:
- Complete command-line argument mapping (Medley flags → Maiko flags)
- All environment variables with usage context
- File format specifications (sysout, vmem, config, greet)
- Script invocation patterns and error handling
- Platform-specific interface variations

**Rationale**: Implementors need complete interface specification to build compatible systems. Missing details would prevent successful implementation.

**Alternatives Considered**:
- High-level overview only: Rejected - insufficient for implementors
- Reference Maiko docs only: Rejected - Medley-specific transformations and context needed

---

### 3. Source Code Analysis Approach

**Question**: How should we extract implementation details from Medley source code?

**Research Findings**:
- Medley scripts are in `medley/scripts/medley/` directory
- Main scripts: `medley_run.sh`, `medley.command`, `medley.ps1` (Windows)
- Scripts contain argument parsing, environment setup, and Maiko invocation logic
- Lisp source code in `medley/sources/`, `medley/library/`, etc.
- Configuration and greet files in `medley/greetfiles/`

**Decision**:
- Analyze script source code to document argument parsing and transformation
- Document script structure and flow
- Reference source code locations in documentation
- Extract file format information from scripts and Lisp code
- Document directory structure and purpose

**Rationale**: Source code is the authoritative source for implementation details. Documentation should reference source locations for maintainability.

**Alternatives Considered**:
- Infer from man page only: Rejected - man page doesn't cover implementation details
- Reverse engineer from behavior: Rejected - source code is available and authoritative

---

### 4. Documentation Format and Tools

**Question**: What format and tools should be used for documentation?

**Research Findings**:
- Existing Maiko documentation uses Markdown with Mermaid diagrams
- Markdown is version-control friendly and widely supported
- Mermaid diagrams useful for architecture and flow documentation
- Documentation should be readable in plain text and rendered formats

**Decision**:
- Use Markdown format consistent with Maiko documentation
- Include Mermaid diagrams for architecture, flows, and relationships
- Use code blocks for file format examples and command-line examples
- Include cross-references to Maiko documentation where relevant

**Rationale**: Consistency with existing documentation. Markdown is standard for technical documentation. Mermaid diagrams enhance understanding without external tools.

**Alternatives Considered**:
- HTML/PDF only: Rejected - not version-control friendly, harder to maintain
- Separate diagram files: Rejected - Mermaid embedded in Markdown is more maintainable

---

### 5. Platform-Specific Documentation Strategy

**Question**: How should platform-specific behaviors be documented?

**Research Findings**:
- Medley supports: Linux, macOS, Windows/Cygwin, WSL
- Display backends: X11, SDL, VNC (WSL)
- Script variations: `medley_run.sh` (Linux/macOS), `medley.command` (macOS), `medley.ps1` (Windows)
- Platform-specific file path handling and environment variable differences

**Decision**:
- Create platform-specific documentation files in `platform/` directory
- Document script differences per platform
- Document platform-specific command-line option behaviors
- Include platform detection and selection logic
- Cross-reference platform docs from main documentation

**Rationale**: Platform differences are significant enough to warrant separate documentation. Centralized platform docs make it easier to find platform-specific information.

**Alternatives Considered**:
- Inline platform notes in main docs: Rejected - would clutter main documentation
- Single platform doc with sections: Rejected - separate files easier to navigate

---

## Key Research Decisions Summary

1. **Documentation Structure**: Mirror Maiko structure with Medley-specific organization, separate interface directory
2. **Interface Documentation**: Comprehensive coverage of all Medley-Maiko communication mechanisms
3. **Source Analysis**: Direct analysis of Medley source code for implementation details
4. **Format**: Markdown with Mermaid diagrams, consistent with Maiko docs
5. **Platform Documentation**: Separate platform-specific files for clarity

## Dependencies Identified

- Access to Medley source code (available in repository)
- Existing Maiko documentation for interface context (available in `documentation`)
- medley.1 man page for command-line reference (available in `medley/docs/man-page/`)
- Understanding of Maiko command-line interface (documented in Maiko docs)

## Open Questions Resolved

All research questions have been resolved. No NEEDS CLARIFICATION markers remain in the plan.
