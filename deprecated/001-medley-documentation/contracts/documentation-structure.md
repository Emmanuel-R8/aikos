# Documentation Structure Contract

**Purpose**: Define the structure and format requirements for Medley documentation

## Documentation File Structure

### Required Sections

Every documentation file MUST include:

1. **Title** (H1): Clear, descriptive title
2. **Navigation** (optional): Links to related documentation
3. **Overview** (H2): Brief description of what this document covers
4. **Content Sections** (H2/H3): Main documentation content
5. **Cross-References** (H2): Links to related documentation
6. **Source References** (H2, optional): Links to relevant source code

### File Naming Convention

- Use kebab-case for file names: `component-name.md`
- Use descriptive names that indicate content
- Index files: `INDEX.md`, `README.md`
- Component files: `component-name.md`
- Interface files: `interface-name.md`
- Platform files: `platform-name.md`

### Markdown Formatting Standards

- Use ATX-style headers (#, ##, ###)
- Use fenced code blocks with language specification
- Use tables for structured data
- Use lists for enumerations
- Use emphasis (bold, italic) for key terms
- Use inline code for file names, commands, variables

### Mermaid Diagram Standards

- Include Mermaid diagrams for:
  - Architecture overviews
  - Component relationships
  - Data flow diagrams
  - Sequence diagrams for interactions
- Diagrams MUST be valid Mermaid syntax
- Diagrams should be readable in both rendered and source form

## Documentation Content Requirements

### Component Documentation

MUST include:
- **Purpose**: What the component does
- **Location**: Where it exists in source tree
- **Structure**: How it's organized
- **Usage**: How it's used
- **Dependencies**: What it depends on
- **Interface**: How it interfaces with other components or Maiko

### Interface Documentation

MUST include:
- **Interface Type**: Command-line, environment variable, file format, protocol
- **Specification**: Complete format/syntax specification
- **Usage Context**: When and how it's used
- **Direction**: Medley → Maiko, Maiko → Medley, or bidirectional
- **Platform Variations**: Platform-specific differences
- **Examples**: Concrete examples of usage

### Platform Documentation

MUST include:
- **Platform Identification**: Which platform(s) this applies to
- **Differences**: How behavior differs from standard
- **Script Variations**: Platform-specific script differences
- **File Path Handling**: Platform-specific path conventions
- **Environment Variables**: Platform-specific environment variable usage

## Cross-Reference Requirements

### Internal References

- Use relative paths: `[Link Text](../other-doc.md)`
- Use section anchors: `[Link Text](doc.md#section-name)`
- All internal links MUST be valid

### External References (Maiko Documentation)

- Use relative paths to Maiko docs: `[Link Text](../../maiko-doc.md)`
- Clearly indicate when linking to Maiko documentation
- Maintain consistency in link text

### Source Code References

- Format: `[description](medley/path/to/file.ext)`
- Include line numbers if specific: `[description](medley/path/to/file.ext#L123-L145)`
- All source references MUST point to valid files

## Documentation Completeness Requirements

### Coverage Requirements

- All command-line options from medley.1 man page MUST be documented
- All environment variables used MUST be documented
- All file formats (sysout, vmem, config, greet) MUST be documented
- All major components MUST be documented
- All platform-specific behaviors MUST be documented

### Quality Requirements

- Documentation MUST be accurate (verified against source)
- Documentation MUST be complete (no missing critical information)
- Documentation MUST be clear (understandable by target audience)
- Documentation MUST be consistent (format, style, terminology)

## Validation Rules

1. Every documentation file MUST be reachable from INDEX.md within 3 navigation steps
2. Every interface mechanism MUST have complete specification
3. Every component MUST have purpose and location documented
4. All source code references MUST point to valid files
5. All cross-references MUST be valid links
6. All Mermaid diagrams MUST be valid syntax
