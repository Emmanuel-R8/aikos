= Contributing to Rewrite Specifications

*Navigation*: README | Index

Guidelines for creating and maintaining rewrite specification documentation.

== Documentation Standards

=== Language-Agnostic Requirements

All specifications MUST:

- Use pseudocode, not C code or any specific language
- Describe algorithms in language-independent terms
- Use formal notation where appropriate (diagrams, mathematical expressions)
- Avoid language-specific constructs (pointers, memory management details, etc.)

=== Format Requirements

- *Markdown*: All documentation in Markdown format
- *Mermaid Diagrams*: Use Mermaid for visual specifications (algorithms, data structures, flows)
- *Code Blocks*: Use pseudocode blocks for algorithms
- *Structure*: Follow the established directory structure

=== Specification Completeness

Each specification MUST include:

- *Purpose*: What this specification describes
- *Algorithm/Format*: Complete description in pseudocode or formal notation
- *Inputs/Outputs*: Clear input and output specifications
- *Side Effects*: Any side effects or state changes
- *Error Conditions*: When and how errors occur
- *Examples*: Reference examples where helpful

== Opcode Documentation Format

Each opcode specification MUST include:

#codeblock(lang: "markdown", [
### Opcode Name (Value: 0xXX)

**Operand Format**: [description]
**Stack Effects**: [how stack changes]
**Execution Semantics**:
  [pseudocode description]
**Side Effects**: [any side effects]
**Error Conditions**: [when it fails]
])

== Data Structure Documentation Format

Each data structure specification MUST include:

#codeblock(lang: "markdown", [
### Structure Name

**Layout**:
  - Field1: Type, offset, description
  - Field2: Type, offset, description
**Size**: X bytes
**Alignment**: X-byte alignment
**Encoding**: [special encoding rules]
**Invariants**: [constraints that must hold]
])

== Algorithm Documentation Format

Each algorithm specification MUST include:

#codeblock(lang: "markdown", [
### Algorithm Name

**Purpose**: [what it does]
**Inputs**: [input parameters]
**Outputs**: [return values and side effects]
**Algorithm**:
  [step-by-step pseudocode]
**Complexity**: [time/space if relevant]
])

== Cross-References

- Link to related specifications using relative paths
- Reference glossary terms where appropriate
- Maintain bidirectional links between related documents

== Validation

Before marking a specification complete:

- Verify all required sections are present
- Check pseudocode for clarity and completeness
- Validate cross-references work correctly
- Ensure language-agnostic compliance

== Platform Abstraction

- Clearly mark required behaviors (must match exactly)
- Clearly mark implementation choices (may differ)
- Distinguish platform-independent from platform-specific

== Review Process

1. Create specification following format guidelines
2. Self-review for completeness and clarity
3. Validate against source code (if available)
4. Mark as complete when ready

== Questions?

Refer to existing specifications as examples, or consult the Quickstart Guide.
