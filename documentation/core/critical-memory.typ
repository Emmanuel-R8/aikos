#import "../prelude.typ": codeblock

= CRITICAL: Documentation Improvement Memory

*Date*: 2025-12-12 15:59
*Status*: ACTIVE - MUST FOLLOW

== CRITICAL: Date/Time Format

*ALWAYS use the Unix `date` command to get the current date and time:*
- *Format*: `YYYY-mm-dd HH:MM` (e.g., `2025-12-12 15:59`)
- *Command*: `date +"%Y-%m-%d %H:%M"`
- *Requirement*: All timestamps in documentation MUST include hours and minutes (HH:MM)
- *Never use*: Bogus dates or dates without time component

== CRITICAL: Debugging Technique for Byte Swaps, Alignment, and Addresses

*ALWAYS apply this technique when debugging byte swaps, alignment, or address calculations:*

For EACH value, consider:
1. The value itself (decimal and hexadecimal)
2. The value divided by 2 (decimal and hexadecimal)
3. The value multiplied by 2 (decimal and hexadecimal)

*See*: `../CRITICAL_DEBUGGING_TECHNIQUE.typ` for full details and examples.

This technique helps identify:
- Byte vs DLword confusion (values off by factors of 2)
- Alignment issues
- Address calculation errors (LispPTR as DLword offset vs byte offset)
- Endianness problems

== CRITICAL: Code Documentation Standards

*ALL complex or non-obvious code sections MUST include comprehensive documentation* using this standardized format:

```zig
// CONFIDENCE LEVEL: [HIGH/MEDIUM/LOW] ([percentage]%)
// - [Brief explanation of why this confidence level was reached]
//
// HOW THIS CONCLUSION WAS REACHED:
// [Detailed explanation including source references, debugging steps,
//  and analysis that led to this implementation]
//
// HOW TO TEST:
// - [Specific test cases or verification methods]
// - [How to ensure the logic remains correct]
//
// HOW TO ENSURE NOT REVERTED:
// - [Code review checklist items]
// - [Unit tests or integration tests]
// - [Documentation references]
```

=== When to Use This Format

Apply this documentation format to:
- Complex algorithms or data transformations
- Non-obvious optimizations or workarounds
- Critical correctness checks
- Byte-swapping and endianness handling
- Address calculations and memory management
- Any code that took significant debugging effort

=== Confidence Level Guidelines

- *VERY HIGH (99%)*: Exhaustive analysis, multiple verification methods, matches C implementation exactly
- *HIGH (90%)*: Thorough analysis, verified against C behavior, comprehensive testing
- *MEDIUM (75%)*: Reasonable analysis, basic verification, may have edge cases
- *LOW (50%)*: Best effort implementation, needs further verification

=== Examples

See `zaiko/src/utils/endianness.zig` and `zaiko/src/data/sysout.zig` for examples of properly documented complex logic.

== CRITICAL RULE: Emulator-Independent Documentation

*ALL improvements to `documentation` documentation MUST be emulator-independent* inpriority so that any implementor in any language benefits from the knowledge.

=== General Principles

1. *Language-Agnostic Content*: Document concepts, algorithms, data structures, and behaviors that apply to all implementations.
2. *Language-Specific Details*: Only include language-specific comments when they:
   - Highlight difficulties specific to that language
   - Document workarounds or solutions unique to that language
   - Explain why certain approaches were chosen for that language
   - Are irrelevant to other implementations

=== Documentation Structure

- *`documentation/specifications/`*: Contains emulator-independent specifications
  - All opcodes, data structures, algorithms, and behaviors documented here
  - No language-specific implementation details
  - Focus on "what" and "why", not "how" in a specific language

- *`documentation/implementations/`*: Contains language-specific implementation notes
  - Document challenges, workarounds, and solutions specific to each language
  - Reference general documentation in `specifications/` for concepts
  - Focus on "how" in that specific language

=== Pre-Commit Checklist

*CRITICAL*: Before making ANY git commit, you MUST:

1. ✅ *Review Discoveries*: Consider what you have discovered or learned
2. ✅ *Identify General Findings*: Extract emulator-independent insights
3. ✅ *Update General Documentation*: Improve `specifications/` with general findings
4. ✅ *Update Language-Specific Documentation*: Document language-specific details in `implementations/`
5. ✅ *Verify Separation*: Ensure general and language-specific content are properly separated
6. ✅ *Make Comprehensive Commit*: Include all documentation improvements in the commit message

=== Commit Message Format

When updating documentation, use this format:

#codeblock(lang: "text", [
Update documentation: [Brief description]

CRITICAL: Document [general findings] and [language-specific findings]

General Documentation Updates:
- Updated [file]: [general finding 1]
- Updated [file]: [general finding 2]

Language-Specific Documentation Updates:
- Updated [file]: [language-specific detail 1]
- Updated [file]: [language-specific detail 2]

Key Findings Documented:
1. [General finding 1] - documented in specifications/
2. [General finding 2] - documented in specifications/
3. [Language-specific finding] - documented in implementations/

All critical insights are now documented for future reference.
])

== Related Documentation

- Contributing Guidelines - Full documentation improvement guidelines
- Rewrite Specifications - Emulator-independent specifications
- Implementation Notes - Language-specific implementation details

== REMINDER: NEVER FORGET THIS CHECKLIST

*CRITICAL*: Before making ANY git commit, ALWAYS:

1. ✅ Review discoveries and learnings
2. ✅ Update general documentation (specifications/)
3. ✅ Update language-specific documentation (implementations/)
4. ✅ Make comprehensive commit with documentation updates

*This checklist MUST be followed for EVERY commit that includes code changes.*
