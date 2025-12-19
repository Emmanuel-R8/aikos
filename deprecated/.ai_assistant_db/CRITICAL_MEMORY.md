---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# CRITICAL: Documentation Improvement Memory

**Date**: 2025-12-12 15:59
**Status**: ACTIVE - MUST FOLLOW

## CRITICAL: Date/Time Format

**ALWAYS use the Unix `date` command to get the current date and time:**
- **Format**: `YYYY-mm-dd HH:MM` (e.g., `2025-12-12 15:59`)
- **Command**: `date +"%Y-%m-%d %H:%M"`
- **Requirement**: All timestamps in documentation MUST include hours and minutes (HH:MM)
- **Never use**: Bogus dates or dates without time component

## CRITICAL: Debugging Technique for Byte Swaps, Alignment, and Addresses

**ALWAYS apply this technique when debugging byte swaps, alignment, or address calculations:**

For EACH value, consider:
1. The value itself (decimal and hexadecimal)
2. The value divided by 2 (decimal and hexadecimal)
3. The value multiplied by 2 (decimal and hexadecimal)

**See**: `.ai_assistant_db/CRITICAL_DEBUGGING_TECHNIQUE.md` for full details and examples.

This technique helps identify:
- Byte vs DLword confusion (values off by factors of 2)
- Alignment issues
- Address calculation errors (LispPTR as DLword offset vs byte offset)
- Endianness problems

## CRITICAL RULE: Emulator-Independent Documentation

**ALL improvements to `.ai_assistant_db` documentation MUST be emulator-independent** inpriority so that any implementor in any language benefits from the knowledge.

### General Principles

1. **Language-Agnostic Content**: Document concepts, algorithms, data structures, and behaviors that apply to all implementations.
2. **Language-Specific Details**: Only include language-specific comments when they:
   - Highlight difficulties specific to that language
   - Document workarounds or solutions unique to that language
   - Explain why certain approaches were chosen for that language
   - Are irrelevant to other implementations

### Documentation Structure

- **`.ai_assistant_db/rewrite-spec/`**: Contains emulator-independent specifications
  - All opcodes, data structures, algorithms, and behaviors documented here
  - No language-specific implementation details
  - Focus on "what" and "why", not "how" in a specific language

- **`.ai_assistant_db/implementations/`**: Contains language-specific implementation notes
  - Document challenges, workarounds, and solutions specific to each language
  - Reference general documentation in `rewrite-spec/` for concepts
  - Focus on "how" in that specific language

### Pre-Commit Checklist

**CRITICAL**: Before making ANY git commit, you MUST:

1. ✅ **Review Discoveries**: Consider what you have discovered or learned
2. ✅ **Identify General Findings**: Extract emulator-independent insights
3. ✅ **Update General Documentation**: Improve `rewrite-spec/` with general findings
4. ✅ **Update Language-Specific Documentation**: Document language-specific details in `implementations/`
5. ✅ **Verify Separation**: Ensure general and language-specific content are properly separated
6. ✅ **Make Comprehensive Commit**: Include all documentation improvements in the commit message

### Commit Message Format

When updating documentation, use this format:

```
Update documentation: [Brief description]

CRITICAL: Document [general findings] and [language-specific findings]

General Documentation Updates:
- Updated [file]: [general finding 1]
- Updated [file]: [general finding 2]

Language-Specific Documentation Updates:
- Updated [file]: [language-specific detail 1]
- Updated [file]: [language-specific detail 2]

Key Findings Documented:
1. [General finding 1] - documented in rewrite-spec/
2. [General finding 2] - documented in rewrite-spec/
3. [Language-specific finding] - documented in implementations/

All critical insights are now documented for future reference.
```

## Related Documentation

- [Contributing Guidelines](CONTRIBUTING.md) - Full documentation improvement guidelines
- [Rewrite Specifications](rewrite-spec/README.md) - Emulator-independent specifications
- [Implementation Notes](implementations/README.md) - Language-specific implementation details

## REMINDER: NEVER FORGET THIS CHECKLIST

**CRITICAL**: Before making ANY git commit, ALWAYS:

1. ✅ Review discoveries and learnings
2. ✅ Update general documentation (rewrite-spec/)
3. ✅ Update language-specific documentation (implementations/)
4. ✅ Make comprehensive commit with documentation updates

**This checklist MUST be followed for EVERY commit that includes code changes.**
