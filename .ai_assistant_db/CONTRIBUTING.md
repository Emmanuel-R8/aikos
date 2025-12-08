# Contributing to Maiko Documentation

**Navigation**: [README](README.md) | [Index](INDEX.md)

## Documentation Improvement Guidelines

### CRITICAL: Emulator-Independent Documentation

**All improvements to `.ai_assistant_db` documentation MUST be emulator-independent** so that any implementor in any language benefits from the knowledge.

#### General Principles

1. **Language-Agnostic Content**: Document concepts, algorithms, data structures, and behaviors that apply to all implementations.
2. **Language-Specific Details**: Only include language-specific comments when they:
   - Highlight difficulties specific to that language
   - Document workarounds or solutions unique to that language
   - Explain why certain approaches were chosen for that language
   - Are irrelevant to other implementations

#### Documentation Structure

- **`.ai_assistant_db/rewrite-spec/`**: Contains emulator-independent specifications
  - All opcodes, data structures, algorithms, and behaviors documented here
  - No language-specific implementation details
  - Focus on "what" and "why", not "how" in a specific language

- **`.ai_assistant_db/implementations/`**: Contains language-specific implementation notes
  - Document challenges, workarounds, and solutions specific to each language
  - Reference general documentation in `rewrite-spec/` for concepts
  - Focus on "how" in that specific language

#### Example: Opcode Documentation

**✅ CORRECT - Emulator-Independent (in `rewrite-spec/instruction-set/opcodes.md`)**:
```markdown
## Common Misconceptions: Non-existent Opcodes

**Myth**: Generic `JUMP`, `FJUMP`, `TJUMP` opcodes exist.
**Reality**: Only specific variants exist: `JUMP0`-`JUMP15`, `JUMPX`, etc.
**Why**: Optimized variants encode small offsets directly in the opcode.
```

**✅ CORRECT - Language-Specific (in `implementations/zig-opcode-findings.md`)**:
```markdown
**Zig-Specific Issue**: Zig enum system requires unique values, making conflicts immediately apparent.
**Zig Resolution**: Commented out conflicting enum definitions.
**Zig Implementation Detail**: Opcodes commented out in `dispatch.zig:73-75`.
```

**❌ INCORRECT - Mixing General and Language-Specific**:
```markdown
The Zig implementation had `JUMP = 0x20` which conflicted with `BIN = 0x20`.
```
This should be split: general finding in `rewrite-spec/`, Zig-specific detail in `implementations/`.

### Documentation Update Process

**CRITICAL**: Before making any git commit, you MUST:

1. **Review Discoveries**: Consider what you have discovered or learned
2. **Identify General Findings**: Extract emulator-independent insights
3. **Update General Documentation**: Improve `rewrite-spec/` with general findings
4. **Update Language-Specific Documentation**: Document language-specific details in `implementations/`
5. **Verify Separation**: Ensure general and language-specific content are properly separated
6. **Make Comprehensive Commit**: Include all documentation improvements in the commit message

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

### Verification Checklist

Before committing documentation changes:

- [ ] General findings are in `rewrite-spec/` (emulator-independent)
- [ ] Language-specific details are in `implementations/` (language-specific)
- [ ] No language-specific details in general documentation
- [ ] General documentation references are updated
- [ ] Language-specific documentation references general documentation
- [ ] Commit message follows the format above
- [ ] All critical insights are documented

## Related Documentation

- [Rewrite Specifications](rewrite-spec/README.md) - Emulator-independent specifications
- [Implementation Notes](implementations/README.md) - Language-specific implementation details
- [Documentation Review](rewrite-spec/DOCUMENTATION_REVIEW.md) - Documentation completeness review
