---
name: debugging
description: Applies critical value analysis technique for debugging byte swaps, alignment issues, and address calculations in the Interlisp emulator. Use when troubleshooting discrepancies between C and Zig implementations, or analyzing PC, stack depth, and frame field values.
---

# Critical Debugging Technique: Value Analysis

*Date*: 2026-01-12
*Status*: CRITICAL - Always use this technique when debugging byte swaps, alignment, and addresses

## Technique

When debugging issues related to:
- Byte swaps (endianness)
- Alignment (byte vs DLword offsets)
- Address calculations (LispPTR as DLword offset vs byte offset)

*For EACH value, ALWAYS consider:*
1. The value itself (decimal and hexadecimal)
2. The value divided by 2 (decimal and hexadecimal)
3. The value multiplied by 2 (decimal and hexadecimal)

## Question the Premise

*Date*: 2026-02-20
*Status*: CRITICAL - Always verify the question itself before debugging

When debugging "why does X happen?", **first verify X actually happens**. Many debugging sessions
are blocked by incorrect assumptions about what the data shows.

### Common Premise Errors

1. **Misinterpreted trace data** - Values in traces may represent different things than assumed
2. **Wrong timing assumptions** - Data may be recorded before/after operations differently than expected
3. **Incorrect causation** - Assuming A caused B when they're unrelated

### The 0x0E Mystery Example

**The blocking question**: "Who writes 0x0E to atom 522?"

**The answer**: 0x0E was never written to atom 522. The introspection data was misinterpreted.

**Why it took so long**:
- Assumed the question was correct instead of questioning the premise
- Missing timing documentation for introspection data
- Looked for memory writes instead of understanding data flow

**Lesson**: Before searching for causes, verify what the data actually represents.

## Why This Works

- *Byte vs DLword*: If a value is in bytes but should be in DLwords, dividing by 2 reveals the DLword value
- *DLword vs byte*: If a value is in DLwords but should be in bytes, multiplying by 2 reveals the byte value
- *Alignment issues*: Values that are off by factors of 2 often indicate byte/DLword confusion
- *Endianness*: Byte-swapped values often show patterns when divided/multiplied by 2

## Example

If you see:
- Value: `23824` (0x5d10) decimal
- Value / 2: `11912` (0x2e88) decimal
- Value * 2: `47648` (0xba20) decimal

And C shows: `5956` (0x1744)

Then:
- `23824 / 4 = 5956` ← This reveals the correct calculation!

## Application

*ALWAYS* apply this technique when:
- Comparing values between C and Zig emulators
- Debugging PC calculations
- Debugging stack depth calculations
- Debugging frame field reads
- Any address/offset calculations

## Usage Instructions

When encountering a debugging issue involving values in the Interlisp emulator:

1. Identify the suspicious value(s) from logs, debug output, or comparisons.
2. For each value, calculate and examine:
   - Original value (dec and hex)
   - Value ÷ 2 (dec and hex)
   - Value × 2 (dec and hex)
3. Look for patterns or matches with expected values from the C reference implementation.
4. Check if the value should be in bytes vs DLwords or vice versa.
5. Document findings and update relevant code or documentation as needed.

This technique helps quickly identify common conversion errors between byte and DLword representations.

## Introspection Timing Convention

*Date*: 2026-02-20
*Status*: CRITICAL - Understand when trace values are recorded

When analyzing introspection data from the C emulator:

- **TOS values** are recorded AFTER the PREVIOUS instruction, BEFORE the current instruction
- This means the TOS shown for instruction N is the result of instruction N-1, not instruction N
- **Always verify** what a trace value represents before drawing conclusions

### Example

If you see:

| PC | Opcode | TOS |
|----|--------|-----|
| A | POP | 0x0 |
| B | GVAR | 0xE |
| C | UNBIND | 0x140000 |

The value 0xE is **NOT** the result of GVAR - it's the TOS after POP, before GVAR executed.

### Why This Matters

1. **Incorrect causation** - You might think GVAR produced 0xE when it didn't
2. **Wild goose chases** - You might search for "who wrote 0xE" when the question itself is wrong
3. **Wasted time** - The 0x0E mystery blocked development for a long time due to this misunderstanding

### Document Conventions

When discovering timing conventions in trace/introspection data:

1. **Document immediately** - Don't assume you'll remember later
2. **Update the skill** - Add findings to this section
3. **Cross-reference** - Link to relevant documentation files