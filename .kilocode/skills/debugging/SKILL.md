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