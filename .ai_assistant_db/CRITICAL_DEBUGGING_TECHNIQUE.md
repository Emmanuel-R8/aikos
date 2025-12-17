# Critical Debugging Technique: Value Analysis

**Date**: 2025-01-27
**Status**: CRITICAL - Always use this technique when debugging byte swaps, alignment, and addresses

## Technique

When debugging issues related to:
- Byte swaps (endianness)
- Alignment (byte vs DLword offsets)
- Address calculations (LispPTR as DLword offset vs byte offset)

**For EACH value, ALWAYS consider:**
1. The value itself (decimal and hexadecimal)
2. The value divided by 2 (decimal and hexadecimal)
3. The value multiplied by 2 (decimal and hexadecimal)

## Why This Works

- **Byte vs DLword**: If a value is in bytes but should be in DLwords, dividing by 2 reveals the DLword value
- **DLword vs byte**: If a value is in DLwords but should be in bytes, multiplying by 2 reveals the byte value
- **Alignment issues**: Values that are off by factors of 2 often indicate byte/DLword confusion
- **Endianness**: Byte-swapped values often show patterns when divided/multiplied by 2

## Example

If you see:
- Value: `23824` (0x5d10) decimal
- Value / 2: `11912` (0x2e88) decimal
- Value * 2: `47648` (0xba20) decimal

And C shows: `5956` (0x1744)

Then:
- `23824 / 4 = 5956` ← This reveals the correct calculation!

## Application

**ALWAYS** apply this technique when:
- Comparing values between C and Zig emulators
- Debugging PC calculations
- Debugging stack depth calculations
- Debugging frame field reads
- Any address/offset calculations

---

**Last Updated**: 2025-01-27
