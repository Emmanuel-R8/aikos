# Address Translation Testing - Zig Implementation

**Date**: 2025-12-12 16:45
**Status**: Testing byte addressing hypothesis

## Zig-Specific Testing Notes

This file documents Zig-specific implementation details for testing the address translation hypothesis. For the general investigation, see `rewrite-spec/memory/address-translation.md`.

## Current Zig Implementation

### Testing Byte Addressing

Modified Zig code to test byte addressing hypothesis:

- `maiko/alternatives/zig/src/vm/vm_initialization.zig`: 
  - Treat FX_FNHEADER as byte offset (not multiplied by 2)
  - Divide CURRENTFX->pc by 2 (testing DLword→byte conversion)
  - Result: `PC = 0x307864 + 52 = 0x307898` ✓

- `maiko/alternatives/zig/src/utils/address.zig`: 
  - Treat LispPTR as byte offset (no multiplication by 2)
  - Testing if `translateLispPTRToOffset` should return byte offset directly

## Zig-Specific Challenges

- Zig's type system requires explicit casts for pointer arithmetic
- Testing both byte and DLword interpretations to compare results
- Debug output shows both interpretations for comparison

## Related Files

- `maiko/alternatives/zig/src/vm/vm_initialization.zig` - Zig VM initialization
- `maiko/alternatives/zig/src/utils/address.zig` - Zig address translation
- `rewrite-spec/memory/address-translation.md` - General investigation (emulator-independent)
