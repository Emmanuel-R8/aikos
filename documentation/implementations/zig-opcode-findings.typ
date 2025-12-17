= Zig Implementation Opcode Findings pointerDate: 2025-12-07
*Status*: Critical Findings from Phase 1 Implementation

== Overview

During Phase 1 implementation of the Zig emulator, several opcode conflicts and discrepancies were discovered between the Zig implementation and the C reference implementation (`maiko/inc/opcodes.h`).

*Note*: General findings about non-existent opcodes have been documented in the emulator-independent Opcodes Reference. This document focuses on Zig-specific implementation details and conflicts encountered during development.

== Opcode Conflicts Discovered

=== Generic Jump Opcodes (Removed)

*Zig-Specific Issue*: The Zig implementation initially defined generic `JUMP`, `FJUMP`, `TJUMP` opcodes with incorrect values that conflicted with existing opcodes.

*Zig Implementation Conflicts*: - `JUMP = 0x20` conflicted with `BIN = 0x20` (32)
- `FJUMP = 0x30` conflicted with `BUSBLT = 0x30` (48) - `TJUMP = 0x31` conflicted with `BUSBLT = 0x30` (48)

*Zig Resolution*: Removed generic jump opcodes from Zig enum. Use `JUMPX`, `JUMPXX`, or optimized variants (`JUMP0`-`JUMP15`, etc.) as documented in Opcodes Reference.

*Zig Implementation Detail*: Zig enum definitions were commented out in `maiko/alternatives/zig/src/vm/dispatch.zig:73-75`. The `executeInstruction` function was updated to remove corresponding case statements.

*General Note*: See Common Misconceptions for emulator-independent documentation.

=== Character Opcodes (Removed)

*Zig-Specific Issue*: Zig implementation defined `CHARCODE` and `CHARN` opcodes that conflicted with existing jump opcodes.

*Zig Implementation Conflicts*: - `CHARCODE = 0xB4` conflicted with `NFJUMPX = 0xB4` (180) - `CHARN = 0xB5` conflicted with `NTJUMPX = 0xB5` (181)

*Zig Resolution*: Commented out Zig enum definitions. Character operations are handled via different mechanisms in the C implementation.

*Zig Implementation Detail*: Opcodes commented out in `maiko/alternatives/zig/src/vm/dispatch.zig:274-275`.

*General Note*: See Common Misconceptions for emulator-independent documentation.

=== Array Element Opcodes (Removed)

*Zig-Specific Issue*: Zig implementation defined `GETAEL1`, `GETAEL2`, `SETAEL1`, `SETAEL2` opcodes that conflicted with optimized jump opcodes.

*Zig Implementation Conflicts*: - `GETAEL1 = 0x80` conflicted with `JUMP0 = 0x80` (128)
- `GETAEL2 = 0x81` conflicted with `JUMP1 = 0x81` (129)
- `SETAEL1 = 0x82` conflicted with `JUMP2 = 0x82` (130) - `SETAEL2 = 0x83` conflicted with `JUMP3 = 0x83` (131)

*Zig Resolution*: Commented out Zig enum definitions. Use `AREF1/2` and `ASET1/2` instead as documented in Opcodes Reference.

*Zig Implementation Detail*: Opcodes commented out in `maiko/alternatives/zig/src/vm/dispatch.zig:246-253`.

*General Note*: See Common Misconceptions for emulator-independent documentation.

=== Type Checking Opcodes (Removed)

*Zig-Specific Issue*: Zig implementation defined `FIXP`, `SMALLP`, `LISTP` opcodes that conflicted with optimized jump opcodes.

*Zig Implementation Conflicts*: - `FIXP = 0xA0` conflicted with `TJUMP0 = 0xA0` (160)
- `SMALLP = 0xA1` conflicted with `TJUMP1 = 0xA1` (161)
- `LISTP = 0xA2` conflicted with `TJUMP2 = 0xA2` (162) - *Note*: `LISTP` (0xA2) actually exists in C implementation pointerZig Resolution: Commented out `FIXP` and `SMALLP`. `LISTP` was incorrectly removed but should be kept. Use `TYPEP` opcode with appropriate type codes for `FIXP` and `SMALLP` checks.

*Zig Implementation Detail*: Opcodes commented out in `maiko/alternatives/zig/src/vm/dispatch.zig:264-270`. `LISTP` should be restored.

*General Note*: See Common Misconceptions for emulator-independent documentation.

=== Stack Operation Opcodes (Removed)

*Zig-Specific Issue*: Zig implementation defined a `PUSH` opcode that conflicted with `ADDBASE`.

*Zig Implementation Conflict*: - `PUSH = 0xD0` conflicted with `ADDBASE = 0xD0` (208)

*Zig Resolution*: Commented out Zig enum definition. Stack operations are handled implicitly by other opcodes as documented in Opcodes Reference.

*Zig Implementation Detail*: Opcode commented out in `maiko/alternatives/zig/src/vm/dispatch.zig:317`.

*General Note*: See Common Misconceptions for emulator-independent documentation.

=== Comparison Opcodes (Fixed)

*Issue*: Comparison opcodes had incorrect values.

*Fixed*: - `EQ = 0xF0` (240) - was incorrectly 0x90
- `EQL = 0x3A` (58) - correct
- `IGREATERP = 0xF1` (241) - was incorrectly 0x94
- `FGREATERP = 0xF2` (242) - was incorrectly 0x95
- `GREATERP = 0xF3` (243) - correct
- `EQUAL = 0xF4` (244) - correct pointerC Reference: `maiko/inc/opcodes.h:264-268`

*Resolution*: Corrected all comparison opcode values to match C implementation.

*Location*: `maiko/alternatives/zig/src/vm/dispatch.zig:255-261`

=== Duplicate Opcodes (Fixed)

*Issue*: Some opcodes were defined multiple times.

*Fixed*: - `UBFLOAT3` - duplicate removed (kept at 0x32)
- `GVAR_` - duplicate removed (kept at 0x17)
- `FGREATERP` - duplicate removed (kept at 0xF2)
- `TYPEP` - duplicate removed (kept at 0x05)

*Resolution*: Removed duplicates, kept correct values matching C implementation.

== Zig-Specific Implementation Challenges

=== Enum Value Conflicts

Zig's enum system requires unique values, making conflicts immediately apparent during compilation. This forced early detection of opcode value mismatches that might have gone unnoticed in languages with less strict type checking.

=== Error Handling

Zig's error union types (`!T`) required explicit handling of all possible errors from opcode execution, leading to more robust error handling than the C implementation's implicit error handling.

=== Type Safety

Zig's strict type system caught several type mismatches (e.g., `u32` vs `u16`, `*const` vs `*`) that required explicit casts (`@truncate`, `@intCast`, `@ptrCast`).

== Impact

These findings reveal that the initial Zig opcode enumeration included several opcodes that don't exist in the C implementation. This was likely due to:
1. Incomplete reference to C opcodes.h
2. Assumptions about opcode naming conventions
3. Missing verification against C implementation pointerZig-Specific Benefit: Zig's compile-time checks caught these issues early, preventing runtime errors.

== Recommendations for Zig Implementation

1. *Verify All Opcodes*: Cross-reference all opcodes in Zig implementation against `maiko/inc/opcodes.h`
2. *Use C Enum Values*: Always use exact values from C enum, not assumed values
3. *Test Opcode Decoding*: Verify opcode decoding matches C implementation behavior
4. *Document Opcode Gaps*: Document which opcodes are missing and why
5. *Leverage Zig's Type System: Use Zig's compile-time checks to catch opcode conflicts early

== Related Documentation

- C Opcodes Reference - Source of truth for opcode values
- Zig Implementation Status - Overall implementation status
- Rewrite Specifications - Language-agnostic specifications
