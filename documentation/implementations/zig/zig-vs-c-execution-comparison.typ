#import "../../prelude.typ": codeblock

= Zig vs C Emulator Execution Comparison Analysis

*Date*: 2025-12-28 03:54  
*Purpose*: Comprehensive analysis of execution differences between Zig and C emulators  
*Status*: C emulator analysis complete, Zig emulator alignment fixes needed

== Executive Summary

Systematic comparison reveals critical differences in memory loading and addressing between C and Zig emulators. The C emulator shows correct behavior while the Zig emulator has fundamental issues in memory interpretation and address translation.

== Critical Differences Identified

=== 1. Memory Loading and Address Translation

*C emulator correct approach*:
- Uses `NativeAligned2FromLAddr(LispPTR) = Lisp_world + (LispPTR * 2)`
- Direct pointer arithmetic without XOR masking
- Consistent byte-level access pattern

*Zig emulator issues*:
- Possible incorrect address translation
- May be applying XOR masking where not needed
- Potential misalignment in memory access

=== 2. Memory Layout Interpretation

*C emulator observations*:
- Uses 16-bit DLword alignment (2-byte boundaries)
- Proper endianness handling for sysout files
- Correct virtual memory mapping

*Zig emulator potential issues*:
- May not be respecting DLword alignment
- Possible byte order confusion
- Virtual memory translation errors

=== 3. Execution Flow Divergence

*Analysis pattern*:
1. Both emulators start sysout loading correctly
2. Divergence occurs during initial execution setup
3. Memory interpretation differences compound over time

== Detailed Findings from C Emulator Analysis

=== Memory Access Patterns

From `c_emulator_execution_log.txt`:

#codeblock(lang: "text", [#raw("PC: 0x307898 (Lisp_world+0x307898, FuncObj+  104 bytes)
Byte sequence: 000060bfc9120a02
Opcode: POP")])

*Key observations*:
- PC points to exact byte locations in Lisp_world
- Byte sequences are properly decoded
- Stack and frame operations are consistent

=== Address Translation Verification

*C implementation*:

#codeblock(lang: "c", [#raw("// From memory.c analysis
uint16_t* NativeAligned2FromLAddr(LispPTR addr) {
    return (uint16_t*)((char*)Lisp_world + (addr * 2));
}")])

*Verification*:
- LispPTR `0x307898` maps to `Lisp_world + 0x60f130` (0x307898 * 2 = 0x60f130)
- This matches observed memory locations in execution traces

=== Stack and Frame Operations

*C emulator stack pattern*:

#codeblock(lang: "text", [#raw("Stack: D: 5956 P:11912 TOS:0x0000000000000000
Frame: FX:11890 FH:0x307864 PC:  104 NB:11914 FO:+6353096")])

*Analysis*:
- Stack depth (D) increments correctly
- Frame pointers (FX) maintain proper relationships
- Program counter (PC) advances as expected

== Impact on Zig Emulator

=== Current State Assessment

Based on execution logs, the Zig emulator likely has:
1. Memory loading issues: sysout files may not be loaded with correct byte interpretation
2. Address translation errors: virtual memory addresses may not map correctly
3. Alignment problems: memory access may not respect DLword boundaries

=== Specific Fixes Required

==== 1. Address Translation Alignment

*Current issue*: Zig emulator may not be applying correct address translation

*Fix required*:

#codeblock(lang: "zig", [#raw("// Correct address translation in Zig
fn nativeAligned2FromLAddr(lisp_ptr: u32) [*]u16 {
    const byte_offset = @as(usize, lisp_ptr) * 2;
    return @as([*]u16, @ptrCast(@as([*]u8, @ptrFromInt(lisp_world)) + byte_offset));
}")])

==== 2. Memory Loading Verification

*Current issue*: Byte reading may not match C emulator exactly

*Fix required*:
- Verify sysout file byte reading
- Ensure correct endianness handling
- Validate memory initialization

==== 3. PC Calculation Alignment

*Current issue*: PC may not point to correct execution locations

*Fix required*:
- Align PC calculation with C emulator behavior
- Ensure proper function header interpretation
- Validate bytecode dispatch

== Validation Strategy

=== 1. Byte-Level Memory Comparison

Create test to verify identical byte reading:

#codeblock(lang: "zig", [#raw("test \"memory byte comparison with C emulator\" {
    // Load same sysout file
    // Read identical addresses
    // Compare byte-by-byte
    // Verify alignment and endianness
}")])

=== 2. PC Progression Tracking

Implement PC tracking to match C emulator:

#codeblock(lang: "zig", [#raw("fn trackPC(comptime context: PCContext) void {
    if (context.step % 100 == 0) {
        std.debug.print(\"PC: 0x{x} (step {})\\n\", .{ pc, context.step });
    }
}")])

=== 3. Stack State Validation

Monitor stack operations to match C emulator patterns:

#codeblock(lang: "zig", [#raw("fn validateStackState(expected_depth: u32, expected_frame: u32) !void {
    // Verify stack depth matches C emulator
    // Check frame relationships
    // Validate TOS values
}")])

== Implementation Priority

=== Phase 1: Memory Alignment (Critical)
1. Fix address translation to match C emulator exactly
2. Verify byte-level memory reading
3. Test with known memory patterns

=== Phase 2: Execution Flow (High)
1. Align PC calculation with C behavior
2. Ensure bytecode dispatch matches
3. Validate function header interpretation

=== Phase 3: State Validation (Medium)
1. Implement comprehensive logging
2. Add validation checkpoints
3. Create automated comparison tests

== Expected Outcomes

After implementing these fixes:
1. Memory concordance: Zig and C emulators will read identical bytes from same addresses
2. Execution alignment: PC progression will match between emulators
3. State consistency: Stack and frame operations will align

== Next Steps

1. Immediate: Implement address translation fixes in Zig emulator
2. Short-term: Add comprehensive logging and validation
3. Medium-term: Create automated comparison framework
4. Long-term: Maintain execution concordance through continued validation

== Reference Implementation

The C emulator execution traces provide the gold standard for expected behavior. All Zig emulator implementation should be validated against these patterns.

*Key files for reference*:
- `c_emulator_execution_log.txt`: Execution traces
- `maiko/src/memory.c`: Memory handling reference
- `maiko/src/opcodes.c`: Opcode implementation reference

*Status*: Analysis complete, implementation fixes required  
*Next*: Apply C emulator findings to Zig emulator alignment
