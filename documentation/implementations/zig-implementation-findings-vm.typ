= VM Execution Findings - Zig Implementation pointerDate: 2025-12-12 16:10
*Status*: Frame header reading fixed, address translation verified

== Frame Header (FX_FNHEADER) Reading Fix

=== Problem
Initial PC calculation was incorrect because frame header fields were being read from wrong byte positions.

=== Root Cause
The actual memory layout of frame fields differs from the C struct definition: - *Struct says*: `[4,5]=lofnheader`, `[6,7]=hi1fnheader_hi2fnheader`
- *Actual memory*: `[4,5]=hi1fnheader_hi2fnheader`, `[6,7]=lofnheader` (SWAPPED)

Additionally, `hi2fnheader` is in the pointerlow byte (bits 0-7) of `hi1fnheader_hi2fnheader`, not the high byte.

=== Solution pointerFile: `maiko/alternatives/zig/src/vm/vm_initialization.zig`

[`// Read swapped: lofnheader is actually at [6,7], hi1fnheader_hi2fnheader is at [4,5]`]
[`const hi1fnheader_hi2fnheader = std.mem.readInt(DLword, frame_bytes[4..6], .little`];
const lofnheader = std.mem.readInt(DLword, frame_bytes[6..8], .little);
// hi2fnheader is in the LOW byte (bits 0-7) of hi1fnheader_hi2fnheader
const hi2fnheader: u8 = @as(u8, @truncate(hi1fnheader_hi2fnheader & 0xFF));
const fnheader_be = (@as(LispPTR, hi2fnheader) << 16) | lofnheader;)

*Result*: FX_FNHEADER now correctly reads as `0x307864` (matches C emulator).

=== Verification
- Frame at offset `0x25ce4` in virtual memory
- Bytes `[4,5] = [0x30, 0x00]` → `hi1fnheader_hi2fnheader = 0x0030`, `hi2fnheader = 0x30`
- Bytes `[6,7] = [0x64, 0x78]` → `lofnheader = 0x7864` - FX_FNHEADER = `(0x30 << 16) | 0x7864 = 0x307864` ✓

== Zig-Specific Implementation Notes

=== Zig Implementation Details pointerFile: `maiko/alternatives/zig/src/vm/vm_initialization.zig`

[`// Treat FX_FNHEADER as byte offset (not multiplied by 2`]
const funcobj_offset_calc: usize = @as(usize, @intCast(fnheader_addr));

const frame_pc_bytes = @as(usize, @intCast(frame_pc)) / 2;

// Calculate PC
const calculated_pc = funcobj_byte_offset + frame_pc_bytes;)

*Result*: PC = `0x307864 + 52 = 0x307898` ✓ (matches C emulator exactly!)

=== Verification
The Zig emulator now calculates the correct initial PC:
- FX_FNHEADER = `0x307864` (treated as byte offset)
- FuncObj = `0x307864` bytes - PC = `0x307864 + 52 = 0x307898` ✓

*Debug output confirms*: [`Using calculated PC=0x307898 (opcode=0x0e`])

== Related Files
- `maiko/alternatives/zig/src/vm/vm_initialization.zig` - Frame reading and PC initialization
- `maiko/alternatives/zig/src/vm/stack.zig` - Frame structure definition
- `.ai_assistant_db/rewrite-spec/vm-core/stack-management.md` - Frame structure documentation (general knowledge)
- `.ai_assistant_db/rewrite-spec/memory/address-translation.md` - Address translation investigation (general knowledge)
- `.ai_assistant_db/rewrite-spec/data-structures/sysout-byte-swapping.md` - Endianness documentation
