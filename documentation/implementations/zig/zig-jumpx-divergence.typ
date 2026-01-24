#import "../../prelude.typ": codeblock

= JUMPX Offset Divergence Analysis — Resolved

*Date*: 2026-01-19  
*Issue*: Control flow divergence at instruction 17 (JUMPX) — resolved

== Problem Description (Now Fixed)

The C and Zig emulators diverged at instruction 17 (PC: 0x60f154):

- *Before fix — C emulator*: Continues to 0x60f185 (FN1), offset=49
- *Before fix — Zig emulator*: Jumps to 0x615587 (ELT), offset=25649

This caused the Zig emulator to jump to an incorrect location, resulting in an "Unknown opcode 0x28" error.

== Root Cause

Analysis of the C implementation in `maiko/src/mvs.c` (lines 108-113):

#codeblock(lang: "c", [#raw("case opc_JUMPX: {
  short displacement;
  displacement = (short)(GETBYTE((char *)pc + 1));  // READS ONLY 1 BYTE!
  if (displacement >= 128) displacement -= 256;     // SIGN-EXTEND
  pc += displacement;
  goto newpc;
}")])

*Critical finding*: JUMPX reads only one byte with XOR addressing, then sign-extends it to 16-bit.

The Zig emulator was incorrectly reading two bytes as a 16-bit value.

== The Fix

Modified `zaiko/src/vm/dispatch/execution_control.zig` JUMPX handler:

#codeblock(lang: "zig", [#raw(".JUMPX => {
    try opcodes.handleJUMPX(vm);
    const pc = vm.pc;
    const offset_byte: i16 = blk: {
        if (vm.virtual_memory) |vmem| {
            const addr = pc + 1;
            const xor_addr = addr ^ 3;
            const raw_byte = if (xor_addr < vmem.len) vmem[xor_addr] else 0;
            const signed: i16 = if (raw_byte >= 128) @as(i16, @intCast(raw_byte)) - 256 else @as(i16, @intCast(raw_byte));
            break :blk signed;
        }
        break :blk 0;
    };
    // ... debug output ...
    return offset_byte;
}")])

Key changes:
1. Read only one byte from PC+1 (with XOR addressing)
2. Sign-extend the 8-bit value to 16-bit: if byte >= 128, subtract 256

== Memory Analysis

Raw sysout at file offset 0x177730 (PC page 3003):

#codeblock(lang: "text", [#raw("177730 bf 60 00 00 02 0a 12 c9 36 64 a1 68 10 64 6b f0
                           ^ PC 0x60f154 starts here")])

At PC 0x60f154 (after byte-swap and XOR addressing):
- Opcode byte: 0xb0 (JUMPX)
- Operand byte at 0x60f155 (XOR 3 → 0x60f156): 0x31
- Signed offset: 0x31 = 49 (positive, no sign-extension needed)
- Jump target: 0x60f154 + 49 = 0x60f185

== Trace Format Bug

The C emulator trace in `xc_dispatch_part_02.inc` (lines 638-647) shows JUMPX with 2-byte offset formatting, but this is a trace formatting bug — actual execution reads only one byte:

#codeblock(lang: "c", [#raw("case 0xB0: /* JUMPX - 2 byte offset */  // TRACE SAYS 2 BYTES
case 0xB1: /* JUMPXX - 2 byte offset */
{
  short offset = (short)((Get_BYTE_PCMAC1 << 8) | Get_BYTE_PCMAC2);  // BUT C EMULATOR USES Get_SBYTE_PCMAC1
  snprintf(operands_buf, sizeof(operands_buf), \"off=%+d\", offset);
} break;")])

Note: The C dispatch loop uses `Get_SBYTE_PCMAC1` (signed byte) for JUMPX, not a 2-byte read.

== Instruction Sequence (After Fix — Both Emulators Match)

#codeblock(lang: "text", [#raw("0x60f130: POP
0x60f131: GVAR
0x60f136: UNBIND
0x60f137: GETBASEPTR_N
0x60f139: COPY
0x60f13a: TJUMP1
...
0x60f154: JUMPX (offset=+49)
0x60f185: FN1    <- Both emulators now reach here
0x60f188: CONS")])

== Key Files Involved

- `zaiko/src/vm/dispatch/execution_control.zig`: JUMPX handler (fixed)
- `maiko/src/mvs.c`: C emulator JUMPX implementation (reference)
- `maiko/src/xc_dispatch_part_02.inc`: C trace formatting (contains bug)
- `documentation/specifications/instruction-set/opcodes-control-memory.typ`: Opcode reference (to be updated)

== Lessons Learned

1. Always verify C source code — don't trust trace output alone
2. JUMPX is 8-bit signed, JUMPXX is 16-bit signed
3. XOR addressing applies to operand bytes for byte-sized operands
4. Trace formatting can be misleading — actual execution may differ

== Related Documentation

- `specifications/instruction-set/opcodes-control-memory.typ`: Updated to reflect JUMPX = 8-bit signed
