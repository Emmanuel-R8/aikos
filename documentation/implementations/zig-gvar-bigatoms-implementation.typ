= Zig GVAR Opcode BIGATOMS Mode Implementation

*Date*: 2026-01-12 19:50
*Status*: ✅ Implemented
*Purpose*: Document GVAR opcode implementation for BIGATOMS+BIGVM mode

== Problem Statement

The Zig emulator's GVAR opcode was advancing PC by 3 bytes (1 byte opcode + 2 bytes atom number), while the C emulator advances PC by 5 bytes (1 byte opcode + 4 bytes atom pointer). This caused immediate divergence after the first GVAR instruction.

== Root Cause Analysis

=== C Emulator GVAR Implementation

*Location*: `maiko/inc/inlineC.h:341-368`

For BIGATOMS+BIGVM mode:
```c
#define GVAR(x) \
  do { \
    LispPTR tx = x; \
    if (tx & SEGMASK) { \
      PUSH(GetLongWord(NativeAligned4FromLAddr((tx) + NEWATOM_VALUE_OFFSET))); \
    } else \
      PUSH(GetLongWord((LispPTR *)AtomSpace + (tx * 5) + NEWATOM_VALUE_PTROFF)); \
    nextop_atom; \
  } while (0)
```

Where:
- `Get_AtomNo_PCMAC1 = Get_Pointer_PCMAC1` (reads 4-byte pointer)
- `nextop_atom = nextop5` (advances PC by 5 bytes total)

=== Zig Emulator Original Implementation

*Location*: `zaiko/src/vm/dispatch/length.zig:103`

```zig
.GVAR => 3, // Opcode + 2-byte atom index (BIGATOMS)
```

This was incorrect for BIGATOMS+BIGVM mode, which requires 4-byte atom pointers.

== Solution

=== 1. Update Instruction Length

*File*: `zaiko/src/vm/dispatch/length.zig`

```zig
// BIGATOMS+BIGVM mode: GVAR uses 4-byte pointer (5 bytes total)
// C: Get_AtomNo_PCMAC1 = Get_Pointer_PCMAC1 (4 bytes), nextop_atom = nextop5
// Non-BIGATOMS: GVAR uses 2-byte DLword (3 bytes total)
// C: Get_AtomNo_PCMAC1 = Get_DLword_PCMAC1 (2 bytes), nextop_atom = nextop3
// Assume BIGATOMS+BIGVM for now (matches C emulator behavior)
.GVAR => 5, // Opcode + 4-byte atom pointer (BIGATOMS+BIGVM)
```

=== 2. Add Pointer Operand Reader

*File*: `zaiko/src/vm/dispatch/instruction_struct.zig`

Added `getPointerOperand()` method to read 4-byte pointer operands with correct byte order:

```zig
/// Extract 4-byte pointer operand (custom byte order for BIGATOMS mode)
/// C: Get_Pointer_PCMAC1 reads bytes in order: [PC+1]<<16 | [PC+2]<<8 | [PC+3]
pub fn getPointerOperand(self: Instruction, index: usize) LispPTR {
    if (index + 3 >= self.operands.len) return 0;
    // C byte order: [index]<<16 | [index+1]<<8 | [index+2]
    return (@as(LispPTR, self.operands[index]) << 16) |
           (@as(LispPTR, self.operands[index + 1]) << 8) |
           (@as(LispPTR, self.operands[index + 2]));
}
```

=== 3. Update GVAR Handler

*File*: `zaiko/src/vm/dispatch/execution_data.zig`

```zig
.GVAR => {
    // BIGATOMS+BIGVM mode: GVAR uses 4-byte pointer operand
    // C: GVAR(Get_AtomNo_PCMAC1) where Get_AtomNo_PCMAC1 = Get_Pointer_PCMAC1 (4 bytes)
    try opcodes.handleGVAR(vm, instruction.getPointerOperand(0));
    return null;
},
```

=== 4. Update handleGVAR Signature

*File*: `zaiko/src/vm/opcodes/variable_access.zig`

Changed from `u16` to `LispPTR` to accept 4-byte atom pointers:

```zig
/// GVAR: Global variable access
/// BIGATOMS+BIGVM mode: atom_index is a 4-byte LispPTR pointer
/// Non-BIGATOMS mode: atom_index is a 2-byte DLword
pub fn handleGVAR(vm: *VM, atom_index: types.LispPTR) errors.VMError!void {
    // ... implementation ...
}
```

== Byte Order Details

=== C Emulator Get_Pointer_PCMAC1

*Location*: `maiko/inc/inlndos.h:245-264`

The C emulator reads pointer bytes in a specific order:
- Byte at `PCMAC+0` (XOR addressed) → high byte (<<16)
- Byte at `PCMAC+2` (XOR addressed) → low byte
- Byte at `PCMAC+1` (XOR addressed) → middle byte (<<8)

Since `PCMAC = pccache - 1` and `pccache = PC + 1`:
- `PCMAC+0 = PC+0` (opcode byte)
- `PCMAC+1 = PC+1` (first operand byte) → high byte
- `PCMAC+2 = PC+2` (second operand byte) → middle byte
- `PCMAC+3 = PC+3` (third operand byte) → low byte

Result: `[PC+1] << 16 | [PC+2] << 8 | [PC+3]`

=== Zig Implementation

The Zig implementation matches this byte order:
```zig
(@as(LispPTR, self.operands[index]) << 16) |      // PC+1 (high byte)
 (@as(LispPTR, self.operands[index + 1]) << 8) |  // PC+2 (middle byte)
 (@as(LispPTR, self.operands[index + 2]))          // PC+3 (low byte)
```

== Verification

After implementation:
- ✅ GVAR PC advancement: Both C and Zig advance by 5 bytes (`0x60f131 → 0x60f136`)
- ✅ First 5 instructions match exactly between C and Zig emulators
- ✅ Instruction sequence: POP → GVAR → UNBIND → GETBASEPTR_N → COPY

== Configuration Detection

Currently, the Zig emulator assumes BIGATOMS+BIGVM mode (hardcoded). Future work should:
1. Detect BIGATOMS/BIGVM mode from sysout file or build configuration
2. Support both BIGATOMS and non-BIGATOMS modes
3. Dynamically adjust instruction lengths based on configuration

== Related Documentation

- C Emulator PC Advancement Fix - Bug fix that enabled comparison
- Unified Logging Format - Execution log format
- Execution Comparison Analysis - Verification results
