---
DEPRECATED: This markdown file has been replaced by Typst documentation.

**Date Deprecated**: 2025-01-27
**Replacement**: See  directory for Typst format documentation.

This file is kept for reference only. All updates should be made to the Typst files in .
---

# Execution Trace Format

**Date**: 2025-12-16 12:41  
**Purpose**: Document the execution trace format used for debugging and validation

## Overview

The execution trace provides a single-line log format for each instruction executed by the VM, enabling comparison between different emulator implementations and debugging of execution flow.

## Format Specification

Each line contains exactly 462 characters (461 + newline) with the following column layout:

### Column Layout

| Columns | Field | Format | Description |
|---------|-------|--------|-------------|
| 1-6 | Instruction Count | `%5d ` | Right-aligned 5-digit instruction number + 1 space |
| 7-68 | PC Information | 62 chars | PC address, Lisp_world offset, FuncObj offset |
| 69-88 | Instruction Bytes | 20 chars | 8 bytes hex (16 chars) + 4 spaces |
| 89-128 | Instruction Name | 40 chars | Opcode name + parameters (left-aligned) |
| 129-298 | Stack Information | 170 chars | Stack depth, pointer offset, TOS, next 4 values |
| 299-461 | Frame Information | 163 chars | Frame offset, function header, PC, nextblock, FuncObj offset |
| 462 | Newline | `\n` | Line terminator |

### Field Details

#### PC Information (Columns 7-68)

**Format**: `PC: 0x%06x (Lisp_world+0x%06x, FuncObj+%5d bytes) `

- **PC Address**: Byte offset from start of Lisp_world (6 hex digits)
- **Lisp_world Offset**: Same as PC address (6 hex digits)
- **FuncObj Offset**: Byte offset from FuncObj to PC (signed, 5 digits right-aligned)

**Padding**: Field padded to exactly 62 characters with spaces.

#### Instruction Bytes (Columns 69-88)

**Format**: 8 bytes in hexadecimal (16 characters) + 4 trailing spaces

- Bytes are read from virtual memory at PC address
- Up to 8 bytes shown (may be less if instruction is shorter)
- Missing bytes padded with `00`
- Format: `%02x%02x%02x%02x%02x%02x%02x%02x    ` (16 hex + 4 spaces)

**Debug Info** (first 10 instructions, expanded for diagnosis):
- `[vpage:%u off:0x%03x]` - Virtual page number and offset within page
- `@mem:%p` (first instruction only) - Actual memory address pointer
- `[MEM_ZEROS]` - Indicates memory at PC location is all zeros (memory loading issue)
- `[PC_MISMATCH_CHECK vpage:%u]` - Special marker for known PC mismatch locations (PC 0x307898/0x307899)

**Note**: Debug info can be expanded as needed for diagnosis, as long as the core format (columns 1-461) remains identical across emulators.

#### Instruction Name (Columns 89-128)

**Format**: `%-40s` (left-aligned, 40 characters)

- Opcode name (up to 18 characters)
- Parameters (if any) in format: `p1=0x%02x`, `atom=0x%02x`, `off=%+d(0x%04x)`, etc.
- Padded to 40 characters with spaces

**Common Parameter Formats**:
- `p1=0x%02x` - Single byte parameter
- `p1=0x%02x p2=0x%02x` - Two byte parameters
- `atom=0x%02x` - Atom number parameter
- `w=0x%04x` - DLword parameter
- `off=%+d(0x%04x)` - Signed jump offset

#### Stack Information (Columns 129-298)

**Format**: `Stack: D:%5d P:%5d TOS:0x%016lx N:[0x%08x 0x%08x 0x%08x 0x%08x]`

- **D**: Stack depth in DLwords (5 digits, right-aligned)
- **P**: Stack pointer offset from Stackspace in DLwords (5 digits, right-aligned)
- **TOS**: Top of stack value (16 hex digits, zero-padded)
- **N**: Next 4 stack values (8 hex digits each, zero-padded)

**Padding**: Field padded to exactly 170 characters with spaces.

**CRITICAL - Stack Depth Calculation** (2025-12-17):
- **C Implementation**: `stack_depth = (CurrentStackPTR - Stackspace) / 2`
  - `CurrentStackPTR` and `Stackspace` are `DLword*` pointers
  - Pointer subtraction gives difference in DLwords (pointer arithmetic)
  - Then divided by 2 to get final stack depth
- **Implementations using byte addresses** (e.g., `@intFromPtr` in Zig):
  - Byte difference must be divided by 4 to match C's behavior
  - Formula: `(byte_diff / 4) = (ptr_diff_in_dlwords / 2)`
  - Example: `23824 bytes / 4 = 5956 DLwords` (matches C's `11912 DLwords / 2 = 5956`)

#### Frame Information (Columns 299-461)

**Format** (with frame): `Frame: FX:%5d FH:0x%06x PC:%5d NB:%5d FO:+%5d`

- **FX**: Current frame offset from Stackspace in DLwords (5 digits, right-aligned)
- **FH**: Frame function header (24-bit or 32-bit depending on BIGVM, 6 hex digits)
- **PC**: Frame PC offset from FuncObj in bytes (5 digits, right-aligned)
- **NB**: Next block pointer in DLwords from Stackspace (5 digits, right-aligned)
- **FO**: FuncObj byte offset from Lisp_world (5 digits, right-aligned, with `+` prefix)

**Format** (no frame): `Frame: FX:----- FH:------ PC:----- NB:----- FO:-----`

**Padding**: Field padded to exactly 163 characters with spaces.

**CRITICAL - Frame Field Memory Layout** (2025-12-17):
- **Actual memory layout may differ from C struct definition**
- In BIGVM mode, observed layout:
  - Offset [4-7]: `fnheader` (32-bit LispPTR, native little-endian)
  - Offset [8-9]: `pc` (DLword, native little-endian) - **NOT nextblock as struct suggests**
  - Offset [10-11]: `nextblock` (DLword, native little-endian) - **NOT pc as struct suggests**
- The struct definition in `maiko/inc/stack.h` may not match actual runtime layout
- Implementations must verify actual byte offsets by examining memory contents
- This discrepancy affects BIGVM mode frame field reading

## Stop Condition

Execution trace stops when PC (in DLwords) reaches `0xf000d5`:

```
STOP: PC=0x%x reached target 0xf000d5, total instructions=%d\n
```

## Implementation Requirements

### Critical Calculations

1. **FuncObj Offset**: Must calculate byte offset from FuncObj to PC
   - `funcobj_byte_offset = (char *)PC - (char *)FuncObj`
   - FuncObj is derived from frame's FX_FNHEADER
   - Format as signed integer with `%5d` (right-aligned, 5 digits)

2. **Stack Depth**: Must use DLword units
   - `stack_depth = (CurrentStackPTR - Stackspace) / 2` (DLwords)
   - NOT byte-based calculation

3. **Frame Header**: Must extract 24-bit value correctly
   - Non-BIGVM: `(hi2fnheader << 16) | lofnheader` (24-bit mask)
   - BIGVM: Direct `fnheader` field
   - Byte order must match memory layout

4. **Instruction Bytes**: Must read from correct virtual memory location
   - **CRITICAL** (2025-12-18 19:48): Logging uses RAW memory (no XOR addressing)
   - **Logging**: Read directly from `virtual_memory[PC + i]` to show actual memory contents
   - **Execution**: Uses XOR addressing (`address ^ 3` for bytes) in BYTESWAP mode
   - This matches C emulator behavior: logging shows raw bytes, execution uses XOR addressing
   - Use PC byte offset to index into virtual memory
   - Handle byte swapping if needed (pages are byte-swapped on load)

## Validation

Execution traces from different implementations should match exactly when:
- Same sysout file is loaded
- Same initial VM state
- Same instruction execution path

Key fields to verify:
- PC addresses should match exactly
- FuncObj offsets should match (within ±1 byte for instruction length)
- Instruction bytes should match exactly
- Opcode names should match
- Stack depth should match (may differ slightly due to timing)
- Frame header should match exactly

## Related Documentation

- [Execution Model](execution-model.md) - VM execution flow
- [Stack Management](stack-management.md) - Stack structure and operations
- [Function Calls](function-calls.md) - Frame structure and function headers
