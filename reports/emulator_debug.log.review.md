# Review of Proposed Execution Log Format

## Current Format Specification

**Total line length: 441 characters (fixed width)**

| Column Range | Width | Field | Example |
|--------------|-------|-------|---------|
| 1-6 | 6 | Instruction count | `___13 ` |
| 7-68 | 62 | PC information | `PC: 0x7f1cdc60f130 (Lisp_world+0x60f130, FuncObj+__104 bytes) ` |
| 69-88 | 20 | Instruction bytes (8 bytes) | `bf00 0060 bfc9 120a ` |
| 89-108 | 20 | Instruction name + parameters | `PVAR_1           ` |
| 109-278 | 170 | Stack information | `--- Stack: Depth: __5956 DLwords (___2978 LispPTRs), PTR: 0x7f1cdc025d10, TopOfStack (cached): 0x1234 5678 9abc def0, Next values: 0xe 0xfffe0002 0xfffe0000 0x6de388 --- ` |
| 279-441 | 163 | Frame information | `Frame: CURRENTFX: 0x7f1cdc025ce4, FX_FNHEADER: 0x307864, CURRENTFX->pc: __104 (0x__68), CURRENTFX->nextblock: 0x2e8a, FuncObj: 0x7f1cdc60f0c8 (Lisp_world+0x60f0c8)` |

## Comments and Improvements

### 1. **Format Duplication**
- **Issue**: The format specification is duplicated (lines 1-11 and 12-22)
- **Recommendation**: Remove duplicate, keep single specification

### 2. **Instruction Count (Columns 1-6)**
- **Current**: `___13 ` (right-aligned, padded with spaces)
- **Issues**:
  - Typo: "instuction" should be "instruction"
  - Need to specify: right-align with leading spaces, trailing space?
  - Maximum count: 6 digits = 999,999 instructions (may need more for long runs)
- **Recommendation**: 
  - Use right-aligned format: `%6d` (no trailing space needed if next field starts with space)
  - Or: `%5d ` (5 digits + 1 space) to allow up to 99,999 instructions per log

### 3. **PC Information (Columns 7-68, 62 chars)**
- **Current**: `PC: 0x7f1cdc60f130 (Lisp_world+0x60f130, FuncObj+__104 bytes) `
- **Issues**:
  - Shows absolute memory addresses (0x7f1cdc60f130) - these differ between runs
  - For comparison, we need relative offsets (DLword offsets from Lisp_world)
  - FuncObj offset format: `__104` (2 leading spaces) - need consistent padding
  - Trailing space needed to pad to 62 chars
- **Recommendation**:
  - **Option A (Relative)**: `PC: 0x780064 (Lisp_world+0x780064, FuncObj+__104 bytes) `
    - Use DLword offset (divide byte offset by 2)
    - More portable, easier to compare
  - **Option B (Absolute)**: Keep absolute addresses but ensure consistent formatting
  - **Format spec**: `PC: 0x%016lx (Lisp_world+0x%08x, FuncObj+%5d bytes) ` (with padding)
  - **FuncObj offset**: Right-align with 5 digits: `%5d` (e.g., `  104`)

### 4. **Instruction Bytes (Columns 69-88, 20 chars)**
- **Current**: `bf00 0060 bfc9 120a ` (8 bytes as 4 hex pairs with spaces)
- **Issues**:
  - Format: 8 bytes = 16 hex digits + 7 spaces = 23 chars (exceeds 20!)
  - Need to clarify: show only first N bytes if instruction is longer?
- **Recommendation**:
  - **Option A**: Show 6 bytes: `bf00 0060 bfc9 ` (12 hex + 5 spaces = 17 chars, fits in 20)
  - **Option B**: Show 8 bytes without spaces: `bf000060bfc9120a` (16 chars, fits in 20)
  - **Option C**: Show 8 bytes with minimal spacing: `bf 00 00 60 bf c9 12 0a` (23 chars - too long)
  - **Recommended**: Option A (6 bytes with spaces) or Option B (8 bytes without spaces)
  - **Format**: `%02x%02x%02x%02x%02x%02x ` (6 bytes) or `%02x%02x%02x%02x%02x%02x%02x%02x` (8 bytes)

### 5. **Instruction Name + Parameters (Columns 89-108, 20 chars)**
- **Current**: `PVAR_1           ` or `PVARX param1=0x43`
- **Issues**:
  - Variable length - need truncation strategy
  - Parameters format not fully specified
  - Need to handle long opcode names (e.g., `IDIFFERENCE_N`)
- **Recommendation**:
  - Left-align instruction name, pad with spaces
  - **Format**: `%-15s` for name (15 chars), then parameters if space allows
  - **Truncation**: If name + params > 20, truncate params or use abbreviation
  - **Parameter format**: 
    - Single param: `param1=0x%02x` (e.g., `param1=0x43`)
    - Two params: `param1=0x%02x param2=0x%02x` (may exceed 20 chars)
    - Jump offset: `offset=0x%04x` (e.g., `offset=0x1234`)
  - **Example**: `PVAR_1           ` (15 chars name + 5 spaces) or `PVARX param1=0x43` (20 chars)

### 6. **Stack Information (Columns 109-278, 170 chars)**
- **Current**: `--- Stack: Depth: __5956 DLwords (___2978 LispPTRs), PTR: 0x7f1cdc025d10, TopOfStack (cached): 0x1234 5678 9abc def0, Next values: 0xe 0xfffe0002 0xfffe0000 0x6de388 --- `
- **Issues**:
  - Very long format - need to verify it fits in 170 chars
  - "Next values" - how many? (example shows 4)
  - TopOfStack format: `0x1234 5678 9abc def0` (16 hex digits with spaces) vs `0x123456789abcdef0` (compact)
  - Absolute addresses (PTR) differ between runs - should use relative?
  - Padding strategy not clear
- **Recommendation**:
  - **Format**: `--- Stack: Depth: %5d DLwords (%5d LispPTRs), PTR: 0x%016lx, TOS: 0x%016lx, Next: 0x%x 0x%x 0x%x 0x%x --- `
  - **TopOfStack**: Use compact format `0x%016lx` (no spaces) for 64-bit values
  - **Stack pointer**: Use relative offset instead of absolute: `PTR: %5d` (DLword offset from Stackspace)
  - **Next values**: Show 4 values (as in example)
  - **Padding**: Right-align numbers, left-align text
  - **Alternative compact format**: 
    `Stack: D:%5d P:%5d TOS:0x%016lx N:[0x%x 0x%x 0x%x 0x%x]`
    (More compact, easier to parse)

### 7. **Frame Information (Columns 279-441, 163 chars)**
- **Current**: `Frame: CURRENTFX: 0x7f1cdc025ce4, FX_FNHEADER: 0x307864, CURRENTFX->pc: __104 (0x__68), CURRENTFX->nextblock: 0x2e8a, FuncObj: 0x7f1cdc60f0c8 (Lisp_world+0x60f0c8)`
- **Issues**:
  - Absolute addresses differ between runs
  - Need to handle NULL/missing frame
  - Padding for numbers not consistent (`__104` vs `0x2e8a`)
  - FuncObj address shown twice (in PC field and Frame field)
- **Recommendation**:
  - **Use relative offsets**: 
    `Frame: FX:%5d FNHEADER:0x%06x PC:%5d (0x%04x) NEXT:%5d FuncObj:+%5d`
  - **Format spec**: 
    - CURRENTFX: DLword offset from Stackspace: `%5d`
    - FX_FNHEADER: 24-bit value: `0x%06x`
    - CURRENTFX->pc: byte offset: `%5d`, hex: `(0x%04x)`
    - CURRENTFX->nextblock: DLword offset: `%5d`
    - FuncObj: byte offset from Lisp_world: `+%5d`
  - **NULL handling**: `Frame: (none)` or `Frame: FX:----- FNHEADER:------ ...`
  - **Compact format**: 
    `Frame: FX:%5d FH:0x%06x PC:%5d NB:%5d FO:+%5d`
    (Uses abbreviations to save space)

## Proposed Improved Format

### Single-Line Format (441 characters)

```
[1-6]    [7-68]                                                          [69-88]        [89-108]           [109-278]                                                                                                    [279-441]
___13 PC: 0x780064 (Lisp_world+0x780064, FuncObj+  104 bytes) bf00 0060 bfc9 120a PVAR_1                Stack: D: 5956 P: 5956 TOS:0x000000000000000e N:[0xe 0xfffe0002 0xfffe0000 0x6de388] Frame: FX:11882 FH:0x780030 PC: 104 NB:11882 FO:+15728672
```

### Field Specifications

1. **Instruction Count (cols 1-6)**: Right-aligned, 5 digits + 1 space
   - Format: `%5d ` (e.g., `   13 `)

2. **PC Information (cols 7-68)**: 62 characters
   - Format: `PC: 0x%06x (Lisp_world+0x%06x, FuncObj+%5d bytes) `
   - Uses DLword offset (divide byte offset by 2 for PC and Lisp_world)
   - FuncObj offset in bytes, right-aligned 5 digits

3. **Instruction Bytes (cols 69-88)**: 20 characters
   - Format: `%02x%02x%02x%02x%02x%02x%02x%02x` (8 bytes, no spaces)
   - Or: `%02x %02x %02x %02x %02x %02x ` (6 bytes with spaces)
   - **Recommendation**: 8 bytes without spaces (16 chars) + 4 trailing spaces

4. **Instruction Name + Params (cols 89-108)**: 20 characters, left-aligned
   - Format: `%-15s` for name, then params if space allows
   - Truncate if total > 20 chars

5. **Stack Information (cols 109-278)**: 170 characters
   - **Compact format**: `Stack: D:%5d P:%5d TOS:0x%016lx N:[0x%08x 0x%08x 0x%08x 0x%08x]`
   - D = Depth (DLwords), P = Pointer offset (DLwords from Stackspace)
   - TOS = TopOfStack (64-bit, compact hex)
   - N = Next 4 stack values (32-bit each)
   - Pad with spaces to 170 chars

6. **Frame Information (cols 279-441)**: 163 characters
   - **Compact format**: `Frame: FX:%5d FH:0x%06x PC:%5d NB:%5d FO:+%5d`
   - FX = CURRENTFX offset (DLwords from Stackspace)
   - FH = FX_FNHEADER (24-bit value)
   - PC = CURRENTFX->pc (byte offset from FuncObj)
   - NB = CURRENTFX->nextblock (DLword offset)
   - FO = FuncObj offset (bytes from Lisp_world)
   - Pad with spaces to 163 chars
   - **NULL handling**: `Frame: (none)` or `Frame: FX:----- FH:------ PC:----- NB:----- FO:-----`

## Implementation Notes

### For C Emulator:
- Use `fprintf(debug_log, "%5d PC: 0x%06x ...", ...)` with fixed-width format specifiers
- Calculate DLword offsets: `pc_dlword = (PCMAC - Lisp_world) / 2`
- Use relative offsets for stack pointer and frame pointer

### For Zig Emulator:
- Use `try writer.print("{d:>5} PC: 0x{x:0>6} ...", .{count, pc_dlword})`
- Calculate DLword offsets: `pc_dlword = vm.pc / 2`
- Use relative offsets matching C emulator

### Comparison Strategy:
- Both emulators should produce identical output for same execution
- Use `diff` or `cmp` to compare logs
- Differences indicate implementation bugs or missing features

## Questions to Resolve

1. **PC Format**: Use relative DLword offsets or absolute addresses?
   - **Recommendation**: Relative offsets (easier to compare)

2. **Instruction Bytes**: Show 6 or 8 bytes? With or without spaces?
   - **Recommendation**: 8 bytes without spaces (16 chars) + 4 trailing spaces

3. **Stack Pointer**: Use absolute address or relative offset?
   - **Recommendation**: Relative offset (DLword offset from Stackspace)

4. **Frame Pointer**: Use absolute address or relative offset?
   - **Recommendation**: Relative offset (DLword offset from Stackspace)

5. **TopOfStack Format**: With spaces (`0x1234 5678`) or compact (`0x12345678`)?
   - **Recommendation**: Compact (`0x%016lx`)

6. **NULL/Missing Values**: How to represent?
   - **Recommendation**: Use `-----` or `(none)` with appropriate field width

## Example Output

```
    1 PC: 0x780064 (Lisp_world+0x780064, FuncObj+  104 bytes) 59424c4f434b2f57 PVAR_1            Stack: D:11912 P:11912 TOS:0x0000000000000000 N:[0x0000000e 0xfffe0002 0xfffe0000 0x006de388] Frame: FX:11882 FH:0x780030 PC:  104 NB:11882 FO:+15728672
    2 PC: 0x780064 (Lisp_world+0x780064, FuncObj+  105 bytes) 424c4f434b2f5754 IVAR2             Stack: D:11910 P:11910 TOS:0x000000000002fffe N:[0x0002fffe 0xaaaaaaaa 0xfffe0002 0xfffe0000] Frame: FX:11882 FH:0x780030 PC:  105 NB:11882 FO:+15728672
```

## Summary of Recommended Changes

1. ✅ Use relative offsets (DLword offsets) instead of absolute addresses
2. ✅ Use compact hex format for TopOfStack (no spaces)
3. ✅ Use abbreviations in Stack and Frame fields to save space
4. ✅ Specify exact format strings for all fields
5. ✅ Define NULL/missing value representation
6. ✅ Clarify instruction bytes format (8 bytes, no spaces)
7. ✅ Remove format duplication in specification file
8. ✅ Fix typo: "instuction" → "instruction"
