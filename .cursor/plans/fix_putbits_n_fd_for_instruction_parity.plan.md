---
name: Fix PUTBITS_N_FD for instruction parity
overview: Rewrite `handlePUTBITS_N_FD` in Zig to exactly match C's `PUTBITS_N_M` macro semantics, ensuring instruction-by-instruction parity for the first divergence point around step 16.
todos:
  - id: add_n_mask_array
    content: Add n_mask_array constant to base_ops.zig matching C definition
    status: completed
  - id: rewrite_putbits
    content: Rewrite handlePUTBITS_N_FD to exactly match PUTBITS_N_M macro semantics
    status: completed
  - id: verify_variable_access
    content: Review FVARX and IVARX_ implementations to ensure they feed correct values
    status: completed
  - id: test_1000_steps
    content: Run 1000-step comparison to verify parity at step 16 and beyond
    status: in_progress
---

# Fix PUTBITS_N_FD for Instruction-by-Instruction Parity

## Goal

Achieve exact instruction-by-instruction parity between Zig and C emulators by fixing the first divergence point around step 16, specifically the `PUTBITS_N_FD` opcode implementation.

## Current Problem

The Zig implementation of `handlePUTBITS_N_FD` in `zaiko/src/vm/opcodes/base_ops.zig` does not exactly match the C macro `PUTBITS_N_M` from `maiko/inc/inlineC.h`, causing divergence in execution traces.

## C Reference Implementation

From `maiko/inc/inlineC.h:585-600`:

```c
#define PUTBITS_N_M(a, b)                                                                \
  do {                                                                                   \
    LispPTR base;                                                                            \
    int bb = b;                                                                 \
    DLword *pword;                                                              \
    int shift_size, field_size, fmask;                                          \
    if ((SEGMASK & TOPOFSTACK) != S_POSITIVE) { goto op_ufn; };                          \
    base = POINTERMASK & POP_TOS_1;                                                      \
    pword = NativeAligned2FromLAddr(base + (a));                                         \
    field_size = 0xF & bb;                                                               \
    shift_size = 15 - (0xF & (bb >> 4)) - field_size;                                    \
    fmask = n_mask_array[field_size] << shift_size;                                      \
    GETWORD(pword) = ((TOPOFSTACK << shift_size) & fmask) | (GETWORD(pword) & (~fmask)); \
    TOPOFSTACK = base;                                                                   \
    nextop3;                                                                             \
  } while (0)
```

Key semantics:

- Stack: `[value, base] -> [base]` (value on TOS, base below; pop base, write bits, set TOS = base)
- Uses `n_mask_array[field_size]`for mask (defined in `maiko/src/xc.c:396-398`)
- `field_size = 0xF & bb` (low 4 bits)
- `shift_size = 15 - (0xF & (bb >> 4)) - field_size`
- Write: `((TOPOFSTACK << shift_size) & fmask) | (GETWORD(pword) & (~fmask))`
- Set `TOPOFSTACK = base` after write

## Implementation Steps

### 1. Add n_mask_array to Zig

Create a constant array matching C's `n_mask_array`:

- Location: `zaiko/src/vm/opcodes/base_ops.zig` (at module level)
- Values: `[1, 3, 7, 0xf, 0x1f, 0x3f, 0x7f, 0xff, 0x1ff, 0x3ff, 0x7ff, 0xfff, 0x1fff, 0x3fff, 0x7fff, 0xffff]`
- Type: `const [16]DLword`

### 2. Rewrite handlePUTBITS_N_FD

Replace the current implementation (lines 433-492 in `zaiko/src/vm/opcodes/base_ops.zig`) to exactly match C semantics:

- **Stack manipulation**:

  - Get `value` from TOS (don't pop yet)
  - Validate `value` has `S_POSITIVE` segment
  - Pop `base` using `popStack()`
  - Extract `base_ptr = POINTERMASK & base`

- **Address calculation**:

  - Calculate target address: `base_ptr + arg1` (where `arg1` is the byte offset `a`)
  - Translate to native pointer using `translateAddressExtended` (if storage) or `translateAddress` (if no storage)
  - Use alignment 2 (DLword alignment)

- **Field descriptor parsing**:

  - `field_size = 0xF & arg2` (low 4 bits)
  - `shift_pos = 0xF & (arg2 >> 4)` (high 4 bits)
  - `shift_size = 15 - shift_pos - field_size` (exactly matching C)

- **Mask calculation**:

  - Use `n_mask_array[field_size]` instead of manual calculation
  - `fmask = n_mask_array[field_size] << shift_size`

- **Memory read/write**:

  - Read current DLword from memory (big-endian format)
  - Apply bit field write: `((value << shift_size) & fmask) | (current_word & (~fmask))`
  - Write back to memory (big-endian format)

- **Stack update**:
  - Set TOS to `base` (not push, but set directly): `setTopOfStack(vm, base)`

### 3. Verify Related Opcodes

Check that `FVARX`, `FVAR6`, and `IVARX_` produce correct values that `PUTBITS_N_FD` expects:

- Review `zaiko/src/vm/opcodes/variable_access.zig` for `handleFVARX` and `handleIVARX_`
- Ensure they match C implementations in `maiko/inc/inlineC.h`
- No changes expected, but verify for correctness

### 4. Test and Validate

- Run 1000-step comparison: `scripts/compare_emulator_execution.sh --with-c EMULATOR_MAX_STEPS=1000`
- Verify that steps 1-16 (and ideally all 1000 steps) now match exactly
- If divergences remain, identify the next divergence point and iterate

## Files to Modify

- `zaiko/src/vm/opcodes/base_ops.zig`: Add `n_mask_array` constant and rewrite `handlePUTBITS_N_FD` function

## Files to Review (No Changes Expected)

- `zaiko/src/vm/opcodes/variable_access.zig`: Verify `handleFVARX` and related opcodes
- `maiko/inc/inlineC.h`: Reference for exact C semantics
- `maiko/src/xc.c`: Reference for `n_mask_array` definition

## Success Criteria

- `handlePUTBITS_N_FD` implementation exactly matches C's `PUTBITS_N_M` macro
- 1000-step trace comparison shows no divergence at step 16 (first divergence point)
- All bit field calculations use `n_mask_array` semantics
- Stack manipulation matches C: `[value, base] -> [base]`
