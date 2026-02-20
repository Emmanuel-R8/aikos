# Laiko GVAR Parity Analysis

**Date**: 2026-02-20
**Purpose**: Document findings from C introspection analysis and required fixes for Laiko GVAR implementation

---

## Executive Summary

The "0x0E atom 522 mystery" has been **SOLVED**. The value 0x0E was **NEVER** written to atom 522's value cell. It was a misinterpretation of the introspection data timing convention.

### Key Finding: Introspection TOS Timing

The introspection module records TOS (Top of Stack) values **AFTER the PREVIOUS instruction**, **BEFORE the current instruction** executes.

**Evidence from C introspection database:**

| Event ID | PC       | Opcode | Name   | TOS      | Interpretation                    |
| -------- | -------- | ------ | ------ | -------- | --------------------------------- |
| 6        | 0x60F130 | 191    | POP    | 0x0      | TOS after previous instruction    |
| 7        | 0x60F131 | 96     | GVAR   | 0xE      | TOS after POP, **BEFORE GVAR**    |
| 8        | 0x60F136 | 18     | UNBIND | 0x140000 | TOS **after GVAR**, before UNBIND |

**Conclusion**: GVAR for atom 522 pushed 0x140000, NOT 0xE. The 0xE was a stale TOS value from a previous POP operation.

**Verification from gvar_executions table:**

```sql
SELECT atom_index, printf('0x%X', value_read) FROM gvar_executions WHERE atom_index = 522;
-- Result: atom_index=522, value_read=0x140000
```

---

## Laiko GVAR Implementation Issues

### Issue 1: `read-pc-32-be` is a Placeholder

**Location**: `laiko/src/vm/op-variable.lisp` lines 331-336

```lisp
(defun read-pc-32-be (vm)
  "Read 32-bit big-endian value from PC and advance PC by 4."
  (declare (type vm vm))
  ;; This needs to be implemented based on VM structure
  ;; Placeholder - actual implementation depends on VM PC handling
  0)
```

**Problem**: Always returns 0, so GVAR always reads atom 0's value.

**Required Fix**: Implement proper PC-relative read from instruction stream.

### Issue 2: Build Configuration Mismatch

**Location**: `laiko/src/data/atom.lisp` line 49

```lisp
(defparameter *bigatoms* nil
  "Whether using BIGATOMS mode (32-bit atom indices)")
```

**Problem**: C emulator uses BIGVM=1, BIGATOMS=1 (from introspection build_config table).

**Required Fix**: Set `*bigatoms*` to `t` or detect from sysout.

### Issue 3: Atom Index Masking

**Location**: `laiko/src/vm/op-variable.lisp` line 203

```lisp
(let ((valspace-index (logand atom-idx #xFFFF)))
```

**Problem**: For BIGVM, the full 32-bit atom index should be used, not masked to 16 bits.

**Required Fix**: Pass the full atom index to `read-atom-value`.

---

## C GVAR Implementation Reference

From `maiko/inc/inlineC.h` lines 438-458 (BIGVM variant):

```c
#define GVAR(x)                                                                       \
  do                                                                                  \
  {                                                                                   \
    LispPTR gvar_atom = (x);                                                          \
    LispPTR *gvar_addr;                                                               \
    LispPTR gvar_val;                                                                 \
    if (gvar_atom & SEGMASK)                                                          \
    {                                                                                 \
      /* NEWATOM: atom_index has segment bits set */                                  \
      gvar_addr = (LispPTR *)NativeAligned4FromLAddr((gvar_atom) + NEWATOM_VALUE_OFFSET); \
      gvar_val = GetLongWord(gvar_addr);                                              \
    }                                                                                 \
    else                                                                              \
    {                                                                                 \
      /* LITATOM: atom_index is small, use AtomSpace */                               \
      gvar_addr = (LispPTR *)((LispPTR *)AtomSpace + (gvar_atom * 5) + NEWATOM_VALUE_PTROFF); \
      gvar_val = GetLongWord(gvar_addr);                                              \
    }                                                                                 \
    PUSH(gvar_val);                                                                   \
    nextop_atom;                                                                      \
  } while (0)
```

**Key Points:**

1. For LITATOM (small atom index, no segment bits): Use AtomSpace with 5 cells per atom
2. For NEWATOM (large atom index with segment bits): Use pointer arithmetic from atom index
3. Value cell offset: `NEWATOM_VALUE_PTROFF = 1` (second cell in atom entry)

---

## Required Fixes for Laiko

### Fix 1: Implement `read-pc-32-be`

The function needs access to the current bytecode array. Options:

1. Store bytecode array in VM structure
2. Use special variable during dispatch
3. Read from virtual memory at PC offset

### Fix 2: Set Build Configuration

Either:

- Set `*bigatoms*` to `t` in `laiko/src/data/atom.lisp`
- Or detect from sysout IFPAGE

### Fix 3: Update GVAR Handler

```lisp
(defop gvar #x60 5
  "GVAR: Push value of global variable (atom value cell)."
  (let ((atom-idx (read-pc-32-be vm)))
    ;; Don't mask atom index for BIGVM
    (let ((value (maiko-lisp.data:read-atom-value vm atom-idx)))
      (vm-push vm value))))
```

---

## Documentation Created

1. **`documentation/specifications/gvar-introspection-timing.typ`**
   - Documents the TOS recording convention
   - Explains why event N shows TOS after instruction N-1

---

## Next Steps

1. Implement `read-pc-32-be` properly
2. Set `*bigatoms*` to `t` for BIGVM compatibility
3. Test GVAR with atom 522 to verify it reads 0x140000
4. Run parity comparison with C emulator

---

## References

- C GVAR macro: `maiko/inc/inlineC.h` lines 419-480
- C dispatch loop: `maiko/src/xc.c` lines 920-1055
- Introspection database: `maiko/test_introspect.db`
- Introspection status: `reports/introspection-status-2026-02-19.md`
