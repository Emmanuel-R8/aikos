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

## Laiko GVAR Implementation Issues - FIXED

### Issue 1: `read-pc-32-be` is a Placeholder - **FIXED**

**Location**: `laiko/src/vm/op-variable.lisp`

**Problem**: Always returned 0, so GVAR always read atom 0's value.

**Fix**: Implemented proper PC-relative read from instruction stream using `*current-code*` special variable bound by dispatch.

```lisp
(defun read-pc-32-be (vm)
  "Read 32-bit big-endian value from instruction stream at PC+1."
  (declare (type vm vm)
           (special *current-code*))
  (let* ((pc (vm-pc vm))
         (code *current-code*))
    (if (and code (< (+ pc 4) (length code)))
        (let ((b0 (aref code (+ pc 1)))
              (b1 (aref code (+ pc 2)))
              (b2 (aref code (+ pc 3)))
              (b3 (aref code (+ pc 4))))
          (logior (ash b0 24) (ash b1 16) (ash b2 8) b3))
        0)))
```

### Issue 2: Build Configuration Mismatch - **FIXED**

**Location**: `laiko/src/data/atom.lisp`

**Problem**: `*bigatoms*` was set to `nil`, but C emulator uses BIGVM=1, BIGATOMS=1.

**Fix**: Set `*bigatoms*` to `t`:

```lisp
(defparameter *bigatoms* t
  "Whether using BIGATOMS mode (32-bit atom indices)")
```

### Issue 3: Atom Index Masking - **FIXED**

**Location**: `laiko/src/vm/op-variable.lisp`

**Problem**: Atom index was masked to 16 bits with `(logand atom-idx #xFFFF)`.

**Fix**: Pass full 32-bit atom index to `read-atom-value`:

```lisp
(defop gvar #x60 5
  "GVAR: Push value of global variable (atom value cell)."
  (let ((atom-idx (read-pc-32-be vm)))
    (let ((value (maiko-lisp.data:read-atom-value vm atom-idx)))
      (vm-push vm value))))
```

---

## Additional Changes

### Dispatch Loop Updates

Added special variables for instruction stream access in `laiko/src/vm/dispatch.lisp`:

```lisp
(defvar *current-code* nil
  "Bound to the current bytecode array during dispatch.")

(defvar *current-base-pc* 0
  "Bound to the base PC offset during dispatch.")
```

These are bound in the `dispatch` function to allow handlers to read operands from the instruction stream.

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

## Build Status

The GVAR fixes compile successfully. There is a pre-existing build error in `laiko/src/vm/op-stack.lisp` (unrelated to GVAR fixes) that needs to be addressed separately:

```
Error while parsing arguments to DEFOP DEFMACRO:
  unknown keyword: "COPY: Duplicate the top of stack."
```

---

## Documentation Created

1. **`documentation/specifications/gvar-introspection-timing.typ`**
   - Documents the TOS recording convention
   - Explains why event N shows TOS after instruction N-1

---

## Next Steps

1. Fix pre-existing error in `op-stack.lisp`
2. Test GVAR with atom 522 to verify it reads 0x140000
3. Run parity comparison with C emulator

---

## References

- C GVAR macro: `maiko/inc/inlineC.h` lines 419-480
- C dispatch loop: `maiko/src/xc.c` lines 920-1055
- Introspection database: `maiko/test_introspect.db`
- Introspection status: `reports/introspection-status-2026-02-19.md`
